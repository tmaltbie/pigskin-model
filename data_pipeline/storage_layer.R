# NFL Storage Layer - SQLite Database for Reliable Data Persistence
# Replaces CSV-based storage with proper database operations,
# transactions, and data integrity constraints

library(RSQLite)
library(DBI)
library(dplyr)
library(lubridate)

# Load validation layer for data integrity
source("data_pipeline/validation_layer.R")

# Database configuration
DB_CONFIG <- list(
  db_path = "data_pipeline/nfl_data.sqlite",
  backup_path = "data_pipeline/backups",
  enable_foreign_keys = TRUE,
  enable_wal_mode = TRUE,
  auto_vacuum = "FULL"
)

# Global database connection pool
.db_env <- new.env()
.db_env$connection <- NULL
.db_env$transaction_count <- 0

#' Initialize Database Connection
#' 
#' Creates and configures the SQLite database with proper settings
#' for performance and data integrity
#' 
#' @param db_path Path to SQLite database file
#' @param reset_db Reset existing database (destructive operation)
#' @return Database connection object
initialize_database <- function(db_path = DB_CONFIG$db_path, reset_db = FALSE) {
  
  cat("🗄️  Initializing NFL data storage layer...\n")
  
  # Ensure directory exists
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
    cat(sprintf("📁 Created database directory: %s\n", db_dir))
  }
  
  # Create backup directory
  if (!dir.exists(DB_CONFIG$backup_path)) {
    dir.create(DB_CONFIG$backup_path, recursive = TRUE)
  }
  
  # Remove existing database if reset requested
  if (reset_db && file.exists(db_path)) {
    file.remove(db_path)
    cat("🗑️  Existing database removed\n")
  }
  
  # Create database connection
  con <- dbConnect(SQLite(), db_path)
  
  # Configure SQLite for performance and reliability
  if (DB_CONFIG$enable_foreign_keys) {
    dbExecute(con, "PRAGMA foreign_keys = ON")
  }
  
  if (DB_CONFIG$enable_wal_mode) {
    dbExecute(con, "PRAGMA journal_mode = WAL")
  }
  
  dbExecute(con, sprintf("PRAGMA auto_vacuum = %s", DB_CONFIG$auto_vacuum))
  dbExecute(con, "PRAGMA synchronous = NORMAL")
  dbExecute(con, "PRAGMA cache_size = -64000")  # 64MB cache
  dbExecute(con, "PRAGMA temp_store = MEMORY")
  
  # Store connection globally
  .db_env$connection <- con
  
  cat(sprintf("✅ Database initialized: %s\n", db_path))
  return(con)
}

#' Create Database Schema
#' 
#' Creates all necessary tables with proper constraints and indexes
#' for the NFL data pipeline
#' 
#' @param con Database connection (optional, uses global if not provided)
create_database_schema <- function(con = NULL) {
  
  if (is.null(con)) {
    con <- get_db_connection()
  }
  
  cat("🏗️  Creating database schema...\n")
  
  # Start transaction for schema creation
  dbBegin(con)
  
  tryCatch({
    
    # Official NFL Schedule table (source of truth)
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS official_schedule (
        game_id TEXT PRIMARY KEY,
        season INTEGER NOT NULL,
        week INTEGER NOT NULL,
        gameday DATE NOT NULL,
        away_team TEXT NOT NULL,
        home_team TEXT NOT NULL,
        away_score INTEGER,
        home_score INTEGER,
        total_score INTEGER GENERATED ALWAYS AS (away_score + home_score) STORED,
        home_margin INTEGER GENERATED ALWAYS AS (home_score - away_score) STORED,
        home_win BOOLEAN GENERATED ALWAYS AS (home_score > away_score) STORED,
        game_completed BOOLEAN GENERATED ALWAYS AS (away_score IS NOT NULL AND home_score IS NOT NULL) STORED,
        data_source TEXT DEFAULT 'nflreadr',
        load_timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
        UNIQUE(season, week, away_team, home_team),
        CHECK(away_team != home_team),
        CHECK(week BETWEEN 1 AND 22),
        CHECK(season >= 1999),
        CHECK(away_score >= 0 OR away_score IS NULL),
        CHECK(home_score >= 0 OR home_score IS NULL)
      )
    ")
    
    # Predictions table (replaces CSV tracking)
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS predictions (
        prediction_id TEXT PRIMARY KEY,
        game_id TEXT NOT NULL,
        prediction_date DATETIME NOT NULL,
        model_version TEXT NOT NULL,
        season INTEGER NOT NULL,
        week INTEGER NOT NULL,
        game_date DATE,
        away_team TEXT NOT NULL,
        home_team TEXT NOT NULL,
        predicted_margin REAL NOT NULL,
        predicted_total REAL NOT NULL,  
        home_win_prob REAL NOT NULL,
        confidence REAL NOT NULL,
        prediction_metadata TEXT,  -- JSON for additional model outputs
        data_validated BOOLEAN DEFAULT FALSE,
        validation_errors TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (game_id) REFERENCES official_schedule(game_id),
        CHECK(home_win_prob BETWEEN 0 AND 1),
        CHECK(confidence BETWEEN 0 AND 1),
        CHECK(predicted_total > 0),
        CHECK(away_team != home_team)
      )
    ")
    
    # Prediction results table (for tracking accuracy)
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS prediction_results (
        result_id TEXT PRIMARY KEY,
        prediction_id TEXT NOT NULL,
        game_id TEXT NOT NULL,
        actual_home_score INTEGER,
        actual_away_score INTEGER,
        actual_margin INTEGER GENERATED ALWAYS AS (actual_home_score - actual_away_score) STORED,
        actual_total INTEGER GENERATED ALWAYS AS (actual_home_score + actual_away_score) STORED,
        spread_error REAL GENERATED ALWAYS AS (predicted_margin - (actual_home_score - actual_away_score)) STORED,
        total_error REAL GENERATED ALWAYS AS (predicted_total - (actual_home_score + actual_away_score)) STORED,
        correct_direction BOOLEAN GENERATED ALWAYS AS (
          (predicted_margin > 0 AND actual_home_score > actual_away_score) OR
          (predicted_margin < 0 AND actual_home_score < actual_away_score) OR
          (predicted_margin = 0 AND actual_home_score = actual_away_score)
        ) STORED,
        confidence_calibration REAL,
        result_processed BOOLEAN DEFAULT TRUE,
        processed_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (prediction_id) REFERENCES predictions(prediction_id),
        FOREIGN KEY (game_id) REFERENCES official_schedule(game_id),
        CHECK(actual_home_score >= 0 OR actual_home_score IS NULL),
        CHECK(actual_away_score >= 0 OR actual_away_score IS NULL)
      )
    ")
    
    # Data validation log table
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS validation_log (
        log_id TEXT PRIMARY KEY,
        validation_type TEXT NOT NULL,
        validation_date DATETIME DEFAULT CURRENT_TIMESTAMP,
        season INTEGER,
        week INTEGER,
        total_records INTEGER,
        valid_records INTEGER,
        invalid_records INTEGER,
        warnings_count INTEGER,
        errors_count INTEGER,
        validation_details TEXT,  -- JSON with detailed results
        status TEXT NOT NULL CHECK(status IN ('success', 'warning', 'error'))
      )
    ")
    
    # Create indexes for performance
    cat("📊 Creating database indexes...\n")
    
    # Official schedule indexes
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_schedule_season_week ON official_schedule(season, week)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_schedule_teams ON official_schedule(away_team, home_team)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_schedule_gameday ON official_schedule(gameday)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_schedule_completed ON official_schedule(game_completed)")
    
    # Predictions indexes
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_predictions_game ON predictions(game_id)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_predictions_season_week ON predictions(season, week)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_predictions_model ON predictions(model_version)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_predictions_date ON predictions(prediction_date)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_predictions_validated ON predictions(data_validated)")
    
    # Results indexes
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_results_prediction ON prediction_results(prediction_id)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_results_game ON prediction_results(game_id)")
    
    # Validation log indexes
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_validation_date ON validation_log(validation_date)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_validation_status ON validation_log(status)")
    
    # Create views for common queries
    cat("👁️  Creating database views...\n")
    
    # View for predictions with results
    dbExecute(con, "
      CREATE VIEW IF NOT EXISTS predictions_with_results AS
      SELECT 
        p.*,
        r.actual_home_score,
        r.actual_away_score,
        r.actual_margin,
        r.actual_total,
        r.spread_error,
        r.total_error,
        r.correct_direction,
        r.confidence_calibration,
        r.result_processed,
        s.game_completed,
        s.gameday
      FROM predictions p
      LEFT JOIN prediction_results r ON p.prediction_id = r.prediction_id
      LEFT JOIN official_schedule s ON p.game_id = s.game_id
    ")
    
    # View for model performance summary
    dbExecute(con, "
      CREATE VIEW IF NOT EXISTS model_performance AS
      SELECT 
        model_version,
        season,
        COUNT(*) as total_predictions,
        SUM(CASE WHEN result_processed THEN 1 ELSE 0 END) as completed_predictions,
        AVG(CASE WHEN result_processed THEN ABS(spread_error) END) as avg_spread_error,
        AVG(CASE WHEN result_processed THEN ABS(total_error) END) as avg_total_error,
        AVG(CASE WHEN result_processed THEN CAST(correct_direction AS REAL) END) as direction_accuracy,
        AVG(confidence) as avg_confidence
      FROM predictions_with_results
      GROUP BY model_version, season
    ")
    
    dbCommit(con)
    cat("✅ Database schema created successfully\n")
    
  }, error = function(e) {
    dbRollback(con)
    stop(sprintf("Failed to create database schema: %s", e$message))
  })
}

#' Get Database Connection
#' 
#' Returns the current database connection, initializing if necessary
#' 
#' @return Database connection object
get_db_connection <- function() {
  if (is.null(.db_env$connection) || !dbIsValid(.db_env$connection)) {
    .db_env$connection <- initialize_database()
    create_database_schema(.db_env$connection)
  }
  return(.db_env$connection)
}

#' Store Official Schedule
#' 
#' Stores official schedule data in the database with validation
#' 
#' @param schedule_data Data frame with official schedule
#' @param season Season being stored
#' @param validate_data Run validation before storing
#' @return Number of records stored
store_official_schedule <- function(schedule_data, season = 2025, validate_data = TRUE) {
  
  con <- get_db_connection()
  
  cat(sprintf("💾 Storing official schedule for season %d (%d games)...\n", 
             season, nrow(schedule_data)))
  
  if (validate_data) {
    validation_result <- validate_game_schedule(schedule_data, season)
    if (!validation_result$valid) {
      stop("Schedule data validation failed. Cannot store invalid data.")
    }
  }
  
  # Prepare data for database
  db_schedule <- schedule_data %>%
    mutate(
      gameday = as.character(as.Date(gameday)),
      load_timestamp = as.character(Sys.time()),
      updated_timestamp = as.character(Sys.time())
    ) %>%
    select(game_id, season, week, gameday, away_team, home_team,
           away_score, home_score, data_source, load_timestamp, updated_timestamp)
  
  # Use upsert to handle duplicates
  dbBegin(con)
  
  tryCatch({
    
    # Delete existing records for this season to avoid conflicts
    dbExecute(con, "DELETE FROM official_schedule WHERE season = ?", params = list(season))
    
    # Insert new records
    dbWriteTable(con, "official_schedule", db_schedule, append = TRUE, row.names = FALSE)
    
    dbCommit(con)
    
    cat(sprintf("✅ Stored %d official schedule records for season %d\n", 
               nrow(db_schedule), season))
    
    return(nrow(db_schedule))
    
  }, error = function(e) {
    dbRollback(con)
    stop(sprintf("Failed to store official schedule: %s", e$message))
  })
}

#' Store Predictions
#' 
#' Stores prediction data with validation against official schedule
#' 
#' @param predictions_data Data frame with predictions
#' @param validate_against_schedule Validate against official schedule
#' @return Number of records stored
store_predictions <- function(predictions_data, validate_against_schedule = TRUE) {
  
  con <- get_db_connection()
  
  cat(sprintf("🔮 Storing %d predictions...\n", nrow(predictions_data)))
  
  # Validate predictions
  if (validate_against_schedule) {
    validation_result <- validate_game_schedule(predictions_data, 
                                                predictions_data$season[1])
    
    # Mark validation status
    predictions_data$data_validated <- validation_result$valid
    if (!validation_result$valid) {
      predictions_data$validation_errors <- paste(validation_result$errors, collapse = "; ")
      cat("⚠️  Some predictions failed validation but will be stored with error flags\n")
    }
  }
  
  # Prepare data for database
  db_predictions <- predictions_data %>%
    mutate(
      created_at = as.character(Sys.time()),
      updated_at = as.character(Sys.time()),
      prediction_date = as.character(prediction_date),
      game_date = as.character(game_date)
    )
  
  dbBegin(con)
  
  tryCatch({
    
    dbWriteTable(con, "predictions", db_predictions, append = TRUE, row.names = FALSE)
    
    dbCommit(con)
    
    cat(sprintf("✅ Stored %d prediction records\n", nrow(db_predictions)))
    
    return(nrow(db_predictions))
    
  }, error = function(e) {
    dbRollback(con)
    stop(sprintf("Failed to store predictions: %s", e$message))
  })
}

#' Update Prediction Results
#' 
#' Updates predictions with actual game results
#' 
#' @param game_results Data frame with actual game results
#' @return Number of predictions updated
update_prediction_results <- function(game_results) {
  
  con <- get_db_connection()
  
  cat(sprintf("📊 Updating prediction results for %d games...\n", nrow(game_results)))
  
  updated_count <- 0
  
  dbBegin(con)
  
  tryCatch({
    
    for (i in 1:nrow(game_results)) {
      game <- game_results[i, ]
      
      # Get predictions for this game
      predictions <- dbGetQuery(con, "
        SELECT prediction_id, predicted_margin, predicted_total 
        FROM predictions 
        WHERE game_id = ?", 
        params = list(game$game_id))
      
      if (nrow(predictions) > 0) {
        # Update official schedule with results
        dbExecute(con, "
          UPDATE official_schedule 
          SET away_score = ?, home_score = ?, updated_timestamp = ?
          WHERE game_id = ?",
          params = list(game$away_score, game$home_score, 
                       as.character(Sys.time()), game$game_id))
        
        # Insert/update prediction results
        for (j in 1:nrow(predictions)) {
          pred <- predictions[j, ]
          
          result_id <- paste("result", pred$prediction_id, sep = "_")
          
          # Check if result already exists
          existing <- dbGetQuery(con, "
            SELECT result_id FROM prediction_results WHERE prediction_id = ?",
            params = list(pred$prediction_id))
          
          if (nrow(existing) == 0) {
            # Insert new result
            dbExecute(con, "
              INSERT INTO prediction_results 
              (result_id, prediction_id, game_id, actual_home_score, actual_away_score)
              VALUES (?, ?, ?, ?, ?)",
              params = list(result_id, pred$prediction_id, game$game_id,
                           game$home_score, game$away_score))
            updated_count <- updated_count + 1
          }
        }
      }
    }
    
    dbCommit(con)
    
    cat(sprintf("✅ Updated results for %d predictions\n", updated_count))
    
    return(updated_count)
    
  }, error = function(e) {
    dbRollback(con)
    stop(sprintf("Failed to update prediction results: %s", e$message))
  })
}

#' Get Predictions with Results
#' 
#' Retrieves predictions along with their results for analysis
#' 
#' @param season Season to retrieve (optional)
#' @param week Week to retrieve (optional)
#' @param model_version Model version to filter by (optional)
#' @return Data frame with predictions and results
get_predictions_with_results <- function(season = NULL, week = NULL, model_version = NULL) {
  
  con <- get_db_connection()
  
  query <- "SELECT * FROM predictions_with_results WHERE 1=1"
  params <- list()
  
  if (!is.null(season)) {
    query <- paste(query, "AND season = ?")
    params <- append(params, season)
  }
  
  if (!is.null(week)) {
    query <- paste(query, "AND week = ?")
    params <- append(params, week)
  }
  
  if (!is.null(model_version)) {
    query <- paste(query, "AND model_version = ?")
    params <- append(params, model_version)
  }
  
  query <- paste(query, "ORDER BY prediction_date DESC")
  
  result <- dbGetQuery(con, query, params = params)
  
  cat(sprintf("📈 Retrieved %d predictions with results\n", nrow(result)))
  
  return(result)
}

#' Migrate CSV Data to Database
#' 
#' Migrates existing CSV data to the new database structure
#' 
#' @param csv_path Path to CSV file to migrate
#' @param backup_csv Create backup of CSV file
#' @return Migration summary
migrate_csv_to_database <- function(csv_path = "learning_system/predictions_tracking.csv", 
                                   backup_csv = TRUE) {
  
  if (!file.exists(csv_path)) {
    cat(sprintf("⚠️  CSV file not found: %s\n", csv_path))
    return(list(success = FALSE, message = "File not found"))
  }
  
  cat(sprintf("🔄 Migrating CSV data to database: %s\n", csv_path))
  
  # Backup CSV if requested
  if (backup_csv) {
    backup_path <- sprintf("%s/predictions_backup_%s.csv", 
                          DB_CONFIG$backup_path, 
                          format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(csv_path, backup_path)
    cat(sprintf("💾 CSV backed up to: %s\n", backup_path))
  }
  
  # Read and clean CSV data
  csv_data <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  cat(sprintf("📋 Found %d records in CSV\n", nrow(csv_data)))
  
  # Clean and validate CSV data
  if (nrow(csv_data) > 0) {
    
    # Check for contaminated Week 3 data
    week3_data <- csv_data %>% filter(week == 3, season == 2025)
    
    if (nrow(week3_data) > 0) {
      cat("🧹 Cleaning Week 3 contaminated data...\n")
      cleaning_result <- clean_contaminated_data(week3_data, 2025)
      
      # Replace contaminated data with cleaned data
      csv_data <- csv_data %>%
        filter(!(week == 3 & season == 2025)) %>%
        bind_rows(cleaning_result$cleaned_data)
      
      cat(sprintf("✅ Cleaned data: %d contaminated records processed\n", 
                 cleaning_result$report$corrected_count))
    }
    
    # Store cleaned data in database
    store_predictions(csv_data, validate_against_schedule = TRUE)
    
    return(list(
      success = TRUE,
      migrated_records = nrow(csv_data),
      cleaning_report = if (exists("cleaning_result")) cleaning_result$report else NULL
    ))
  }
  
  return(list(success = FALSE, message = "No data to migrate"))
}

#' Create Database Backup
#' 
#' Creates a backup of the current database
#' 
#' @return Backup file path
create_database_backup <- function() {
  
  backup_filename <- sprintf("nfl_data_backup_%s.sqlite", 
                            format(Sys.time(), "%Y%m%d_%H%M%S"))
  backup_path <- file.path(DB_CONFIG$backup_path, backup_filename)
  
  file.copy(DB_CONFIG$db_path, backup_path)
  
  cat(sprintf("💾 Database backed up to: %s\n", backup_path))
  
  return(backup_path)
}

#' Close Database Connection
#' 
#' Properly closes the database connection
close_database <- function() {
  if (!is.null(.db_env$connection) && dbIsValid(.db_env$connection)) {
    dbDisconnect(.db_env$connection)
    .db_env$connection <- NULL
    cat("🔐 Database connection closed\n")
  }
}

cat("🗄️  Storage Layer loaded successfully\n")
cat("Use initialize_database() to set up the database\n")