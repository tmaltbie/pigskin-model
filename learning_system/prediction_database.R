# NFL Prediction Database System
# Production-ready SQLite database for tracking predictions vs actual results
# Enables continuous learning and performance monitoring

library(DBI)
library(RSQLite)
library(dplyr)
library(jsonlite)
library(lubridate)

# Global database connection (will be initialized)
.prediction_db_conn <- NULL

#' Initialize the prediction tracking database
#' 
#' Creates SQLite database with complete schema for tracking NFL predictions,
#' actual results, betting performance, and systematic error patterns.
#' 
#' @param db_path Path to SQLite database file (default: "predictions.db")
#' @param backup_existing Whether to backup existing database (default: TRUE)
#' @return Database connection object
#' 
#' @examples
#' db <- initialize_prediction_db("nfl_predictions.db")
initialize_prediction_db <- function(db_path = "learning_system/predictions.db", backup_existing = TRUE) {
  
  cat("🗄️  Initializing NFL prediction database...\n")
  
  # Backup existing database if it exists
  if (file.exists(db_path) && backup_existing) {
    backup_path <- paste0(db_path, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(db_path, backup_path)
    cat("📋 Existing database backed up to:", backup_path, "\n")
  }
  
  # Connect to database (creates file if doesn't exist)
  conn <- dbConnect(SQLite(), db_path)
  
  # Enable WAL mode for better concurrent access
  dbExecute(conn, "PRAGMA journal_mode = WAL")
  dbExecute(conn, "PRAGMA synchronous = NORMAL")
  dbExecute(conn, "PRAGMA foreign_keys = ON")
  
  # Create predictions table - core prediction tracking
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS predictions (
      prediction_id INTEGER PRIMARY KEY AUTOINCREMENT,
      game_id TEXT NOT NULL,
      prediction_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      model_version TEXT NOT NULL,
      
      -- Game information
      home_team TEXT NOT NULL,
      away_team TEXT NOT NULL,
      season INTEGER NOT NULL,
      week INTEGER NOT NULL,
      game_date DATE,
      
      -- Predictions (model outputs)
      predicted_home_score REAL CHECK (predicted_home_score >= 0),
      predicted_away_score REAL CHECK (predicted_away_score >= 0),
      predicted_margin REAL,      -- positive = home favored
      predicted_total REAL CHECK (predicted_total > 0),
      predicted_home_win_prob REAL CHECK (predicted_home_win_prob BETWEEN 0 AND 1),
      confidence_score REAL CHECK (confidence_score BETWEEN 0 AND 1),
      
      -- Market data at prediction time
      opening_spread REAL,
      closing_spread REAL,
      opening_total REAL,
      closing_total REAL,
      home_ml_odds REAL,
      away_ml_odds REAL,
      
      -- Actual results (filled after game)
      actual_home_score INTEGER,
      actual_away_score INTEGER,
      actual_margin REAL,
      actual_total INTEGER,
      
      -- Performance metrics (calculated after game)
      spread_error REAL,          -- |predicted_margin - actual_margin|
      total_error REAL,           -- |predicted_total - actual_total|
      beat_closing_spread BOOLEAN,
      correct_winner BOOLEAN,
      confidence_calibration REAL, -- |confidence - (1 if correct else 0)|
      
      -- Betting recommendations
      recommended_bet_type TEXT,   -- 'home_spread', 'away_spread', 'over', 'under', 'home_ml', 'away_ml'
      recommended_bet_size REAL,   -- in units (0.5, 1.0, 1.5)
      bet_odds REAL,              -- odds when bet was placed
      expected_value REAL,        -- calculated EV of recommended bet
      
      -- Feature tracking (for learning)
      features_json TEXT,         -- JSON of all input features used
      
      -- Processing status
      result_processed BOOLEAN DEFAULT FALSE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      
      UNIQUE(game_id, model_version)
    )
  ")
  
  # Create model performance table - aggregated metrics by time period
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS model_performance (
      performance_id INTEGER PRIMARY KEY AUTOINCREMENT,
      model_version TEXT NOT NULL,
      period_start DATE NOT NULL,
      period_end DATE NOT NULL,
      period_type TEXT NOT NULL,    -- 'weekly', 'monthly', 'season'
      
      -- Prediction accuracy metrics
      total_predictions INTEGER DEFAULT 0,
      avg_spread_error REAL,
      avg_total_error REAL,
      spread_rmse REAL,
      total_rmse REAL,
      directional_accuracy REAL,   -- % correct winners
      confidence_calibration REAL,
      
      -- Betting performance
      total_bets INTEGER DEFAULT 0,
      winning_bets INTEGER DEFAULT 0,
      ats_record TEXT,             -- '15-8-1' format
      ats_percentage REAL,
      total_units_bet REAL,
      total_units_won REAL,
      roi REAL,                    -- return on investment
      avg_bet_size REAL,
      max_bet_size REAL,
      
      -- Risk metrics
      win_streak INTEGER DEFAULT 0,
      loss_streak INTEGER DEFAULT 0,
      max_drawdown REAL,
      sharpe_ratio REAL,
      
      -- Market beating metrics
      beat_closing_line_pct REAL,
      avg_closing_line_value REAL,
      
      -- High confidence subset (confidence >= 0.7)
      hc_total_bets INTEGER DEFAULT 0,
      hc_ats_percentage REAL,
      hc_roi REAL,
      
      -- Feature importance (JSON)
      top_features_json TEXT,
      
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      
      UNIQUE(model_version, period_start, period_end, period_type)
    )
  ")
  
  # Create error patterns table - systematic mistake tracking
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS error_patterns (
      error_id INTEGER PRIMARY KEY AUTOINCREMENT,
      model_version TEXT NOT NULL,
      pattern_type TEXT NOT NULL,   -- 'systematic_bias', 'feature_drift', 'team_specific', etc.
      pattern_name TEXT NOT NULL,
      
      -- Pattern description
      description TEXT,
      conditions TEXT,              -- conditions when this error occurs
      
      -- Impact metrics
      frequency INTEGER DEFAULT 0,
      avg_error_magnitude REAL,
      total_games_affected INTEGER DEFAULT 0,
      
      -- Pattern detection
      first_detected DATE,
      last_detected DATE,
      detection_confidence REAL,
      
      -- Correction tracking
      corrective_action TEXT,
      correction_applied BOOLEAN DEFAULT FALSE,
      correction_date DATE,
      post_correction_performance REAL,
      
      -- Priority and status
      priority INTEGER DEFAULT 1,   -- 1=low, 2=medium, 3=high, 4=critical
      status TEXT DEFAULT 'active', -- 'active', 'monitoring', 'resolved'
      
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create betting results table - detailed bet tracking
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS betting_results (
      bet_id INTEGER PRIMARY KEY AUTOINCREMENT,
      prediction_id INTEGER NOT NULL,
      game_id TEXT NOT NULL,
      
      -- Bet details
      bet_type TEXT NOT NULL,      -- 'spread', 'total', 'moneyline'
      bet_side TEXT NOT NULL,      -- 'home', 'away', 'over', 'under'
      bet_amount REAL NOT NULL,    -- in units
      bet_odds REAL NOT NULL,
      potential_payout REAL NOT NULL,
      
      -- Market context
      line_at_bet REAL,
      closing_line REAL,
      line_movement REAL,          -- closing_line - line_at_bet
      
      -- Risk management
      bankroll_pct REAL,          -- % of bankroll this bet represents
      kelly_criterion REAL,       -- optimal Kelly bet size
      bet_vs_kelly REAL,          -- actual bet size vs Kelly recommendation
      
      -- Results (filled after game)
      bet_result TEXT,            -- 'win', 'loss', 'push'
      actual_payout REAL,
      net_result REAL,            -- payout - bet_amount
      
      -- Performance tracking
      running_roi REAL,           -- ROI including this bet
      running_units REAL,         -- total units after this bet
      
      bet_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      result_date TIMESTAMP,
      
      FOREIGN KEY (prediction_id) REFERENCES predictions(prediction_id)
    )
  ")
  
  # Create indexes for performance
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_predictions_game_model ON predictions(game_id, model_version)")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_predictions_date ON predictions(prediction_date)")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_predictions_season_week ON predictions(season, week)")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_predictions_team_season ON predictions(season, week, home_team, away_team)")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_predictions_model_processed ON predictions(model_version, result_processed)")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_performance_period ON model_performance(period_start, period_end)")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_betting_results_game ON betting_results(game_id)")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_error_patterns_type ON error_patterns(pattern_type, status)")
  
  # Store connection globally
  .prediction_db_conn <<- conn
  
  cat("✅ Database initialized successfully\n")
  cat("📊 Tables created: predictions, model_performance, error_patterns, betting_results\n")
  
  return(conn)
}

#' Insert a new prediction record
#' 
#' Stores a complete prediction with all metadata for later performance tracking
#' 
#' @param game_id Unique game identifier
#' @param home_team Home team abbreviation
#' @param away_team Away team abbreviation  
#' @param predictions List with predicted_margin, predicted_total, home_win_prob, confidence
#' @param market_lines List with opening/closing spreads and totals, ML odds
#' @param betting_rec List with bet_type, bet_size, expected_value
#' @param features Named list or vector of all input features used
#' @param model_version String identifying the model version
#' @param game_date Date of the game
#' @param season Season year
#' @param week Week number
#' 
#' @return prediction_id of inserted record
insert_prediction <- function(game_id, home_team, away_team, predictions, 
                             market_lines = NULL, betting_rec = NULL, 
                             features = NULL, model_version = "v1.0",
                             game_date = NULL, season = NULL, week = NULL) {
  
  # Validate required inputs
  if (is.null(.prediction_db_conn)) {
    stop("Database not initialized. Call initialize_prediction_db() first.")
  }
  
  if (missing(game_id) || missing(home_team) || missing(away_team) || missing(predictions)) {
    stop("Required parameters: game_id, home_team, away_team, predictions")
  }
  
  # Validate predictions structure
  required_pred_fields <- c("predicted_margin", "predicted_total", "home_win_prob", "confidence")
  if (!all(required_pred_fields %in% names(predictions))) {
    stop("Predictions must include: ", paste(required_pred_fields, collapse = ", "))
  }
  
  # Validate prediction values
  if (predictions$home_win_prob < 0 || predictions$home_win_prob > 1) {
    stop("home_win_prob must be between 0 and 1, got: ", predictions$home_win_prob)
  }
  if (predictions$confidence < 0 || predictions$confidence > 1) {
    stop("confidence must be between 0 and 1, got: ", predictions$confidence)
  }
  if (predictions$predicted_total <= 0) {
    stop("predicted_total must be positive, got: ", predictions$predicted_total)
  }
  
  # Validate team names (basic check)
  if (nchar(home_team) == 0 || nchar(away_team) == 0) {
    stop("Team names cannot be empty")
  }
  if (home_team == away_team) {
    stop("Home team and away team cannot be the same: ", home_team)
  }
  
  tryCatch({
    
    # Convert features to JSON if provided
    features_json <- if (!is.null(features)) toJSON(features, auto_unbox = TRUE) else NULL
    
    # Prepare market lines
    ml <- if (!is.null(market_lines)) market_lines else list()
    
    # Prepare betting recommendation
    bet <- if (!is.null(betting_rec)) betting_rec else list()
    
    # Calculate predicted scores from margin and total
    pred_total <- predictions$predicted_total
    pred_margin <- predictions$predicted_margin
    pred_home_score <- (pred_total + pred_margin) / 2
    pred_away_score <- (pred_total - pred_margin) / 2
    
    # Insert prediction record
    result <- dbExecute(.prediction_db_conn, "
      INSERT INTO predictions (
        game_id, model_version, home_team, away_team, season, week, game_date,
        predicted_home_score, predicted_away_score, predicted_margin, predicted_total,
        predicted_home_win_prob, confidence_score,
        opening_spread, closing_spread, opening_total, closing_total,
        home_ml_odds, away_ml_odds,
        recommended_bet_type, recommended_bet_size, expected_value,
        features_json
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      game_id, model_version, home_team, away_team, season, week, game_date,
      pred_home_score, pred_away_score, pred_margin, pred_total,
      predictions$home_win_prob, predictions$confidence,
      ml$opening_spread, ml$closing_spread, ml$opening_total, ml$closing_total,
      ml$home_ml_odds, ml$away_ml_odds,
      bet$bet_type, bet$bet_size, bet$expected_value,
      features_json
    ))
    
    # Get the inserted prediction_id
    prediction_id <- dbGetQuery(.prediction_db_conn, "SELECT last_insert_rowid() as id")$id
    
    cat(sprintf("✅ Prediction stored: %s vs %s (ID: %d)\n", away_team, home_team, prediction_id))
    
    return(prediction_id)
    
  }, error = function(e) {
    cat("❌ Error inserting prediction:", e$message, "\n")
    return(NULL)
  })
}

#' Update prediction with actual game results
#' 
#' Updates a prediction record with actual game outcomes and calculates performance metrics
#' 
#' @param game_id Game identifier to update
#' @param actual_home_score Actual home team score
#' @param actual_away_score Actual away team score
#' @param model_version Model version to update (default: latest)
#' 
#' @return TRUE if successful, FALSE otherwise
update_prediction_results <- function(game_id, actual_home_score, actual_away_score, model_version = NULL) {
  
  if (is.null(.prediction_db_conn)) {
    stop("Database not initialized. Call initialize_prediction_db() first.")
  }
  
  tryCatch({
    
    # Find the prediction to update
    where_clause <- "game_id = ?"
    params <- list(game_id)
    
    if (!is.null(model_version)) {
      where_clause <- paste(where_clause, "AND model_version = ?")
      params <- append(params, model_version)
    }
    
    prediction <- dbGetQuery(.prediction_db_conn, 
      paste("SELECT * FROM predictions WHERE", where_clause, "LIMIT 1"), 
      params = params)
    
    if (nrow(prediction) == 0) {
      cat("⚠️  No prediction found for game:", game_id, "\n")
      return(FALSE)
    }
    
    # Calculate actual results
    actual_margin <- actual_home_score - actual_away_score
    actual_total <- actual_home_score + actual_away_score
    
    # Calculate performance metrics
    spread_error <- abs(prediction$predicted_margin - actual_margin)
    total_error <- abs(prediction$predicted_total - actual_total)
    
    # Check if beat closing spread (if available)
    beat_closing_spread <- NULL
    if (!is.na(prediction$closing_spread)) {
      closing_error <- abs(prediction$closing_spread - actual_margin)
      beat_closing_spread <- spread_error < closing_error
    }
    
    # Check if predicted winner correctly
    predicted_home_wins <- prediction$predicted_margin > 0
    actual_home_wins <- actual_margin > 0
    correct_winner <- predicted_home_wins == actual_home_wins
    
    # Calculate confidence calibration
    confidence_calibration <- abs(prediction$confidence_score - ifelse(correct_winner, 1, 0))
    
    # Update the prediction record
    dbExecute(.prediction_db_conn, "
      UPDATE predictions 
      SET actual_home_score = ?, actual_away_score = ?, actual_margin = ?, actual_total = ?,
          spread_error = ?, total_error = ?, beat_closing_spread = ?, correct_winner = ?,
          confidence_calibration = ?, result_processed = TRUE, updated_at = CURRENT_TIMESTAMP
      WHERE prediction_id = ?
    ", params = list(
      actual_home_score, actual_away_score, actual_margin, actual_total,
      spread_error, total_error, beat_closing_spread, correct_winner,
      confidence_calibration, prediction$prediction_id
    ))
    
    cat(sprintf("✅ Results updated: %s %d - %d %s (Spread Error: %.1f)\n", 
                prediction$home_team, actual_home_score, actual_away_score, 
                prediction$away_team, spread_error))
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error updating results:", e$message, "\n")
    return(FALSE)
  })
}

#' Calculate model performance metrics for a time period
#' 
#' Aggregates prediction performance and betting results for analysis
#' 
#' @param start_date Start of period (Date)
#' @param end_date End of period (Date) 
#' @param model_version Model version to analyze (default: all)
#' @param period_type Type of period ('weekly', 'monthly', 'season')
#' 
#' @return Data frame with performance metrics
calculate_model_performance <- function(start_date, end_date, model_version = NULL, period_type = "weekly") {
  
  if (is.null(.prediction_db_conn)) {
    stop("Database not initialized. Call initialize_prediction_db() first.")
  }
  
  # Build WHERE clause
  where_conditions <- c("result_processed = TRUE", 
                       "prediction_date >= ?", 
                       "prediction_date <= ?")
  params <- list(start_date, end_date)
  
  if (!is.null(model_version)) {
    where_conditions <- c(where_conditions, "model_version = ?")
    params <- append(params, model_version)
  }
  
  where_clause <- paste(where_conditions, collapse = " AND ")
  
  # Get performance metrics
  performance <- dbGetQuery(.prediction_db_conn, paste("
    SELECT 
      COUNT(*) as total_predictions,
      AVG(spread_error) as avg_spread_error,
      AVG(total_error) as avg_total_error,
      SQRT(AVG(spread_error * spread_error)) as spread_rmse,
      SQRT(AVG(total_error * total_error)) as total_rmse,
      AVG(CASE WHEN correct_winner THEN 1.0 ELSE 0.0 END) as directional_accuracy,
      AVG(confidence_calibration) as confidence_calibration,
      AVG(CASE WHEN beat_closing_spread THEN 1.0 ELSE 0.0 END) as beat_closing_line_pct,
      COUNT(CASE WHEN recommended_bet_type IS NOT NULL THEN 1 END) as total_bets
    FROM predictions 
    WHERE", where_clause
  ), params = params)
  
  # Add metadata
  performance$model_version <- model_version %||% "all"
  performance$period_start <- start_date
  performance$period_end <- end_date
  performance$period_type <- period_type
  performance$calculated_at <- Sys.time()
  
  return(performance)
}

#' Get database statistics and health metrics
#' 
#' @return List with database statistics
get_database_stats <- function() {
  
  if (is.null(.prediction_db_conn)) {
    stop("Database not initialized. Call initialize_prediction_db() first.")
  }
  
  stats <- list()
  
  # Basic counts
  stats$total_predictions <- dbGetQuery(.prediction_db_conn, "SELECT COUNT(*) as count FROM predictions")$count
  stats$processed_predictions <- dbGetQuery(.prediction_db_conn, "SELECT COUNT(*) as count FROM predictions WHERE result_processed = TRUE")$count
  stats$total_bets <- dbGetQuery(.prediction_db_conn, "SELECT COUNT(*) as count FROM betting_results")$count
  stats$error_patterns <- dbGetQuery(.prediction_db_conn, "SELECT COUNT(*) as count FROM error_patterns")$count
  
  # Date ranges
  date_range <- dbGetQuery(.prediction_db_conn, "SELECT MIN(prediction_date) as earliest, MAX(prediction_date) as latest FROM predictions")
  stats$date_range <- date_range
  
  # Model versions
  stats$model_versions <- dbGetQuery(.prediction_db_conn, "SELECT DISTINCT model_version, COUNT(*) as predictions FROM predictions GROUP BY model_version")
  
  # Recent performance (last 30 days)
  recent_perf <- dbGetQuery(.prediction_db_conn, "
    SELECT 
      AVG(spread_error) as avg_spread_error,
      AVG(CASE WHEN correct_winner THEN 1.0 ELSE 0.0 END) as directional_accuracy
    FROM predictions 
    WHERE result_processed = TRUE AND prediction_date >= date('now', '-30 days')
  ")
  stats$recent_performance <- recent_perf
  
  # Database file info
  db_info <- file.info(.prediction_db_conn@dbname)
  stats$db_size_mb <- round(db_info$size / 1024 / 1024, 2)
  
  return(stats)
}

#' Backup database to timestamped file
#' 
#' @param backup_dir Directory for backups (default: "backups/")
#' @return Path to backup file
backup_database <- function(backup_dir = "learning_system/backups/") {
  
  if (is.null(.prediction_db_conn)) {
    stop("Database not initialized.")
  }
  
  # Create backup directory if it doesn't exist
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }
  
  # Generate backup filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_file <- file.path(backup_dir, paste0("predictions_backup_", timestamp, ".db"))
  
  tryCatch({
    # Copy database file
    file.copy(.prediction_db_conn@dbname, backup_file)
    
    cat("✅ Database backed up to:", backup_file, "\n")
    return(backup_file)
    
  }, error = function(e) {
    cat("❌ Backup failed:", e$message, "\n")
    return(NULL)
  })
}

# Close database connection when done
close_prediction_db <- function() {
  if (!is.null(.prediction_db_conn)) {
    dbDisconnect(.prediction_db_conn)
    .prediction_db_conn <<- NULL
    cat("📫 Database connection closed\n")
  }
}

# Helper function for null coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

cat("NFL Prediction Database System loaded! 🗄️\n\n")
cat("Available functions:\n")
cat("- initialize_prediction_db(): Create database with complete schema\n")
cat("- insert_prediction(): Store new predictions with metadata\n")
cat("- update_prediction_results(): Add actual game results\n")
cat("- calculate_model_performance(): Analyze performance metrics\n")
cat("- get_database_stats(): Database statistics and health\n")
cat("- backup_database(): Create timestamped backup\n")
cat("\n🚀 Quick start: db <- initialize_prediction_db()\n")