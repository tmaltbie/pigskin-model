# NFL Data Pipeline - Master Initialization
# Production-ready pipeline replacing unreliable CSV system

cat("🏗️ INITIALIZING NFL DATA PIPELINE (PRODUCTION-READY)\n")
cat("====================================================\n")

# Create directory structure
pipeline_dirs <- c(
  "data_pipeline",
  "data_pipeline/logs", 
  "data_pipeline/cache",
  "data_pipeline/backups"
)

for (dir in pipeline_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(sprintf("✅ Created directory: %s\n", dir))
  }
}

# Core pipeline functions
initialize_nfl_pipeline <- function() {
  
  cat("\n🔧 PIPELINE INITIALIZATION\n")
  cat("========================\n")
  
  # 1. Setup database
  setup_result <- setup_nfl_database()
  if (!setup_result$success) {
    stop("Failed to setup database: ", setup_result$error)
  }
  
  # 2. Validate data sources
  validation_result <- validate_data_sources()
  if (!validation_result$success) {
    cat("⚠️ Warning: ", validation_result$error, "\n")
  }
  
  # 3. Initialize validation layer
  init_validation_layer()
  
  # 4. Setup error handling
  init_error_handling()
  
  cat("✅ Pipeline initialization complete!\n")
  return(TRUE)
}

# Database setup
setup_nfl_database <- function() {
  tryCatch({
    library(DBI)
    library(RSQLite)
    
    # Create database connection
    db_path <- "data_pipeline/nfl_predictions.db"
    con <- dbConnect(SQLite(), db_path)
    
    # Create core tables
    create_tables_sql <- "
    -- Games table (source of truth for NFL schedule)
    CREATE TABLE IF NOT EXISTS games (
      game_id TEXT PRIMARY KEY,
      season INTEGER NOT NULL,
      week INTEGER NOT NULL,
      game_date TEXT NOT NULL,
      away_team TEXT NOT NULL,
      home_team TEXT NOT NULL,
      away_score INTEGER,
      home_score INTEGER,
      completed BOOLEAN DEFAULT FALSE,
      data_source TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
    
    -- Predictions table (replaces CSV system)
    CREATE TABLE IF NOT EXISTS predictions (
      prediction_id TEXT PRIMARY KEY,
      game_id TEXT NOT NULL,
      model_version TEXT NOT NULL,
      predicted_margin REAL NOT NULL,
      predicted_total REAL NOT NULL,
      confidence REAL NOT NULL,
      prediction_date TIMESTAMP NOT NULL,
      result_processed BOOLEAN DEFAULT FALSE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (game_id) REFERENCES games(game_id)
    );
    
    -- Data validation log
    CREATE TABLE IF NOT EXISTS validation_log (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      validation_type TEXT NOT NULL,
      status TEXT NOT NULL,
      details TEXT,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
    
    -- Create indexes for performance
    CREATE INDEX IF NOT EXISTS idx_games_season_week ON games(season, week);
    CREATE INDEX IF NOT EXISTS idx_predictions_game ON predictions(game_id);
    CREATE INDEX IF NOT EXISTS idx_validation_timestamp ON validation_log(timestamp);
    "
    
    dbExecute(con, create_tables_sql)
    dbDisconnect(con)
    
    cat("✅ Database setup complete: ", db_path, "\n")
    return(list(success = TRUE, db_path = db_path))
    
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# Data source validation
validate_data_sources <- function() {
  tryCatch({
    # Check nflfastR availability
    nflfastR_available <- requireNamespace("nflfastR", quietly = TRUE)
    nflreadr_available <- requireNamespace("nflreadr", quietly = TRUE)
    
    if (!nflfastR_available && !nflreadr_available) {
      return(list(success = FALSE, error = "No NFL data sources available"))
    }
    
    cat("✅ Data sources validated\n")
    return(list(success = TRUE))
    
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# Validation layer initialization
init_validation_layer <- function() {
  cat("✅ Validation layer initialized\n")
  
  # Log validation initialization
  log_validation("system_init", "success", "Validation layer started")
}

# Error handling initialization  
init_error_handling <- function() {
  # Setup global error handler
  options(error = function() {
    cat("❌ Pipeline error occurred. Check logs for details.\n")
  })
  
  cat("✅ Error handling initialized\n")
}

# Validation logging
log_validation <- function(type, status, details = NULL) {
  tryCatch({
    library(DBI)
    library(RSQLite)
    
    con <- dbConnect(SQLite(), "data_pipeline/nfl_predictions.db")
    
    dbExecute(con, 
      "INSERT INTO validation_log (validation_type, status, details) VALUES (?, ?, ?)",
      params = list(type, status, details))
    
    dbDisconnect(con)
  }, error = function(e) {
    cat("Warning: Could not log validation:", e$message, "\n")
  })
}

# Quick contamination check for Week 3
quick_week3_contamination_check <- function() {
  tryCatch({
    library(nflreadr)
    
    # Get actual Week 3 schedule
    actual_schedule <- load_schedules(2025)
    actual_week3 <- actual_schedule[actual_schedule$week == 3, c("away_team", "home_team")]
    
    # Check for known contaminated matchups
    contaminated_games <- c("ATL@KC", "DAL@BAL", "MIA@SEA")
    
    for (i in 1:nrow(actual_week3)) {
      game <- actual_week3[i,]
      matchup <- paste0(game$away_team, "@", game$home_team)
      
      if (matchup %in% contaminated_games) {
        cat("❌ Contamination detected:", matchup, "\n")
        return(FALSE)
      }
    }
    
    cat("✅ No contamination detected in Week 3 schedule\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("⚠️ Could not check contamination:", e$message, "\n")
    return(FALSE)
  })
}

cat("🚀 NFL Pipeline ready for initialization!\n")
cat("Usage: initialize_nfl_pipeline()\n")