# NFL Prediction Database System
# Production-ready database for tracking predictions vs outcomes and enabling continuous learning
# Author: NFL ML System
# Version: 1.0

library(RSQLite)
library(DBI)
library(dplyr)
library(data.table)
library(lubridate)
library(jsonlite)

# ==============================================================================
# DATABASE SCHEMA CONSTANTS
# ==============================================================================

DB_VERSION <- "1.0.0"
DEFAULT_DB_PATH <- "nfl_predictions.db"

# ==============================================================================
# DATABASE INITIALIZATION
# ==============================================================================

#' Initialize NFL Predictions Database
#' 
#' Creates a new SQLite database with all required tables and indexes.
#' Safe to run multiple times - will not overwrite existing data.
#'
#' @param db_path Path to SQLite database file
#' @param force_recreate If TRUE, drops and recreates all tables (DANGER!)
#' @return Database connection object
#' @export
initialize_prediction_db <- function(db_path = DEFAULT_DB_PATH, force_recreate = FALSE) {
  
  tryCatch({
    
    # Connect to database
    con <- dbConnect(SQLite(), db_path)
    
    # Enable foreign keys and WAL mode for better concurrency
    dbExecute(con, "PRAGMA foreign_keys = ON")
    dbExecute(con, "PRAGMA journal_mode = WAL")
    dbExecute(con, "PRAGMA synchronous = NORMAL")
    
    if (force_recreate) {
      cat("WARNING: Force recreating database. All existing data will be lost!\n")
      drop_all_tables(con)
    }
    
    # Create tables if they don't exist
    create_predictions_table(con)
    create_model_performance_table(con)
    create_error_patterns_table(con)
    create_betting_results_table(con)
    create_metadata_table(con)
    
    # Create indexes for performance
    create_database_indexes(con)
    
    # Initialize metadata
    initialize_metadata(con)
    
    cat("Database initialized successfully at:", db_path, "\n")
    return(con)
    
  }, error = function(e) {
    cat("ERROR initializing database:", e$message, "\n")
    if (exists("con") && !is.null(con)) {
      dbDisconnect(con)
    }
    stop(e)
  })
}

#' Create predictions table
create_predictions_table <- function(con) {
  
  sql <- "
  CREATE TABLE IF NOT EXISTS predictions (
    prediction_id TEXT PRIMARY KEY,
    game_id TEXT NOT NULL,
    season INTEGER NOT NULL,
    week INTEGER NOT NULL,
    game_date TEXT NOT NULL,
    home_team TEXT NOT NULL,
    away_team TEXT NOT NULL,
    
    -- Prediction data
    prediction_timestamp TEXT NOT NULL,
    model_version TEXT NOT NULL,
    feature_set_hash TEXT NOT NULL,
    
    -- Spread predictions
    predicted_spread REAL,
    vegas_spread REAL,
    spread_confidence REAL,
    
    -- Total predictions
    predicted_total REAL,
    vegas_total REAL,
    total_confidence REAL,
    
    -- Moneyline predictions
    predicted_home_prob REAL,
    vegas_home_odds REAL,
    vegas_away_odds REAL,
    moneyline_confidence REAL,
    
    -- Betting recommendations
    spread_recommendation TEXT,
    total_recommendation TEXT,
    moneyline_recommendation TEXT,
    recommended_bet_size REAL,
    
    -- Model features (JSON)
    feature_values TEXT,
    
    -- Actual results (updated post-game)
    actual_home_score INTEGER,
    actual_away_score INTEGER,
    actual_spread REAL,
    actual_total REAL,
    game_completed INTEGER DEFAULT 0,
    result_updated_timestamp TEXT,
    
    -- Performance metrics (calculated post-game)
    spread_error REAL,
    total_error REAL,
    spread_correct INTEGER,
    total_correct INTEGER,
    moneyline_correct INTEGER,
    
    -- Betting results
    spread_bet_result REAL,
    total_bet_result REAL,
    moneyline_bet_result REAL,
    
    -- Metadata
    created_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
    updated_timestamp TEXT DEFAULT CURRENT_TIMESTAMP
  )"
  
  dbExecute(con, sql)
}

#' Create model performance table
create_model_performance_table <- function(con) {
  
  sql <- "
  CREATE TABLE IF NOT EXISTS model_performance (
    performance_id TEXT PRIMARY KEY,
    model_version TEXT NOT NULL,
    evaluation_period TEXT NOT NULL, -- 'weekly', 'monthly', 'season'
    season INTEGER NOT NULL,
    week INTEGER, -- NULL for monthly/season aggregates
    
    -- Prediction volume
    total_predictions INTEGER NOT NULL,
    completed_games INTEGER NOT NULL,
    
    -- Spread performance
    spread_predictions INTEGER,
    spread_correct INTEGER,
    spread_accuracy REAL,
    spread_mae REAL, -- Mean Absolute Error
    spread_rmse REAL, -- Root Mean Square Error
    spread_calibration_score REAL,
    
    -- Total performance
    total_predictions_count INTEGER,
    total_correct INTEGER,
    total_accuracy REAL,
    total_mae REAL,
    total_rmse REAL,
    
    -- Moneyline performance
    moneyline_predictions_count INTEGER,
    moneyline_correct INTEGER,
    moneyline_accuracy REAL,
    moneyline_log_loss REAL,
    moneyline_brier_score REAL,
    
    -- Betting performance
    total_bets INTEGER,
    winning_bets INTEGER,
    bet_win_rate REAL,
    total_wagered REAL,
    total_profit REAL,
    roi REAL,
    sharpe_ratio REAL,
    max_drawdown REAL,
    
    -- Confidence calibration
    high_confidence_bets INTEGER,
    high_confidence_wins INTEGER,
    high_confidence_roi REAL,
    
    -- Feature importance (top 5 features as JSON)
    top_features TEXT,
    
    -- Metadata
    calculation_timestamp TEXT DEFAULT CURRENT_TIMESTAMP
  )"
  
  dbExecute(con, sql)
}

#' Create error patterns table
create_error_patterns_table <- function(con) {
  
  sql <- "
  CREATE TABLE IF NOT EXISTS error_patterns (
    pattern_id TEXT PRIMARY KEY,
    model_version TEXT NOT NULL,
    season INTEGER NOT NULL,
    
    -- Pattern identification
    pattern_type TEXT NOT NULL, -- 'systematic_bias', 'feature_drift', 'team_specific', 'situational'
    pattern_category TEXT NOT NULL, -- 'spread', 'total', 'moneyline'
    pattern_description TEXT NOT NULL,
    
    -- Pattern metrics
    occurrence_count INTEGER NOT NULL,
    error_magnitude REAL NOT NULL,
    confidence_score REAL NOT NULL,
    
    -- Affected conditions
    affected_teams TEXT, -- JSON array of team codes
    affected_weeks TEXT, -- JSON array of week numbers
    affected_situations TEXT, -- JSON object describing conditions
    
    -- Impact assessment
    total_predictions_affected INTEGER,
    average_error_increase REAL,
    roi_impact REAL,
    
    -- Suggested corrections
    suggested_feature_adjustments TEXT, -- JSON object
    suggested_model_changes TEXT,
    priority_level TEXT NOT NULL, -- 'LOW', 'MEDIUM', 'HIGH', 'CRITICAL'
    
    -- Status tracking
    status TEXT DEFAULT 'IDENTIFIED', -- 'IDENTIFIED', 'INVESTIGATING', 'RESOLVED', 'MONITORING'
    resolution_notes TEXT,
    
    -- Metadata
    identified_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
    last_updated TEXT DEFAULT CURRENT_TIMESTAMP
  )"
  
  dbExecute(con, sql)
}

#' Create betting results table
create_betting_results_table <- function(con) {
  
  sql <- "
  CREATE TABLE IF NOT EXISTS betting_results (
    bet_id TEXT PRIMARY KEY,
    prediction_id TEXT NOT NULL,
    
    -- Bet details
    bet_type TEXT NOT NULL, -- 'spread', 'total', 'moneyline'
    bet_side TEXT NOT NULL, -- 'home', 'away', 'over', 'under'
    bet_amount REAL NOT NULL,
    bet_odds REAL NOT NULL,
    recommended_by_model INTEGER DEFAULT 1,
    
    -- Risk management
    bankroll_percentage REAL,
    kelly_criterion_size REAL,
    confidence_threshold_met INTEGER,
    
    -- Results
    bet_result TEXT, -- 'WIN', 'LOSS', 'PUSH', 'VOID'
    payout REAL,
    profit_loss REAL,
    
    -- Performance tracking
    running_bankroll REAL,
    running_roi REAL,
    bet_sequence_number INTEGER,
    
    -- Market context
    closing_line_value REAL, -- How much odds moved from bet placement to close
    market_efficiency_score REAL,
    
    -- Metadata
    bet_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
    settled_timestamp TEXT,
    
    FOREIGN KEY (prediction_id) REFERENCES predictions(prediction_id)
  )"
  
  dbExecute(con, sql)
}

#' Create metadata table
create_metadata_table <- function(con) {
  
  sql <- "
  CREATE TABLE IF NOT EXISTS database_metadata (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    updated_timestamp TEXT DEFAULT CURRENT_TIMESTAMP
  )"
  
  dbExecute(con, sql)
}

#' Create database indexes for performance
create_database_indexes <- function(con) {
  
  indexes <- c(
    "CREATE INDEX IF NOT EXISTS idx_predictions_game ON predictions(game_id)",
    "CREATE INDEX IF NOT EXISTS idx_predictions_season_week ON predictions(season, week)",
    "CREATE INDEX IF NOT EXISTS idx_predictions_teams ON predictions(home_team, away_team)",
    "CREATE INDEX IF NOT EXISTS idx_predictions_model ON predictions(model_version)",
    "CREATE INDEX IF NOT EXISTS idx_predictions_completed ON predictions(game_completed)",
    "CREATE INDEX IF NOT EXISTS idx_performance_model ON model_performance(model_version)",
    "CREATE INDEX IF NOT EXISTS idx_performance_period ON model_performance(evaluation_period, season, week)",
    "CREATE INDEX IF NOT EXISTS idx_error_patterns_type ON error_patterns(pattern_type, pattern_category)",
    "CREATE INDEX IF NOT EXISTS idx_betting_prediction ON betting_results(prediction_id)",
    "CREATE INDEX IF NOT EXISTS idx_betting_type ON betting_results(bet_type)"
  )
  
  for (idx in indexes) {
    dbExecute(con, idx)
  }
}

#' Initialize database metadata
initialize_metadata <- function(con) {
  
  # Check if metadata already exists
  existing <- dbGetQuery(con, "SELECT COUNT(*) as count FROM database_metadata WHERE key = 'version'")
  
  if (existing$count == 0) {
    metadata <- list(
      c("version", DB_VERSION),
      c("created_date", Sys.time()),
      c("last_backup", ""),
      c("total_predictions", "0"),
      c("total_completed_games", "0"),
      c("current_model_version", "1.0.0")
    )
    
    for (item in metadata) {
      dbExecute(con, 
                "INSERT INTO database_metadata (key, value) VALUES (?, ?)",
                params = list(item[1], item[2]))
    }
  }
}

#' Drop all tables (DANGER - only for force recreate)
drop_all_tables <- function(con) {
  tables <- c("betting_results", "error_patterns", "model_performance", 
              "predictions", "database_metadata")
  
  for (table in tables) {
    dbExecute(con, paste("DROP TABLE IF EXISTS", table))
  }
}

# ==============================================================================
# PREDICTION INSERTION FUNCTIONS
# ==============================================================================

#' Insert new prediction record
#' 
#' @param con Database connection
#' @param game_data List containing game information
#' @param predictions List containing model predictions
#' @param model_info List containing model metadata
#' @return Prediction ID
#' @export
insert_prediction <- function(con, game_data, predictions, model_info) {
  
  # Input validation
  validate_game_data(game_data)
  validate_predictions(predictions)
  validate_model_info(model_info)
  
  tryCatch({
    
    # Generate unique prediction ID
    prediction_id <- generate_prediction_id(game_data, model_info)
    
    # Prepare data
    insert_data <- list(
      prediction_id = prediction_id,
      game_id = game_data$game_id,
      season = as.integer(game_data$season),
      week = as.integer(game_data$week),
      game_date = game_data$game_date,
      home_team = game_data$home_team,
      away_team = game_data$away_team,
      
      prediction_timestamp = Sys.time(),
      model_version = model_info$version,
      feature_set_hash = model_info$feature_hash,
      
      predicted_spread = predictions$spread$predicted,
      vegas_spread = predictions$spread$vegas,
      spread_confidence = predictions$spread$confidence,
      
      predicted_total = predictions$total$predicted,
      vegas_total = predictions$total$vegas,
      total_confidence = predictions$total$confidence,
      
      predicted_home_prob = predictions$moneyline$home_prob,
      vegas_home_odds = predictions$moneyline$home_odds,
      vegas_away_odds = predictions$moneyline$away_odds,
      moneyline_confidence = predictions$moneyline$confidence,
      
      spread_recommendation = predictions$recommendations$spread,
      total_recommendation = predictions$recommendations$total,
      moneyline_recommendation = predictions$recommendations$moneyline,
      recommended_bet_size = predictions$recommendations$bet_size,
      
      feature_values = toJSON(model_info$features, auto_unbox = TRUE)
    )
    
    # Insert into database
    sql <- build_insert_sql("predictions", names(insert_data))
    dbExecute(con, sql, params = insert_data)
    
    # Update metadata
    update_prediction_count(con)
    
    cat("Inserted prediction:", prediction_id, "\n")
    return(prediction_id)
    
  }, error = function(e) {
    cat("ERROR inserting prediction:", e$message, "\n")
    stop(e)
  })
}

#' Update prediction with actual game results
#' 
#' @param con Database connection  
#' @param prediction_id Unique prediction identifier
#' @param actual_results List containing game outcomes
#' @export
update_prediction_results <- function(con, prediction_id, actual_results) {
  
  validate_actual_results(actual_results)
  
  tryCatch({
    
    # Calculate derived metrics
    actual_spread <- actual_results$home_score - actual_results$away_score
    actual_total <- actual_results$home_score + actual_results$away_score
    
    # Get original prediction for error calculation
    original <- dbGetQuery(con, 
                          "SELECT * FROM predictions WHERE prediction_id = ?",
                          params = list(prediction_id))
    
    if (nrow(original) == 0) {
      stop("Prediction ID not found: ", prediction_id)
    }
    
    # Calculate errors and correctness
    spread_error <- actual_spread - original$predicted_spread
    total_error <- actual_total - original$predicted_total
    
    spread_correct <- ifelse(
      is.na(original$vegas_spread), NA,
      ifelse(sign(actual_spread + original$vegas_spread) == sign(original$predicted_spread + original$vegas_spread), 1, 0)
    )
    
    total_correct <- ifelse(
      is.na(original$vegas_total), NA,
      ifelse((actual_total > original$vegas_total) == (original$predicted_total > original$vegas_total), 1, 0)
    )
    
    # Determine moneyline correctness
    home_won <- actual_results$home_score > actual_results$away_score
    moneyline_correct <- ifelse(
      is.na(original$predicted_home_prob), NA,
      ifelse((original$predicted_home_prob > 0.5) == home_won, 1, 0)
    )
    
    # Calculate betting results (simplified - assumes standard odds)
    betting_results <- calculate_betting_results(original, actual_results)
    
    # Update record
    update_data <- list(
      actual_home_score = actual_results$home_score,
      actual_away_score = actual_results$away_score,
      actual_spread = actual_spread,
      actual_total = actual_total,
      game_completed = 1,
      result_updated_timestamp = Sys.time(),
      
      spread_error = spread_error,
      total_error = total_error,
      spread_correct = spread_correct,
      total_correct = total_correct,
      moneyline_correct = moneyline_correct,
      
      spread_bet_result = betting_results$spread,
      total_bet_result = betting_results$total,
      moneyline_bet_result = betting_results$moneyline,
      
      updated_timestamp = Sys.time(),
      
      prediction_id = prediction_id
    )
    
    # Build and execute update
    set_clause <- paste(names(update_data)[-length(update_data)], "= ?", collapse = ", ")
    sql <- paste("UPDATE predictions SET", set_clause, "WHERE prediction_id = ?")
    
    dbExecute(con, sql, params = update_data)
    
    # Update metadata
    update_completed_games_count(con)
    
    cat("Updated prediction results:", prediction_id, "\n")
    
  }, error = function(e) {
    cat("ERROR updating prediction results:", e$message, "\n")
    stop(e)
  })
}

# ==============================================================================
# PERFORMANCE ANALYSIS FUNCTIONS  
# ==============================================================================

#' Calculate and store model performance metrics
#' 
#' @param con Database connection
#' @param model_version Model version to analyze
#' @param period Analysis period ('weekly', 'monthly', 'season')
#' @param season Season to analyze
#' @param week Week to analyze (for weekly periods)
#' @export
calculate_model_performance <- function(con, model_version, period = "weekly", 
                                      season = NULL, week = NULL) {
  
  tryCatch({
    
    # Build query based on period
    base_query <- "SELECT * FROM predictions WHERE model_version = ? AND game_completed = 1"
    params <- list(model_version)
    
    if (!is.null(season)) {
      base_query <- paste(base_query, "AND season = ?")
      params <- append(params, season)
    }
    
    if (period == "weekly" && !is.null(week)) {
      base_query <- paste(base_query, "AND week = ?")
      params <- append(params, week)
    } else if (period == "monthly") {
      # Add month filtering logic here
    }
    
    # Get completed predictions
    predictions <- dbGetQuery(con, base_query, params = params)
    
    if (nrow(predictions) == 0) {
      cat("No completed predictions found for analysis\n")
      return(NULL)
    }
    
    # Calculate performance metrics
    performance <- calculate_performance_metrics(predictions)
    
    # Generate performance ID
    performance_id <- generate_performance_id(model_version, period, season, week)
    
    # Prepare performance record
    perf_data <- list(
      performance_id = performance_id,
      model_version = model_version,
      evaluation_period = period,
      season = ifelse(is.null(season), as.integer(predictions$season[1]), season),
      week = week,
      
      total_predictions = nrow(predictions),
      completed_games = nrow(predictions),
      
      # Add all calculated metrics
      spread_predictions = performance$spread$count,
      spread_correct = performance$spread$correct,
      spread_accuracy = performance$spread$accuracy,
      spread_mae = performance$spread$mae,
      spread_rmse = performance$spread$rmse,
      spread_calibration_score = performance$spread$calibration,
      
      total_predictions_count = performance$total$count,
      total_correct = performance$total$correct,
      total_accuracy = performance$total$accuracy,
      total_mae = performance$total$mae,
      total_rmse = performance$total$rmse,
      
      moneyline_predictions_count = performance$moneyline$count,
      moneyline_correct = performance$moneyline$correct,
      moneyline_accuracy = performance$moneyline$accuracy,
      moneyline_log_loss = performance$moneyline$log_loss,
      moneyline_brier_score = performance$moneyline$brier_score,
      
      total_bets = performance$betting$total_bets,
      winning_bets = performance$betting$winning_bets,
      bet_win_rate = performance$betting$win_rate,
      total_wagered = performance$betting$total_wagered,
      total_profit = performance$betting$total_profit,
      roi = performance$betting$roi,
      sharpe_ratio = performance$betting$sharpe_ratio,
      max_drawdown = performance$betting$max_drawdown,
      
      high_confidence_bets = performance$betting$high_conf_bets,
      high_confidence_wins = performance$betting$high_conf_wins,
      high_confidence_roi = performance$betting$high_conf_roi,
      
      top_features = toJSON(performance$feature_importance, auto_unbox = TRUE)
    )
    
    # Insert or update performance record
    sql <- build_insert_sql("model_performance", names(perf_data))
    sql <- paste(sql, "ON CONFLICT(performance_id) DO UPDATE SET")
    
    # Add all update assignments
    updates <- paste(names(perf_data), "= excluded." %+% names(perf_data), collapse = ", ")
    sql <- paste(sql, updates)
    
    dbExecute(con, sql, params = perf_data)
    
    cat("Calculated performance metrics:", performance_id, "\n")
    return(performance)
    
  }, error = function(e) {
    cat("ERROR calculating performance:", e$message, "\n")
    stop(e)
  })
}

#' Get performance trends over time
#' 
#' @param con Database connection
#' @param model_version Model version to analyze
#' @param metric Specific metric to trend ('accuracy', 'roi', 'mae', etc.)
#' @param period Period granularity ('weekly', 'monthly')
#' @return Data frame with trend data
#' @export
get_performance_trends <- function(con, model_version, metric = "spread_accuracy", 
                                 period = "weekly") {
  
  sql <- "
  SELECT 
    season, 
    week,
    evaluation_period,
    %s as metric_value,
    calculation_timestamp
  FROM model_performance 
  WHERE model_version = ? AND evaluation_period = ?
  ORDER BY season, week"
  
  sql <- sprintf(sql, metric)
  
  trends <- dbGetQuery(con, sql, params = list(model_version, period))
  
  if (nrow(trends) > 0) {
    trends$date <- as.Date(trends$calculation_timestamp)
    trends$period_label <- ifelse(period == "weekly", 
                                 paste("Week", trends$week), 
                                 format(trends$date, "%Y-%m"))
  }
  
  return(trends)
}

# ==============================================================================
# ERROR PATTERN ANALYSIS
# ==============================================================================

#' Identify systematic error patterns in predictions
#' 
#' @param con Database connection
#' @param model_version Model version to analyze
#' @param min_occurrences Minimum pattern occurrences to flag
#' @export
identify_error_patterns <- function(con, model_version, min_occurrences = 5) {
  
  tryCatch({
    
    # Get all completed predictions for analysis
    predictions <- dbGetQuery(con, 
                             "SELECT * FROM predictions WHERE model_version = ? AND game_completed = 1",
                             params = list(model_version))
    
    if (nrow(predictions) < min_occurrences * 2) {
      cat("Insufficient data for pattern analysis\n")
      return(NULL)
    }
    
    patterns <- list()
    
    # 1. Team-specific bias patterns
    team_patterns <- identify_team_bias_patterns(predictions, min_occurrences)
    patterns <- append(patterns, team_patterns)
    
    # 2. Situational bias patterns  
    situational_patterns <- identify_situational_patterns(predictions, min_occurrences)
    patterns <- append(patterns, situational_patterns)
    
    # 3. Systematic spread bias
    spread_patterns <- identify_spread_bias_patterns(predictions, min_occurrences)
    patterns <- append(patterns, spread_patterns)
    
    # 4. Feature drift patterns
    drift_patterns <- identify_feature_drift_patterns(predictions, min_occurrences)
    patterns <- append(patterns, drift_patterns)
    
    # Store identified patterns
    for (pattern in patterns) {
      store_error_pattern(con, pattern, model_version)
    }
    
    cat("Identified", length(patterns), "error patterns\n")
    return(patterns)
    
  }, error = function(e) {
    cat("ERROR in pattern identification:", e$message, "\n")
    stop(e)
  })
}

# ==============================================================================
# UTILITY AND HELPER FUNCTIONS
# ==============================================================================

#' Validate game data input
validate_game_data <- function(game_data) {
  required_fields <- c("game_id", "season", "week", "game_date", "home_team", "away_team")
  
  for (field in required_fields) {
    if (is.null(game_data[[field]]) || is.na(game_data[[field]])) {
      stop("Missing required game data field: ", field)
    }
  }
  
  if (!is.numeric(game_data$season) || game_data$season < 2000 || game_data$season > 2030) {
    stop("Invalid season: ", game_data$season)
  }
  
  if (!is.numeric(game_data$week) || game_data$week < 1 || game_data$week > 22) {
    stop("Invalid week: ", game_data$week)
  }
}

#' Validate predictions input
validate_predictions <- function(predictions) {
  required_sections <- c("spread", "total", "moneyline", "recommendations")
  
  for (section in required_sections) {
    if (is.null(predictions[[section]])) {
      stop("Missing predictions section: ", section)
    }
  }
  
  # Validate spread predictions
  if (is.null(predictions$spread$predicted) || is.null(predictions$spread$confidence)) {
    stop("Invalid spread predictions")
  }
  
  # Validate confidence scores are between 0 and 1
  confidences <- c(predictions$spread$confidence, predictions$total$confidence, 
                  predictions$moneyline$confidence)
  
  for (conf in confidences) {
    if (!is.null(conf) && (conf < 0 || conf > 1)) {
      stop("Confidence scores must be between 0 and 1")
    }
  }
}

#' Generate unique prediction ID
generate_prediction_id <- function(game_data, model_info) {
  base_string <- paste(game_data$game_id, model_info$version, 
                      as.character(Sys.time()), sep = "_")
  return(digest::digest(base_string, algo = "md5"))
}

#' Build parameterized INSERT SQL
build_insert_sql <- function(table_name, column_names) {
  placeholders <- paste(rep("?", length(column_names)), collapse = ", ")
  columns <- paste(column_names, collapse = ", ")
  
  return(paste("INSERT INTO", table_name, "(", columns, ") VALUES (", placeholders, ")"))
}

#' Calculate comprehensive performance metrics
calculate_performance_metrics <- function(predictions) {
  
  # Initialize results structure
  performance <- list()
  
  # Spread performance
  spread_data <- predictions[!is.na(predictions$spread_error), ]
  performance$spread <- list(
    count = nrow(spread_data),
    correct = sum(spread_data$spread_correct, na.rm = TRUE),
    accuracy = mean(spread_data$spread_correct, na.rm = TRUE),
    mae = mean(abs(spread_data$spread_error), na.rm = TRUE),
    rmse = sqrt(mean(spread_data$spread_error^2, na.rm = TRUE)),
    calibration = calculate_calibration_score(spread_data$spread_confidence, 
                                            spread_data$spread_correct)
  )
  
  # Total performance
  total_data <- predictions[!is.na(predictions$total_error), ]
  performance$total <- list(
    count = nrow(total_data),
    correct = sum(total_data$total_correct, na.rm = TRUE),
    accuracy = mean(total_data$total_correct, na.rm = TRUE),
    mae = mean(abs(total_data$total_error), na.rm = TRUE),
    rmse = sqrt(mean(total_data$total_error^2, na.rm = TRUE))
  )
  
  # Moneyline performance
  ml_data <- predictions[!is.na(predictions$moneyline_correct), ]
  if (nrow(ml_data) > 0) {
    performance$moneyline <- list(
      count = nrow(ml_data),
      correct = sum(ml_data$moneyline_correct, na.rm = TRUE),
      accuracy = mean(ml_data$moneyline_correct, na.rm = TRUE),
      log_loss = calculate_log_loss(ml_data$predicted_home_prob, ml_data$moneyline_correct),
      brier_score = calculate_brier_score(ml_data$predicted_home_prob, ml_data$moneyline_correct)
    )
  }
  
  # Betting performance (simplified)
  betting_data <- predictions[!is.na(predictions$spread_bet_result), ]
  if (nrow(betting_data) > 0) {
    total_results <- rowSums(cbind(
      betting_data$spread_bet_result %||% 0,
      betting_data$total_bet_result %||% 0,
      betting_data$moneyline_bet_result %||% 0
    ), na.rm = TRUE)
    
    performance$betting <- list(
      total_bets = nrow(betting_data),
      winning_bets = sum(total_results > 0, na.rm = TRUE),
      win_rate = mean(total_results > 0, na.rm = TRUE),
      total_wagered = nrow(betting_data), # Simplified assumption
      total_profit = sum(total_results, na.rm = TRUE),
      roi = sum(total_results, na.rm = TRUE) / nrow(betting_data),
      sharpe_ratio = calculate_sharpe_ratio(total_results),
      max_drawdown = calculate_max_drawdown(total_results),
      high_conf_bets = sum(betting_data$spread_confidence > 0.7, na.rm = TRUE),
      high_conf_wins = sum(betting_data$spread_confidence > 0.7 & total_results > 0, na.rm = TRUE),
      high_conf_roi = mean(total_results[betting_data$spread_confidence > 0.7], na.rm = TRUE)
    )
  }
  
  # Feature importance (placeholder)
  performance$feature_importance <- list(
    feature1 = 0.25,
    feature2 = 0.20,
    feature3 = 0.15,
    feature4 = 0.10,
    feature5 = 0.08
  )
  
  return(performance)
}

#' Helper function for null coalescing
`%||%` <- function(x, y) if(is.null(x) || is.na(x)) y else x

#' Calculate calibration score
calculate_calibration_score <- function(confidences, outcomes) {
  if (length(confidences) == 0 || all(is.na(confidences))) return(NA)
  
  # Bin predictions and calculate calibration
  bins <- seq(0, 1, by = 0.1)
  bin_indices <- cut(confidences, bins, include.lowest = TRUE)
  
  calibration_error <- 0
  total_samples <- 0
  
  for (bin in levels(bin_indices)) {
    mask <- bin_indices == bin & !is.na(outcomes)
    if (sum(mask) > 0) {
      bin_confidence <- mean(confidences[mask], na.rm = TRUE)
      bin_accuracy <- mean(outcomes[mask], na.rm = TRUE)
      bin_size <- sum(mask)
      
      calibration_error <- calibration_error + bin_size * abs(bin_confidence - bin_accuracy)
      total_samples <- total_samples + bin_size
    }
  }
  
  return(ifelse(total_samples > 0, calibration_error / total_samples, NA))
}

#' Calculate log loss for probability predictions
calculate_log_loss <- function(predicted_probs, actual_outcomes) {
  if (length(predicted_probs) == 0 || all(is.na(predicted_probs))) return(NA)
  
  # Clip probabilities to avoid log(0)
  predicted_probs <- pmax(pmin(predicted_probs, 0.999), 0.001)
  
  log_loss <- -mean(actual_outcomes * log(predicted_probs) + 
                   (1 - actual_outcomes) * log(1 - predicted_probs), na.rm = TRUE)
  
  return(log_loss)
}

#' Calculate Brier score
calculate_brier_score <- function(predicted_probs, actual_outcomes) {
  if (length(predicted_probs) == 0) return(NA)
  return(mean((predicted_probs - actual_outcomes)^2, na.rm = TRUE))
}

#' Calculate Sharpe ratio
calculate_sharpe_ratio <- function(returns) {
  if (length(returns) < 2 || all(is.na(returns))) return(NA)
  return(mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE))
}

#' Calculate maximum drawdown
calculate_max_drawdown <- function(returns) {
  if (length(returns) == 0) return(NA)
  
  cumulative <- cumsum(returns)
  running_max <- cummax(cumulative)
  drawdown <- running_max - cumulative
  
  return(max(drawdown, na.rm = TRUE))
}

#' Database backup function
backup_database <- function(db_path = DEFAULT_DB_PATH, backup_dir = "backups") {
  
  tryCatch({
    
    if (!file.exists(db_path)) {
      stop("Database file not found: ", db_path)
    }
    
    # Create backup directory
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }
    
    # Generate backup filename with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_name <- paste0("nfl_predictions_backup_", timestamp, ".db")
    backup_path <- file.path(backup_dir, backup_name)
    
    # Copy database file
    file.copy(db_path, backup_path)
    
    # Update backup metadata
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, 
             "UPDATE database_metadata SET value = ?, updated_timestamp = ? WHERE key = 'last_backup'",
             params = list(backup_path, Sys.time()))
    dbDisconnect(con)
    
    cat("Database backed up to:", backup_path, "\n")
    return(backup_path)
    
  }, error = function(e) {
    cat("ERROR backing up database:", e$message, "\n")
    stop(e)
  })
}

#' Get database statistics
get_database_stats <- function(con) {
  
  stats <- list()
  
  # Basic counts
  stats$total_predictions <- dbGetQuery(con, "SELECT COUNT(*) as count FROM predictions")$count
  stats$completed_games <- dbGetQuery(con, "SELECT COUNT(*) as count FROM predictions WHERE game_completed = 1")$count
  stats$pending_games <- stats$total_predictions - stats$completed_games
  
  # Performance overview
  if (stats$completed_games > 0) {
    perf_query <- "
    SELECT 
      AVG(spread_correct) as avg_spread_accuracy,
      AVG(total_correct) as avg_total_accuracy, 
      AVG(moneyline_correct) as avg_moneyline_accuracy,
      AVG(ABS(spread_error)) as avg_spread_mae,
      COUNT(DISTINCT model_version) as model_versions
    FROM predictions 
    WHERE game_completed = 1"
    
    perf_stats <- dbGetQuery(con, perf_query)
    stats <- append(stats, perf_stats)
  }
  
  # Recent activity
  stats$predictions_last_7_days <- dbGetQuery(con, 
    "SELECT COUNT(*) as count FROM predictions WHERE prediction_timestamp >= datetime('now', '-7 days')")$count
  
  return(stats)
}

# Pattern identification helper functions (simplified implementations)
identify_team_bias_patterns <- function(predictions, min_occurrences) {
  # Placeholder for team-specific bias detection
  return(list())
}

identify_situational_patterns <- function(predictions, min_occurrences) {
  # Placeholder for situational pattern detection
  return(list())
}

identify_spread_bias_patterns <- function(predictions, min_occurrences) {
  # Placeholder for spread bias detection
  return(list())
}

identify_feature_drift_patterns <- function(predictions, min_occurrences) {
  # Placeholder for feature drift detection
  return(list())
}

store_error_pattern <- function(con, pattern, model_version) {
  # Placeholder for storing error patterns
  return(NULL)
}

# Additional helper functions
update_prediction_count <- function(con) {
  count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM predictions")$count
  dbExecute(con, 
           "UPDATE database_metadata SET value = ?, updated_timestamp = ? WHERE key = 'total_predictions'",
           params = list(as.character(count), Sys.time()))
}

update_completed_games_count <- function(con) {
  count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM predictions WHERE game_completed = 1")$count
  dbExecute(con, 
           "UPDATE database_metadata SET value = ?, updated_timestamp = ? WHERE key = 'total_completed_games'",
           params = list(as.character(count), Sys.time()))
}

validate_model_info <- function(model_info) {
  if (is.null(model_info$version) || is.null(model_info$feature_hash)) {
    stop("Model info must include version and feature_hash")
  }
}

validate_actual_results <- function(actual_results) {
  if (is.null(actual_results$home_score) || is.null(actual_results$away_score)) {
    stop("Actual results must include home_score and away_score")
  }
  
  if (!is.numeric(actual_results$home_score) || !is.numeric(actual_results$away_score)) {
    stop("Scores must be numeric")
  }
}

calculate_betting_results <- function(original, actual_results) {
  # Simplified betting result calculation
  actual_spread <- actual_results$home_score - actual_results$away_score
  actual_total <- actual_results$home_score + actual_results$away_score
  
  spread_result <- 0
  total_result <- 0
  moneyline_result <- 0
  
  # Spread betting result
  if (!is.na(original$vegas_spread) && !is.na(original$spread_recommendation)) {
    if (original$spread_recommendation == "BET HOME") {
      spread_result <- ifelse(actual_spread > -original$vegas_spread, 1, -1)
    } else if (original$spread_recommendation == "BET AWAY") {
      spread_result <- ifelse(actual_spread < -original$vegas_spread, 1, -1)
    }
  }
  
  # Total betting result
  if (!is.na(original$vegas_total) && !is.na(original$total_recommendation)) {
    if (original$total_recommendation == "BET OVER") {
      total_result <- ifelse(actual_total > original$vegas_total, 1, -1)
    } else if (original$total_recommendation == "BET UNDER") {
      total_result <- ifelse(actual_total < original$vegas_total, 1, -1)
    }
  }
  
  # Moneyline betting result
  if (!is.na(original$moneyline_recommendation)) {
    home_won <- actual_results$home_score > actual_results$away_score
    if (original$moneyline_recommendation == "BET HOME") {
      moneyline_result <- ifelse(home_won, 1, -1)
    } else if (original$moneyline_recommendation == "BET AWAY") {
      moneyline_result <- ifelse(!home_won, 1, -1)
    }
  }
  
  return(list(
    spread = spread_result,
    total = total_result,
    moneyline = moneyline_result
  ))
}

generate_performance_id <- function(model_version, period, season, week) {
  base_string <- paste(model_version, period, season, week %||% "all", sep = "_")
  return(digest::digest(base_string, algo = "md5"))
}

# ==============================================================================
# INITIALIZATION MESSAGE
# ==============================================================================

cat("NFL Prediction Database System v", DB_VERSION, " loaded successfully!\n")
cat("Key functions available:\n")
cat("  - initialize_prediction_db(): Set up database\n")
cat("  - insert_prediction(): Add new predictions\n") 
cat("  - update_prediction_results(): Update with game outcomes\n")
cat("  - calculate_model_performance(): Analyze model performance\n")
cat("  - get_performance_trends(): Track performance over time\n")
cat("  - identify_error_patterns(): Find systematic errors\n")
cat("  - backup_database(): Create database backup\n")
cat("  - get_database_stats(): View database statistics\n")
cat("\nExample usage:\n")
cat("  con <- initialize_prediction_db('my_predictions.db')\n")
cat("  stats <- get_database_stats(con)\n")
cat("  dbDisconnect(con)\n")