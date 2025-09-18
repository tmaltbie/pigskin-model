# Production ML Pipeline for NFL Spread Prediction
# Complete production system with model serving, monitoring, and updates

library(plumber)
library(jsonlite)
library(logging)
library(DBI)
library(RSQLite)
library(future)
library(promises)

source("ensemble_system.R")
source("enhanced_data_sources.R")

# ============= PRODUCTION DATA PIPELINE =============

# Initialize production environment
initialize_production_system <- function() {
  
  cat("Initializing NFL ML Production System...\n")
  
  # Create directories
  dirs <- c("data", "models", "logs", "monitoring", "cache")
  for(dir in dirs) {
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat(sprintf("Created directory: %s\n", dir))
    }
  }
  
  # Initialize logging
  basicConfig(level = "INFO")
  addHandler(writeToFile, file = "logs/nfl_ml_system.log")
  
  # Initialize SQLite database for monitoring
  init_monitoring_db()
  
  # Set up async processing
  plan(multisession, workers = 4)
  
  cat("Production system initialized!\n")
}

# Initialize monitoring database
init_monitoring_db <- function() {
  
  con <- dbConnect(SQLite(), "monitoring/nfl_predictions.db")
  
  # Predictions table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS predictions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT,
      season INTEGER,
      week INTEGER,
      home_team TEXT,
      away_team TEXT,
      vegas_spread REAL,
      ensemble_margin REAL,
      ensemble_confidence REAL,
      recommendation TEXT,
      bet_size REAL,
      model_versions TEXT,
      execution_time_ms INTEGER
    )
  ")
  
  # Results table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS results (
      prediction_id INTEGER,
      actual_margin REAL,
      prediction_error REAL,
      bet_result TEXT,
      profit_loss REAL,
      updated_timestamp TEXT,
      FOREIGN KEY(prediction_id) REFERENCES predictions(id)
    )
  ")
  
  # Model performance table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS model_performance (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      model_name TEXT,
      metric_name TEXT,
      metric_value REAL,
      evaluation_date TEXT,
      data_version TEXT
    )
  ")
  
  dbDisconnect(con)
  cat("Monitoring database initialized\n")
}

# ============= MODEL SERVING API =============

# Main prediction API
create_prediction_api <- function() {
  
  pr() %>%
    
    # Health check endpoint
    pr_get("/health", function() {
      list(
        status = "healthy",
        timestamp = Sys.time(),
        version = "1.0.0",
        models_loaded = file.exists("models/nfl_ml_models.rds")
      )
    }) %>%
    
    # Single game prediction
    pr_post("/predict", function(req, res) {
      
      start_time <- Sys.time()
      
      tryCatch({
        # Parse request
        game_data <- jsonlite::fromJSON(req$postBody)
        
        # Validate input
        required_fields <- c("home_team", "away_team", "vegas_spread", "season", "week")
        if(!all(required_fields %in% names(game_data))) {
          res$status <- 400
          return(list(error = "Missing required fields"))
        }
        
        # Load models and data
        ml_models <- readRDS("models/nfl_ml_models.rds")
        pbp_data <- load_cached_pbp_data()
        schedule_data <- load_cached_schedule_data()
        
        # Make prediction
        prediction <- ensemble_predict_game(
          home_team = game_data$home_team,
          away_team = game_data$away_team,
          vegas_spread = game_data$vegas_spread,
          pbp_data = pbp_data,
          schedule_data = schedule_data,
          season = game_data$season,
          week = game_data$week,
          ml_models = ml_models,
          additional_info = game_data$additional_info
        )
        
        # Log prediction
        execution_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
        log_prediction(prediction, execution_time)
        
        # Return prediction
        return(prediction)
        
      }, error = function(e) {
        logwarn(sprintf("Prediction error: %s", e$message))
        res$status <- 500
        return(list(error = "Internal server error"))
      })
    }) %>%
    
    # Batch predictions
    pr_post("/predict_batch", function(req, res) {
      
      tryCatch({
        games_data <- jsonlite::fromJSON(req$postBody)
        
        if(!"games" %in% names(games_data)) {
          res$status <- 400
          return(list(error = "Missing 'games' field"))
        }
        
        # Load models once for batch
        ml_models <- readRDS("models/nfl_ml_models.rds")
        pbp_data <- load_cached_pbp_data()
        schedule_data <- load_cached_schedule_data()
        
        # Process games in parallel
        predictions <- future_map(games_data$games, function(game) {
          ensemble_predict_game(
            home_team = game$home_team,
            away_team = game$away_team,
            vegas_spread = game$vegas_spread,
            pbp_data = pbp_data,
            schedule_data = schedule_data,
            season = game$season,
            week = game$week,
            ml_models = ml_models,
            additional_info = game$additional_info
          )
        })
        
        return(list(predictions = predictions))
        
      }, error = function(e) {
        logwarn(sprintf("Batch prediction error: %s", e$message))
        res$status <- 500
        return(list(error = "Internal server error"))
      })
    }) %>%
    
    # Model performance metrics
    pr_get("/metrics", function() {
      get_model_performance_metrics()
    }) %>%
    
    # Update actual results
    pr_post("/update_results", function(req, res) {
      
      tryCatch({
        results_data <- jsonlite::fromJSON(req$postBody)
        update_prediction_results(results_data)
        return(list(status = "success"))
        
      }, error = function(e) {
        logwarn(sprintf("Results update error: %s", e$message))
        res$status <- 500
        return(list(error = "Failed to update results"))
      })
    })
}

# ============= DATA CACHING SYSTEM =============

# Cache management for production data
load_cached_pbp_data <- function(max_age_hours = 24) {
  
  cache_file <- "cache/pbp_data.rds"
  
  # Check if cache exists and is fresh
  if(file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
    if(cache_age < max_age_hours) {
      loginfo("Loading PBP data from cache")
      return(readRDS(cache_file))
    }
  }
  
  # Refresh cache
  loginfo("Refreshing PBP data cache")
  pbp_data <- load_pbp(2020:2025)  # Load recent seasons
  saveRDS(pbp_data, cache_file)
  
  return(pbp_data)
}

load_cached_schedule_data <- function(max_age_hours = 6) {
  
  cache_file <- "cache/schedule_data.rds"
  
  if(file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
    if(cache_age < max_age_hours) {
      loginfo("Loading schedule data from cache")
      return(readRDS(cache_file))
    }
  }
  
  loginfo("Refreshing schedule data cache")
  schedule_data <- load_schedules(2020:2025)
  saveRDS(schedule_data, cache_file)
  
  return(schedule_data)
}

# ============= MONITORING AND LOGGING =============

# Log predictions to database
log_prediction <- function(prediction, execution_time_ms) {
  
  con <- dbConnect(SQLite(), "monitoring/nfl_predictions.db")
  
  tryCatch({
    dbExecute(con, "
      INSERT INTO predictions (
        timestamp, season, week, home_team, away_team,
        vegas_spread, ensemble_margin, ensemble_confidence,
        recommendation, bet_size, model_versions, execution_time_ms
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", list(
      as.character(Sys.time()),
      prediction$season,
      prediction$week,
      prediction$home_team,
      prediction$away_team,
      prediction$vegas_spread,
      prediction$ensemble_margin,
      prediction$ensemble_confidence,
      prediction$recommendation,
      prediction$bet_size,
      jsonlite::toJSON(list(ensemble = "v1.0")),
      execution_time_ms
    ))
    
  }, finally = {
    dbDisconnect(con)
  })
}

# Update prediction results
update_prediction_results <- function(results_data) {
  
  con <- dbConnect(SQLite(), "monitoring/nfl_predictions.db")
  
  tryCatch({
    for(result in results_data$results) {
      
      # Find matching prediction
      prediction_id_query <- dbGetQuery(con, "
        SELECT id FROM predictions 
        WHERE season = ? AND week = ? AND home_team = ? AND away_team = ?
        ORDER BY timestamp DESC LIMIT 1
      ", list(result$season, result$week, result$home_team, result$away_team))
      
      if(nrow(prediction_id_query) > 0) {
        prediction_id <- prediction_id_query$id[1]
        
        # Calculate metrics
        prediction_query <- dbGetQuery(con, "
          SELECT ensemble_margin, recommendation, bet_size 
          FROM predictions WHERE id = ?
        ", list(prediction_id))
        
        if(nrow(prediction_query) > 0) {
          pred_margin <- prediction_query$ensemble_margin[1]
          recommendation <- prediction_query$recommendation[1]
          bet_size <- prediction_query$bet_size[1]
          
          prediction_error <- abs(result$actual_margin - pred_margin)
          
          # Calculate betting result
          bet_result <- calculate_bet_result(
            result$actual_margin, result$vegas_spread, 
            recommendation, bet_size
          )
          
          # Insert result
          dbExecute(con, "
            INSERT OR REPLACE INTO results (
              prediction_id, actual_margin, prediction_error,
              bet_result, profit_loss, updated_timestamp
            ) VALUES (?, ?, ?, ?, ?, ?)
          ", list(
            prediction_id,
            result$actual_margin,
            prediction_error,
            bet_result$result,
            bet_result$profit_loss,
            as.character(Sys.time())
          ))
        }
      }
    }
    
  }, finally = {
    dbDisconnect(con)
  })
}

# Calculate betting result
calculate_bet_result <- function(actual_margin, vegas_spread, recommendation, bet_size) {
  
  if(bet_size == 0 || grepl("PASS", recommendation)) {
    return(list(result = "NO_BET", profit_loss = 0))
  }
  
  # Determine if bet won
  home_covered <- (actual_margin + vegas_spread) > 0
  bet_on_home <- grepl("HOME", recommendation)
  
  won_bet <- (bet_on_home && home_covered) || (!bet_on_home && !home_covered)
  
  if(abs(actual_margin + vegas_spread) < 0.5) {
    return(list(result = "PUSH", profit_loss = 0))
  }
  
  if(won_bet) {
    profit_loss <- bet_size * 0.909  # Win $0.909 per $1 bet
    return(list(result = "WIN", profit_loss = profit_loss))
  } else {
    profit_loss <- -bet_size  # Lose $1 per $1 bet
    return(list(result = "LOSS", profit_loss = profit_loss))
  }
}

# Get performance metrics
get_model_performance_metrics <- function() {
  
  con <- dbConnect(SQLite(), "monitoring/nfl_predictions.db")
  
  tryCatch({
    # Recent performance
    recent_results <- dbGetQuery(con, "
      SELECT p.ensemble_margin, p.ensemble_confidence, p.recommendation,
             r.actual_margin, r.prediction_error, r.bet_result, r.profit_loss
      FROM predictions p
      LEFT JOIN results r ON p.id = r.prediction_id
      WHERE p.timestamp > datetime('now', '-30 days')
      ORDER BY p.timestamp DESC
    ")
    
    if(nrow(recent_results) == 0) {
      return(list(status = "No recent data"))
    }
    
    # Calculate metrics
    completed_predictions <- recent_results[!is.na(recent_results$actual_margin), ]
    
    metrics <- list(
      total_predictions = nrow(recent_results),
      completed_predictions = nrow(completed_predictions),
      
      # Prediction accuracy
      avg_error = mean(completed_predictions$prediction_error, na.rm = TRUE),
      predictions_within_3 = mean(completed_predictions$prediction_error <= 3, na.rm = TRUE),
      predictions_within_7 = mean(completed_predictions$prediction_error <= 7, na.rm = TRUE),
      
      # Betting performance
      total_bets = sum(!is.na(completed_predictions$bet_result) & 
                      completed_predictions$bet_result != "NO_BET"),
      win_rate = mean(completed_predictions$bet_result == "WIN", na.rm = TRUE),
      total_profit = sum(completed_predictions$profit_loss, na.rm = TRUE),
      roi = sum(completed_predictions$profit_loss, na.rm = TRUE) / 
            sum(abs(completed_predictions$profit_loss), na.rm = TRUE) * 100,
      
      # System health
      avg_confidence = mean(recent_results$ensemble_confidence, na.rm = TRUE),
      last_updated = max(recent_results$timestamp, na.rm = TRUE)
    )
    
    return(metrics)
    
  }, finally = {
    dbDisconnect(con)
  })
}

# ============= MODEL UPDATE SYSTEM =============

# Automated model retraining
retrain_models_pipeline <- function() {
  
  loginfo("Starting model retraining pipeline")
  
  tryCatch({
    # Load latest data
    pbp_data <- load_pbp(1999:2025)
    schedule_data <- load_schedules(1999:2025)
    
    # Create synthetic spreads data (replace with real data source)
    spreads_data <- create_synthetic_spreads_data(schedule_data)
    
    # Prepare training data
    training_data <- prepare_training_data(pbp_data, schedule_data, spreads_data)
    training_clean <- prepare_model_features(training_data)
    
    # Train new models
    new_models <- train_ensemble_models(training_clean)
    
    # Validate new models
    validation_results <- run_complete_validation(pbp_data, schedule_data, spreads_data)
    
    # Compare with current models
    if(file.exists("models/nfl_ml_models.rds")) {
      current_models <- readRDS("models/nfl_ml_models.rds")
      
      # Simple validation comparison (implement proper A/B testing)
      if(validation_results$summary$margin_mae$mean < 5.0) {  # Threshold
        # Replace models
        saveRDS(new_models, "models/nfl_ml_models.rds")
        saveRDS(new_models, sprintf("models/nfl_ml_models_backup_%s.rds", 
                                   format(Sys.time(), "%Y%m%d_%H%M%S")))
        loginfo("Models updated successfully")
      } else {
        logwarn("New models did not meet performance threshold")
      }
    } else {
      # First time setup
      saveRDS(new_models, "models/nfl_ml_models.rds")
      loginfo("Initial models saved")
    }
    
  }, error = function(e) {
    logerror(sprintf("Model retraining failed: %s", e$message))
  })
}

# Create synthetic spreads for demo (replace with real data)
create_synthetic_spreads_data <- function(schedule_data) {
  
  completed_games <- schedule_data[!is.na(schedule_data$result), ]
  
  # Simulate spreads based on actual results
  spreads_data <- completed_games %>%
    mutate(
      spread = rnorm(n(), mean = -(home_score - away_score), sd = 3)
    ) %>%
    select(season, week, home_team, away_team, spread)
  
  return(spreads_data)
}

# ============= DEPLOYMENT FUNCTIONS =============

# Start production server
start_production_server <- function(port = 8000) {
  
  initialize_production_system()
  
  api <- create_prediction_api()
  
  loginfo(sprintf("Starting NFL ML API server on port %d", port))
  
  api %>% 
    pr_run(port = port, host = "0.0.0.0")
}

# Deployment health check
deployment_health_check <- function() {
  
  checks <- list()
  
  # Check models exist
  checks$models_exist <- file.exists("models/nfl_ml_models.rds")
  
  # Check database
  checks$database_ok <- tryCatch({
    con <- dbConnect(SQLite(), "monitoring/nfl_predictions.db")
    result <- dbGetQuery(con, "SELECT COUNT(*) as count FROM predictions")
    dbDisconnect(con)
    TRUE
  }, error = function(e) FALSE)
  
  # Check data cache
  checks$cache_fresh <- tryCatch({
    cache_age <- difftime(Sys.time(), file.mtime("cache/pbp_data.rds"), units = "hours")
    cache_age < 48  # Less than 2 days old
  }, error = function(e) FALSE)
  
  # Overall health
  checks$overall_health <- all(unlist(checks))
  
  return(checks)
}

cat("Production ML Pipeline loaded!\n")
cat("Features:\n")
cat("✅ RESTful API with /predict and /predict_batch endpoints\n")
cat("✅ SQLite monitoring database\n")
cat("✅ Automated data caching and refresh\n")
cat("✅ Performance metrics and logging\n")
cat("✅ Model retraining pipeline\n")
cat("✅ Health checks and monitoring\n")
cat("✅ Async processing for batch predictions\n")
cat("\nAPI Endpoints:\n")
cat("- GET  /health - System health check\n")
cat("- POST /predict - Single game prediction\n")
cat("- POST /predict_batch - Batch predictions\n")
cat("- GET  /metrics - Performance metrics\n")
cat("- POST /update_results - Update actual results\n")
cat("\nTo start: start_production_server(port = 8000)\n")
cat("To check health: deployment_health_check()\n")