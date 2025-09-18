# Simple CSV-Based Outcome Tracker
# Lightweight tracking system using CSV files while database packages are being installed
# Fully functional learning system that works immediately with actual 2025 data

library(dplyr)
library(jsonlite)

# Configuration
PREDICTIONS_FILE <- "learning_system/predictions_tracking.csv"
PERFORMANCE_FILE <- "learning_system/performance_results.csv"

#' Initialize CSV-based tracking system
#' 
#' Creates CSV files for tracking predictions and performance if they don't exist
#' 
#' @return TRUE if successful
initialize_csv_tracker <- function() {
  
  cat("📋 Initializing CSV-based outcome tracker...\n")
  
  # Create predictions tracking file if it doesn't exist
  if (!file.exists(PREDICTIONS_FILE)) {
    
    # Create directory if needed
    dir.create(dirname(PREDICTIONS_FILE), recursive = TRUE, showWarnings = FALSE)
    
    predictions_df <- data.frame(
      prediction_id = character(),
      game_id = character(),
      prediction_date = character(),
      model_version = character(),
      
      # Game info
      home_team = character(),
      away_team = character(),
      season = integer(),
      week = integer(),
      game_date = character(),
      
      # Predictions
      predicted_margin = numeric(),
      predicted_total = numeric(),
      home_win_prob = numeric(),
      confidence = numeric(),
      
      # Actual results (filled later)
      actual_home_score = integer(),
      actual_away_score = integer(),
      actual_margin = numeric(),
      actual_total = integer(),
      
      # Performance metrics
      spread_error = numeric(),
      total_error = numeric(),
      correct_direction = logical(),
      confidence_calibration = numeric(),
      
      # Status
      result_processed = logical(),
      created_at = character(),
      updated_at = character(),
      
      stringsAsFactors = FALSE
    )
    
    write.csv(predictions_df, PREDICTIONS_FILE, row.names = FALSE)
    cat("✅ Created predictions tracking file:", PREDICTIONS_FILE, "\n")
  }
  
  # Create performance results file if it doesn't exist
  if (!file.exists(PERFORMANCE_FILE)) {
    
    performance_df <- data.frame(
      analysis_date = character(),
      model_version = character(),
      period_start = character(),
      period_end = character(),
      
      # Basic metrics
      total_predictions = integer(),
      processed_predictions = integer(),
      avg_spread_error = numeric(),
      avg_total_error = numeric(),
      directional_accuracy = numeric(),
      avg_confidence = numeric(),
      confidence_calibration = numeric(),
      
      # Performance by confidence level
      high_conf_accuracy = numeric(),  # confidence >= 0.7
      medium_conf_accuracy = numeric(), # 0.5 <= confidence < 0.7
      low_conf_accuracy = numeric(),   # confidence < 0.5
      
      stringsAsFactors = FALSE
    )
    
    write.csv(performance_df, PERFORMANCE_FILE, row.names = FALSE)
    cat("✅ Created performance tracking file:", PERFORMANCE_FILE, "\n")
  }
  
  return(TRUE)
}

#' Store a prediction in CSV file
#' 
#' @param game_id Unique game identifier
#' @param home_team Home team abbreviation
#' @param away_team Away team abbreviation  
#' @param predicted_margin Predicted point spread (positive = home favored)
#' @param predicted_total Predicted total points
#' @param home_win_prob Probability home team wins (0-1)
#' @param confidence Model confidence (0-1)
#' @param model_version Model version string
#' @param season Season year
#' @param week Week number
#' @param game_date Date of game
#' 
#' @return prediction_id if successful, NULL if failed
store_prediction_csv <- function(game_id, home_team, away_team, 
                                predicted_margin, predicted_total, home_win_prob, confidence,
                                model_version = "csv_v1.0", season = 2025, week = NULL, game_date = NULL) {
  
  # Input validation
  if (home_win_prob < 0 || home_win_prob > 1) {
    stop("home_win_prob must be between 0 and 1, got: ", home_win_prob)
  }
  if (confidence < 0 || confidence > 1) {
    stop("confidence must be between 0 and 1, got: ", confidence)
  }
  if (predicted_total <= 0) {
    stop("predicted_total must be positive, got: ", predicted_total)
  }
  
  tryCatch({
    
    # Load existing predictions
    predictions <- read.csv(PREDICTIONS_FILE, stringsAsFactors = FALSE)
    
    # Check for duplicate
    existing <- predictions[predictions$game_id == game_id & 
                          predictions$model_version == model_version,]
    
    if (nrow(existing) > 0) {
      cat("⚠️  Prediction already exists for", game_id, "with model", model_version, "\n")
      return(existing$prediction_id[1])
    }
    
    # Generate new prediction ID
    prediction_id <- paste0("pred_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", 
                           sample(1000:9999, 1))
    
    # Create new prediction row
    new_prediction <- data.frame(
      prediction_id = prediction_id,
      game_id = game_id,
      prediction_date = as.character(Sys.time()),
      model_version = model_version,
      
      home_team = home_team,
      away_team = away_team,
      season = season,
      week = week %||% NA,
      game_date = as.character(game_date %||% NA),
      
      predicted_margin = predicted_margin,
      predicted_total = predicted_total,
      home_win_prob = home_win_prob,
      confidence = confidence,
      
      actual_home_score = NA,
      actual_away_score = NA,
      actual_margin = NA,
      actual_total = NA,
      
      spread_error = NA,
      total_error = NA,
      correct_direction = NA,
      confidence_calibration = NA,
      
      result_processed = FALSE,
      created_at = as.character(Sys.time()),
      updated_at = as.character(Sys.time()),
      
      stringsAsFactors = FALSE
    )
    
    # Append to predictions file
    predictions <- rbind(predictions, new_prediction)
    write.csv(predictions, PREDICTIONS_FILE, row.names = FALSE)
    
    cat(sprintf("✅ Prediction stored: %s vs %s (ID: %s)\n", 
               away_team, home_team, prediction_id))
    
    return(prediction_id)
    
  }, error = function(e) {
    cat("❌ Error storing prediction:", e$message, "\n")
    return(NULL)
  })
}

#' Update prediction with actual game results
#' 
#' @param game_id Game identifier to update
#' @param actual_home_score Actual home team score
#' @param actual_away_score Actual away team score
#' @param model_version Model version (optional, updates all if NULL)
#' 
#' @return TRUE if successful, FALSE otherwise
update_prediction_results_csv <- function(game_id, actual_home_score, actual_away_score, model_version = NULL) {
  
  tryCatch({
    
    # Load predictions
    predictions <- read.csv(PREDICTIONS_FILE, stringsAsFactors = FALSE)
    
    # Find matching predictions
    if (!is.null(model_version)) {
      matches <- predictions$game_id == game_id & predictions$model_version == model_version
    } else {
      matches <- predictions$game_id == game_id
    }
    
    matching_rows <- which(matches)
    
    if (length(matching_rows) == 0) {
      cat("⚠️  No predictions found for game:", game_id, "\n")
      return(FALSE)
    }
    
    # Update each matching prediction
    for (i in matching_rows) {
      
      # Calculate actual results
      actual_margin <- actual_home_score - actual_away_score
      actual_total <- actual_home_score + actual_away_score
      
      # Calculate performance metrics
      spread_error <- abs(predictions$predicted_margin[i] - actual_margin)
      total_error <- abs(predictions$predicted_total[i] - actual_total)
      
      # Check directional correctness
      predicted_home_wins <- predictions$predicted_margin[i] > 0
      actual_home_wins <- actual_margin > 0
      correct_direction <- predicted_home_wins == actual_home_wins
      
      # Calculate confidence calibration
      confidence_calibration <- abs(predictions$confidence[i] - ifelse(correct_direction, 1, 0))
      
      # Update the row
      predictions$actual_home_score[i] <- actual_home_score
      predictions$actual_away_score[i] <- actual_away_score
      predictions$actual_margin[i] <- actual_margin
      predictions$actual_total[i] <- actual_total
      
      predictions$spread_error[i] <- spread_error
      predictions$total_error[i] <- total_error
      predictions$correct_direction[i] <- correct_direction
      predictions$confidence_calibration[i] <- confidence_calibration
      
      predictions$result_processed[i] <- TRUE
      predictions$updated_at[i] <- as.character(Sys.time())
      
      cat(sprintf("✅ Updated %s: %s %d-%d %s (Error: %.1f)\n", 
                 predictions$prediction_id[i], predictions$home_team[i], 
                 actual_home_score, actual_away_score, predictions$away_team[i], 
                 spread_error))
    }
    
    # Save updated predictions
    write.csv(predictions, PREDICTIONS_FILE, row.names = FALSE)
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error updating results:", e$message, "\n")
    return(FALSE)
  })
}

#' Calculate performance metrics for a period
#' 
#' @param start_date Start date for analysis  
#' @param end_date End date for analysis
#' @param model_version Model version to analyze (optional)
#' 
#' @return Data frame with performance metrics
calculate_csv_performance <- function(start_date = NULL, end_date = NULL, model_version = NULL) {
  
  cat("📊 Calculating performance metrics...\n")
  
  tryCatch({
    
    # Load predictions
    predictions <- read.csv(PREDICTIONS_FILE, stringsAsFactors = FALSE)
    
    # Filter processed predictions
    processed <- predictions[predictions$result_processed == TRUE,]
    
    if (nrow(processed) == 0) {
      cat("⚠️  No processed predictions found\n")
      return(NULL)
    }
    
    # Apply filters
    if (!is.null(start_date)) {
      processed$prediction_date <- as.Date(processed$prediction_date)
      processed <- processed[processed$prediction_date >= as.Date(start_date),]
    }
    
    if (!is.null(end_date)) {
      processed$prediction_date <- as.Date(processed$prediction_date)
      processed <- processed[processed$prediction_date <= as.Date(end_date),]
    }
    
    if (!is.null(model_version)) {
      processed <- processed[processed$model_version == model_version,]
    }
    
    if (nrow(processed) == 0) {
      cat("⚠️  No predictions found for specified period\n")
      return(NULL)
    }
    
    # Calculate overall metrics
    total_predictions <- nrow(processed)
    avg_spread_error <- mean(processed$spread_error, na.rm = TRUE)
    avg_total_error <- mean(processed$total_error, na.rm = TRUE)
    directional_accuracy <- mean(processed$correct_direction, na.rm = TRUE)
    avg_confidence <- mean(processed$confidence, na.rm = TRUE)
    confidence_calibration <- mean(processed$confidence_calibration, na.rm = TRUE)
    
    # Performance by confidence levels
    high_conf <- processed[processed$confidence >= 0.7,]
    medium_conf <- processed[processed$confidence >= 0.5 & processed$confidence < 0.7,]
    low_conf <- processed[processed$confidence < 0.5,]
    
    high_conf_accuracy <- if(nrow(high_conf) > 0) mean(high_conf$correct_direction, na.rm = TRUE) else NA
    medium_conf_accuracy <- if(nrow(medium_conf) > 0) mean(medium_conf$correct_direction, na.rm = TRUE) else NA
    low_conf_accuracy <- if(nrow(low_conf) > 0) mean(low_conf$correct_direction, na.rm = TRUE) else NA
    
    # Create results
    results <- data.frame(
      analysis_date = as.character(Sys.time()),
      model_version = model_version %||% "all_models",
      period_start = as.character(start_date %||% "all_time"),
      period_end = as.character(end_date %||% "all_time"),
      
      total_predictions = total_predictions,
      processed_predictions = total_predictions,
      avg_spread_error = round(avg_spread_error, 2),
      avg_total_error = round(avg_total_error, 2),
      directional_accuracy = round(directional_accuracy, 3),
      avg_confidence = round(avg_confidence, 3),
      confidence_calibration = round(confidence_calibration, 3),
      
      high_conf_accuracy = round(high_conf_accuracy, 3),
      medium_conf_accuracy = round(medium_conf_accuracy, 3),
      low_conf_accuracy = round(low_conf_accuracy, 3),
      
      stringsAsFactors = FALSE
    )
    
    # Store results
    if (file.exists(PERFORMANCE_FILE)) {
      performance_history <- read.csv(PERFORMANCE_FILE, stringsAsFactors = FALSE)
      performance_history <- rbind(performance_history, results)
    } else {
      performance_history <- results
    }
    
    write.csv(performance_history, PERFORMANCE_FILE, row.names = FALSE)
    
    # Print summary
    cat("✅ Performance Analysis Complete:\n")
    cat(sprintf("- Total Predictions: %d\n", total_predictions))
    cat(sprintf("- Avg Spread Error: %.2f points\n", avg_spread_error))
    cat(sprintf("- Directional Accuracy: %.1f%%\n", directional_accuracy * 100))
    cat(sprintf("- High Confidence Accuracy: %.1f%% (%d games)\n", 
               high_conf_accuracy * 100, nrow(high_conf)))
    
    return(results)
    
  }, error = function(e) {
    cat("❌ Error calculating performance:", e$message, "\n")
    return(NULL)
  })
}

#' Process actual results for completed games
#' 
#' Automatically matches predictions with actual game results from nfldata
#' 
#' @param season Season to process (default: 2025)
#' @param week Specific week to process (optional)
#' 
#' @return Number of predictions updated
process_completed_games_csv <- function(season = 2025, week = NULL) {
  
  cat("🔄 Processing completed games for learning...\n")
  
  # Load actual game results using unified data access
  tryCatch({
    
    # Use unified data access system instead of hard-coded path
    source('learning_system/unified_data_access.R')
    completed_games <- load_actual_game_results(seasons = season, completed_only = TRUE)
    
    if (is.null(completed_games) || nrow(completed_games) == 0) {
      cat("❌ No completed games found for season", season, "\n")
      return(0)
    }
    
    # Filter to specific week if requested
    if (!is.null(week)) {
      completed_games <- completed_games[completed_games$week == week,]
      if (nrow(completed_games) == 0) {
        cat("⚠️  No completed games found for season", season, "week", week, "\n")
        return(0)
      }
    }
    
    cat(sprintf("📊 Found %d completed games\n", nrow(completed_games)))
    
    # Load existing predictions  
    if (!file.exists(PREDICTIONS_FILE)) {
      cat("⚠️  No predictions file found\n")
      return(0)
    }
    
    predictions <- read.csv(PREDICTIONS_FILE, stringsAsFactors = FALSE)
    unprocessed <- predictions[predictions$result_processed == FALSE,]
    
    if (nrow(unprocessed) == 0) {
      cat("⚠️  No unprocessed predictions found\n") 
      return(0)
    }
    
    # Match predictions with completed games
    updates_made <- 0
    
    for (i in 1:nrow(completed_games)) {
      game <- completed_games[i,]
      
      # Find matching predictions by team names
      matching_preds <- unprocessed[
        unprocessed$home_team == game$home_team & 
        unprocessed$away_team == game$away_team &
        unprocessed$season == game$season &
        (is.na(unprocessed$week) | unprocessed$week == game$week),
      ]
      
      if (nrow(matching_preds) > 0) {
        # Update each matching prediction
        for (j in 1:nrow(matching_preds)) {
          pred <- matching_preds[j,]
          
          updated <- update_prediction_results_csv(
            game_id = pred$game_id,
            actual_home_score = game$home_score,
            actual_away_score = game$away_score,
            model_version = pred$model_version
          )
          
          if (updated) {
            updates_made <- updates_made + 1
          }
        }
      }
    }
    
    cat(sprintf("✅ Updated %d predictions with actual results\n", updates_made))
    return(updates_made)
    
  }, error = function(e) {
    cat("❌ Error processing completed games:", e$message, "\n")
    return(0)
  })
}

# Helper function for null coalescing
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# Initialize the system
initialize_csv_tracker()

cat("CSV-Based Outcome Tracker loaded! 📊\n\n")
cat("Available functions:\n")
cat("- store_prediction_csv(): Store a new prediction\n")
cat("- update_prediction_results_csv(): Update with actual results\n")
cat("- calculate_csv_performance(): Analyze performance metrics\n") 
cat("- process_completed_games_csv(): Auto-process completed games\n")
cat("\n🚀 Quick start example:\n")
cat("# Store prediction\n")
cat("id <- store_prediction_csv('test_game', 'LAC', 'KC', -4, 48.5, 0.35, 0.72)\n")
cat("\n# Update with results\n") 
cat("update_prediction_results_csv('test_game', 27, 21)\n")
cat("\n# Analyze performance\n")
cat("perf <- calculate_csv_performance()\n")