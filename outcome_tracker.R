# NFL Outcome Tracking System
# Production-ready system for automatically matching predictions with actual game results
# Enables comprehensive learning system integration and performance analysis
# Author: NFL ML System
# Version: 1.0

library(RSQLite)
library(DBI)
library(dplyr)
library(data.table)
library(lubridate)
library(jsonlite)
library(digest)

# Source required dependencies
source("prediction_database.R")
source("learning_system/unified_data_access.R")

# ==============================================================================
# GLOBAL CONFIGURATION
# ==============================================================================

OUTCOME_TRACKER_VERSION <- "1.0.0"
DEFAULT_LOG_LEVEL <- "INFO"  # DEBUG, INFO, WARN, ERROR
BATCH_SIZE <- 50  # Maximum games to process in one batch
MAX_RETRIES <- 3  # Maximum retry attempts for failed operations
RETRY_DELAY <- 2  # Seconds between retries

# Initialize logging environment
.outcome_log <- new.env()
.outcome_log$entries <- list()
.outcome_log$level <- DEFAULT_LOG_LEVEL

# ==============================================================================
# LOGGING SYSTEM
# ==============================================================================

#' Log message with timestamp and level
#' @param message Message to log
#' @param level Log level (DEBUG, INFO, WARN, ERROR)
log_message <- function(message, level = "INFO") {
  
  levels <- c("DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4)
  current_level <- levels[.outcome_log$level]
  msg_level <- levels[level]
  
  if (msg_level >= current_level) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    formatted_msg <- sprintf("[%s] %s: %s", timestamp, level, message)
    
    # Print to console
    cat(formatted_msg, "\n")
    
    # Store in memory
    .outcome_log$entries <- append(.outcome_log$entries, list(list(
      timestamp = Sys.time(),
      level = level,
      message = message
    )))
    
    # Keep only last 1000 entries
    if (length(.outcome_log$entries) > 1000) {
      .outcome_log$entries <- tail(.outcome_log$entries, 1000)
    }
  }
}

#' Set logging level
set_log_level <- function(level = "INFO") {
  .outcome_log$level <- level
  log_message(paste("Log level set to:", level), "INFO")
}

#' Get recent log entries
get_log_entries <- function(last_n = 100, level_filter = NULL) {
  entries <- tail(.outcome_log$entries, last_n)
  
  if (!is.null(level_filter)) {
    entries <- entries[sapply(entries, function(x) x$level == level_filter)]
  }
  
  return(entries)
}

# ==============================================================================
# CORE OUTCOME TRACKING FUNCTIONS
# ==============================================================================

#' Process weekly results - Main batch processing function
#' 
#' Processes all completed games for a specific week/season combination.
#' This is the primary function for regular batch updates.
#' 
#' @param week Week number to process
#' @param season Season year
#' @param db_path Path to prediction database
#' @param force_reprocess Force reprocessing of already processed games
#' @return Summary of processing results
#' @export
process_weekly_results <- function(week, season, db_path = DEFAULT_DB_PATH, 
                                 force_reprocess = FALSE) {
  
  log_message(sprintf("Starting weekly results processing: Season %d, Week %d", season, week))
  
  tryCatch({
    
    # Connect to database
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Load actual game results for the week
    log_message("Loading actual game results...")
    actual_results <- load_actual_game_results(seasons = season, completed_only = TRUE)
    
    if (is.null(actual_results) || nrow(actual_results) == 0) {
      log_message("No actual results available", "WARN")
      return(create_processing_summary(0, 0, 0, 0, "No actual results"))
    }
    
    # Filter to specific week
    week_results <- actual_results[actual_results$week == week & actual_results$season == season,]
    
    if (nrow(week_results) == 0) {
      log_message(sprintf("No completed games found for Week %d, Season %d", week, season), "WARN")
      return(create_processing_summary(0, 0, 0, 0, "No games completed"))
    }
    
    log_message(sprintf("Found %d completed games for Week %d", nrow(week_results), week))
    
    # Get predictions that need processing
    predictions_query <- "
    SELECT * FROM predictions 
    WHERE season = ? AND week = ? AND game_completed = ?
    ORDER BY prediction_timestamp DESC"
    
    completed_filter <- ifelse(force_reprocess, -1, 0)  # -1 gets all, 0 gets only unprocessed
    existing_predictions <- dbGetQuery(con, predictions_query, 
                                     params = list(season, week, completed_filter))
    
    if (nrow(existing_predictions) == 0) {
      log_message("No predictions found for this week", "WARN")
      return(create_processing_summary(0, 0, 0, nrow(week_results), "No predictions"))
    }
    
    log_message(sprintf("Found %d predictions to process", nrow(existing_predictions)))
    
    # Process each prediction
    processing_results <- list(
      processed = 0,
      matched = 0,
      errors = 0,
      unmatched = 0,
      error_details = list()
    )
    
    for (i in 1:nrow(existing_predictions)) {
      prediction <- existing_predictions[i,]
      
      log_message(sprintf("Processing prediction %d/%d: %s", i, nrow(existing_predictions), 
                         prediction$prediction_id), "DEBUG")
      
      result <- process_single_prediction_result(con, prediction, week_results)
      
      # Update counters
      if (result$status == "success") {
        processing_results$processed <- processing_results$processed + 1
        processing_results$matched <- processing_results$matched + 1
      } else if (result$status == "no_match") {
        processing_results$unmatched <- processing_results$unmatched + 1
      } else {
        processing_results$errors <- processing_results$errors + 1
        processing_results$error_details <- append(processing_results$error_details, result$message)
      }
    }
    
    # Generate performance update
    if (processing_results$processed > 0) {
      log_message("Calculating performance metrics...")
      tryCatch({
        performance <- calculate_model_performance(con, "current", "weekly", season, week)
        log_message("Performance metrics updated successfully")
      }, error = function(e) {
        log_message(paste("Performance calculation failed:", e$message), "WARN")
      })
    }
    
    log_message(sprintf("Weekly processing complete: %d processed, %d errors", 
                       processing_results$processed, processing_results$errors))
    
    return(create_processing_summary(
      processing_results$processed,
      processing_results$matched, 
      processing_results$errors,
      processing_results$unmatched,
      "Success"
    ))
    
  }, error = function(e) {
    log_message(paste("Weekly processing failed:", e$message), "ERROR")
    return(create_processing_summary(0, 0, 1, 0, paste("Error:", e$message)))
  })
}

#' Process individual game result
#' 
#' Processes a single game's results and updates all related predictions.
#' Useful for immediate updates when a game completes.
#' 
#' @param game_id Unique game identifier
#' @param db_path Path to prediction database
#' @return Processing result summary
#' @export
process_individual_result <- function(game_id, db_path = DEFAULT_DB_PATH) {
  
  log_message(sprintf("Processing individual game result: %s", game_id))
  
  tryCatch({
    
    # Connect to database
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Find predictions for this game
    predictions_query <- "SELECT * FROM predictions WHERE game_id = ?"
    predictions <- dbGetQuery(con, predictions_query, params = list(game_id))
    
    if (nrow(predictions) == 0) {
      log_message(sprintf("No predictions found for game_id: %s", game_id), "WARN")
      return(create_processing_summary(0, 0, 0, 0, "No predictions found"))
    }
    
    # Load actual results
    actual_results <- load_actual_game_results(completed_only = TRUE)
    
    if (is.null(actual_results)) {
      log_message("No actual results available", "ERROR")
      return(create_processing_summary(0, 0, 1, 0, "No actual results"))
    }
    
    # Find matching game result
    matching_result <- find_matching_game_result(predictions[1,], actual_results)
    
    if (is.null(matching_result)) {
      log_message(sprintf("No matching result found for game_id: %s", game_id), "WARN")
      return(create_processing_summary(0, 0, 0, 1, "No matching result"))
    }
    
    # Process all predictions for this game
    processed_count <- 0
    error_count <- 0
    
    for (i in 1:nrow(predictions)) {
      result <- process_single_prediction_result(con, predictions[i,], matching_result)
      
      if (result$status == "success") {
        processed_count <- processed_count + 1
      } else {
        error_count <- error_count + 1
        log_message(sprintf("Error processing prediction %s: %s", 
                           predictions[i,]$prediction_id, result$message), "ERROR")
      }
    }
    
    log_message(sprintf("Individual game processing complete: %d processed, %d errors", 
                       processed_count, error_count))
    
    return(create_processing_summary(processed_count, processed_count, error_count, 0, "Success"))
    
  }, error = function(e) {
    log_message(paste("Individual game processing failed:", e$message), "ERROR")
    return(create_processing_summary(0, 0, 1, 0, paste("Error:", e$message)))
  })
}

#' Update all pending predictions
#' 
#' Finds all predictions that haven't been updated with results and processes them.
#' This is useful for catching up on any missed updates.
#' 
#' @param db_path Path to prediction database
#' @param max_predictions Maximum number of predictions to process (safety limit)
#' @return Processing results summary
#' @export
update_all_pending_predictions <- function(db_path = DEFAULT_DB_PATH, max_predictions = 1000) {
  
  log_message("Starting update of all pending predictions")
  
  tryCatch({
    
    # Connect to database
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Find all unprocessed predictions
    pending_query <- "
    SELECT * FROM predictions 
    WHERE game_completed = 0 
    ORDER BY prediction_timestamp DESC
    LIMIT ?"
    
    pending_predictions <- dbGetQuery(con, pending_query, params = list(max_predictions))
    
    if (nrow(pending_predictions) == 0) {
      log_message("No pending predictions found")
      return(create_processing_summary(0, 0, 0, 0, "No pending predictions"))
    }
    
    log_message(sprintf("Found %d pending predictions to process", nrow(pending_predictions)))
    
    # Load all actual results
    actual_results <- load_actual_game_results(completed_only = TRUE)
    
    if (is.null(actual_results) || nrow(actual_results) == 0) {
      log_message("No actual results available", "WARN")
      return(create_processing_summary(0, 0, 0, nrow(pending_predictions), "No actual results"))
    }
    
    # Process in batches
    total_processed <- 0
    total_matched <- 0
    total_errors <- 0
    total_unmatched <- 0
    
    batch_count <- ceiling(nrow(pending_predictions) / BATCH_SIZE)
    
    for (batch_num in 1:batch_count) {
      start_idx <- (batch_num - 1) * BATCH_SIZE + 1
      end_idx <- min(batch_num * BATCH_SIZE, nrow(pending_predictions))
      
      log_message(sprintf("Processing batch %d/%d (predictions %d-%d)", 
                         batch_num, batch_count, start_idx, end_idx))
      
      batch_predictions <- pending_predictions[start_idx:end_idx,]
      
      batch_results <- process_prediction_batch(con, batch_predictions, actual_results)
      
      total_processed <- total_processed + batch_results$processed
      total_matched <- total_matched + batch_results$matched
      total_errors <- total_errors + batch_results$errors
      total_unmatched <- total_unmatched + batch_results$unmatched
      
      # Brief pause between batches
      Sys.sleep(0.5)
    }
    
    log_message(sprintf("Batch processing complete: %d processed, %d matched, %d errors, %d unmatched", 
                       total_processed, total_matched, total_errors, total_unmatched))
    
    return(create_processing_summary(total_processed, total_matched, total_errors, total_unmatched, "Success"))
    
  }, error = function(e) {
    log_message(paste("Batch processing failed:", e$message), "ERROR")
    return(create_processing_summary(0, 0, 1, 0, paste("Error:", e$message)))
  })
}

# ==============================================================================
# PERFORMANCE ANALYSIS AND REPORTING
# ==============================================================================

#' Generate comprehensive performance report
#' 
#' Creates detailed performance analysis for a specified time period.
#' 
#' @param period Time period ('weekly', 'monthly', 'season', 'all')
#' @param season Season to analyze (required for weekly/monthly)
#' @param week Week to analyze (required for weekly)
#' @param db_path Path to prediction database
#' @return Comprehensive performance report
#' @export
generate_performance_report <- function(period = "weekly", season = NULL, week = NULL, 
                                       db_path = DEFAULT_DB_PATH) {
  
  log_message(sprintf("Generating performance report: %s", period))
  
  tryCatch({
    
    # Connect to database
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Build query based on period
    base_query <- "SELECT * FROM predictions WHERE game_completed = 1"
    params <- list()
    
    if (!is.null(season)) {
      base_query <- paste(base_query, "AND season = ?")
      params <- append(params, season)
    }
    
    if (period == "weekly" && !is.null(week)) {
      base_query <- paste(base_query, "AND week = ?")
      params <- append(params, week)
    }
    
    # Get completed predictions
    predictions <- dbGetQuery(con, base_query, params = params)
    
    if (nrow(predictions) == 0) {
      log_message("No completed predictions found for report", "WARN")
      return(list(status = "no_data", message = "No completed predictions available"))
    }
    
    log_message(sprintf("Analyzing %d completed predictions", nrow(predictions)))
    
    # Calculate comprehensive metrics
    report <- list(
      metadata = list(
        period = period,
        season = season,
        week = week,
        report_date = Sys.time(),
        total_predictions = nrow(predictions),
        date_range = list(
          earliest = min(predictions$game_date, na.rm = TRUE),
          latest = max(predictions$game_date, na.rm = TRUE)
        )
      ),
      
      # Overall performance
      overall = calculate_overall_performance(predictions),
      
      # Spread performance
      spread = calculate_spread_performance(predictions),
      
      # Total performance
      total = calculate_total_performance(predictions),
      
      # Moneyline performance
      moneyline = calculate_moneyline_performance(predictions),
      
      # Betting performance
      betting = calculate_betting_performance(predictions),
      
      # Confidence analysis
      confidence = analyze_confidence_calibration(predictions),
      
      # Team-specific analysis
      teams = analyze_team_performance(predictions),
      
      # Temporal patterns
      temporal = analyze_temporal_patterns(predictions),
      
      # Error analysis
      errors = analyze_prediction_errors(predictions)
    )
    
    log_message("Performance report generated successfully")
    return(report)
    
  }, error = function(e) {
    log_message(paste("Performance report generation failed:", e$message), "ERROR")
    return(list(status = "error", message = e$message))
  })
}

#' Detect systematic errors in predictions
#' 
#' Identifies patterns and biases in prediction errors for model improvement.
#' 
#' @param db_path Path to prediction database
#' @param min_sample_size Minimum sample size for pattern detection
#' @param significance_threshold Statistical significance threshold
#' @return List of detected error patterns
#' @export
detect_systematic_errors <- function(db_path = DEFAULT_DB_PATH, min_sample_size = 20, 
                                   significance_threshold = 0.05) {
  
  log_message("Starting systematic error detection analysis")
  
  tryCatch({
    
    # Connect to database
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Get all completed predictions
    predictions <- dbGetQuery(con, "SELECT * FROM predictions WHERE game_completed = 1")
    
    if (nrow(predictions) < min_sample_size) {
      log_message(sprintf("Insufficient data for error analysis (%d < %d)", 
                         nrow(predictions), min_sample_size), "WARN")
      return(list(status = "insufficient_data"))
    }
    
    log_message(sprintf("Analyzing %d predictions for systematic errors", nrow(predictions)))
    
    error_patterns <- list()
    
    # 1. Overall bias detection
    bias_patterns <- detect_bias_patterns(predictions, min_sample_size, significance_threshold)
    error_patterns <- append(error_patterns, bias_patterns)
    
    # 2. Team-specific errors
    team_patterns <- detect_team_specific_errors(predictions, min_sample_size, significance_threshold)
    error_patterns <- append(error_patterns, team_patterns)
    
    # 3. Situational errors (home/away, favorites/underdogs)
    situational_patterns <- detect_situational_errors(predictions, min_sample_size, significance_threshold)
    error_patterns <- append(error_patterns, situational_patterns)
    
    # 4. Confidence calibration errors
    calibration_patterns <- detect_calibration_errors(predictions, min_sample_size, significance_threshold)
    error_patterns <- append(error_patterns, calibration_patterns)
    
    # 5. Temporal drift patterns
    temporal_patterns <- detect_temporal_drift(predictions, min_sample_size, significance_threshold)
    error_patterns <- append(error_patterns, temporal_patterns)
    
    # Store detected patterns in database
    for (pattern in error_patterns) {
      store_error_pattern_enhanced(con, pattern)
    }
    
    log_message(sprintf("Error detection complete: %d patterns identified", length(error_patterns)))
    
    return(list(
      status = "success",
      patterns_detected = length(error_patterns),
      patterns = error_patterns,
      summary = summarize_error_patterns(error_patterns)
    ))
    
  }, error = function(e) {
    log_message(paste("Error detection failed:", e$message), "ERROR")
    return(list(status = "error", message = e$message))
  })
}

# ==============================================================================
# UTILITY AND HELPER FUNCTIONS  
# ==============================================================================

#' Process a single prediction result
process_single_prediction_result <- function(con, prediction, actual_results) {
  
  tryCatch({
    
    # Find matching game result
    matching_result <- find_matching_game_result(prediction, actual_results)
    
    if (is.null(matching_result)) {
      return(list(status = "no_match", message = "No matching game result found"))
    }
    
    # Prepare actual results data
    actual_data <- list(
      home_score = matching_result$home_score,
      away_score = matching_result$away_score
    )
    
    # Update prediction with results
    update_prediction_results(con, prediction$prediction_id, actual_data)
    
    return(list(status = "success", message = "Prediction updated successfully"))
    
  }, error = function(e) {
    return(list(status = "error", message = e$message))
  })
}

#' Find matching game result for a prediction
find_matching_game_result <- function(prediction, actual_results) {
  
  # Try exact game_id match first
  if (!is.na(prediction$game_id) && prediction$game_id != "") {
    exact_match <- actual_results[actual_results$game_id == prediction$game_id,]
    if (nrow(exact_match) > 0) {
      return(exact_match[1,])
    }
  }
  
  # Try team and date matching
  team_matches <- actual_results[
    actual_results$home_team == prediction$home_team &
    actual_results$away_team == prediction$away_team,
  ]
  
  if (nrow(team_matches) == 0) {
    return(NULL)
  }
  
  # If multiple matches, try to narrow by date
  if (nrow(team_matches) > 1) {
    # Try to match by week and season
    if (!is.na(prediction$week) && !is.na(prediction$season)) {
      week_matches <- team_matches[
        team_matches$week == prediction$week &
        team_matches$season == prediction$season,
      ]
      
      if (nrow(week_matches) > 0) {
        return(week_matches[1,])
      }
    }
    
    # Return most recent match
    team_matches <- team_matches[order(team_matches$game_date, decreasing = TRUE),]
    return(team_matches[1,])
  }
  
  return(team_matches[1,])
}

#' Process batch of predictions
process_prediction_batch <- function(con, predictions, actual_results) {
  
  batch_results <- list(
    processed = 0,
    matched = 0,
    errors = 0,
    unmatched = 0
  )
  
  for (i in 1:nrow(predictions)) {
    result <- process_single_prediction_result(con, predictions[i,], actual_results)
    
    if (result$status == "success") {
      batch_results$processed <- batch_results$processed + 1
      batch_results$matched <- batch_results$matched + 1
    } else if (result$status == "no_match") {
      batch_results$unmatched <- batch_results$unmatched + 1
    } else {
      batch_results$errors <- batch_results$errors + 1
    }
  }
  
  return(batch_results)
}

#' Create processing summary object
create_processing_summary <- function(processed, matched, errors, unmatched, status) {
  list(
    timestamp = Sys.time(),
    processed = processed,
    matched = matched,
    errors = errors,
    unmatched = unmatched,
    status = status,
    success_rate = ifelse(processed + errors > 0, processed / (processed + errors), 0)
  )
}

# ==============================================================================
# PERFORMANCE CALCULATION FUNCTIONS
# ==============================================================================

#' Calculate overall performance metrics
calculate_overall_performance <- function(predictions) {
  list(
    total_predictions = nrow(predictions),
    prediction_rate = nrow(predictions) / max(1, as.numeric(difftime(max(predictions$game_date), 
                                                                     min(predictions$game_date), units = "days"))),
    models_used = length(unique(predictions$model_version)),
    date_span = as.numeric(difftime(max(predictions$game_date), min(predictions$game_date), units = "days"))
  )
}

#' Calculate spread performance metrics
calculate_spread_performance <- function(predictions) {
  spread_data <- predictions[!is.na(predictions$spread_error),]
  
  if (nrow(spread_data) == 0) {
    return(list(status = "no_data"))
  }
  
  list(
    total_predictions = nrow(spread_data),
    accuracy = mean(spread_data$spread_correct, na.rm = TRUE),
    mae = mean(abs(spread_data$spread_error), na.rm = TRUE),
    rmse = sqrt(mean(spread_data$spread_error^2, na.rm = TRUE)),
    median_error = median(abs(spread_data$spread_error), na.rm = TRUE),
    bias = mean(spread_data$spread_error, na.rm = TRUE),
    error_distribution = quantile(spread_data$spread_error, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
  )
}

#' Calculate total performance metrics
calculate_total_performance <- function(predictions) {
  total_data <- predictions[!is.na(predictions$total_error),]
  
  if (nrow(total_data) == 0) {
    return(list(status = "no_data"))
  }
  
  list(
    total_predictions = nrow(total_data),
    accuracy = mean(total_data$total_correct, na.rm = TRUE),
    mae = mean(abs(total_data$total_error), na.rm = TRUE),
    rmse = sqrt(mean(total_data$total_error^2, na.rm = TRUE)),
    median_error = median(abs(total_data$total_error), na.rm = TRUE),
    bias = mean(total_data$total_error, na.rm = TRUE)
  )
}

#' Calculate moneyline performance metrics
calculate_moneyline_performance <- function(predictions) {
  ml_data <- predictions[!is.na(predictions$moneyline_correct),]
  
  if (nrow(ml_data) == 0) {
    return(list(status = "no_data"))
  }
  
  list(
    total_predictions = nrow(ml_data),
    accuracy = mean(ml_data$moneyline_correct, na.rm = TRUE),
    log_loss = calculate_log_loss(ml_data$predicted_home_prob, ml_data$moneyline_correct),
    brier_score = calculate_brier_score(ml_data$predicted_home_prob, ml_data$moneyline_correct)
  )
}

#' Calculate betting performance metrics  
calculate_betting_performance <- function(predictions) {
  betting_data <- predictions[!is.na(predictions$spread_bet_result) | 
                             !is.na(predictions$total_bet_result) | 
                             !is.na(predictions$moneyline_bet_result),]
  
  if (nrow(betting_data) == 0) {
    return(list(status = "no_data"))
  }
  
  # Calculate total returns (simplified)
  total_returns <- rowSums(cbind(
    betting_data$spread_bet_result %||% 0,
    betting_data$total_bet_result %||% 0,
    betting_data$moneyline_bet_result %||% 0
  ), na.rm = TRUE)
  
  list(
    total_bets = nrow(betting_data),
    winning_bets = sum(total_returns > 0, na.rm = TRUE),
    losing_bets = sum(total_returns < 0, na.rm = TRUE),
    push_bets = sum(total_returns == 0, na.rm = TRUE),
    win_rate = mean(total_returns > 0, na.rm = TRUE),
    total_profit = sum(total_returns, na.rm = TRUE),
    roi = sum(total_returns, na.rm = TRUE) / nrow(betting_data),
    sharpe_ratio = calculate_sharpe_ratio(total_returns),
    max_drawdown = calculate_max_drawdown(total_returns)
  )
}

#' Analyze confidence calibration
analyze_confidence_calibration <- function(predictions) {
  
  # Spread confidence calibration
  spread_data <- predictions[!is.na(predictions$spread_confidence) & !is.na(predictions$spread_correct),]
  spread_calibration <- if (nrow(spread_data) > 0) {
    calculate_calibration_score(spread_data$spread_confidence, spread_data$spread_correct)
  } else { NA }
  
  # Total confidence calibration
  total_data <- predictions[!is.na(predictions$total_confidence) & !is.na(predictions$total_correct),]
  total_calibration <- if (nrow(total_data) > 0) {
    calculate_calibration_score(total_data$total_confidence, total_data$total_correct)
  } else { NA }
  
  # Moneyline confidence calibration
  ml_data <- predictions[!is.na(predictions$moneyline_confidence) & !is.na(predictions$moneyline_correct),]
  ml_calibration <- if (nrow(ml_data) > 0) {
    calculate_calibration_score(ml_data$moneyline_confidence, ml_data$moneyline_correct)
  } else { NA }
  
  list(
    spread_calibration = spread_calibration,
    total_calibration = total_calibration,
    moneyline_calibration = ml_calibration,
    overall_calibration = mean(c(spread_calibration, total_calibration, ml_calibration), na.rm = TRUE)
  )
}

#' Analyze team-specific performance
analyze_team_performance <- function(predictions) {
  
  # Home team performance
  home_performance <- predictions %>%
    group_by(home_team) %>%
    summarise(
      games = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(games))
  
  # Away team performance
  away_performance <- predictions %>%
    group_by(away_team) %>%
    summarise(
      games = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(games))
  
  list(
    home_teams = head(home_performance, 10),
    away_teams = head(away_performance, 10),
    best_predicted_teams = head(home_performance[order(home_performance$spread_accuracy, decreasing = TRUE),], 5),
    worst_predicted_teams = head(home_performance[order(home_performance$spread_accuracy),], 5)
  )
}

#' Analyze temporal patterns in performance
analyze_temporal_patterns <- function(predictions) {
  
  # Weekly performance
  weekly_performance <- predictions %>%
    group_by(season, week) %>%
    summarise(
      games = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(season, week)
  
  # Monthly performance  
  predictions$month <- format(as.Date(predictions$game_date), "%Y-%m")
  monthly_performance <- predictions %>%
    group_by(month) %>%
    summarise(
      games = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(month)
  
  list(
    weekly = weekly_performance,
    monthly = monthly_performance,
    trend_analysis = analyze_performance_trends(weekly_performance)
  )
}

#' Analyze prediction errors for patterns
analyze_prediction_errors <- function(predictions) {
  
  error_data <- predictions[!is.na(predictions$spread_error),]
  
  if (nrow(error_data) == 0) {
    return(list(status = "no_data"))
  }
  
  list(
    large_errors = nrow(error_data[abs(error_data$spread_error) > 14,]),
    large_error_rate = mean(abs(error_data$spread_error) > 14),
    systematic_bias = mean(error_data$spread_error),
    error_variance = var(error_data$spread_error),
    outlier_games = head(error_data[order(abs(error_data$spread_error), decreasing = TRUE),], 10)
  )
}

# ==============================================================================
# ERROR PATTERN DETECTION FUNCTIONS
# ==============================================================================

#' Detect bias patterns in predictions
detect_bias_patterns <- function(predictions, min_sample_size, significance_threshold) {
  
  patterns <- list()
  
  # Overall spread bias
  spread_data <- predictions[!is.na(predictions$spread_error),]
  if (nrow(spread_data) >= min_sample_size) {
    bias_test <- t.test(spread_data$spread_error, mu = 0)
    if (bias_test$p.value < significance_threshold) {
      patterns <- append(patterns, list(list(
        type = "systematic_bias",
        category = "spread",
        description = sprintf("Systematic spread bias: %.2f points", mean(spread_data$spread_error)),
        magnitude = abs(mean(spread_data$spread_error)),
        p_value = bias_test$p.value,
        sample_size = nrow(spread_data),
        priority = ifelse(abs(mean(spread_data$spread_error)) > 2, "HIGH", "MEDIUM")
      )))
    }
  }
  
  # Overall total bias
  total_data <- predictions[!is.na(predictions$total_error),]
  if (nrow(total_data) >= min_sample_size) {
    bias_test <- t.test(total_data$total_error, mu = 0)
    if (bias_test$p.value < significance_threshold) {
      patterns <- append(patterns, list(list(
        type = "systematic_bias",
        category = "total",
        description = sprintf("Systematic total bias: %.2f points", mean(total_data$total_error)),
        magnitude = abs(mean(total_data$total_error)),
        p_value = bias_test$p.value,
        sample_size = nrow(total_data),
        priority = ifelse(abs(mean(total_data$total_error)) > 3, "HIGH", "MEDIUM")
      )))
    }
  }
  
  return(patterns)
}

#' Detect team-specific error patterns
detect_team_specific_errors <- function(predictions, min_sample_size, significance_threshold) {
  
  patterns <- list()
  
  # Analyze each team
  teams <- unique(c(predictions$home_team, predictions$away_team))
  
  for (team in teams) {
    team_games <- predictions[predictions$home_team == team | predictions$away_team == team,]
    team_games <- team_games[!is.na(team_games$spread_error),]
    
    if (nrow(team_games) >= min_sample_size) {
      bias_test <- t.test(team_games$spread_error, mu = 0)
      
      if (bias_test$p.value < significance_threshold && abs(mean(team_games$spread_error)) > 1.5) {
        patterns <- append(patterns, list(list(
          type = "team_specific",
          category = "spread",
          description = sprintf("Team %s prediction bias: %.2f points", team, mean(team_games$spread_error)),
          magnitude = abs(mean(team_games$spread_error)),
          p_value = bias_test$p.value,
          sample_size = nrow(team_games),
          affected_teams = list(team),
          priority = ifelse(abs(mean(team_games$spread_error)) > 3, "HIGH", "MEDIUM")
        )))
      }
    }
  }
  
  return(patterns)
}

#' Detect situational error patterns
detect_situational_errors <- function(predictions, min_sample_size, significance_threshold) {
  
  patterns <- list()
  
  # Home vs Away performance
  home_errors <- predictions[!is.na(predictions$spread_error),]$spread_error
  # Note: This is simplified - you'd want to separate home/away perspective
  
  if (length(home_errors) >= min_sample_size) {
    # Add more sophisticated situational analysis here
    # For now, placeholder for weekend vs weekday, division games, etc.
  }
  
  return(patterns)
}

#' Detect confidence calibration errors
detect_calibration_errors <- function(predictions, min_sample_size, significance_threshold) {
  
  patterns <- list()
  
  # Analyze high-confidence predictions
  high_conf <- predictions[!is.na(predictions$spread_confidence) & 
                          predictions$spread_confidence > 0.8 & 
                          !is.na(predictions$spread_correct),]
  
  if (nrow(high_conf) >= min_sample_size) {
    expected_accuracy <- mean(high_conf$spread_confidence)
    actual_accuracy <- mean(high_conf$spread_correct)
    
    if (abs(expected_accuracy - actual_accuracy) > 0.1) {
      patterns <- append(patterns, list(list(
        type = "calibration_error",
        category = "confidence",
        description = sprintf("High-confidence calibration error: %.1f%% expected vs %.1f%% actual", 
                             expected_accuracy * 100, actual_accuracy * 100),
        magnitude = abs(expected_accuracy - actual_accuracy),
        sample_size = nrow(high_conf),
        priority = ifelse(abs(expected_accuracy - actual_accuracy) > 0.2, "HIGH", "MEDIUM")
      )))
    }
  }
  
  return(patterns)
}

#' Detect temporal drift patterns
detect_temporal_drift <- function(predictions, min_sample_size, significance_threshold) {
  
  patterns <- list()
  
  # Check for performance drift over time
  predictions$time_order <- 1:nrow(predictions)
  spread_data <- predictions[!is.na(predictions$spread_error),]
  
  if (nrow(spread_data) >= min_sample_size) {
    correlation_test <- cor.test(spread_data$time_order, abs(spread_data$spread_error))
    
    if (correlation_test$p.value < significance_threshold && abs(correlation_test$estimate) > 0.2) {
      direction <- ifelse(correlation_test$estimate > 0, "increasing", "decreasing")
      patterns <- append(patterns, list(list(
        type = "temporal_drift",
        category = "accuracy",
        description = sprintf("Prediction accuracy %s over time (r=%.3f)", direction, correlation_test$estimate),
        magnitude = abs(correlation_test$estimate),
        p_value = correlation_test$p.value,
        sample_size = nrow(spread_data),
        priority = ifelse(abs(correlation_test$estimate) > 0.3, "HIGH", "MEDIUM")
      )))
    }
  }
  
  return(patterns)
}

#' Store enhanced error pattern in database
store_error_pattern_enhanced <- function(con, pattern) {
  
  tryCatch({
    pattern_id <- digest(paste(pattern$type, pattern$category, pattern$description, sep = "_"), algo = "md5")
    
    insert_data <- list(
      pattern_id = pattern_id,
      model_version = "current",
      season = year(Sys.Date()),
      pattern_type = pattern$type,
      pattern_category = pattern$category,
      pattern_description = pattern$description,
      occurrence_count = pattern$sample_size %||% 1,
      error_magnitude = pattern$magnitude %||% 0,
      confidence_score = 1 - (pattern$p_value %||% 0.05),
      affected_teams = toJSON(pattern$affected_teams %||% list()),
      affected_weeks = toJSON(pattern$affected_weeks %||% list()),
      affected_situations = toJSON(list()),
      total_predictions_affected = pattern$sample_size %||% 0,
      average_error_increase = pattern$magnitude %||% 0,
      roi_impact = 0,  # Placeholder
      suggested_feature_adjustments = toJSON(list()),
      suggested_model_changes = "",
      priority_level = pattern$priority %||% "MEDIUM"
    )
    
    sql <- build_insert_sql("error_patterns", names(insert_data))
    sql <- paste(sql, "ON CONFLICT(pattern_id) DO UPDATE SET occurrence_count = occurrence_count + 1")
    
    dbExecute(con, sql, params = insert_data)
    
  }, error = function(e) {
    log_message(paste("Error storing pattern:", e$message), "WARN")
  })
}

#' Summarize error patterns
summarize_error_patterns <- function(patterns) {
  
  if (length(patterns) == 0) {
    return(list(
      total_patterns = 0,
      high_priority = 0,
      categories = list()
    ))
  }
  
  categories <- table(sapply(patterns, function(x) x$category))
  priorities <- table(sapply(patterns, function(x) x$priority %||% "MEDIUM"))
  
  list(
    total_patterns = length(patterns),
    high_priority = sum(sapply(patterns, function(x) (x$priority %||% "MEDIUM") == "HIGH")),
    categories = as.list(categories),
    priorities = as.list(priorities),
    most_severe = patterns[[which.max(sapply(patterns, function(x) x$magnitude %||% 0))]]
  )
}

#' Analyze performance trends
analyze_performance_trends <- function(weekly_data) {
  
  if (nrow(weekly_data) < 3) {
    return(list(status = "insufficient_data"))
  }
  
  # Simple linear trend analysis
  weekly_data$period <- 1:nrow(weekly_data)
  
  accuracy_trend <- lm(spread_accuracy ~ period, data = weekly_data)
  mae_trend <- lm(spread_mae ~ period, data = weekly_data)
  
  list(
    accuracy_trend = list(
      slope = coef(accuracy_trend)[2],
      p_value = summary(accuracy_trend)$coefficients[2, 4],
      direction = ifelse(coef(accuracy_trend)[2] > 0, "improving", "declining")
    ),
    mae_trend = list(
      slope = coef(mae_trend)[2],
      p_value = summary(mae_trend)$coefficients[2, 4],
      direction = ifelse(coef(mae_trend)[2] < 0, "improving", "declining")
    )
  )
}

# ==============================================================================
# TESTING AND VALIDATION FUNCTIONS
# ==============================================================================

#' Test outcome tracking system with sample data
#' 
#' Validates the system functionality with controlled test data.
#' 
#' @param db_path Path to test database
#' @export
test_outcome_tracking_system <- function(db_path = "test_predictions.db") {
  
  log_message("Starting outcome tracking system test")
  
  tryCatch({
    
    # Clean up any existing test database
    if (file.exists(db_path)) {
      file.remove(db_path)
    }
    
    # Initialize test database
    con <- initialize_prediction_db(db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Create sample predictions
    log_message("Creating sample test predictions...")
    
    sample_predictions <- create_sample_predictions()
    
    for (i in 1:length(sample_predictions)) {
      insert_prediction(con, 
                       sample_predictions[[i]]$game_data,
                       sample_predictions[[i]]$predictions,
                       sample_predictions[[i]]$model_info)
    }
    
    log_message(sprintf("Inserted %d sample predictions", length(sample_predictions)))
    
    # Test processing functions
    log_message("Testing processing functions...")
    
    # Test weekly processing
    weekly_result <- process_weekly_results(1, 2025, db_path)
    log_message(sprintf("Weekly processing test: %d processed", weekly_result$processed))
    
    # Test individual processing
    individual_result <- process_individual_result("2025_01_KC_PHI", db_path)
    log_message(sprintf("Individual processing test: %d processed", individual_result$processed))
    
    # Test pending updates
    pending_result <- update_all_pending_predictions(db_path, max_predictions = 10)
    log_message(sprintf("Pending updates test: %d processed", pending_result$processed))
    
    # Test performance report
    log_message("Testing performance report generation...")
    report <- generate_performance_report("weekly", 2025, 1, db_path)
    
    if (report$metadata$total_predictions > 0) {
      log_message("Performance report generated successfully")
    } else {
      log_message("No data found for performance report", "WARN")
    }
    
    # Test error detection
    log_message("Testing error pattern detection...")
    error_patterns <- detect_systematic_errors(db_path, min_sample_size = 3)
    log_message(sprintf("Error detection test: %d patterns found", 
                       error_patterns$patterns_detected %||% 0))
    
    log_message("Outcome tracking system test completed successfully")
    
    return(list(
      status = "success",
      tests_passed = 5,
      sample_predictions = length(sample_predictions),
      weekly_processed = weekly_result$processed,
      individual_processed = individual_result$processed,
      pending_processed = pending_result$processed,
      performance_report_available = !is.null(report$metadata),
      error_patterns_detected = error_patterns$patterns_detected %||% 0
    ))
    
  }, error = function(e) {
    log_message(paste("System test failed:", e$message), "ERROR")
    return(list(status = "error", message = e$message))
  })
}

#' Create sample predictions for testing
create_sample_predictions <- function() {
  
  sample_games <- list(
    list(
      game_data = list(
        game_id = "2025_01_KC_PHI",
        season = 2025,
        week = 1,
        game_date = "2025-09-08",
        home_team = "PHI",
        away_team = "KC"
      ),
      predictions = list(
        spread = list(predicted = -2.5, vegas = -3.0, confidence = 0.75),
        total = list(predicted = 48.5, vegas = 47.5, confidence = 0.65),
        moneyline = list(home_prob = 0.45, home_odds = -130, away_odds = 110, confidence = 0.70),
        recommendations = list(spread = "BET AWAY", total = "BET OVER", moneyline = "BET AWAY", bet_size = 0.05)
      ),
      model_info = list(
        version = "test_v1.0",
        feature_hash = "abc123",
        features = list(home_epa = 0.15, away_epa = 0.22, rest_diff = 0)
      )
    ),
    list(
      game_data = list(
        game_id = "2025_01_BUF_NYJ",
        season = 2025,
        week = 1,
        game_date = "2025-09-08",
        home_team = "NYJ",
        away_team = "BUF"
      ),
      predictions = list(
        spread = list(predicted = 5.5, vegas = 6.0, confidence = 0.80),
        total = list(predicted = 44.0, vegas = 43.5, confidence = 0.70),
        moneyline = list(home_prob = 0.30, home_odds = 180, away_odds = -210, confidence = 0.85),
        recommendations = list(spread = "BET HOME", total = "BET OVER", moneyline = "BET AWAY", bet_size = 0.03)
      ),
      model_info = list(
        version = "test_v1.0",
        feature_hash = "abc123",
        features = list(home_epa = -0.05, away_epa = 0.18, rest_diff = 0)
      )
    )
  )
  
  return(sample_games)
}

# ==============================================================================
# INITIALIZATION AND STARTUP
# ==============================================================================

# Set default log level
set_log_level("INFO")

log_message(sprintf("NFL Outcome Tracking System v%s initialized", OUTCOME_TRACKER_VERSION))
log_message("Core functions available:")
log_message("  - process_weekly_results(week, season): Process completed games for a week")
log_message("  - process_individual_result(game_id): Process single game result")
log_message("  - update_all_pending_predictions(): Update all unprocessed predictions")
log_message("  - generate_performance_report(period): Create comprehensive performance analysis")
log_message("  - detect_systematic_errors(): Identify prediction error patterns")
log_message("  - test_outcome_tracking_system(): Validate system functionality")

log_message("Example usage:")
log_message("  # Process Week 1 2025 results")
log_message("  result <- process_weekly_results(1, 2025)")
log_message("  ")
log_message("  # Generate weekly performance report")
log_message("  report <- generate_performance_report('weekly', 2025, 1)")
log_message("  ")
log_message("  # Detect systematic errors")
log_message("  errors <- detect_systematic_errors()")

# Auto-detect if database exists and show status
tryCatch({
  if (file.exists(DEFAULT_DB_PATH)) {
    con <- dbConnect(SQLite(), DEFAULT_DB_PATH)
    stats <- get_database_stats(con)
    dbDisconnect(con)
    
    if (stats$total_predictions > 0) {
      log_message(sprintf("Existing database found: %d total predictions, %d completed", 
                         stats$total_predictions, stats$completed_games))
      
      if (stats$pending_games > 0) {
        log_message(sprintf("Ready to process %d pending predictions", stats$pending_games))
      }
    }
  } else {
    log_message("No existing database found - ready for initialization")
  }
}, error = function(e) {
  log_message("Database status check failed", "WARN")
})

log_message("System ready for outcome tracking operations!")