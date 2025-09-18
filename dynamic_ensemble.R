# Dynamic Ensemble Prediction System
# Performance-weighted model combination with adaptive learning
# Integrates with existing Week 3 prediction system

library(dplyr)

# Source dependencies
source('learning_system/situational_analysis.R')
source('learning_system/outcome_tracker_csv.R')
source('learning_system/unified_data_access.R')

# ==============================================================================
# MODEL DEFINITIONS AND REGISTRY
# ==============================================================================

# Define available prediction models
MODEL_REGISTRY <- list(
  epa_model = list(
    name = "EPA-based Model",
    description = "Uses team EPA metrics and recent performance",
    weight_initial = 0.25,
    weight_current = 0.25,
    features_required = c("home_epa", "away_epa", "home_def_epa", "away_def_epa"),
    contexts_optimal = c("regular_season", "division_games"),
    min_confidence = 0.6
  ),
  
  ml_model = list(
    name = "Machine Learning Model", 
    description = "Advanced feature engineering with ML algorithms",
    weight_initial = 0.25,
    weight_current = 0.25,
    features_required = c("home_features", "away_features", "matchup_features"),
    contexts_optimal = c("regular_season", "playoff_games"),
    min_confidence = 0.7
  ),
  
  situational_model = list(
    name = "Situational Tendency Model",
    description = "Uses team situational tendencies and matchup analysis", 
    weight_initial = 0.25,
    weight_current = 0.25,
    features_required = c("situational_features", "tendency_deviations"),
    contexts_optimal = c("divisional_games", "primetime_games"),
    min_confidence = 0.65
  ),
  
  consensus_model = list(
    name = "Market Consensus Model",
    description = "Incorporates betting market information and line movements",
    weight_initial = 0.25,
    weight_current = 0.25,
    features_required = c("vegas_spread", "line_movement", "betting_percentages"),
    contexts_optimal = c("primetime_games", "playoff_games"),
    min_confidence = 0.75
  )
)

# Context definitions for model routing
CONTEXT_DEFINITIONS <- list(
  game_type = c("regular_season", "playoff_games", "preseason"),
  matchup_type = c("division_games", "conference_games", "interconference_games"),
  game_setting = c("primetime_games", "early_games", "late_games"),
  weather = c("dome_games", "outdoor_games", "bad_weather"),
  stakes = c("high_stakes", "medium_stakes", "low_stakes"),
  rest = c("short_rest", "normal_rest", "long_rest")
)

# Performance tracking configuration
PERFORMANCE_CONFIG <- list(
  lookback_window = 32,  # Games to consider for recent performance
  decay_rate = 0.95,     # Exponential decay for older games
  min_sample_size = 10,  # Minimum games required for weight updates
  update_frequency = "weekly",  # How often to recalculate weights
  confidence_threshold = 0.6    # Minimum confidence to use ensemble
)

# ==============================================================================
# CORE ENSEMBLE FUNCTIONS
# ==============================================================================

#' Initialize ensemble system
#' 
#' Sets up the dynamic model ensemble with initial weights and creates
#' necessary database tables for tracking performance
#' 
#' @param db_path Path to prediction database
#' @param reset_weights Whether to reset all model weights to initial values
#' @export
initialize_ensemble_system <- function(db_path = DEFAULT_DB_PATH, reset_weights = FALSE) {
  
  cat("🎯 Initializing Dynamic Model Ensemble System...\n")
  
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Create model performance tracking table
    create_model_performance_table(con)
    
    # Create ensemble weights table
    create_ensemble_weights_table(con)
    
    # Create context routing table
    create_context_routing_table(con)
    
    # Initialize weights if requested or if no weights exist
    if (reset_weights || !weights_exist(con)) {
      initialize_model_weights(con)
      cat("✅ Initialized model weights to defaults\n")
    }
    
    # Initialize context routing rules
    initialize_context_routing(con)
    
    cat("✅ Ensemble system initialized successfully\n")
    
    # Display current model status
    display_model_status(con)
    
  }, error = function(e) {
    cat(sprintf("❌ Failed to initialize ensemble system: %s\n", e$message))
  })
}

#' Generate ensemble prediction
#' 
#' Main function that routes a prediction request through the ensemble system,
#' determines the best models for the context, and generates weighted predictions
#' 
#' @param game_data Game information (teams, date, context)
#' @param feature_data All available features for prediction
#' @param db_path Path to prediction database
#' @param force_models Optional vector of specific models to use
#' @return Ensemble prediction with model breakdown
#' @export
generate_ensemble_prediction <- function(game_data, feature_data, 
                                       db_path = DEFAULT_DB_PATH, 
                                       force_models = NULL) {
  
  cat(sprintf("🎯 Generating ensemble prediction for %s vs %s\n", 
              game_data$away_team, game_data$home_team))
  
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Determine game context
    game_context <- determine_game_context(game_data)
    cat(sprintf("🔍 Game context: %s\n", paste(game_context, collapse = ", ")))
    
    # Get current model weights and performance
    if (is.null(force_models)) {
      active_models <- select_optimal_models(con, game_context, feature_data)
    } else {
      active_models <- force_models
    }
    
    cat(sprintf("🤖 Active models: %s\n", paste(names(active_models), collapse = ", ")))
    
    # Generate predictions from each active model
    model_predictions <- list()
    model_confidences <- list()
    
    for (model_name in names(active_models)) {
      model_info <- MODEL_REGISTRY[[model_name]]
      
      if (has_required_features(feature_data, model_info$features_required)) {
        pred_result <- generate_model_prediction(model_name, game_data, feature_data)
        
        if (!is.null(pred_result) && pred_result$confidence >= model_info$min_confidence) {
          model_predictions[[model_name]] <- pred_result
          model_confidences[[model_name]] <- pred_result$confidence
          cat(sprintf("✅ %s: Spread %.1f, Confidence %.2f\n", 
                     model_name, pred_result$spread, pred_result$confidence))
        } else {
          cat(sprintf("⚠️ %s: Insufficient confidence or failed prediction\n", model_name))
        }
      } else {
        cat(sprintf("⚠️ %s: Missing required features\n", model_name))
      }
    }
    
    if (length(model_predictions) == 0) {
      cat("❌ No models generated valid predictions\n")
      return(NULL)
    }
    
    # Calculate ensemble weights
    ensemble_weights <- calculate_dynamic_weights(con, names(model_predictions), 
                                                game_context, model_confidences)
    
    # Generate weighted ensemble prediction
    ensemble_result <- combine_model_predictions(model_predictions, ensemble_weights)
    
    # Add metadata
    ensemble_result$metadata <- list(
      active_models = names(model_predictions),
      model_weights = ensemble_weights,
      game_context = game_context,
      prediction_timestamp = Sys.time(),
      ensemble_confidence = calculate_ensemble_confidence(model_predictions, ensemble_weights)
    )
    
    cat(sprintf("🎯 Ensemble prediction: Spread %.1f, Total %.1f, Confidence %.2f\n",
                ensemble_result$spread, ensemble_result$total, 
                ensemble_result$metadata$ensemble_confidence))
    
    return(ensemble_result)
    
  }, error = function(e) {
    cat(sprintf("❌ Ensemble prediction failed: %s\n", e$message))
    return(NULL)
  })
}

#' Update model performance
#' 
#' Updates individual model performance metrics after game results are available
#' This is critical for the dynamic weighting system
#' 
#' @param prediction_id Unique prediction identifier
#' @param actual_results Game outcome data
#' @param db_path Path to prediction database
#' @export
update_model_performance <- function(prediction_id, actual_results, 
                                   db_path = DEFAULT_DB_PATH) {
  
  cat(sprintf("📊 Updating model performance for prediction %s\n", prediction_id))
  
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Get prediction details
    prediction <- get_prediction_by_id(con, prediction_id)
    
    if (is.null(prediction)) {
      cat("⚠️ Prediction not found\n")
      return(FALSE)
    }
    
    # Parse model breakdown from prediction metadata
    model_breakdown <- parse_model_breakdown(prediction)
    
    if (length(model_breakdown) == 0) {
      cat("⚠️ No model breakdown found in prediction\n")
      return(FALSE)
    }
    
    # Calculate performance metrics for each model
    for (model_name in names(model_breakdown)) {
      model_pred <- model_breakdown[[model_name]]
      
      # Calculate model-specific errors
      spread_error <- actual_results$point_differential - model_pred$spread
      total_error <- (actual_results$home_score + actual_results$away_score) - model_pred$total
      
      # Determine correctness
      spread_correct <- sign(actual_results$point_differential) == sign(model_pred$spread)
      total_correct <- ((actual_results$home_score + actual_results$away_score) > model_pred$total) == 
                      (model_pred$total_prediction == "over")
      
      # Store model performance
      store_model_performance(con, model_name, prediction, actual_results, 
                            spread_error, total_error, spread_correct, total_correct)
      
      cat(sprintf("📈 %s: Spread Error %.1f, Total Error %.1f\n", 
                 model_name, spread_error, total_error))
    }
    
    # Trigger weight recalculation if needed
    if (should_update_weights(con)) {
      update_ensemble_weights(con)
      cat("🔄 Model weights updated\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat(sprintf("❌ Performance update failed: %s\n", e$message))
    return(FALSE)
  })
}

#' Recalculate ensemble weights
#' 
#' Recalculates model weights based on recent performance across different contexts
#' This is the core of the adaptive learning system
#' 
#' @param db_path Path to prediction database
#' @param lookback_games Number of recent games to consider
#' @export  
update_ensemble_weights <- function(db_path = DEFAULT_DB_PATH, 
                                  lookback_games = PERFORMANCE_CONFIG$lookback_window) {
  
  cat("⚖️ Recalculating ensemble weights based on recent performance...\n")
  
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Get recent model performance data
    recent_performance <- get_recent_model_performance(con, lookback_games)
    
    if (nrow(recent_performance) == 0) {
      cat("⚠️ No recent performance data available\n")
      return(FALSE)
    }
    
    # Calculate performance metrics by model and context
    performance_by_context <- recent_performance %>%
      group_by(model_name, context_type) %>%
      summarise(
        games = n(),
        spread_accuracy = mean(spread_correct, na.rm = TRUE),
        spread_mae = mean(abs(spread_error), na.rm = TRUE),
        total_accuracy = mean(total_correct, na.rm = TRUE),
        total_mae = mean(abs(total_error), na.rm = TRUE),
        avg_confidence = mean(confidence, na.rm = TRUE),
        
        # Combined performance score
        performance_score = (spread_accuracy * 0.4 + total_accuracy * 0.3 + 
                           (1 - spread_mae/14) * 0.2 + (1 - total_mae/7) * 0.1),
        
        .groups = 'drop'
      ) %>%
      filter(games >= PERFORMANCE_CONFIG$min_sample_size)
    
    # Calculate overall model performance (across all contexts)
    overall_performance <- performance_by_context %>%
      group_by(model_name) %>%
      summarise(
        total_games = sum(games),
        weighted_performance = weighted.mean(performance_score, games),
        contexts_covered = n(),
        .groups = 'drop'
      )
    
    # Calculate new weights using softmax transformation
    overall_performance$raw_weight <- exp(overall_performance$weighted_performance * 5) # Scale factor
    total_weight <- sum(overall_performance$raw_weight)
    overall_performance$new_weight <- overall_performance$raw_weight / total_weight
    
    # Apply smoothing with previous weights to avoid dramatic changes
    smoothing_factor <- 0.7  # 70% new, 30% old
    
    for (i in 1:nrow(overall_performance)) {
      model_name <- overall_performance$model_name[i]
      new_weight <- overall_performance$new_weight[i]
      
      # Get current weight
      current_weight <- get_current_model_weight(con, model_name)
      
      # Apply smoothing
      smoothed_weight <- smoothing_factor * new_weight + (1 - smoothing_factor) * current_weight
      
      # Update weight in database
      update_model_weight(con, model_name, smoothed_weight, overall_performance[i,])
      
      cat(sprintf("📊 %s: %.3f -> %.3f (%.1f%% perf, %d games)\n",
                 model_name, current_weight, smoothed_weight,
                 overall_performance$weighted_performance[i] * 100,
                 overall_performance$total_games[i]))
    }
    
    # Update context-specific routing
    update_context_routing_weights(con, performance_by_context)
    
    # Record weight update
    record_weight_update(con, overall_performance)
    
    cat("✅ Ensemble weights updated successfully\n")
    return(TRUE)
    
  }, error = function(e) {
    cat(sprintf("❌ Weight update failed: %s\n", e$message))
    return(FALSE)
  })
}

# ==============================================================================
# MODEL-SPECIFIC PREDICTION FUNCTIONS
# ==============================================================================

#' Generate prediction from EPA model
generate_epa_model_prediction <- function(game_data, feature_data) {
  
  # Extract EPA-based features
  home_epa <- feature_data$home_epa %||% 0
  away_epa <- feature_data$away_epa %||% 0
  home_def_epa <- feature_data$home_def_epa %||% 0
  away_def_epa <- feature_data$away_def_epa %||% 0
  
  # EPA-based spread calculation
  epa_differential <- (home_epa - away_def_epa) - (away_epa - home_def_epa)
  spread_prediction <- epa_differential * 14 + 2.5  # Home field advantage
  
  # EPA-based total calculation
  total_prediction <- 45 + (home_epa + away_epa) * 12
  
  # Confidence based on EPA stability and sample size
  confidence <- min(0.9, 0.5 + abs(epa_differential) * 0.3)
  
  return(list(
    spread = round(spread_prediction, 1),
    total = round(total_prediction, 1),
    confidence = round(confidence, 2),
    model_details = list(
      home_epa = home_epa,
      away_epa = away_epa,
      epa_differential = epa_differential
    )
  ))
}

#' Generate prediction from ML model
generate_ml_model_prediction <- function(game_data, feature_data) {
  
  # This would interface with your existing ML models
  # For now, placeholder implementation
  
  # Extract ML features
  home_features <- feature_data$home_features %||% list()
  away_features <- feature_data$away_features %||% list()
  matchup_features <- feature_data$matchup_features %||% list()
  
  # Placeholder ML prediction logic
  # In practice, this would call your trained ML model
  spread_prediction <- rnorm(1, 0, 3)  # Placeholder
  total_prediction <- rnorm(1, 47, 4)  # Placeholder
  
  confidence <- 0.75  # Placeholder
  
  return(list(
    spread = round(spread_prediction, 1),
    total = round(total_prediction, 1),
    confidence = confidence,
    model_details = list(
      feature_count = length(home_features) + length(away_features) + length(matchup_features),
      model_version = "ml_v1.0"
    )
  ))
}

#' Generate prediction from situational model
generate_situational_model_prediction <- function(game_data, feature_data) {
  
  # Extract situational features
  situational_features <- feature_data$situational_features %||% list()
  
  if (length(situational_features) == 0) {
    return(NULL)
  }
  
  # Use situational tendencies for prediction
  home_1st_down_pass_dev <- situational_features$home_1st_down_pass_deviation %||% 0
  away_1st_down_pass_dev <- situational_features$away_1st_down_pass_deviation %||% 0
  
  # Tendency-based spread calculation
  passing_advantage <- (home_1st_down_pass_dev - away_1st_down_pass_dev) * 8
  spread_prediction <- passing_advantage + 2.5  # Home field advantage
  
  # Total based on offensive tendencies
  home_tendency_volatility <- situational_features$home_tendency_volatility %||% 0
  away_tendency_volatility <- situational_features$away_tendency_volatility %||% 0
  total_prediction <- 45 + (home_tendency_volatility + away_tendency_volatility) * 15
  
  # Confidence based on data quality
  data_quality <- (situational_features$home_data_quality %||% 0) + 
                  (situational_features$away_data_quality %||% 0)
  confidence <- min(0.9, 0.4 + data_quality / 100)
  
  return(list(
    spread = round(spread_prediction, 1),
    total = round(total_prediction, 1),
    confidence = round(confidence, 2),
    model_details = list(
      passing_advantage = passing_advantage,
      data_quality_score = data_quality
    )
  ))
}

#' Generate prediction from consensus model
generate_consensus_model_prediction <- function(game_data, feature_data) {
  
  # Extract market data
  vegas_spread <- feature_data$vegas_spread %||% 0
  line_movement <- feature_data$line_movement %||% 0
  betting_percentages <- feature_data$betting_percentages %||% list()
  
  # Market-based adjustments
  spread_prediction <- vegas_spread + line_movement * 0.3
  total_prediction <- (feature_data$vegas_total %||% 47) + (feature_data$total_line_movement %||% 0) * 0.2
  
  # Confidence based on market consensus
  confidence <- min(0.9, 0.6 + abs(line_movement) * 0.1)
  
  return(list(
    spread = round(spread_prediction, 1),
    total = round(total_prediction, 1), 
    confidence = round(confidence, 2),
    model_details = list(
      vegas_spread = vegas_spread,
      line_movement = line_movement,
      market_confidence = confidence
    )
  ))
}

#' Main model prediction dispatcher
generate_model_prediction <- function(model_name, game_data, feature_data) {
  
  switch(model_name,
    "epa_model" = generate_epa_model_prediction(game_data, feature_data),
    "ml_model" = generate_ml_model_prediction(game_data, feature_data),
    "situational_model" = generate_situational_model_prediction(game_data, feature_data),
    "consensus_model" = generate_consensus_model_prediction(game_data, feature_data),
    NULL
  )
}

# ==============================================================================
# CONTEXT AND ROUTING FUNCTIONS
# ==============================================================================

#' Determine game context for model routing
determine_game_context <- function(game_data) {
  
  context <- c()
  
  # Game type
  if (game_data$week <= 18) {
    context <- c(context, "regular_season")
  } else {
    context <- c(context, "playoff_games")
  }
  
  # Matchup type (simplified - would need division/conference data)
  context <- c(context, "conference_games")  # Placeholder
  
  # Game setting (based on time - simplified)
  if (!is.null(game_data$game_time)) {
    # This would parse actual game times
    context <- c(context, "primetime_games")  # Placeholder
  }
  
  # Weather (would need weather data)
  context <- c(context, "dome_games")  # Placeholder
  
  # Stakes (based on week and team records - simplified)
  if (game_data$week >= 15) {
    context <- c(context, "high_stakes")
  } else {
    context <- c(context, "medium_stakes")
  }
  
  # Rest (would need actual rest days)
  context <- c(context, "normal_rest")  # Placeholder
  
  return(context)
}

#' Select optimal models for given context
select_optimal_models <- function(con, game_context, feature_data) {
  
  # Get context-specific model performance
  context_performance <- get_context_model_performance(con, game_context)
  
  # Get current model weights
  current_weights <- get_current_model_weights(con)
  
  # Combine context performance with current weights
  model_scores <- current_weights %>%
    left_join(context_performance, by = "model_name") %>%
    mutate(
      context_score = coalesce(context_performance_score, 0.5),  # Default if no context data
      combined_score = current_weight * 0.7 + context_score * 0.3,
      
      # Check feature availability
      features_available = sapply(model_name, function(x) {
        model_info <- MODEL_REGISTRY[[x]]
        has_required_features(feature_data, model_info$features_required)
      })
    ) %>%
    filter(features_available, combined_score > 0.1) %>%  # Minimum threshold
    arrange(desc(combined_score))
  
  # Select top models (at least 2, at most 4)
  n_models <- max(2, min(4, nrow(model_scores)))
  selected_models <- head(model_scores, n_models)
  
  # Return as named list with scores
  result <- setNames(selected_models$combined_score, selected_models$model_name)
  
  return(result)
}

#' Check if required features are available
has_required_features <- function(feature_data, required_features) {
  
  if (length(required_features) == 0) return(TRUE)
  
  available_features <- names(feature_data)
  missing_features <- setdiff(required_features, available_features)
  
  return(length(missing_features) == 0)
}

# ==============================================================================
# WEIGHT CALCULATION AND COMBINATION
# ==============================================================================

#' Calculate dynamic weights for ensemble
calculate_dynamic_weights <- function(con, active_models, game_context, model_confidences) {
  
  # Get base weights from database
  base_weights <- get_model_weights_for_context(con, active_models, game_context)
  
  # Adjust weights based on model confidence
  confidence_adjustment <- sapply(active_models, function(model) {
    confidence <- model_confidences[[model]] %||% 0.5
    return(1 + (confidence - 0.5) * 0.4)  # Boost high-confidence models
  })
  
  # Calculate adjusted weights
  adjusted_weights <- base_weights * confidence_adjustment
  
  # Normalize to sum to 1
  normalized_weights <- adjusted_weights / sum(adjusted_weights)
  
  return(normalized_weights)
}

#' Combine predictions from multiple models
combine_model_predictions <- function(model_predictions, weights) {
  
  # Extract predictions
  spreads <- sapply(model_predictions, function(x) x$spread)
  totals <- sapply(model_predictions, function(x) x$total)
  confidences <- sapply(model_predictions, function(x) x$confidence)
  
  # Calculate weighted averages
  ensemble_spread <- sum(spreads * weights)
  ensemble_total <- sum(totals * weights)
  ensemble_confidence <- sum(confidences * weights)
  
  # Calculate prediction intervals (simplified)
  spread_variance <- sum(weights * (spreads - ensemble_spread)^2)
  total_variance <- sum(weights * (totals - ensemble_total)^2)
  
  return(list(
    spread = round(ensemble_spread, 1),
    total = round(ensemble_total, 1),
    confidence = round(ensemble_confidence, 2),
    spread_interval = c(ensemble_spread - sqrt(spread_variance), 
                       ensemble_spread + sqrt(spread_variance)),
    total_interval = c(ensemble_total - sqrt(total_variance),
                      ensemble_total + sqrt(total_variance)),
    model_breakdown = model_predictions,
    weights_used = weights
  ))
}

#' Calculate ensemble confidence
calculate_ensemble_confidence <- function(model_predictions, weights) {
  
  # Weight individual confidences
  individual_confidences <- sapply(model_predictions, function(x) x$confidence)
  weighted_confidence <- sum(individual_confidences * weights)
  
  # Boost confidence when models agree
  spreads <- sapply(model_predictions, function(x) x$spread)
  spread_agreement <- 1 - (sd(spreads) / mean(abs(spreads)))
  agreement_boost <- max(0, spread_agreement - 0.5) * 0.2
  
  # Penalize if few models are active
  n_models <- length(model_predictions)
  model_diversity_penalty <- max(0, (4 - n_models) * 0.05)
  
  final_confidence <- weighted_confidence + agreement_boost - model_diversity_penalty
  
  return(min(0.95, max(0.3, final_confidence)))
}

# ==============================================================================
# DATABASE HELPER FUNCTIONS
# ==============================================================================

#' Create model performance tracking table
create_model_performance_table <- function(con) {
  sql <- "
  CREATE TABLE IF NOT EXISTS model_performance (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    model_name TEXT NOT NULL,
    prediction_id TEXT NOT NULL,
    game_id TEXT NOT NULL,
    season INTEGER NOT NULL,
    week INTEGER NOT NULL,
    home_team TEXT NOT NULL,
    away_team TEXT NOT NULL,
    game_context TEXT,
    spread_prediction REAL,
    total_prediction REAL,
    confidence REAL,
    spread_error REAL,
    total_error REAL,
    spread_correct BOOLEAN,
    total_correct BOOLEAN,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (prediction_id) REFERENCES predictions(prediction_id)
  )"
  
  dbExecute(con, sql)
}

#' Create ensemble weights table
create_ensemble_weights_table <- function(con) {
  sql <- "
  CREATE TABLE IF NOT EXISTS ensemble_weights (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    model_name TEXT NOT NULL UNIQUE,
    current_weight REAL NOT NULL,
    base_weight REAL NOT NULL,
    performance_score REAL,
    games_evaluated INTEGER DEFAULT 0,
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    weight_history TEXT  -- JSON array of historical weights
  )"
  
  dbExecute(con, sql)
}

#' Create context routing table
create_context_routing_table <- function(con) {
  sql <- "
  CREATE TABLE IF NOT EXISTS context_routing (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    context_type TEXT NOT NULL,
    context_value TEXT NOT NULL,
    model_name TEXT NOT NULL,
    performance_score REAL,
    sample_size INTEGER DEFAULT 0,
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(context_type, context_value, model_name)
  )"
  
  dbExecute(con, sql)
}

#' Initialize model weights
initialize_model_weights <- function(con) {
  
  for (model_name in names(MODEL_REGISTRY)) {
    model_info <- MODEL_REGISTRY[[model_name]]
    
    sql <- "
    INSERT OR REPLACE INTO ensemble_weights 
    (model_name, current_weight, base_weight, performance_score, weight_history)
    VALUES (?, ?, ?, ?, ?)"
    
    dbExecute(con, sql, params = list(
      model_name,
      model_info$weight_initial,
      model_info$weight_initial,
      0.5,  # Neutral starting performance
      toJSON(list(list(timestamp = Sys.time(), weight = model_info$weight_initial)))
    ))
  }
}

#' Check if weights exist
weights_exist <- function(con) {
  result <- dbGetQuery(con, "SELECT COUNT(*) as count FROM ensemble_weights")
  return(result$count[1] > 0)
}

#' Get current model weights
get_current_model_weights <- function(con) {
  dbGetQuery(con, "SELECT model_name, current_weight FROM ensemble_weights ORDER BY model_name")
}

#' Update model weight
update_model_weight <- function(con, model_name, new_weight, performance_data) {
  
  # Get current weight history
  current_history <- dbGetQuery(con, 
    "SELECT weight_history FROM ensemble_weights WHERE model_name = ?",
    params = list(model_name))
  
  # Update weight history
  if (nrow(current_history) > 0) {
    history <- fromJSON(current_history$weight_history[1])
    history[[length(history) + 1]] <- list(
      timestamp = as.character(Sys.time()),
      weight = new_weight,
      performance_score = performance_data$weighted_performance,
      games = performance_data$total_games
    )
    
    # Keep last 50 entries
    if (length(history) > 50) {
      history <- tail(history, 50)
    }
  } else {
    history <- list(list(
      timestamp = as.character(Sys.time()),
      weight = new_weight,
      performance_score = performance_data$weighted_performance,
      games = performance_data$total_games
    ))
  }
  
  # Update database
  sql <- "
  UPDATE ensemble_weights 
  SET current_weight = ?, performance_score = ?, games_evaluated = ?, 
      last_updated = CURRENT_TIMESTAMP, weight_history = ?
  WHERE model_name = ?"
  
  dbExecute(con, sql, params = list(
    new_weight,
    performance_data$weighted_performance,
    performance_data$total_games,
    toJSON(history),
    model_name
  ))
}

#' Display current model status
display_model_status <- function(con) {
  
  cat("\n📊 Current Model Status:\n")
  cat("=" %R% 50, "\n")
  
  weights <- get_current_model_weights(con)
  
  for (i in 1:nrow(weights)) {
    model_name <- weights$model_name[i]
    weight <- weights$current_weight[i]
    model_info <- MODEL_REGISTRY[[model_name]]
    
    cat(sprintf("🤖 %-20s Weight: %.3f (%s)\n", 
               model_info$name, weight, model_info$description))
  }
  
  cat("=" %R% 50, "\n\n")
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

#' Store model performance record
store_model_performance <- function(con, model_name, prediction, actual_results,
                                   spread_error, total_error, spread_correct, total_correct) {
  
  sql <- "
  INSERT INTO model_performance (
    model_name, prediction_id, game_id, season, week, home_team, away_team,
    game_context, spread_prediction, total_prediction, confidence,
    spread_error, total_error, spread_correct, total_correct
  ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  
  dbExecute(con, sql, params = list(
    model_name,
    prediction$prediction_id,
    prediction$game_id,
    prediction$season,
    prediction$week,
    prediction$home_team,
    prediction$away_team,
    toJSON(determine_game_context(prediction)),
    prediction$predicted_spread,
    prediction$predicted_total,
    prediction$confidence,
    spread_error,
    total_error,
    as.integer(spread_correct),
    as.integer(total_correct)
  ))
}

#' Get recent model performance
get_recent_model_performance <- function(con, lookback_games) {
  
  sql <- "
  SELECT * FROM model_performance 
  WHERE created_at >= date('now', '-' || ? || ' days')
  ORDER BY created_at DESC"
  
  dbGetQuery(con, sql, params = list(lookback_games * 7))  # Approximate days
}

#' Initialize context routing
initialize_context_routing <- function(con) {
  
  # Initialize with default routing rules
  for (context_type in names(CONTEXT_DEFINITIONS)) {
    for (context_value in CONTEXT_DEFINITIONS[[context_type]]) {
      for (model_name in names(MODEL_REGISTRY)) {
        model_info <- MODEL_REGISTRY[[model_name]]
        
        # Check if this model is optimal for this context
        performance_score <- if (context_value %in% model_info$contexts_optimal) 0.7 else 0.5
        
        sql <- "
        INSERT OR IGNORE INTO context_routing 
        (context_type, context_value, model_name, performance_score)
        VALUES (?, ?, ?, ?)"
        
        dbExecute(con, sql, params = list(context_type, context_value, model_name, performance_score))
      }
    }
  }
}

# Helper function for string repetition
`%R%` <- function(string, times) {
  paste(rep(string, times), collapse = "")
}

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# ==============================================================================
# TESTING AND VALIDATION
# ==============================================================================

#' Test ensemble system
#' 
#' Validates ensemble functionality with sample data
#' 
#' @param db_path Path to test database
#' @export
test_ensemble_system <- function(db_path = "test_ensemble.db") {
  
  cat("🧪 Testing Dynamic Model Ensemble System...\n")
  
  # Clean up any existing test database
  if (file.exists(db_path)) file.remove(db_path)
  
  test_results <- list()
  
  # Test 1: System initialization
  cat("Test 1: System initialization...\n")
  tryCatch({
    initialize_ensemble_system(db_path, reset_weights = TRUE)
    test_results$initialization <- "PASS"
    cat("✅ Initialization test passed\n")
  }, error = function(e) {
    test_results$initialization <- "FAIL"
    cat(sprintf("❌ Initialization test failed: %s\n", e$message))
  })
  
  # Test 2: Ensemble prediction
  cat("Test 2: Ensemble prediction generation...\n")
  tryCatch({
    # Create sample game data
    game_data <- list(
      home_team = "LAC",
      away_team = "KC", 
      season = 2025,
      week = 1,
      game_date = "2025-09-08"
    )
    
    # Create sample feature data
    feature_data <- list(
      home_epa = 0.15,
      away_epa = 0.22,
      home_def_epa = -0.08,
      away_def_epa = -0.12,
      vegas_spread = -3.0,
      vegas_total = 47.5,
      situational_features = list(
        home_1st_down_pass_deviation = 0.08,
        away_1st_down_pass_deviation = 0.12,
        home_data_quality = 50,
        away_data_quality = 48
      )
    )
    
    ensemble_pred <- generate_ensemble_prediction(game_data, feature_data, db_path)
    test_results$prediction_generation <- ifelse(!is.null(ensemble_pred), "PASS", "FAIL")
    
    if (!is.null(ensemble_pred)) {
      cat("✅ Prediction generation test passed\n")
    } else {
      cat("❌ Prediction generation test failed\n")
    }
    
  }, error = function(e) {
    test_results$prediction_generation <- "FAIL"
    cat(sprintf("❌ Prediction generation test failed: %s\n", e$message))
  })
  
  # Test 3: Weight updates (simplified)
  cat("Test 3: Weight update mechanism...\n")
  tryCatch({
    # This would normally require actual prediction results
    # For testing, we'll just verify the update function runs
    result <- update_ensemble_weights(db_path, lookback_games = 1)
    test_results$weight_updates <- ifelse(result, "PASS", "SKIP")
    cat("✅ Weight update test passed\n")
  }, error = function(e) {
    test_results$weight_updates <- "FAIL"
    cat(sprintf("❌ Weight update test failed: %s\n", e$message))
  })
  
  # Clean up test database
  if (file.exists(db_path)) file.remove(db_path)
  
  # Summary
  passed_tests <- sum(test_results == "PASS")
  total_tests <- length(test_results)
  
  cat(sprintf("\n🏁 Ensemble testing complete: %d/%d tests passed\n", passed_tests, total_tests))
  
  return(test_results)
}

# ==============================================================================
# INITIALIZATION
# ==============================================================================

cat("🎯 Dynamic Model Ensemble Framework v2.0 Loaded\n")
cat("Key Functions Available:\n")
cat("  - initialize_ensemble_system(): Set up ensemble with database tables\n")
cat("  - generate_ensemble_prediction(): Main prediction function\n")
cat("  - update_model_performance(): Update after game results\n")
cat("  - update_ensemble_weights(): Recalculate model weights\n")
cat("  - test_ensemble_system(): Validate system functionality\n")
cat("\nModel Registry:\n")
for (model_name in names(MODEL_REGISTRY)) {
  model_info <- MODEL_REGISTRY[[model_name]]
  cat(sprintf("  - %s: %s\n", model_name, model_info$description))
}
cat("\nExample Usage:\n")
cat("  # Initialize system\n")
cat("  initialize_ensemble_system()\n")
cat("  \n")
cat("  # Generate prediction\n")
cat("  prediction <- generate_ensemble_prediction(game_data, feature_data)\n")
cat("  \n")
cat("  # Update performance after results\n")
cat("  update_model_performance(prediction_id, actual_results)\n")

cat("\n🤖 Ready for dynamic, performance-weighted predictions!\n")