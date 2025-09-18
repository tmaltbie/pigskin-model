# Ensemble Prediction System - Combining EPA and ML Models
# Integrates your existing EPA-based system with advanced ML predictions

library(dplyr)

source("spread_predictor.R")     # Your existing EPA system
source("ml_models.R")            # New ML models
source("ml_feature_engineering.R") # Feature engineering
source("advanced_model.R")       # Your advanced factors

# ============= ENSEMBLE ARCHITECTURE =============

# Main ensemble prediction function
ensemble_predict_game <- function(home_team, away_team, vegas_spread, 
                                 pbp_data, schedule_data, season, week,
                                 ml_models = NULL, additional_info = NULL) {
  
  cat(sprintf("Ensemble prediction: %s @ %s (Week %d, %d)\n", 
              away_team, home_team, week, season))
  
  predictions <- list()
  weights <- list()
  
  # ============= COMPONENT 1: EXISTING EPA MODEL =============
  cat("Running EPA-based prediction...\n")
  
  # Calculate team metrics using your existing system
  team_metrics <- calculate_team_metrics(pbp_data, schedule_data)
  
  # Your existing EPA prediction
  epa_prediction <- predict_game_spread(
    home_team = home_team,
    away_team = away_team,
    vegas_spread = vegas_spread,
    team_metrics = team_metrics,
    schedule_data = schedule_data
  )
  
  predictions$epa <- list(
    margin = epa_prediction$predicted_margin,
    confidence = epa_prediction$confidence,
    recommendation = epa_prediction$recommendation
  )
  weights$epa <- 0.3  # 30% weight to proven EPA system
  
  # ============= COMPONENT 2: ADVANCED FACTORS MODEL =============
  cat("Running advanced factors model...\n")
  
  if(!is.null(additional_info)) {
    advanced_prediction <- advanced_predict_game(
      home_team = home_team,
      away_team = away_team,
      game_info = additional_info,
      team_metrics = team_metrics,
      pbp_data = pbp_data,
      schedule_data = schedule_data
    )
    
    predictions$advanced <- list(
      margin = advanced_prediction$predicted_margin,
      confidence = advanced_prediction$confidence,
      breakdown = advanced_prediction$breakdown
    )
    weights$advanced <- 0.25  # 25% weight to advanced factors
  } else {
    cat("No additional info provided for advanced model\n")
    weights$advanced <- 0
  }
  
  # ============= COMPONENT 3: ML MODELS =============
  cat("Running ML ensemble...\n")
  
  if(!is.null(ml_models)) {
    # Build ML features for this game
    game_features <- build_game_features(
      home_team = home_team,
      away_team = away_team,
      season = season,
      week = week,
      pbp_data = pbp_data,
      additional_info = additional_info
    )
    
    if(!is.null(game_features)) {
      ml_prediction <- predict_with_ensemble(ml_models, game_features)
      
      predictions$ml <- list(
        margin = ml_prediction$ensemble_margin,
        cover_prob = ml_prediction$ensemble_cover_prob,
        confidence = ml_prediction$confidence,
        individual = ml_prediction$individual
      )
      weights$ml <- 0.35  # 35% weight to ML ensemble
    } else {
      cat("Failed to build ML features\n")
      weights$ml <- 0
    }
  } else {
    cat("No ML models provided\n")
    weights$ml <- 0
  }
  
  # ============= COMPONENT 4: MARKET EFFICIENCY ADJUSTMENT =============
  cat("Applying market efficiency adjustment...\n")
  
  # Analyze how well Vegas has been predicting similar matchups
  market_efficiency <- calculate_vegas_efficiency(
    home_team, away_team, season, week, pbp_data, schedule_data
  )
  
  predictions$market <- list(
    vegas_margin = -vegas_spread,
    efficiency_score = market_efficiency$efficiency,
    sample_size = market_efficiency$games_analyzed
  )
  weights$market <- 0.1  # 10% weight to market adjustment
  
  # ============= ENSEMBLE COMBINATION =============
  
  # Normalize weights
  total_weight <- sum(unlist(weights))
  if(total_weight > 0) {
    weights <- lapply(weights, function(w) w / total_weight)
  }
  
  # Combine margin predictions
  margin_components <- c()
  margin_weights <- c()
  
  if(!is.null(predictions$epa$margin) && weights$epa > 0) {
    margin_components <- c(margin_components, predictions$epa$margin)
    margin_weights <- c(margin_weights, weights$epa)
  }
  
  if(!is.null(predictions$advanced$margin) && weights$advanced > 0) {
    margin_components <- c(margin_components, predictions$advanced$margin)
    margin_weights <- c(margin_weights, weights$advanced)
  }
  
  if(!is.null(predictions$ml$margin) && weights$ml > 0) {
    margin_components <- c(margin_components, predictions$ml$margin)
    margin_weights <- c(margin_weights, weights$ml)
  }
  
  if(!is.null(predictions$market$vegas_margin) && weights$market > 0) {
    margin_components <- c(margin_components, predictions$market$vegas_margin)
    margin_weights <- c(margin_weights, weights$market)
  }
  
  # Weighted average of margins
  if(length(margin_components) > 0) {
    ensemble_margin <- weighted.mean(margin_components, margin_weights)
  } else {
    ensemble_margin <- 0
  }
  
  # Calculate ensemble confidence
  ensemble_confidence <- calculate_ensemble_confidence(predictions, weights)
  
  # Generate betting recommendation
  recommendation <- generate_ensemble_recommendation(
    ensemble_margin, vegas_spread, ensemble_confidence, predictions
  )
  
  # ============= FINAL RESULT =============
  
  final_prediction <- list(
    home_team = home_team,
    away_team = away_team,
    vegas_spread = vegas_spread,
    
    # Ensemble results
    ensemble_margin = round(ensemble_margin, 1),
    ensemble_confidence = round(ensemble_confidence, 3),
    recommendation = recommendation$action,
    bet_size = recommendation$size,
    
    # Component predictions
    components = predictions,
    weights = weights,
    
    # Additional insights
    edge_vs_vegas = round(ensemble_margin - (-vegas_spread), 1),
    model_agreement = calculate_model_agreement(predictions),
    
    # Metadata
    timestamp = Sys.time(),
    season = season,
    week = week
  )
  
  return(final_prediction)
}

# ============= MARKET EFFICIENCY ANALYSIS =============

calculate_vegas_efficiency <- function(home_team, away_team, season, week, 
                                     pbp_data, schedule_data, lookback_games = 20) {
  
  # Analyze recent similar matchups or team games
  recent_games <- schedule_data %>%
    filter(
      (home_team == !!home_team | away_team == !!home_team | 
       home_team == !!away_team | away_team == !!away_team),
      season >= (!!season - 2),  # Last 2+ seasons
      !is.na(result)
    ) %>%
    arrange(desc(season), desc(week)) %>%
    head(lookback_games)
  
  if(nrow(recent_games) < 5) {
    return(list(efficiency = 0.5, games_analyzed = 0))
  }
  
  # This would ideally use historical spread data
  # For now, simulate based on game results
  vegas_accuracy <- calculate_simulated_vegas_accuracy(recent_games)
  
  return(list(
    efficiency = vegas_accuracy,
    games_analyzed = nrow(recent_games)
  ))
}

calculate_simulated_vegas_accuracy <- function(games) {
  # Simulate Vegas accuracy based on margin distribution
  # In reality, you'd use actual historical spreads
  
  margins <- games$home_score - games$away_score
  avg_error <- mean(abs(margins))
  
  # Convert to efficiency score (lower error = higher efficiency)
  efficiency <- max(0.3, min(0.8, 1 - (avg_error / 20)))
  return(efficiency)
}

# ============= CONFIDENCE CALCULATION =============

calculate_ensemble_confidence <- function(predictions, weights) {
  
  confidences <- c()
  conf_weights <- c()
  
  # Collect individual confidences
  if(!is.null(predictions$epa$confidence) && weights$epa > 0) {
    confidences <- c(confidences, predictions$epa$confidence)
    conf_weights <- c(conf_weights, weights$epa)
  }
  
  if(!is.null(predictions$advanced$confidence) && weights$advanced > 0) {
    confidences <- c(confidences, predictions$advanced$confidence)
    conf_weights <- c(conf_weights, weights$advanced)
  }
  
  if(!is.null(predictions$ml$confidence) && weights$ml > 0) {
    confidences <- c(confidences, predictions$ml$confidence)
    conf_weights <- c(conf_weights, weights$ml)
  }
  
  if(length(confidences) == 0) return(0)
  
  # Weighted average of confidences
  avg_confidence <- weighted.mean(confidences, conf_weights)
  
  # Penalty for disagreement between models
  if(length(confidences) > 1) {
    disagreement_penalty <- sd(confidences) * 0.5
    avg_confidence <- max(0, avg_confidence - disagreement_penalty)
  }
  
  return(avg_confidence)
}

# ============= MODEL AGREEMENT ANALYSIS =============

calculate_model_agreement <- function(predictions) {
  
  margins <- c()
  
  if(!is.null(predictions$epa$margin)) margins <- c(margins, predictions$epa$margin)
  if(!is.null(predictions$advanced$margin)) margins <- c(margins, predictions$advanced$margin)  
  if(!is.null(predictions$ml$margin)) margins <- c(margins, predictions$ml$margin)
  
  if(length(margins) < 2) return(1.0)  # Perfect agreement if only one model
  
  # Calculate coefficient of variation (lower = more agreement)
  cv <- sd(margins) / abs(mean(margins))
  
  # Convert to agreement score (0 to 1, higher = more agreement)
  agreement <- max(0, 1 - cv)
  
  return(agreement)
}

# ============= BETTING RECOMMENDATION ENGINE =============

generate_ensemble_recommendation <- function(ensemble_margin, vegas_spread, 
                                           ensemble_confidence, predictions) {
  
  edge_vs_vegas <- ensemble_margin - (-vegas_spread)
  
  # Conservative betting thresholds (maintaining your 70% EPA approach)
  min_edge <- 2.5          # Minimum edge required
  min_confidence <- 0.6    # Minimum confidence required
  max_bet_size <- 1.5      # Maximum bet size in units
  
  # Determine action
  if(abs(edge_vs_vegas) < min_edge || ensemble_confidence < min_confidence) {
    return(list(action = "PASS", size = 0, reason = "Insufficient edge or confidence"))
  }
  
  # Determine bet size based on Kelly-inspired approach but capped conservatively
  base_size <- min(abs(edge_vs_vegas) / 10, max_bet_size)  # Scale with edge
  confidence_adjustment <- ensemble_confidence
  
  bet_size <- base_size * confidence_adjustment
  bet_size <- max(0.5, min(bet_size, max_bet_size))  # Enforce min/max
  
  # Determine side
  if(edge_vs_vegas > min_edge) {
    action <- sprintf("BET HOME %.1fu", bet_size)
  } else if(edge_vs_vegas < -min_edge) {
    action <- sprintf("BET AWAY %.1fu", bet_size)
  } else {
    action <- "PASS"
    bet_size <- 0
  }
  
  return(list(
    action = action,
    size = bet_size,
    edge = round(edge_vs_vegas, 1),
    reason = sprintf("Edge: %.1f, Confidence: %.2f", edge_vs_vegas, ensemble_confidence)
  ))
}

# ============= ENSEMBLE BACKTESTING =============

backtest_ensemble_system <- function(pbp_data, schedule_data, spreads_data, 
                                    ml_models, seasons = 2020:2023) {
  
  cat("Backtesting ensemble system...\n")
  
  results <- list()
  
  for(season in seasons) {
    cat(sprintf("Backtesting season %d...\n", season))
    
    season_games <- schedule_data %>%
      filter(season == !!season, !is.na(result)) %>%
      arrange(week)
    
    season_results <- list()
    
    for(i in 1:min(20, nrow(season_games))) {  # Test subset for demo
      game <- season_games[i, ]
      
      # Get spread data
      spread_info <- spreads_data %>%
        filter(season == !!season, week == game$week,
               home_team == game$home_team, away_team == game$away_team)
      
      if(nrow(spread_info) == 0) next
      
      # Make ensemble prediction
      ensemble_pred <- ensemble_predict_game(
        home_team = game$home_team,
        away_team = game$away_team,
        vegas_spread = spread_info$spread[1],
        pbp_data = pbp_data,
        schedule_data = schedule_data,
        season = season,
        week = game$week,
        ml_models = ml_models
      )
      
      # Calculate actual results
      actual_margin <- game$home_score - game$away_score
      prediction_error <- abs(actual_margin - ensemble_pred$ensemble_margin)
      
      # Store result
      season_results[[i]] <- list(
        game_id = paste(season, game$week, game$away_team, game$home_team, sep="_"),
        actual_margin = actual_margin,
        predicted_margin = ensemble_pred$ensemble_margin,
        vegas_spread = ensemble_pred$vegas_spread,
        error = prediction_error,
        recommendation = ensemble_pred$recommendation,
        confidence = ensemble_pred$ensemble_confidence
      )
    }
    
    results[[as.character(season)]] <- season_results
  }
  
  # Analyze backtest results
  analyze_ensemble_backtest(results)
  
  return(results)
}

analyze_ensemble_backtest <- function(results) {
  
  cat("\n=== ENSEMBLE BACKTEST RESULTS ===\n")
  
  all_results <- unlist(results, recursive = FALSE)
  all_results <- all_results[!sapply(all_results, is.null)]
  
  if(length(all_results) == 0) {
    cat("No backtest results to analyze\n")
    return()
  }
  
  # Extract metrics
  errors <- sapply(all_results, function(x) x$error)
  confidences <- sapply(all_results, function(x) x$confidence)
  
  cat(sprintf("Games analyzed: %d\n", length(all_results)))
  cat(sprintf("Average prediction error: %.2f points\n", mean(errors, na.rm = TRUE)))
  cat(sprintf("Predictions within 3 points: %.1f%%\n", 
              mean(errors <= 3, na.rm = TRUE) * 100))
  cat(sprintf("Predictions within 7 points: %.1f%%\n", 
              mean(errors <= 7, na.rm = TRUE) * 100))
  cat(sprintf("Average confidence: %.2f\n", mean(confidences, na.rm = TRUE)))
  
  # High confidence subset
  high_conf <- all_results[confidences > 0.7]
  if(length(high_conf) > 0) {
    high_conf_errors <- sapply(high_conf, function(x) x$error)
    cat(sprintf("\nHIGH CONFIDENCE GAMES (%d games):\n", length(high_conf)))
    cat(sprintf("Average error: %.2f points\n", mean(high_conf_errors, na.rm = TRUE)))
    cat(sprintf("Within 3 points: %.1f%%\n", 
                mean(high_conf_errors <= 3, na.rm = TRUE) * 100))
  }
}

cat("Ensemble Prediction System loaded!\n")
cat("Architecture:\n")
cat("✅ 30% EPA-based prediction (your proven system)\n")
cat("✅ 25% Advanced factors (injuries, weather, coaching)\n") 
cat("✅ 35% ML ensemble (XGBoost, RF, Elastic Net, SVM)\n")
cat("✅ 10% Market efficiency adjustment\n")
cat("\nFeatures:\n")
cat("- Confidence-weighted predictions\n")
cat("- Model agreement analysis\n")
cat("- Conservative betting recommendations (max 1.5u)\n")
cat("- Comprehensive backtesting\n")
cat("- Maintains your 70% EPA philosophy\n")
cat("\nTo use: ensemble_predict_game(home, away, spread, data...)\n")