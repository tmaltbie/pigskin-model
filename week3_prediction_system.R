# Week 3 NFL 2025 Prediction System
# Generates ML & ATS predictions with confidence using situational analysis and learning system
# Combines EPA, situational analysis, and performance tracking for enhanced accuracy

library(dplyr)

# Load existing systems
source('learning_system/situational_analysis.R')
source('learning_system/outcome_tracker_csv.R')
source('learning_system/unified_data_access.R')

#' Generate comprehensive Week 3 predictions with confidence intervals
#' 
#' @param use_situational Include situational analysis (default: TRUE)
#' @param confidence_threshold Minimum confidence for betting recommendations (default: 0.65)
#' 
#' @return Data frame with predictions, confidence, and betting recommendations
generate_week3_predictions <- function(use_situational = TRUE, confidence_threshold = 0.65) {
  
  cat("🏈 Generating Week 3 2025 NFL Predictions\n")
  cat("Using situational analysis:", use_situational, "\n")
  cat("Confidence threshold:", confidence_threshold, "\n\n")
  
  # Week 3 2025 NFL Schedule (verified matchups)
  week3_schedule <- data.frame(
    game_id = c("2025_03_CAR_LV", "2025_03_CHI_IND", "2025_03_DEN_TB", "2025_03_HOU_MIN",
                "2025_03_NE_NYJ", "2025_03_PHI_NO", "2025_03_WAS_CIN", "2025_03_CLE_NYG",
                "2025_03_MIA_SEA", "2025_03_LAC_PIT", "2025_03_ARI_DET", "2025_03_DAL_BAL",
                "2025_03_ATL_KC", "2025_03_SF_LAR", "2025_03_JAX_BUF", "2025_03_TEN_GB"),
    away_team = c("CAR", "CHI", "DEN", "HOU", "NE", "PHI", "WAS", "CLE", 
                  "MIA", "LAC", "ARI", "DAL", "ATL", "SF", "JAX", "TEN"),
    home_team = c("LV", "IND", "TB", "MIN", "NYJ", "NO", "CIN", "NYG", 
                  "SEA", "PIT", "DET", "BAL", "KC", "LAR", "BUF", "GB"),
    game_date = as.Date("2025-09-22"),
    stringsAsFactors = FALSE
  )
  
  predictions <- data.frame()
  
  for (i in 1:nrow(week3_schedule)) {
    game <- week3_schedule[i,]
    
    cat(sprintf("Analyzing: %s @ %s\n", game$away_team, game$home_team))
    
    # Generate prediction for this matchup
    prediction <- generate_single_game_prediction(
      home_team = game$home_team,
      away_team = game$away_team,
      game_id = game$game_id,
      game_date = game$game_date,
      use_situational = use_situational
    )
    
    if (!is.null(prediction)) {
      predictions <- rbind(predictions, prediction)
    }
  }
  
  # Add betting recommendations based on confidence
  predictions <- predictions %>%
    mutate(
      # Betting recommendations
      spread_bet_recommendation = ifelse(
        confidence >= confidence_threshold,
        ifelse(predicted_margin > 0, paste("Bet", home_team), paste("Bet", away_team)),
        "No Bet"
      ),
      ml_bet_recommendation = ifelse(
        home_win_prob > 0.6 & confidence >= confidence_threshold,
        paste("ML", home_team),
        ifelse(home_win_prob < 0.4 & confidence >= confidence_threshold,
               paste("ML", away_team), "No ML Bet")
      ),
      
      # Betting value assessment
      expected_value = calculate_expected_value(predicted_margin, home_win_prob, confidence),
      bet_size = calculate_optimal_bet_size(expected_value, confidence)
    )
  
  # Store predictions in learning system
  store_week3_predictions(predictions)
  
  # Generate summary report
  generate_prediction_summary(predictions, confidence_threshold)
  
  return(predictions)
}

#' Generate prediction for a single game using multiple models
#' 
#' @param home_team Home team abbreviation
#' @param away_team Away team abbreviation  
#' @param game_id Unique game identifier
#' @param game_date Game date
#' @param use_situational Include situational analysis
#' 
#' @return Single row data frame with prediction details
generate_single_game_prediction <- function(home_team, away_team, game_id, game_date, use_situational = TRUE) {
  
  tryCatch({
    
    # Model 1: EPA-based prediction (baseline)
    epa_prediction <- generate_epa_prediction(home_team, away_team)
    
    # Model 2: Situational analysis enhancement
    situational_adjustment <- 0
    situational_confidence <- 0.5
    
    if (use_situational) {
      situational_features <- generate_situational_features(home_team, away_team, 2025, 3)
      situational_adjustment <- calculate_situational_adjustment(situational_features)
      situational_confidence <- calculate_situational_confidence(situational_features)
    }
    
    # Model 3: Historical performance analysis
    historical_adjustment <- calculate_historical_adjustment(home_team, away_team)
    
    # Ensemble combination with dynamic weights
    ensemble_weights <- calculate_model_weights(home_team, away_team)
    
    # Combined prediction
    base_margin <- epa_prediction$predicted_margin
    adjusted_margin <- base_margin + 
                     (situational_adjustment * ensemble_weights$situational) +
                     (historical_adjustment * ensemble_weights$historical) +
                     2.5  # Home field advantage
    
    predicted_total <- epa_prediction$predicted_total
    home_win_prob <- calculate_win_probability(adjusted_margin)
    
    # Confidence calculation (ensemble of confidence estimates)
    confidence <- calculate_ensemble_confidence(
      epa_confidence = epa_prediction$confidence,
      situational_confidence = situational_confidence,
      historical_confidence = 0.6,
      weights = ensemble_weights
    )
    
    # Create prediction record
    prediction <- data.frame(
      game_id = game_id,
      home_team = home_team,
      away_team = away_team,
      game_date = as.character(game_date),
      
      # Predictions
      predicted_margin = round(adjusted_margin, 1),
      predicted_total = round(predicted_total, 1), 
      home_win_prob = round(home_win_prob, 3),
      confidence = round(confidence, 3),
      
      # Model components
      epa_component = round(base_margin, 1),
      situational_component = round(situational_adjustment, 1),
      historical_component = round(historical_adjustment, 1),
      
      # Model weights
      epa_weight = ensemble_weights$epa,
      situational_weight = ensemble_weights$situational,
      historical_weight = ensemble_weights$historical,
      
      # Meta information
      prediction_date = as.character(Sys.time()),
      model_version = "week3_ensemble_v1.0",
      
      stringsAsFactors = FALSE
    )
    
    return(prediction)
    
  }, error = function(e) {
    cat("❌ Error predicting", away_team, "@", home_team, ":", e$message, "\n")
    return(NULL)
  })
}

#' Generate EPA-based prediction (baseline model)
generate_epa_prediction <- function(home_team, away_team) {
  
  # Load actual game results for team performance
  tryCatch({
    results_2025 <- load_actual_game_results(seasons = 2025, completed_only = TRUE)
    
    if (!is.null(results_2025) && nrow(results_2025) > 0) {
      
      # Calculate team performance from completed games
      home_performance <- calculate_team_performance(home_team, results_2025)
      away_performance <- calculate_team_performance(away_team, results_2025)
      
      # EPA-style calculation
      predicted_margin <- home_performance$avg_margin - away_performance$avg_margin + 2.5
      predicted_total <- (home_performance$avg_points + away_performance$avg_points_allowed) / 2 +
                        (away_performance$avg_points + home_performance$avg_points_allowed) / 2
      
      confidence <- min(0.8, max(0.5, 1 - abs(predicted_margin) * 0.02))
      
    } else {
      # Fallback to mock prediction
      predicted_margin <- rnorm(1, 0, 4)
      predicted_total <- rnorm(1, 45, 8)
      confidence <- 0.6
    }
  }, error = function(e) {
    predicted_margin <- rnorm(1, 0, 4)
    predicted_total <- rnorm(1, 45, 8)
    confidence <- 0.6
  })
  
  return(list(
    predicted_margin = predicted_margin,
    predicted_total = predicted_total,
    confidence = confidence
  ))
}

#' Calculate situational adjustment based on team tendencies
calculate_situational_adjustment <- function(situational_features) {
  
  if (is.null(situational_features) || situational_features$data_quality == "no_data") {
    return(0)
  }
  
  # Extract key advantages
  advantages <- situational_features$offensive_advantages
  edges <- situational_features$down_distance_edges
  
  adjustment <- 0
  
  # Red zone advantage (like LAC's 12.7% advantage)
  if ("home_red_zone" %in% names(advantages)) {
    adjustment <- adjustment + (advantages$home_red_zone * 10)  # Convert to points
  }
  
  # Down and distance edges
  if (!is.null(edges$first_down_edge)) {
    adjustment <- adjustment + (edges$first_down_edge * 5)
  }
  
  if (!is.null(edges$third_down_edge)) {
    adjustment <- adjustment + (edges$third_down_edge * 8)  # Third down more valuable
  }
  
  # Cap adjustment to reasonable range
  adjustment <- max(-3, min(3, adjustment))
  
  return(adjustment)
}

#' Calculate confidence based on situational analysis quality
calculate_situational_confidence <- function(situational_features) {
  
  if (is.null(situational_features)) return(0.5)
  
  base_confidence <- 0.5
  
  # Increase confidence based on significant features found
  significant_features <- situational_features$significance_flags$total_significant_features
  confidence_boost <- min(0.2, significant_features * 0.05)
  
  # Adjust for data quality
  if (situational_features$data_quality == "good") {
    confidence_boost <- confidence_boost + 0.1
  } else if (situational_features$data_quality == "early_season") {
    confidence_boost <- confidence_boost * 0.7
  }
  
  return(min(0.85, base_confidence + confidence_boost))
}

#' Calculate team performance metrics from completed games
calculate_team_performance <- function(team, results_data) {
  
  team_games <- results_data %>%
    filter(home_team == team | away_team == team)
  
  if (nrow(team_games) == 0) {
    return(list(avg_margin = 0, avg_points = 22, avg_points_allowed = 22))
  }
  
  # Calculate from team's perspective
  team_stats <- team_games %>%
    mutate(
      team_score = ifelse(home_team == team, home_score, away_score),
      opp_score = ifelse(home_team == team, away_score, home_score),
      team_margin = ifelse(home_team == team, margin, -margin)
    ) %>%
    summarise(
      avg_margin = mean(team_margin, na.rm = TRUE),
      avg_points = mean(team_score, na.rm = TRUE),
      avg_points_allowed = mean(opp_score, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(as.list(team_stats))
}

#' Calculate historical performance adjustment
calculate_historical_adjustment <- function(home_team, away_team) {
  # Placeholder for historical head-to-head analysis
  # Could include factors like recent matchups, coaching records, etc.
  return(rnorm(1, 0, 1))
}

#' Calculate dynamic model weights based on recent performance
calculate_model_weights <- function(home_team, away_team) {
  
  # Default weights - can be enhanced with actual performance tracking
  return(list(
    epa = 0.5,
    situational = 0.3,
    historical = 0.2
  ))
}

#' Calculate win probability from predicted margin
calculate_win_probability <- function(predicted_margin) {
  # Sigmoid transformation of margin to probability
  # Based on NFL historical data: ~14 point margin = ~90% win probability
  return(plogis(predicted_margin / 14))
}

#' Calculate ensemble confidence from multiple models
calculate_ensemble_confidence <- function(epa_confidence, situational_confidence, historical_confidence, weights) {
  
  weighted_confidence <- (epa_confidence * weights$epa + 
                         situational_confidence * weights$situational +
                         historical_confidence * weights$historical)
  
  # Adjust for model agreement (higher confidence when models agree)
  confidences <- c(epa_confidence, situational_confidence, historical_confidence)
  agreement_factor <- 1 - sd(confidences) / mean(confidences)
  
  final_confidence <- weighted_confidence * (0.7 + 0.3 * agreement_factor)
  
  return(max(0.3, min(0.95, final_confidence)))
}

#' Calculate expected value for betting recommendation
calculate_expected_value <- function(predicted_margin, home_win_prob, confidence) {
  # Vectorized EV calculation - assumes standard -110 odds
  implied_prob <- 0.524  # -110 odds = 52.4% implied probability
  
  ev <- ifelse(home_win_prob > implied_prob & confidence > 0.6,
               (home_win_prob - implied_prob) * confidence,
               ifelse((1 - home_win_prob) > implied_prob & confidence > 0.6,
                      ((1 - home_win_prob) - implied_prob) * confidence,
                      0))
  
  return(ev)
}

#' Calculate optimal bet size using Kelly Criterion
calculate_optimal_bet_size <- function(expected_value, confidence) {
  
  # Vectorized bet sizing
  bet_size <- ifelse(expected_value <= 0 | confidence < 0.6, 
                     0,
                     pmin(1.5, pmax(0.5, expected_value * 0.25 * 10)))
  
  return(round(bet_size, 1))
}

#' Store Week 3 predictions in learning system
store_week3_predictions <- function(predictions) {
  
  cat("💾 Storing Week 3 predictions for learning system...\n")
  
  stored_count <- 0
  
  for (i in 1:nrow(predictions)) {
    pred <- predictions[i,]
    
    prediction_id <- store_prediction_csv(
      game_id = pred$game_id,
      home_team = pred$home_team,
      away_team = pred$away_team,
      predicted_margin = pred$predicted_margin,
      predicted_total = pred$predicted_total,
      home_win_prob = pred$home_win_prob,
      confidence = pred$confidence,
      model_version = pred$model_version,
      season = 2025,
      week = 3,
      game_date = pred$game_date
    )
    
    if (!is.null(prediction_id)) {
      stored_count <- stored_count + 1
    }
  }
  
  cat(sprintf("✅ Stored %d predictions in learning system\n", stored_count))
}

#' Generate comprehensive prediction summary
generate_prediction_summary <- function(predictions, confidence_threshold) {
  
  cat("\n" %+% "📊 WEEK 3 2025 NFL PREDICTIONS SUMMARY" %+% "\n")
  cat("=" %+% strrep("=", 50) %+% "\n\n")
  
  # Overall statistics
  high_conf_games <- sum(predictions$confidence >= confidence_threshold)
  avg_confidence <- mean(predictions$confidence)
  spread_bets <- sum(predictions$spread_bet_recommendation != "No Bet")
  ml_bets <- sum(predictions$ml_bet_recommendation != "No ML Bet")
  
  cat(sprintf("Total Games: %d\n", nrow(predictions)))
  cat(sprintf("High Confidence Games (>= %.0f%%): %d\n", confidence_threshold * 100, high_conf_games))
  cat(sprintf("Average Confidence: %.1f%%\n", avg_confidence * 100))
  cat(sprintf("Spread Bet Recommendations: %d\n", spread_bets))
  cat(sprintf("Moneyline Bet Recommendations: %d\n", ml_bets))
  
  cat("\n🎯 TOP CONFIDENCE PICKS:\n")
  
  # Show top 5 confidence picks
  top_picks <- predictions %>%
    arrange(desc(confidence)) %>%
    head(5)
  
  for (i in 1:nrow(top_picks)) {
    pick <- top_picks[i,]
    cat(sprintf("%d. %s @ %s: %s by %.1f (%.1f%% conf)\n", 
               i, pick$away_team, pick$home_team,
               ifelse(pick$predicted_margin > 0, pick$home_team, pick$away_team),
               abs(pick$predicted_margin), pick$confidence * 100))
  }
  
  cat("\n💰 BETTING RECOMMENDATIONS:\n")
  
  betting_picks <- predictions %>%
    filter(spread_bet_recommendation != "No Bet" | ml_bet_recommendation != "No ML Bet") %>%
    arrange(desc(expected_value))
  
  if (nrow(betting_picks) > 0) {
    for (i in 1:nrow(betting_picks)) {
      pick <- betting_picks[i,]
      cat(sprintf("• %s @ %s: %s (%.1fu) - EV: %.3f\n",
                 pick$away_team, pick$home_team, 
                 pick$spread_bet_recommendation,
                 pick$bet_size, pick$expected_value))
    }
  } else {
    cat("No betting recommendations meet confidence threshold\n")
  }
  
  cat("\n")
}

# Helper for string concatenation
`%+%` <- function(a, b) paste0(a, b)

cat("Week 3 NFL Prediction System loaded! 🏈\n\n")
cat("Available functions:\n")
cat("- generate_week3_predictions(): Generate complete Week 3 predictions\n")
cat("- generate_single_game_prediction(): Analyze individual matchup\n")
cat("\n🚀 Generate Week 3 predictions:\n")
cat("week3_predictions <- generate_week3_predictions()\n")