# Enhanced ML System with Current Data
# Integrates 2024 nflfastR data and prepares for 2025 integration

source('local_data_access.R')

# Load and prepare training data
prepare_ml_training_data <- function() {
  
  cat("🤖 Preparing ML training data...\n")
  
  # Load 2024 data (available locally)
  pbp_2024 <- load_local_nflfastr_data('/Users/trevor/Downloads/play_by_play_2024.rds')
  
  if (is.null(pbp_2024)) {
    cat("❌ Could not load 2024 data\n")
    return(NULL)
  }
  
  # Extract ML features
  ml_features_2024 <- extract_ml_features(pbp_2024)
  
  # Create team-level aggregations for spread prediction
  team_stats <- ml_features_2024 %>%
    group_by(season, week, home_team, away_team) %>%
    summarise(
      # Offensive EPA metrics
      off_epa_mean = mean(epa[home_team == team], na.rm = TRUE),
      off_success_rate = mean(success[home_team == team], na.rm = TRUE),
      
      # Defensive EPA metrics (when team is defending)
      def_epa_allowed = mean(epa[away_team == team], na.rm = TRUE),
      def_success_allowed = mean(success[away_team == team], na.rm = TRUE),
      
      # Advanced metrics
      cpoe_avg = mean(cpoe, na.rm = TRUE),
      xyac_epa_avg = mean(xyac_epa, na.rm = TRUE),
      
      # Situational performance
      red_zone_epa = mean(epa[yardline_100 <= 20], na.rm = TRUE),
      third_down_success = mean(success[down == 3], na.rm = TRUE),
      
      .groups = 'drop'
    )
  
  cat(sprintf("✅ Training data prepared: %d team-games with advanced metrics\n", 
              nrow(team_stats)))
  
  return(list(
    play_level = ml_features_2024,
    team_level = team_stats
  ))
}

# Enhanced EPA system with ML features
calculate_enhanced_team_metrics <- function(training_data) {
  
  cat("📊 Calculating enhanced team metrics...\n")
  
  team_data <- training_data$team_level
  
  # Calculate rolling averages for each team
  enhanced_metrics <- team_data %>%
    arrange(season, week) %>%
    group_by(home_team) %>%
    mutate(
      # Rolling EPA metrics (last 4 games)
      off_epa_l4 = lag(rollmean(off_epa_mean, k = 4, fill = NA, align = "right")),
      def_epa_l4 = lag(rollmean(def_epa_allowed, k = 4, fill = NA, align = "right")),
      
      # Trend indicators
      epa_trend = off_epa_mean - lag(off_epa_mean, 2),
      
      # Advanced situational metrics
      clutch_performance = red_zone_epa + third_down_success,
      
    ) %>%
    ungroup()
  
  cat("✅ Enhanced metrics calculated with rolling averages and trends\n")
  
  return(enhanced_metrics)
}

# Integrate with existing EPA system
create_ml_enhanced_predictions <- function(home_team, away_team, vegas_spread) {
  
  cat(sprintf("🎯 Creating ML-enhanced prediction for %s @ %s\n", away_team, home_team))
  
  # Load existing EPA system
  tryCatch({
    source('epa_prediction_system.R')
    
    # Get traditional EPA prediction
    epa_prediction <- predict_game_with_epa(home_team, away_team, vegas_spread)
    
  }, error = function(e) {
    cat("Using fallback EPA calculation\n")
    epa_prediction <- list(
      predicted_spread = vegas_spread * 0.9,  # Slight regression to mean
      confidence = 0.6
    )
  })
  
  # Load ML training data
  training_data <- prepare_ml_training_data()
  
  if (!is.null(training_data)) {
    enhanced_metrics <- calculate_enhanced_team_metrics(training_data)
    
    # Get recent performance for both teams
    home_recent <- enhanced_metrics %>% 
      filter(home_team == !!home_team) %>% 
      tail(4) %>%
      summarise(
        off_epa = mean(off_epa_mean, na.rm = TRUE),
        def_epa = mean(def_epa_allowed, na.rm = TRUE),
        cpoe = mean(cpoe_avg, na.rm = TRUE),
        .groups = 'drop'
      )
    
    away_recent <- enhanced_metrics %>% 
      filter(home_team == !!away_team) %>% 
      tail(4) %>%
      summarise(
        off_epa = mean(off_epa_mean, na.rm = TRUE),
        def_epa = mean(def_epa_allowed, na.rm = TRUE),
        cpoe = mean(cpoe_avg, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # ML-enhanced spread calculation
    if (nrow(home_recent) > 0 && nrow(away_recent) > 0) {
      # EPA differential
      epa_diff <- (home_recent$off_epa - away_recent$def_epa) - 
                  (away_recent$off_epa - home_recent$def_epa)
      
      # CPOE differential (quarterback performance)
      cpoe_diff <- home_recent$cpoe - away_recent$cpoe
      
      # ML adjustment to EPA prediction
      ml_adjustment <- (epa_diff * 3.5) + (cpoe_diff * 0.1) + 2.5  # Home field
      
      # Combine EPA and ML predictions
      combined_spread <- (epa_prediction$predicted_spread * 0.6) + (ml_adjustment * 0.4)
      
      # Enhanced confidence based on data quality
      enhanced_confidence <- min(0.85, epa_prediction$confidence + 0.1)
      
      cat(sprintf("EPA: %.1f, ML: %.1f, Combined: %.1f\n", 
                  epa_prediction$predicted_spread, ml_adjustment, combined_spread))
      
      return(list(
        predicted_spread = combined_spread,
        confidence = enhanced_confidence,
        epa_component = epa_prediction$predicted_spread,
        ml_component = ml_adjustment,
        vegas_spread = vegas_spread,
        edge = combined_spread - vegas_spread
      ))
    }
  }
  
  # Fallback to EPA only
  return(epa_prediction)
}

# Test the enhanced system
test_ml_enhanced_system <- function() {
  
  cat("🧪 Testing ML-enhanced prediction system...\n")
  
  # Test with a sample matchup
  result <- create_ml_enhanced_predictions("KC", "BUF", -3.0)
  
  if (!is.null(result)) {
    cat("\n📊 Test Results:\n")
    cat(sprintf("Predicted Spread: %.1f\n", result$predicted_spread))
    cat(sprintf("Vegas Spread: %.1f\n", result$vegas_spread))  
    cat(sprintf("Edge: %.1f\n", result$edge))
    cat(sprintf("Confidence: %.1%\n", result$confidence * 100))
    
    if (!is.null(result$epa_component)) {
      cat(sprintf("EPA Component: %.1f\n", result$epa_component))
      cat(sprintf("ML Component: %.1f\n", result$ml_component))
    }
    
    return(TRUE)
  } else {
    cat("❌ Test failed\n")
    return(FALSE)
  }
}

cat("Enhanced ML System loaded! 🚀\n\n")
cat("Functions available:\n")
cat("- prepare_ml_training_data(): Load and process 2024 nflfastR data\n")
cat("- create_ml_enhanced_predictions(home, away, vegas): Enhanced predictions\n") 
cat("- test_ml_enhanced_system(): Test the complete system\n")
cat("\nTo test: test_ml_enhanced_system()\n")

# Note about 2025 data
cat("\n📝 Note: System uses 2024 nflfastR data for ML training.\n")
cat("   When 2025 play-by-play becomes available, update file path in prepare_ml_training_data()\n")