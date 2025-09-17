# Enhanced Complete Prediction System
# Now with real ESPN Injuries API + Unified Odds System

source('complete_prediction_system.R')
source('espn_injuries_api.R')
source('espn_odds_api.R')
source('weighted_epa_system.R')

# Fallback team strength estimation when EPA data unavailable
get_fallback_prediction <- function(home_team, away_team) {
  
  # Simple team strength based on recent performance (Week 1-2 2025 results)
  team_strength <- list(
    # Strong teams (Week 1-2 winners/good performances)
    "BUF" = 3.0, "KC" = 2.5, "LAC" = 2.0, "PHI" = 2.0, "DET" = 3.5,
    "GB" = 2.0, "SEA" = 1.5, "ATL" = 2.5, "TB" = 1.0, "LAR" = 1.5,
    "CLE" = 1.0, "IND" = 2.0, "JAX" = 1.5, "NO" = 1.0, "DEN" = 1.0,
    
    # Average teams
    "NYJ" = 0.0, "WAS" = 0.0, "NE" = 0.0, "ARI" = 0.5, "SF" = 0.5,
    "PIT" = 0.0, "TEN" = -0.5, "CHI" = -1.0, "BAL" = 1.0, "CIN" = 0.5,
    
    # Weaker teams (poor Week 1-2 performances)
    "MIN" = -1.5, "HOU" = -1.0, "DAL" = 0.5, "NYG" = -1.0, "CAR" = -2.0,
    "MIA" = -1.5, "LV" = -2.0
  )
  
  home_strength <- ifelse(home_team %in% names(team_strength), team_strength[[home_team]], 0)
  away_strength <- ifelse(away_team %in% names(team_strength), team_strength[[away_team]], 0)
  
  # Add home field advantage
  fallback_margin <- (home_strength - away_strength) + 2.5
  
  cat(sprintf("Fallback: %s (%.1f) vs %s (%.1f) = %.1f\n", 
              home_team, home_strength + 2.5, away_team, away_strength, fallback_margin))
  
  return(fallback_margin)
}

# Enhanced prediction function with real injury data
predict_game_enhanced <- function(home_team, away_team, vegas_spread = 0, 
                                 epa_metrics = NULL, game_date = NULL,
                                 is_primetime = FALSE, is_divisional = FALSE,
                                 home_rest_days = 7, away_rest_days = 7,
                                 weather_impact = 0, dome_game = FALSE,
                                 use_live_injuries = TRUE) {
  
  cat(sprintf("=== ENHANCED PREDICTION: %s @ %s ===\n", away_team, home_team))
  
  # 1. Base EPA Prediction (55% weight - reduced to make room for injuries)
  if(is.null(epa_metrics)) {
    # Try to create EPA metrics, with fallback if data unavailable
    tryCatch({
      epa_metrics <- create_weighted_epa_metrics(1.0, 0.0)
    }, error = function(e) {
      cat("⚠️ EPA data unavailable, using fallback prediction\n")
      epa_metrics <- NULL
    })
  }
  
  # Handle EPA prediction with fallback
  if(!is.null(epa_metrics) && is.data.frame(epa_metrics) && nrow(epa_metrics) > 0) {
    home_epa <- epa_metrics$net_epa[epa_metrics$team == home_team]
    away_epa <- epa_metrics$net_epa[epa_metrics$team == away_team]
    
    if(length(home_epa) > 0 && length(away_epa) > 0) {
      epa_diff <- home_epa - away_epa
      base_margin <- epa_diff * 15 + 2.5  # Scale + home field advantage
    } else {
      base_margin <- 2.5  # Just home field advantage
    }
  } else {
    # Fallback: use simple team strength based on recent performance
    cat("Using fallback team strength estimation\n")
    base_margin <- get_fallback_prediction(home_team, away_team)
  }
  
  epa_weight <- 0.55
  cat(sprintf("EPA Base Prediction: %.1f (weight: %.0f%%)\n", base_margin, epa_weight * 100))
  
  # 2. Real Injury Analysis (15% weight - enhanced from 5%)
  injury_adjustment <- 0
  
  if(use_live_injuries) {
    cat("Getting real-time injury data...\n")
    injury_data <- get_matchup_injury_adjustments(home_team, away_team)
    
    if(!is.null(injury_data)) {
      # Net adjustment: positive helps home team
      injury_adjustment <- injury_data$net_adjustment
      
      cat(sprintf("Real injury impact: %+.1f points\n", injury_adjustment))
    } else {
      cat("No injury data available\n")  
    }
  }
  
  injury_weight <- 0.15
  
  # 3. Rest/Schedule Adjustments (12% weight)
  rest_adjustment <- 0
  
  # Short rest penalty
  if(home_rest_days < 6) rest_adjustment <- rest_adjustment - 1.5
  if(away_rest_days < 6) rest_adjustment <- rest_adjustment + 1.5
  
  # Extra rest advantage
  if(home_rest_days > 7 && away_rest_days <= 7) rest_adjustment <- rest_adjustment + 1.0
  if(away_rest_days > 7 && home_rest_days <= 7) rest_adjustment <- rest_adjustment - 1.0
  
  # Primetime adjustments
  if(is_primetime && !is.null(epa_metrics) && is.data.frame(epa_metrics)) {
    home_epa_net <- epa_metrics$net_epa[epa_metrics$team == home_team]
    away_epa_net <- epa_metrics$net_epa[epa_metrics$team == away_team]
    
    if(length(home_epa_net) > 0 && length(away_epa_net) > 0) {
      if(home_epa_net > away_epa_net) rest_adjustment <- rest_adjustment + 0.5
      else rest_adjustment <- rest_adjustment - 0.5
    }
  }
  
  rest_weight <- 0.12
  cat(sprintf("Rest/Schedule Adjustment: %+.1f (weight: %.0f%%)\n", 
              rest_adjustment, rest_weight * 100))
  
  # 4. Divisional Game Adjustments (8% weight)
  divisional_adjustment <- 0
  if(is_divisional) {
    divisional_adjustment <- -abs(base_margin) * 0.10
  }
  
  divisional_weight <- 0.08
  cat(sprintf("Divisional Adjustment: %+.1f (weight: %.0f%%)\n", 
              divisional_adjustment, divisional_weight * 100))
  
  # 5. Weather/Venue Adjustments (10% weight)
  weather_adjustment <- weather_impact
  
  if(dome_game && !is.null(epa_metrics) && is.data.frame(epa_metrics)) {
    # Dome helps passing teams (simplified)
    home_off_epa <- epa_metrics$off_epa_play[epa_metrics$team == home_team]
    away_off_epa <- epa_metrics$off_epa_play[epa_metrics$team == away_team]
    
    if(length(home_off_epa) > 0 && length(away_off_epa) > 0) {
      if(home_off_epa > away_off_epa) weather_adjustment <- weather_adjustment + 0.5
      else weather_adjustment <- weather_adjustment - 0.5
    }
  }
  
  weather_weight <- 0.10
  cat(sprintf("Weather/Venue Adjustment: %+.1f (weight: %.0f%%)\n", 
              weather_adjustment, weather_weight * 100))
  
  # Calculate final prediction with weighted factors
  final_margin <- (base_margin * epa_weight) + 
                  (injury_adjustment * injury_weight) +
                  (rest_adjustment * rest_weight) +
                  (divisional_adjustment * divisional_weight) +
                  (weather_adjustment * weather_weight)
  
  # Calculate confidence
  total_adjustments <- abs(injury_adjustment) + abs(rest_adjustment) + 
                      abs(divisional_adjustment) + abs(weather_adjustment)
  
  confidence_penalty <- min(total_adjustments * 0.03, 0.10)
  base_confidence <- 0.75
  final_confidence <- max(base_confidence - confidence_penalty, 0.60)
  
  cat(sprintf("\n🎯 FINAL PREDICTION: %s by %.1f\n", 
              ifelse(final_margin > 0, home_team, away_team), abs(final_margin)))
  cat(sprintf("📊 Confidence: %.2f\n", final_confidence))
  cat(sprintf("📈 vs Vegas: %+.1f point edge\n", final_margin - (-vegas_spread)))
  
  # Enhanced betting recommendation
  spread_diff <- final_margin - (-vegas_spread)
  
  if(abs(spread_diff) < 1.5) {
    recommendation <- "PASS - Too close to market"
    bet_size <- "None"
  } else if(abs(spread_diff) < 2.5) {
    recommendation <- sprintf("SMALL BET %s", ifelse(spread_diff > 0, home_team, away_team))
    bet_size <- "1 unit"
  } else if(abs(spread_diff) < 3.5) {
    recommendation <- sprintf("MEDIUM BET %s", ifelse(spread_diff > 0, home_team, away_team))
    bet_size <- "2 units"
  } else if(abs(spread_diff) < 5.0) {
    recommendation <- sprintf("LARGE BET %s", ifelse(spread_diff > 0, home_team, away_team))
    bet_size <- "3 units"
  } else {
    recommendation <- sprintf("MAX BET %s", ifelse(spread_diff > 0, home_team, away_team))
    bet_size <- "5 units"
  }
  
  cat(sprintf("💰 Betting Rec: %s (%s)\n", recommendation, bet_size))
  
  return(list(
    predicted_margin = final_margin,
    confidence = final_confidence,
    recommendation = recommendation,
    bet_size = bet_size,
    components = list(
      epa_base = base_margin,
      injury_adjustment = injury_adjustment,
      rest_adjustment = rest_adjustment,
      divisional_adjustment = divisional_adjustment,
      weather_adjustment = weather_adjustment
    ),
    edge_vs_vegas = spread_diff
  ))
}

# Function to get Week 3 games and test enhanced system
test_enhanced_system_week3 <- function() {
  
  cat("=== TESTING ENHANCED SYSTEM ON WEEK 3 2025 ===\n\n")
  
  # Sample Week 3 games (would be replaced with real upcoming games)
  week3_games <- data.frame(
    away_team = c("NE", "DEN", "LAC", "CHI", "HOU"),
    home_team = c("NYJ", "TB", "PIT", "IND", "MIN"),
    vegas_spread = c(-4.0, -3.0, -3.5, -2.5, -6.0),  # Estimated spreads
    is_primetime = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    dome_game = c(FALSE, TRUE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  
  cat("Loading EPA metrics (with fallback if unavailable)...\n")
  epa_metrics <- NULL  # Let the prediction function handle EPA loading with fallback
  
  predictions <- list()
  
  for(i in 1:nrow(week3_games)) {
    game <- week3_games[i,]
    
    cat(sprintf("\n%s. %s @ %s\n", i, game$away_team, game$home_team))
    cat(paste(rep("=", 50), collapse = ""), "\n")
    
    pred <- predict_game_enhanced(
      home_team = game$home_team,
      away_team = game$away_team,
      vegas_spread = game$vegas_spread,
      epa_metrics = epa_metrics,
      is_primetime = game$is_primetime,
      dome_game = game$dome_game,
      use_live_injuries = TRUE
    )
    
    predictions[[i]] <- pred
    cat("\n")
  }
  
  # Summary
  cat("=== WEEK 3 PREDICTIONS SUMMARY ===\n")
  
  large_bets <- sum(sapply(predictions, function(x) grepl("LARGE|MAX", x$recommendation)))
  medium_bets <- sum(sapply(predictions, function(x) grepl("MEDIUM", x$recommendation)))
  small_bets <- sum(sapply(predictions, function(x) grepl("SMALL", x$recommendation)))
  
  cat(sprintf("🔥 Large/Max Bets: %d\n", large_bets))
  cat(sprintf("🎯 Medium Bets: %d\n", medium_bets))
  cat(sprintf("💡 Small Bets: %d\n", small_bets))
  cat(sprintf("⏸️  Pass Plays: %d\n", length(predictions) - large_bets - medium_bets - small_bets))
  
  avg_confidence <- mean(sapply(predictions, function(x) x$confidence))
  cat(sprintf("📊 Average Confidence: %.2f\n", avg_confidence))
  
  return(predictions)
}

cat("Enhanced Complete Prediction System loaded! 🚀\n\n")
cat("🔧 ENHANCED FEATURES:\n")
cat("✅ Real ESPN injury data (15% weight)\n")
cat("✅ Pure 2025 EPA metrics (55% weight)\n") 
cat("✅ Unified odds system with fallback\n")
cat("✅ Enhanced betting recommendations\n")
cat("✅ Improved confidence scoring\n\n")

cat("📊 FUNCTIONS AVAILABLE:\n")
cat("- predict_game_enhanced(): Enhanced prediction with real injuries\n")
cat("- test_enhanced_system_week3(): Test system on Week 3 games\n\n")

cat("🎮 To test enhanced system: test_enhanced_system_week3()\n")