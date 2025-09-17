# Complete NFL Prediction System
# Combines EPA + Injuries + Travel + Weather + Rest factors

source('epa_prediction_system.R')
source('epa_live_betting.R')
library(dplyr)

# Enhanced prediction function with all factors
predict_game_complete <- function(home_team, away_team, vegas_spread = 0, 
                                 epa_metrics = NULL, game_date = NULL,
                                 is_primetime = FALSE, is_divisional = FALSE,
                                 home_rest_days = 7, away_rest_days = 7,
                                 weather_impact = 0, dome_game = FALSE) {
  
  cat(sprintf("=== COMPLETE PREDICTION: %s @ %s ===\n", away_team, home_team))
  
  # 1. Base EPA Prediction (60% weight)
  epa_pred <- predict_game_with_epa(home_team, away_team, vegas_spread, epa_metrics)
  base_margin <- epa_pred$predicted_margin
  epa_weight <- 0.60
  
  cat(sprintf("EPA Base Prediction: %.1f (weight: %.0f%%)\n", base_margin, epa_weight * 100))
  
  # 2. Rest/Schedule Adjustments (15% weight)
  rest_adjustment <- 0
  
  # Short rest penalty (Thursday games, etc.)
  if(home_rest_days < 6) rest_adjustment <- rest_adjustment - 1.5
  if(away_rest_days < 6) rest_adjustment <- rest_adjustment + 1.5
  
  # Extra rest advantage
  if(home_rest_days > 7 && away_rest_days <= 7) rest_adjustment <- rest_adjustment + 1.0
  if(away_rest_days > 7 && home_rest_days <= 7) rest_adjustment <- rest_adjustment - 1.0
  
  # Primetime game adjustments (better teams perform better in primetime)
  if(is_primetime) {
    # Teams with higher EPA typically perform better in primetime
    # Handle both data frame and list structures for epa_metrics
    if(is.data.frame(epa_metrics)) {
      home_metrics <- epa_metrics[epa_metrics$team == home_team, ]
      away_metrics <- epa_metrics[epa_metrics$team == away_team, ]
      
      # Check if team data exists
      if(nrow(home_metrics) == 0 || nrow(away_metrics) == 0) {
        cat(sprintf("Warning: Missing EPA data for %s or %s, skipping primetime adjustment\n", 
                    home_team, away_team))
      } else {
        # Calculate net EPA (offensive EPA - defensive EPA allowed)
        home_epa <- home_metrics$off_epa_play - home_metrics$def_epa_play
        away_epa <- away_metrics$off_epa_play - away_metrics$def_epa_play
        
        # Validate EPA values are not empty/NA
        if(length(home_epa) > 0 && length(away_epa) > 0 && 
           !is.na(home_epa) && !is.na(away_epa)) {
          if(home_epa > away_epa) rest_adjustment <- rest_adjustment + 0.5
          else rest_adjustment <- rest_adjustment - 0.5
          cat(sprintf("Primetime adjustment: %s EPA=%.3f vs %s EPA=%.3f\n", 
                      home_team, home_epa, away_team, away_epa))
        } else {
          cat("Warning: Invalid EPA values, skipping primetime adjustment\n")
        }
      }
    } else {
      # Handle list structure (legacy support)
      if(!is.null(epa_metrics[[home_team]]) && !is.null(epa_metrics[[away_team]])) {
        home_off_epa <- ifelse(is.null(epa_metrics[[home_team]]$off_epa_play), 0, epa_metrics[[home_team]]$off_epa_play)
        home_def_epa <- ifelse(is.null(epa_metrics[[home_team]]$def_epa_play), 0, epa_metrics[[home_team]]$def_epa_play)
        away_off_epa <- ifelse(is.null(epa_metrics[[away_team]]$off_epa_play), 0, epa_metrics[[away_team]]$off_epa_play)
        away_def_epa <- ifelse(is.null(epa_metrics[[away_team]]$def_epa_play), 0, epa_metrics[[away_team]]$def_epa_play)
        
        home_epa <- home_off_epa - home_def_epa
        away_epa <- away_off_epa - away_def_epa
        
        if(home_epa > away_epa) rest_adjustment <- rest_adjustment + 0.5
        else rest_adjustment <- rest_adjustment - 0.5
      } else {
        cat(sprintf("Warning: Missing EPA data for %s or %s, skipping primetime adjustment\n", 
                    home_team, away_team))
      }
    }
  }
  
  rest_weight <- 0.15
  cat(sprintf("Rest/Schedule Adjustment: %+.1f (weight: %.0f%%)\n", 
              rest_adjustment, rest_weight * 100))
  
  # 3. Divisional Game Adjustments (10% weight)
  divisional_adjustment <- 0
  if(is_divisional) {
    # Divisional games are typically closer, reduce margin by 10%
    divisional_adjustment <- -abs(base_margin) * 0.10
  }
  
  divisional_weight <- 0.10
  cat(sprintf("Divisional Adjustment: %+.1f (weight: %.0f%%)\n", 
              divisional_adjustment, divisional_weight * 100))
  
  # 4. Weather/Venue Adjustments (10% weight)
  weather_adjustment <- weather_impact
  
  # Dome advantage for passing teams
  if(dome_game) {
    # Handle both data frame and list structures for epa_metrics
    if(is.data.frame(epa_metrics)) {
      home_metrics <- epa_metrics[epa_metrics$team == home_team, ]
      away_metrics <- epa_metrics[epa_metrics$team == away_team, ]
      
      # Check if team data exists
      if(nrow(home_metrics) == 0 || nrow(away_metrics) == 0) {
        cat(sprintf("Warning: Missing EPA data for %s or %s, skipping dome adjustment\n", 
                    home_team, away_team))
      } else {
        home_pass_epa <- home_metrics$pass_epa_play
        away_pass_epa <- away_metrics$pass_epa_play
        
        # Validate EPA values are not empty/NA
        if(length(home_pass_epa) > 0 && length(away_pass_epa) > 0 && 
           !is.na(home_pass_epa) && !is.na(away_pass_epa)) {
          if(home_pass_epa > away_pass_epa) weather_adjustment <- weather_adjustment + 0.5
          else weather_adjustment <- weather_adjustment - 0.5
          cat(sprintf("Dome adjustment: %s Pass EPA=%.3f vs %s Pass EPA=%.3f\n", 
                      home_team, home_pass_epa, away_team, away_pass_epa))
        } else {
          cat("Warning: Invalid pass EPA values, skipping dome adjustment\n")
        }
      }
    } else {
      # Handle list structure (legacy support)
      if(!is.null(epa_metrics[[home_team]]) && !is.null(epa_metrics[[away_team]])) {
        home_pass_epa <- ifelse(is.null(epa_metrics[[home_team]]$pass_epa_play), 0, epa_metrics[[home_team]]$pass_epa_play)
        away_pass_epa <- ifelse(is.null(epa_metrics[[away_team]]$pass_epa_play), 0, epa_metrics[[away_team]]$pass_epa_play)
        
        if(home_pass_epa > away_pass_epa) weather_adjustment <- weather_adjustment + 0.5
        else weather_adjustment <- weather_adjustment - 0.5
      } else {
        cat(sprintf("Warning: Missing EPA data for %s or %s, skipping dome adjustment\n", 
                    home_team, away_team))
      }
    }
  }
  
  weather_weight <- 0.10
  cat(sprintf("Weather/Venue Adjustment: %+.1f (weight: %.0f%%)\n", 
              weather_adjustment, weather_weight * 100))
  
  # 5. Injury Placeholder (5% weight) - Ready for integration
  injury_adjustment <- 0  # Would be calculated from injury data
  injury_weight <- 0.05
  cat(sprintf("Injury Adjustment: %+.1f (weight: %.0f%%) [PLACEHOLDER]\n", 
              injury_adjustment, injury_weight * 100))
  
  # Calculate final prediction with weighted factors
  final_margin <- (base_margin * epa_weight) + 
                  (rest_adjustment * rest_weight) +
                  (divisional_adjustment * divisional_weight) +
                  (weather_adjustment * weather_weight) +
                  (injury_adjustment * injury_weight)
  
  # Calculate confidence based on EPA confidence and adjustment magnitude
  total_adjustments <- abs(rest_adjustment) + abs(divisional_adjustment) + 
                      abs(weather_adjustment) + abs(injury_adjustment)
  
  # Reduce confidence if large adjustments are made
  confidence_penalty <- min(total_adjustments * 0.05, 0.15)
  final_confidence <- max(epa_pred$confidence - confidence_penalty, 0.60)
  
  cat(sprintf("\n🎯 FINAL PREDICTION: %s by %.1f\n", 
              ifelse(final_margin > 0, home_team, away_team), abs(final_margin)))
  cat(sprintf("📊 Confidence: %.2f\n", final_confidence))
  cat(sprintf("📈 vs Vegas: %+.1f point edge\n", final_margin - (-vegas_spread)))
  
  # Betting recommendation
  spread_diff <- final_margin - (-vegas_spread)
  
  if(abs(spread_diff) < 2.0) {
    recommendation <- "PASS - Close to market"
    bet_size <- "None"
  } else if(abs(spread_diff) < 3.0) {
    recommendation <- sprintf("SMALL BET %s", ifelse(spread_diff > 0, home_team, away_team))
    bet_size <- "1 unit"
  } else if(abs(spread_diff) < 4.0) {
    recommendation <- sprintf("MEDIUM BET %s", ifelse(spread_diff > 0, home_team, away_team))
    bet_size <- "2 units"
  } else {
    recommendation <- sprintf("LARGE BET %s", ifelse(spread_diff > 0, home_team, away_team))
    bet_size <- "3 units"
  }
  
  cat(sprintf("💰 Betting Rec: %s (%s)\n", recommendation, bet_size))
  
  # Return comprehensive prediction object
  return(list(
    predicted_margin = final_margin,
    confidence = final_confidence,
    recommendation = recommendation,
    bet_size = bet_size,
    components = list(
      epa_base = base_margin,
      rest_adjustment = rest_adjustment,
      divisional_adjustment = divisional_adjustment,
      weather_adjustment = weather_adjustment,
      injury_adjustment = injury_adjustment
    ),
    breakdown = epa_pred$breakdown,
    edge_vs_vegas = spread_diff
  ))
}

# Function to analyze a full slate of games
analyze_weekly_slate <- function(week_games, epa_metrics) {
  
  cat("=== WEEKLY SLATE ANALYSIS ===\n\n")
  
  predictions <- list()
  
  for(i in 1:nrow(week_games)) {
    game <- week_games[i,]
    
    # Detect game characteristics
    is_thursday <- grepl("Thu", game$game_day, ignore.case = TRUE)
    is_monday <- grepl("Mon", game$game_day, ignore.case = TRUE)
    is_primetime <- is_thursday || is_monday || grepl("SNF|MNF", game$game_type, ignore.case = TRUE)
    
    # Calculate rest days (simplified)
    home_rest <- ifelse(is_thursday, 4, 7)
    away_rest <- ifelse(is_thursday, 4, 7)
    
    # Check if divisional (would need division mapping)
    is_divisional <- FALSE  # Placeholder
    
    # Weather (placeholder - would integrate weather API)
    weather_impact <- 0
    dome_game <- game$home_team %in% c("ATL", "DET", "HOU", "IND", "LV", "LAR", "NO", "MIN")
    
    pred <- predict_game_complete(
      home_team = game$home_team,
      away_team = game$away_team,
      vegas_spread = ifelse(is.null(game$spread), 0, game$spread),
      epa_metrics = epa_metrics,
      is_primetime = is_primetime,
      is_divisional = is_divisional,
      home_rest_days = home_rest,
      away_rest_days = away_rest,
      weather_impact = weather_impact,
      dome_game = dome_game
    )
    
    predictions[[i]] <- pred
    cat("\n" %+% paste(rep("=", 50), collapse = "") %+% "\n\n")
  }
  
  # Summary of betting opportunities
  cat("=== BETTING SUMMARY ===\n")
  
  large_bets <- sum(sapply(predictions, function(x) grepl("LARGE", x$recommendation)))
  medium_bets <- sum(sapply(predictions, function(x) grepl("MEDIUM", x$recommendation)))
  small_bets <- sum(sapply(predictions, function(x) grepl("SMALL", x$recommendation)))
  
  cat(sprintf("🔥 Large Bets (3+ units): %d\n", large_bets))
  cat(sprintf("🎯 Medium Bets (2 units): %d\n", medium_bets))
  cat(sprintf("💡 Small Bets (1 unit): %d\n", small_bets))
  cat(sprintf("⏸️  Pass Plays: %d\n", length(predictions) - large_bets - medium_bets - small_bets))
  
  return(predictions)
}

# Helper function for string concatenation
`%+%` <- function(x, y) paste0(x, y)

cat("Complete NFL Prediction System loaded! 🏈\n\n")
cat("🔧 SYSTEM COMPONENTS:\n")
cat("✅ EPA-based team metrics (60% weight)\n")
cat("✅ Rest/Schedule factors (15% weight)\n") 
cat("✅ Divisional game adjustments (10% weight)\n")
cat("✅ Weather/Venue factors (10% weight)\n")
cat("🔄 Injury impact system (5% weight - ready for data)\n\n")

cat("📊 FUNCTIONS AVAILABLE:\n")
cat("- predict_game_complete(): Complete prediction with all factors\n")
cat("- analyze_weekly_slate(): Analyze full week of games\n\n")

cat("🎮 EXAMPLE USAGE:\n")
cat("pbp <- load_pbp(2025)\n")
cat("epa_metrics <- calculate_all_team_epa_metrics(pbp)\n")
cat("pred <- predict_game_complete('KC', 'BUF', vegas_spread = -2.5, epa_metrics = epa_metrics)\n")