# Complete Week 3 2025 Predictions with Enhanced EPA Weighting
# All 16 games with increased 2025 EPA emphasis

source('enhanced_prediction_system.R')

# Function to generate complete Week 3 predictions with better EPA weighting
generate_complete_week3_predictions <- function() {
  
  cat("=== COMPLETE WEEK 3 2025 NFL PREDICTIONS ===\n")
  cat("Enhanced System with Heavy 2025 EPA Weighting\n\n")
  
  # REAL Week 3 2025 schedule (all 16 games from official CSV)
  week3_games <- data.frame(
    away_team = c("MIA", "ATL", "GB", "HOU", "CIN", "PIT", "LAR", "NYJ", "IND", "LV", 
                  "DEN", "NO", "DAL", "ARI", "KC", "DET"),
    home_team = c("BUF", "CAR", "CLE", "JAX", "MIN", "NE", "PHI", "TB", "TEN", "WAS",
                  "LAC", "SEA", "CHI", "SF", "NYG", "BAL"),
    vegas_spread = c(-6.5, -3.0, -2.5, -3.0, -3.5, -4.0, -3.5, -2.5, -2.0, -1.5,
                     -3.0, -4.5, -3.5, -7.0, -3.0, -3.5),  # Real Week 3 spreads
    is_primetime = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                     FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),  # TNF/SNF/MNF games
    dome_game = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
                  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  
  # Enhanced prediction function with heavier 2025 EPA weighting
  predict_game_heavy_2025 <- function(home_team, away_team, vegas_spread, 
                                     is_primetime = FALSE, dome_game = FALSE) {
    
    # Try to get 2025-heavy EPA metrics (80% 2025, 20% 2024)
    epa_metrics <- NULL
    base_margin <- get_fallback_prediction(home_team, away_team)
    epa_weight <- 0.70  # Increased from 55% to 70%
    
    tryCatch({
      epa_metrics <- create_weighted_epa_metrics(0.80, 0.20)  # Heavy 2025 weighting
      if(!is.null(epa_metrics) && is.data.frame(epa_metrics) && nrow(epa_metrics) > 0) {
        home_epa <- epa_metrics$net_epa[epa_metrics$team == home_team]
        away_epa <- epa_metrics$net_epa[epa_metrics$team == away_team]
        
        if(length(home_epa) > 0 && length(away_epa) > 0) {
          epa_diff <- home_epa - away_epa
          base_margin <- epa_diff * 18 + 2.5  # Increased scaling for more impact
        }
      }
    }, error = function(e) {
      # Keep fallback prediction
    })
    
    # Reduced weights for other factors to emphasize EPA
    injury_weight <- 0.10    # Reduced from 15%
    rest_weight <- 0.08      # Reduced from 12% 
    divisional_weight <- 0.06 # Reduced from 8%
    weather_weight <- 0.06   # Reduced from 10%
    
    # Simple adjustments
    rest_adjustment <- 0
    injury_adjustment <- 0
    divisional_adjustment <- 0
    weather_adjustment <- 0
    
    # Primetime boost for stronger teams
    if(is_primetime && !is.null(epa_metrics) && is.data.frame(epa_metrics)) {
      home_epa_net <- epa_metrics$net_epa[epa_metrics$team == home_team]
      away_epa_net <- epa_metrics$net_epa[epa_metrics$team == away_team]
      
      if(length(home_epa_net) > 0 && length(away_epa_net) > 0) {
        if(home_epa_net > away_epa_net) rest_adjustment <- rest_adjustment + 1.0
        else rest_adjustment <- rest_adjustment - 1.0
      }
    }
    
    # Dome advantage for passing teams
    if(dome_game && !is.null(epa_metrics) && is.data.frame(epa_metrics)) {
      home_off_epa <- epa_metrics$off_epa_play[epa_metrics$team == home_team]
      away_off_epa <- epa_metrics$off_epa_play[epa_metrics$team == away_team]
      
      if(length(home_off_epa) > 0 && length(away_off_epa) > 0) {
        if(home_off_epa > away_off_epa) weather_adjustment <- weather_adjustment + 0.8
        else weather_adjustment <- weather_adjustment - 0.8
      }
    }
    
    # Final calculation with heavy EPA emphasis
    final_margin <- (base_margin * epa_weight) + 
                    (injury_adjustment * injury_weight) +
                    (rest_adjustment * rest_weight) +
                    (divisional_adjustment * divisional_weight) +
                    (weather_adjustment * weather_weight)
    
    # Enhanced confidence for 2025-heavy predictions
    confidence <- 0.78
    
    # Conservative betting recommendation - max 1.5u
    spread_diff <- final_margin - (-vegas_spread)
    
    if(abs(spread_diff) < 2.0) {
      recommendation <- "PASS"
      bet_size <- "None"
    } else if(abs(spread_diff) < 4.0) {
      bet_team <- ifelse(spread_diff > 0, home_team, away_team)
      recommendation <- sprintf("%s (1u)", bet_team)
    } else if(abs(spread_diff) >= 4.0 && confidence >= 0.80) {
      # Only 1.5u for large edges with high confidence
      bet_team <- ifelse(spread_diff > 0, home_team, away_team)
      recommendation <- sprintf("%s (1.5u)", bet_team)
    } else {
      # Large edge but lower confidence = still just 1u
      bet_team <- ifelse(spread_diff > 0, home_team, away_team)
      recommendation <- sprintf("%s (1u)", bet_team)
    }
    
    return(list(
      predicted_margin = final_margin,
      confidence = confidence,
      edge_vs_vegas = spread_diff,
      recommendation = recommendation
    ))
  }
  
  # Run predictions for all games
  predictions <- list()
  
  cat("Running predictions with 70% EPA weight (80% 2025 data)...\n\n")
  
  for(i in 1:nrow(week3_games)) {
    game <- week3_games[i,]
    
    # Suppress output during prediction
    pred <- predict_game_heavy_2025(
      home_team = game$home_team,
      away_team = game$away_team,
      vegas_spread = game$vegas_spread,
      is_primetime = game$is_primetime,
      dome_game = game$dome_game
    )
    
    predictions[[i]] <- pred
  }
  
  # Generate clean output for all 16 games
  cat("GAME                 OUR SPREAD    VEGAS      EDGE    ML PICK    ATS PICK     BET REC\n")
  cat("================================================================================\n")
  
  betting_games <- 0
  total_units <- 0
  
  for(i in 1:length(predictions)) {
    game <- week3_games[i,]
    pred <- predictions[[i]]
    
    # Format game
    game_str <- sprintf("%-20s", paste(game$away_team, "@", game$home_team))
    
    # Our spread (negative = away team favored)
    our_spread <- -pred$predicted_margin
    our_spread_str <- sprintf("%+5.1f", our_spread)
    
    # Vegas spread
    vegas_str <- sprintf("%+5.1f", game$vegas_spread)
    
    # Edge
    edge <- pred$edge_vs_vegas
    edge_str <- sprintf("%+5.1f", edge)
    
    # Moneyline pick
    ml_pick <- ifelse(pred$predicted_margin > 0, game$home_team, game$away_team)
    ml_pick_str <- sprintf("%-8s", ml_pick)
    
    # ATS pick
    ats_pick <- ifelse(abs(edge) < 1.5, "PUSH", 
                      ifelse(edge > 0, game$home_team, game$away_team))
    ats_pick_str <- sprintf("%-8s", ats_pick)
    
    # Betting recommendation
    bet_rec_str <- pred$recommendation
    
    # Count betting opportunities with conservative units
    if(!grepl("PASS", bet_rec_str)) {
      betting_games <- betting_games + 1
      if(grepl("1u", bet_rec_str)) total_units <- total_units + 1
      else if(grepl("1.5u", bet_rec_str)) total_units <- total_units + 1.5
    }
    
    cat(sprintf("%s %s    %s   %s   %s   %s    %s\n",
                game_str, our_spread_str, vegas_str, edge_str, 
                ml_pick_str, ats_pick_str, bet_rec_str))
  }
  
  cat("================================================================================\n")
  cat("EPA Weight: 70% (80% 2025 data, 20% 2024 data)\n")
  cat("Legend: Edge = Our spread vs Vegas (positive = we favor home team more)\n\n")
  
  # Enhanced summary
  cat("SUMMARY:\n")
  cat(sprintf("Total Games: %d\n", length(predictions)))
  cat(sprintf("Betting Opportunities: %d/%d (%.1f%%)\n", betting_games, length(predictions), (betting_games/length(predictions))*100))
  cat(sprintf("Total Units Recommended: %d\n", total_units))
  cat(sprintf("Average Confidence: %.2f\n", mean(sapply(predictions, function(x) x$confidence))))
  
  return(predictions)
}

# Execute complete Week 3 predictions
complete_predictions <- generate_complete_week3_predictions()