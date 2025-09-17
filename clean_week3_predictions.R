# Clean Week 3 2025 Predictions Output
# Enhanced system predictions vs Vegas with ML/ATS recommendations

source('enhanced_prediction_system.R')

# Function to generate clean Week 3 predictions
generate_clean_week3_predictions <- function() {
  
  cat("=== WEEK 3 2025 NFL PREDICTIONS ===\n")
  cat("Enhanced System vs Vegas Lines\n\n")
  
  # Week 3 games with estimated Vegas spreads
  week3_games <- data.frame(
    away_team = c("NE", "DEN", "LAC", "CHI", "HOU"),
    home_team = c("NYJ", "TB", "PIT", "IND", "MIN"),
    vegas_spread = c(-4.0, -3.0, -3.5, -2.5, -6.0),
    is_primetime = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    dome_game = c(FALSE, TRUE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  
  # Run predictions quietly
  predictions <- list()
  
  for(i in 1:nrow(week3_games)) {
    game <- week3_games[i,]
    
    # Suppress output during prediction
    capture.output({
      pred <- predict_game_enhanced(
        home_team = game$home_team,
        away_team = game$away_team,
        vegas_spread = game$vegas_spread,
        epa_metrics = NULL,  # Use fallback
        is_primetime = game$is_primetime,
        dome_game = game$dome_game,
        use_live_injuries = FALSE  # Skip injury API calls
      )
    })
    
    predictions[[i]] <- pred
  }
  
  # Generate clean output
  cat("GAME                 OUR SPREAD    VEGAS      EDGE    ML PICK    ATS PICK     BET REC\n")
  cat("================================================================================\n")
  
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
    
    # Moneyline pick (who wins straight up)
    ml_pick <- ifelse(pred$predicted_margin > 0, game$home_team, game$away_team)
    ml_pick_str <- sprintf("%-8s", ml_pick)
    
    # ATS pick (who covers the spread)
    vegas_favorite <- ifelse(game$vegas_spread < 0, game$home_team, game$away_team)
    ats_pick <- ifelse(abs(edge) < 1.5, "PUSH", 
                      ifelse(edge > 0, game$home_team, game$away_team))
    ats_pick_str <- sprintf("%-8s", ats_pick)
    
    # Betting recommendation (simplified)
    bet_rec <- pred$recommendation
    if(grepl("PASS", bet_rec)) {
      bet_rec_str <- "PASS"
    } else if(grepl("SMALL", bet_rec)) {
      bet_team <- ifelse(edge > 0, game$home_team, game$away_team)
      bet_rec_str <- sprintf("%s (1u)", bet_team)
    } else if(grepl("MEDIUM", bet_rec)) {
      bet_team <- ifelse(edge > 0, game$home_team, game$away_team)
      bet_rec_str <- sprintf("%s (2u)", bet_team)
    } else if(grepl("LARGE", bet_rec)) {
      bet_team <- ifelse(edge > 0, game$home_team, game$away_team)
      bet_rec_str <- sprintf("%s (3u)", bet_team)
    } else {
      bet_rec_str <- "PASS"
    }
    
    cat(sprintf("%s %s    %s   %s   %s   %s    %s\n",
                game_str, our_spread_str, vegas_str, edge_str, 
                ml_pick_str, ats_pick_str, bet_rec_str))
  }
  
  cat("================================================================================\n")
  cat("Legend: Edge = Our spread vs Vegas (positive = we favor home team more)\n")
  cat("        ML Pick = Moneyline winner prediction\n") 
  cat("        ATS Pick = Against the spread pick\n")
  cat("        Bet Rec = Betting recommendation with unit size\n\n")
  
  # Summary stats
  total_games <- length(predictions)
  bet_games <- sum(!grepl("PASS", sapply(predictions, function(x) x$recommendation)))
  avg_confidence <- mean(sapply(predictions, function(x) x$confidence))
  
  cat("SUMMARY:\n")
  cat(sprintf("Games Analyzed: %d\n", total_games))
  cat(sprintf("Betting Opportunities: %d/%d (%.1f%%)\n", bet_games, total_games, (bet_games/total_games)*100))
  cat(sprintf("Average Confidence: %.2f\n", avg_confidence))
  
  return(predictions)
}

# Execute and return clean predictions
clean_predictions <- generate_clean_week3_predictions()