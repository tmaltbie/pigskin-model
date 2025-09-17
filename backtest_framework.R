# NFL Spread Prediction Backtesting Framework

source("spread_predictor.R")

# Backtesting function
backtest_predictions <- function(completed_games, historical_spreads) {
  
  results <- data.frame(
    game_id = character(),
    home_team = character(),
    away_team = character(),
    actual_margin = numeric(),
    predicted_margin = numeric(),
    vegas_spread = numeric(),
    prediction_accuracy = numeric(),
    spread_bet_result = character(),
    prediction_correct = logical(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:nrow(completed_games)) {
    game <- completed_games[i,]
    
    # Get historical spread for this game
    spread <- historical_spreads[historical_spreads$game_id == game$game_id, "spread"]
    if(length(spread) == 0) next
    
    # Make prediction using historical data up to this game
    pred <- predict_game_spread(
      home_team = game$home_team,
      away_team = game$away_team,
      vegas_spread = spread,
      team_metrics = team_metrics,  # This should be calculated up to this point
      schedule_data = schedule_data  # Historical data only
    )
    
    # Calculate actual margin
    actual_margin <- game$home_score - game$away_score
    
    # Calculate prediction accuracy
    prediction_error <- abs(actual_margin - pred$predicted_margin)
    
    # Determine spread betting result
    home_covered <- (actual_margin + spread) > 0
    our_pick <- pred$recommendation
    
    spread_result <- case_when(
      our_pick == "PASS" ~ "NO BET",
      our_pick == "BET HOME" & home_covered ~ "WIN",
      our_pick == "BET HOME" & !home_covered ~ "LOSS",
      our_pick == "BET AWAY" & !home_covered ~ "WIN",
      our_pick == "BET AWAY" & home_covered ~ "LOSS",
      TRUE ~ "PUSH"
    )
    
    # Add to results
    results <- rbind(results, data.frame(
      game_id = game$game_id,
      home_team = game$home_team,
      away_team = game$away_team,
      actual_margin = actual_margin,
      predicted_margin = pred$predicted_margin,
      vegas_spread = spread,
      prediction_accuracy = prediction_error,
      spread_bet_result = spread_result,
      prediction_correct = prediction_error < 7,  # Within 1 touchdown
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

# Function to analyze backtest results
analyze_backtest_results <- function(results) {
  
  cat("=== BACKTESTING RESULTS ANALYSIS ===\n\n")
  
  # Overall prediction accuracy
  cat("PREDICTION ACCURACY:\n")
  cat(sprintf("Average prediction error: %.1f points\n", 
              mean(results$prediction_accuracy, na.rm = TRUE)))
  cat(sprintf("Predictions within 7 points: %.1f%%\n", 
              mean(results$prediction_correct, na.rm = TRUE) * 100))
  cat(sprintf("Predictions within 3 points: %.1f%%\n", 
              mean(results$prediction_accuracy <= 3, na.rm = TRUE) * 100))
  
  # Spread betting performance
  bets_made <- results[results$spread_bet_result %in% c("WIN", "LOSS"), ]
  
  if(nrow(bets_made) > 0) {
    cat("\nSPREAD BETTING RESULTS:\n")
    cat(sprintf("Total bets made: %d\n", nrow(bets_made)))
    cat(sprintf("Wins: %d\n", sum(bets_made$spread_bet_result == "WIN")))
    cat(sprintf("Losses: %d\n", sum(bets_made$spread_bet_result == "LOSS")))
    cat(sprintf("Win rate: %.1f%%\n", 
                mean(bets_made$spread_bet_result == "WIN") * 100))
    
    # Calculate profit/loss (assuming -110 odds)
    wins <- sum(bets_made$spread_bet_result == "WIN")
    losses <- sum(bets_made$spread_bet_result == "LOSS")
    profit <- (wins * 0.909) - (losses * 1)  # Win $0.909 for each $1 bet, lose $1
    cat(sprintf("Profit/Loss (per $1 bet): $%.2f\n", profit))
    cat(sprintf("ROI: %.1f%%\n", (profit / nrow(bets_made)) * 100))
  }
  
  # Best and worst predictions
  cat("\nBEST PREDICTIONS:\n")
  best <- results[order(results$prediction_accuracy)[1:3], ]
  for(i in 1:nrow(best)) {
    cat(sprintf("%s @ %s: Predicted %.1f, Actual %.1f (Error: %.1f)\n",
                best$away_team[i], best$home_team[i], 
                best$predicted_margin[i], best$actual_margin[i],
                best$prediction_accuracy[i]))
  }
  
  cat("\nWORST PREDICTIONS:\n")
  worst <- results[order(-results$prediction_accuracy)[1:3], ]
  for(i in 1:nrow(worst)) {
    cat(sprintf("%s @ %s: Predicted %.1f, Actual %.1f (Error: %.1f)\n",
                worst$away_team[i], worst$home_team[i], 
                worst$predicted_margin[i], worst$actual_margin[i],
                worst$prediction_accuracy[i]))
  }
}

# Example of how to structure historical spreads data
create_sample_spreads <- function() {
  # This would come from actual betting data sources
  sample_spreads <- data.frame(
    game_id = c("2025_01_KC_BAL", "2025_01_PHI_ATL", "2025_01_BUF_ARI"),
    home_team = c("BAL", "ATL", "ARI"),
    away_team = c("KC", "PHI", "BUF"),
    spread = c(3, -2.5, 6.5),  # Positive means home team favored
    week = c(1, 1, 1),
    season = c(2025, 2025, 2025)
  )
  
  return(sample_spreads)
}

cat("Backtesting framework loaded!\n")
cat("To run backtest: backtest_predictions(completed_games, historical_spreads)\n")
cat("Next: Get actual 2025 game data and historical spreads\n")