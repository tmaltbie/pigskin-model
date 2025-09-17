# Backtest Complete Prediction System vs Week 1 2025 Results
# Test our model against actual results and Vegas performance

source('complete_prediction_system.R')
library(dplyr)

# Week 1 2025 Games with CORRECT results from ESPN
week1_games <- data.frame(
  game_id = 1:16,
  away_team = c("DAL", "KC", "TB", "CIN", "MIA", "LV", "ARI", "PIT", "NYG", "CAR", "TEN", "SF", "DET", "HOU", "BAL", "MIN"),
  home_team = c("PHI", "LAC", "ATL", "CLE", "IND", "NE", "NO", "NYJ", "WAS", "JAX", "DEN", "SEA", "GB", "LAR", "BUF", "CHI"),
  vegas_spread = c(-6.5, -4.0, -2.5, -4.5, -7.0, -6.5, -2.5, -1.5, -1.0, -4.0, -3.0, -3.5, -2.5, -3.0, -2.5, -1.5),  # Estimated spreads
  actual_home_score = c(24, 27, 23, 17, 33, 20, 20, 34, 21, 26, 20, 17, 27, 14, 41, 27),
  actual_away_score = c(20, 21, 20, 16, 8, 13, 13, 32, 18, 10, 12, 13, 13, 9, 40, 24),
  home_won = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  actual_margin = c(4, 6, 3, 1, 25, 7, 7, 2, 3, 16, 8, 4, 14, 5, 1, 3),  # positive = home win
  game_day = c("Thursday", "Friday", rep("Sunday", 13), "Monday"),
  stringsAsFactors = FALSE
)

# Convert actual results to readable format
week1_games$winner <- ifelse(week1_games$home_won, week1_games$home_team, week1_games$away_team)
week1_games$actual_spread_result <- week1_games$actual_margin - (-week1_games$vegas_spread)  # positive = home covered

# Function to backtest our complete system
backtest_complete_system <- function(games, epa_metrics) {
  
  cat("=== BACKTESTING COMPLETE PREDICTION SYSTEM ===\n")
  cat("Week 1 2025 NFL Games\n\n")
  
  predictions <- list()
  results_summary <- data.frame(
    game = character(),
    our_prediction = numeric(),
    our_winner = character(),
    vegas_spread = numeric(),
    vegas_winner = character(),
    actual_winner = character(),
    actual_margin = numeric(),
    our_error = numeric(),
    vegas_error = numeric(),
    our_ats = character(),
    vegas_ats = character(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:nrow(games)) {
    game <- games[i,]
    
    cat(sprintf("Game %d: %s @ %s\n", i, game$away_team, game$home_team))
    cat(sprintf("Vegas Line: %s %+.1f\n", 
                ifelse(game$vegas_spread > 0, game$away_team, game$home_team),
                abs(game$vegas_spread)))
    
    # Get our prediction using complete system
    pred <- predict_game_complete(
      home_team = game$home_team,
      away_team = game$away_team,
      vegas_spread = game$vegas_spread,
      epa_metrics = epa_metrics,
      is_primetime = FALSE,  # Week 1 Sunday games
      is_divisional = FALSE, # Simplified for now
      dome_game = game$home_team %in% c("ATL", "DET", "HOU", "IND", "LV", "LAR", "NO", "MIN")
    )
    
    our_winner <- ifelse(pred$predicted_margin > 0, game$home_team, game$away_team)
    vegas_winner <- ifelse(game$vegas_spread > 0, game$away_team, game$home_team)
    
    # Calculate errors
    our_error <- abs(pred$predicted_margin - game$actual_margin)
    vegas_error <- abs((-game$vegas_spread) - game$actual_margin)
    
    # ATS results
    our_ats_result <- ifelse(
      (pred$predicted_margin > 0 && game$actual_margin > 0) ||
      (pred$predicted_margin < 0 && game$actual_margin < 0), "WIN", "LOSS"
    )
    
    vegas_ats_result <- ifelse(
      (game$vegas_spread < 0 && game$actual_margin > -game$vegas_spread) ||
      (game$vegas_spread > 0 && game$actual_margin < -game$vegas_spread), "COVER", "NO COVER"
    )
    
    # Store results
    results_summary[i,] <- list(
      game = sprintf("%s @ %s", game$away_team, game$home_team),
      our_prediction = pred$predicted_margin,
      our_winner = our_winner,
      vegas_spread = -game$vegas_spread,
      vegas_winner = vegas_winner,
      actual_winner = game$winner,
      actual_margin = game$actual_margin,
      our_error = our_error,
      vegas_error = vegas_error,
      our_ats = our_ats_result,
      vegas_ats = vegas_ats_result
    )
    
    cat(sprintf("Our Prediction: %s by %.1f\n", our_winner, abs(pred$predicted_margin)))
    cat(sprintf("Actual Result: %s by %d\n", game$winner, abs(game$actual_margin)))
    cat(sprintf("Our Error: %.1f points | Vegas Error: %.1f points\n", our_error, vegas_error))
    cat(sprintf("Our ATS: %s | Vegas ATS: %s\n", our_ats_result, vegas_ats_result))
    cat(paste(rep("-", 50), collapse = ""), "\n")
    
    predictions[[i]] <- pred
  }
  
  return(list(predictions = predictions, results = results_summary))
}

# Function to analyze backtest results
analyze_backtest_results <- function(results) {
  
  cat("\n=== BACKTEST RESULTS ANALYSIS ===\n\n")
  
  # Overall Accuracy
  our_avg_error <- mean(results$our_error, na.rm = TRUE)
  vegas_avg_error <- mean(results$vegas_error, na.rm = TRUE)
  
  cat("📊 PREDICTION ACCURACY:\n")
  cat(sprintf("Our Model Average Error: %.2f points\n", our_avg_error))
  cat(sprintf("Vegas Average Error: %.2f points\n", vegas_avg_error))
  cat(sprintf("Improvement vs Vegas: %+.2f points (%.1f%%)\n", 
              vegas_avg_error - our_avg_error,
              ((vegas_avg_error - our_avg_error) / vegas_avg_error) * 100))
  
  # Winner Predictions
  our_winner_correct <- sum(results$our_winner == results$actual_winner, na.rm = TRUE)
  vegas_winner_correct <- sum(results$vegas_winner == results$actual_winner, na.rm = TRUE)
  total_games <- nrow(results)
  
  cat(sprintf("\n🏆 WINNER PREDICTIONS:\n"))
  cat(sprintf("Our Model: %d/%d correct (%.1f%%)\n", 
              our_winner_correct, total_games, (our_winner_correct/total_games)*100))
  cat(sprintf("Vegas: %d/%d correct (%.1f%%)\n", 
              vegas_winner_correct, total_games, (vegas_winner_correct/total_games)*100))
  
  # ATS Performance
  our_ats_wins <- sum(results$our_ats == "WIN", na.rm = TRUE)
  
  cat(sprintf("\n💰 AGAINST THE SPREAD:\n"))
  cat(sprintf("Our Model ATS: %d/%d wins (%.1f%%)\n", 
              our_ats_wins, total_games, (our_ats_wins/total_games)*100))
  
  # Best and Worst Predictions
  best_game_idx <- which.min(results$our_error)
  worst_game_idx <- which.max(results$our_error)
  
  cat(sprintf("\n🎯 BEST PREDICTION:\n"))
  cat(sprintf("%s - Error: %.1f points\n", 
              results$game[best_game_idx], results$our_error[best_game_idx]))
  
  cat(sprintf("\n❌ WORST PREDICTION:\n"))
  cat(sprintf("%s - Error: %.1f points\n", 
              results$game[worst_game_idx], results$our_error[worst_game_idx]))
  
  # Games where we significantly outperformed Vegas
  better_than_vegas <- results$vegas_error - results$our_error
  significant_improvements <- which(better_than_vegas > 3.0)
  
  if(length(significant_improvements) > 0) {
    cat(sprintf("\n🚀 GAMES WHERE WE SIGNIFICANTLY BEAT VEGAS (3+ points better):\n"))
    for(idx in significant_improvements) {
      cat(sprintf("%s: Our error %.1f vs Vegas %.1f (%.1f points better)\n",
                  results$game[idx], results$our_error[idx], 
                  results$vegas_error[idx], better_than_vegas[idx]))
    }
  }
  
  # Summary table
  cat(sprintf("\n📋 SUMMARY TABLE:\n"))
  cat(sprintf("%-20s %8s %8s %8s %8s %8s\n", 
              "Game", "Our Pred", "Vegas", "Actual", "Our Err", "Vegas Err"))
  cat(paste(rep("-", 70), collapse = ""), "\n")
  
  for(i in 1:nrow(results)) {
    cat(sprintf("%-20s %+8.1f %+8.1f %+8d %8.1f %8.1f\n",
                results$game[i], results$our_prediction[i], results$vegas_spread[i],
                results$actual_margin[i], results$our_error[i], results$vegas_error[i]))
  }
  
  return(list(
    our_avg_error = our_avg_error,
    vegas_avg_error = vegas_avg_error,
    our_winner_pct = (our_winner_correct/total_games)*100,
    vegas_winner_pct = (vegas_winner_correct/total_games)*100,
    our_ats_pct = (our_ats_wins/total_games)*100
  ))
}

# Main execution function
run_week1_backtest <- function() {
  
  cat("Loading 2024 EPA metrics for prediction base...\n")
  pbp_2024 <- load_pbp(2024)
  epa_metrics <- calculate_all_team_epa_metrics(pbp_2024)
  
  cat("Running backtest on Week 1 2025...\n\n")
  backtest_results <- backtest_complete_system(week1_games, epa_metrics)
  
  # Analyze results
  analysis <- analyze_backtest_results(backtest_results$results)
  
  return(list(
    games = week1_games,
    predictions = backtest_results$predictions,
    results = backtest_results$results,
    analysis = analysis
  ))
}

cat("Week 1 2025 Backtest System loaded!\n")
cat("Run: results <- run_week1_backtest()\n")