# Build 2025 EPA Metrics and Re-run Backtest
# Now using actual 2025 data instead of 2024!

source('epa_prediction_system.R')
source('complete_prediction_system.R')
library(dplyr)

# Load 2025 data and build EPA metrics
cat("=== BUILDING 2025 EPA METRICS ===\n")
pbp_2025 <- load_pbp(2025)
pbp_df <- as.data.frame(pbp_2025)

# Use our existing EPA calculation function but with 2025 data
epa_metrics_2025 <- calculate_all_team_epa_metrics(pbp_df)

cat("✅ 2025 EPA metrics calculated for", nrow(epa_metrics_2025), "teams\n\n")

# Show top EPA teams from 2025 data
cat("🏆 TOP 2025 EPA TEAMS (Weeks 1-2):\n")
epa_metrics_2025 <- epa_metrics_2025 %>%
  mutate(net_epa = off_epa_play - def_epa_play) %>%
  arrange(desc(net_epa))

print(epa_metrics_2025[1:10, c("team", "off_epa_play", "def_epa_play", "net_epa")])

# Corrected Week 1 2025 games (from ESPN data you provided)
week1_games_corrected <- data.frame(
  game_id = 1:16,
  away_team = c("DAL", "KC", "TB", "CIN", "MIA", "LV", "ARI", "PIT", "NYG", "CAR", "TEN", "SF", "DET", "HOU", "BAL", "MIN"),
  home_team = c("PHI", "LAC", "ATL", "CLE", "IND", "NE", "NO", "NYJ", "WAS", "JAX", "DEN", "SEA", "GB", "LAR", "BUF", "CHI"),
  vegas_spread = c(-6.5, -4.0, -2.5, -4.5, -7.0, -6.5, -2.5, -1.5, -1.0, -4.0, -3.0, -3.5, -2.5, -3.0, -2.5, -1.5),
  actual_home_score = c(24, 27, 23, 17, 33, 20, 20, 34, 21, 26, 20, 17, 27, 14, 41, 27),
  actual_away_score = c(20, 21, 20, 16, 8, 13, 13, 32, 18, 10, 12, 13, 13, 9, 40, 24),
  actual_margin = c(4, 6, 3, 1, 25, 7, 7, 2, 3, 16, 8, 4, 14, 5, 1, 3),
  stringsAsFactors = FALSE
)

# Function to backtest with 2025 EPA data
backtest_with_2025_epa <- function(games, epa_metrics) {
  
  cat("\n=== BACKTESTING WITH 2025 EPA DATA ===\n")
  cat("Using actual 2025 Weeks 1-2 EPA metrics\n\n")
  
  results <- data.frame(
    game = character(),
    our_prediction = numeric(),
    vegas_spread = numeric(),
    actual_margin = numeric(),
    our_error = numeric(),
    vegas_error = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:nrow(games)) {
    game <- games[i,]
    
    # Use our complete prediction system with 2025 EPA data
    pred <- predict_game_complete(
      home_team = game$home_team,
      away_team = game$away_team,
      vegas_spread = game$vegas_spread,
      epa_metrics = epa_metrics,
      is_primetime = game$away_team == "DAL" || game$away_team == "KC", # Thu/Fri games
      is_divisional = FALSE
    )
    
    our_error <- abs(pred$predicted_margin - game$actual_margin)
    vegas_error <- abs((-game$vegas_spread) - game$actual_margin)
    
    results[i,] <- list(
      game = sprintf("%s @ %s", game$away_team, game$home_team),
      our_prediction = pred$predicted_margin,
      vegas_spread = -game$vegas_spread,
      actual_margin = game$actual_margin,
      our_error = our_error,
      vegas_error = vegas_error
    )
    
    cat(sprintf("%s @ %s: Pred %+.1f, Vegas %+.1f, Actual %+d | Errors: Us %.1f, Vegas %.1f\n",
                game$away_team, game$home_team, pred$predicted_margin, 
                -game$vegas_spread, game$actual_margin, our_error, vegas_error))
  }
  
  # Summary
  our_avg_error <- mean(results$our_error)
  vegas_avg_error <- mean(results$vegas_error)
  improvement <- vegas_avg_error - our_avg_error
  improvement_pct <- (improvement / vegas_avg_error) * 100
  
  cat(sprintf("\n📊 RESULTS WITH 2025 EPA DATA:\n"))
  cat(sprintf("Our Average Error: %.2f points\n", our_avg_error))
  cat(sprintf("Vegas Average Error: %.2f points\n", vegas_avg_error))
  cat(sprintf("Improvement: %+.2f points (%.1f%%)\n", improvement, improvement_pct))
  
  better_games <- sum(results$our_error < results$vegas_error)
  cat(sprintf("Games where we beat Vegas: %d/%d (%.1f%%)\n", 
              better_games, nrow(results), (better_games/nrow(results))*100))
  
  return(results)
}

# Run the corrected backtest
cat("Running backtest with 2025 EPA data...\n")
backtest_results_2025 <- backtest_with_2025_epa(week1_games_corrected, epa_metrics_2025)