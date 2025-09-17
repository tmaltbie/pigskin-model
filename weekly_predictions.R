# Weekly NFL Spread Predictions
# Run this script to predict games for a specific week

source("spread_predictor.R")

# Load current season data
cat("Loading 2024 NFL data...\n")
pbp_2024 <- load_pbp(2024)
schedule_2024 <- load_schedules(2024)

cat("Calculating team metrics...\n")
team_metrics <- calculate_team_metrics(pbp_2024, schedule_2024)

# Define games and spreads for week of 9/21/25 (Week 3 of 2025 season)
# Note: You'll need to update these with actual spreads from sportsbooks
week_3_games <- data.frame(
  home_team = c("BUF", "GB", "LAC", "PHI", "TB", "HOU", "IND", "CAR", 
                "DEN", "LV", "WAS", "KC", "ATL", "CIN", "NYJ", "SF"),
  away_team = c("JAX", "TEN", "PIT", "NO", "DET", "MIN", "CHI", "SEA",
                "MIA", "BAL", "CLE", "LAR", "DAL", "NE", "ARI", "NYG"),
  vegas_spread = c(-6.5, -3, -2.5, -2, 3, -2.5, -7, 3,
                   -1, 3.5, -9.5, -7.5, 3, -8.5, -1.5, -6),  # Example spreads
  game_time = c("1:00 PM", "1:00 PM", "1:00 PM", "1:00 PM", "1:00 PM", "1:00 PM",
                "1:00 PM", "4:05 PM", "4:05 PM", "4:25 PM", "4:25 PM", 
                "8:20 PM", "8:20 PM", "1:00 PM", "4:05 PM", "8:15 PM")
)

# Run predictions for all games
cat("\n=== WEEK 3 SPREAD PREDICTIONS (9/21/25) ===\n\n")

predictions <- list()
for(i in 1:nrow(week_3_games)) {
  game <- week_3_games[i,]
  
  pred <- predict_game_spread(
    home_team = game$home_team,
    away_team = game$away_team, 
    vegas_spread = game$vegas_spread,
    team_metrics = team_metrics,
    schedule_data = schedule_2024
  )
  
  predictions[[i]] <- pred
  
  # Print prediction
  cat(sprintf("%s @ %s (%s)\n", 
              pred$away_team, pred$home_team, game$game_time))
  cat(sprintf("Vegas Spread: %s %+.1f\n", 
              pred$home_team, -game$vegas_spread))
  cat(sprintf("Predicted Margin: %s by %.1f\n", 
              pred$home_team, pred$predicted_margin))
  cat(sprintf("Recommendation: %s (Confidence: %.0f%%)\n", 
              pred$recommendation, pred$confidence * 100))
  cat("----------------------------------------\n")
}

# Summary of recommendations
cat("\n=== BETTING RECOMMENDATIONS SUMMARY ===\n")
strong_bets <- predictions[sapply(predictions, function(x) x$confidence > 0.7)]
moderate_bets <- predictions[sapply(predictions, function(x) x$confidence > 0.4 & x$confidence <= 0.7)]

cat("STRONG RECOMMENDATIONS (70%+ confidence):\n")
if(length(strong_bets) > 0) {
  for(bet in strong_bets) {
    cat(sprintf("- %s: %s\n", paste(bet$away_team, "@", bet$home_team), bet$recommendation))
  }
} else {
  cat("- None this week\n")
}

cat("\nMODERATE RECOMMENDATIONS (40-70% confidence):\n")
if(length(moderate_bets) > 0) {
  for(bet in moderate_bets) {
    cat(sprintf("- %s: %s\n", paste(bet$away_team, "@", bet$home_team), bet$recommendation))
  }
} else {
  cat("- None this week\n")
}

cat("\nNote: This is a basic model for demonstration. Always do your own research!\n")