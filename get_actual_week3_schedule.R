# Get ACTUAL Week 3 NFL Schedule from 2025 Data
# Fix the incorrect matchups in predictions

library(nflreadr)
library(dplyr)

cat("🏈 GETTING ACTUAL WEEK 3 NFL SCHEDULE FROM 2025 DATA\n")
cat("==================================================\n")

# Load actual 2025 schedule
games_2025 <- load_schedules(2025)

# Get actual Week 3 games
actual_week3 <- games_2025 %>%
  filter(week == 3) %>%
  select(game_id, week, gameday, away_team, home_team, away_score, home_score) %>%
  arrange(gameday, away_team)

cat(sprintf("Actual Week 3 games found: %d\n", nrow(actual_week3)))

if (nrow(actual_week3) > 0) {
  cat("\n🎯 ACTUAL WEEK 3 NFL SCHEDULE:\n")
  cat("=============================\n")
  
  for (i in 1:nrow(actual_week3)) {
    game <- actual_week3[i,]
    
    # Check if game is completed
    completed <- !is.na(game$home_score) && !is.na(game$away_score)
    
    if (completed) {
      winner <- ifelse(game$home_score > game$away_score, game$home_team, game$away_team)
      margin <- abs(game$home_score - game$away_score) 
      cat(sprintf("✅ %s @ %s (COMPLETED: %s won by %d)\n", 
                 game$away_team, game$home_team, winner, margin))
    } else {
      cat(sprintf("📅 %s @ %s (Scheduled: %s)\n", 
                 game$away_team, game$home_team, game$gameday))
    }
  }
  
  # Compare to what we have stored
  if (file.exists("learning_system/predictions_tracking.csv")) {
    stored_predictions <- read.csv("learning_system/predictions_tracking.csv")
    stored_week3 <- stored_predictions[stored_predictions$week == 3,]
    
    cat(sprintf("\n🔍 COMPARISON:\n"))
    cat("==============\n")
    cat(sprintf("Actual Week 3 games: %d\n", nrow(actual_week3)))
    cat(sprintf("Stored predictions: %d\n", nrow(stored_week3)))
    
    # Check for mismatches
    cat("\n❌ INCORRECT STORED MATCHUPS:\n")
    mismatches <- 0
    
    for (i in 1:min(nrow(actual_week3), nrow(stored_week3))) {
      actual_game <- actual_week3[i,]
      stored_game <- stored_week3[i,]
      
      if (actual_game$away_team != stored_game$away_team || 
          actual_game$home_team != stored_game$home_team) {
        cat(sprintf("Game %d: Stored '%s @ %s' → Should be '%s @ %s'\n",
                   i, stored_game$away_team, stored_game$home_team,
                   actual_game$away_team, actual_game$home_team))
        mismatches <- mismatches + 1
      }
    }
    
    if (mismatches == 0) {
      cat("✅ All matchups are correct!\n")
    } else {
      cat(sprintf("❌ Found %d incorrect matchups\n", mismatches))
    }
  }
  
  # Save actual schedule for correct predictions
  write.csv(actual_week3, "actual_week3_schedule.csv", row.names = FALSE)
  cat(sprintf("\n💾 Actual Week 3 schedule saved to: actual_week3_schedule.csv\n"))
  
} else {
  cat("No Week 3 games found in 2025 data\n")
}

cat("\n🎯 Next step: Generate predictions using ACTUAL matchups!\n")