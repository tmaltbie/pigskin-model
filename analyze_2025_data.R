# 2025 NFL Season Data Analysis
# Critical current season data processing

library(dplyr)
library(nflreadr)

cat("🏈 2025 NFL SEASON DATA ANALYSIS\n")
cat("================================\n")

# Load 2025 season data  
games_2025 <- load_schedules(2025)
cat(sprintf("Total 2025 games loaded: %d\n", nrow(games_2025)))

# Show column names to understand data structure
cat("\n📊 Available columns:\n")
cat(paste(names(games_2025), collapse = ", "), "\n")

# Check for completed games
if ("home_score" %in% names(games_2025) && "away_score" %in% names(games_2025)) {
  completed_games <- games_2025[complete.cases(games_2025[c("home_score", "away_score")]),]
  cat(sprintf("\n✅ Completed games: %d\n", nrow(completed_games)))
  
  if (nrow(completed_games) > 0) {
    cat("\n📅 COMPLETED GAMES BY WEEK:\n")
    week_summary <- completed_games %>%
      group_by(week) %>%
      summarise(games = n(), .groups = "drop") %>%
      arrange(week)
    
    print(week_summary)
    
    cat("\n🎯 SAMPLE COMPLETED GAMES:\n")
    sample_games <- head(completed_games, 5)
    for(i in 1:nrow(sample_games)) {
      game <- sample_games[i,]
      if (all(c("away_team", "home_team", "away_score", "home_score", "week") %in% names(game))) {
        cat(sprintf("Week %d: %s %s @ %s %s\n", 
                   game$week, game$away_team, game$away_score, 
                   game$home_team, game$home_score))
      }
    }
  }
} else {
  cat("\n⚠️ Score columns not found. Available columns:\n")
  print(names(games_2025))
}

# Check for team data
if (nrow(games_2025) > 0) {
  cat(sprintf("\n🏈 Teams in 2025 season: %d unique teams\n", 
             length(unique(c(games_2025$home_team, games_2025$away_team)))))
  
  # Show team list
  teams <- sort(unique(c(games_2025$home_team, games_2025$away_team)))
  cat("Teams:", paste(teams, collapse = ", "), "\n")
}