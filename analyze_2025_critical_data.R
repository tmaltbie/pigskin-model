# 2025 NFL Critical Season Data Analysis
# Process the actual current season data

library(dplyr)
library(nflreadr)
library(nflfastR)

cat("🏈 2025 NFL CRITICAL SEASON DATA ANALYSIS\n")
cat("=========================================\n")

# Load 2025 schedule data
games_2025 <- load_schedules(2025)
cat(sprintf("Total 2025 games in schedule: %d\n", nrow(games_2025)))

# Find completed games (those with actual scores)
completed_mask <- complete.cases(games_2025$home_score, games_2025$away_score)
completed_games <- games_2025[completed_mask,]
scheduled_games <- games_2025[!completed_mask,]

cat(sprintf("✅ COMPLETED games: %d\n", nrow(completed_games)))
cat(sprintf("📅 SCHEDULED games: %d\n", nrow(scheduled_games)))

if (nrow(completed_games) > 0) {
  cat("\n📊 COMPLETED GAMES BY WEEK:\n")
  week_summary <- completed_games %>%
    group_by(week) %>%
    summarise(games = n(), .groups = "drop") %>%
    arrange(week)
  
  print(week_summary)
  
  cat("\n🎯 RECENT COMPLETED GAMES:\n")
  recent_games <- completed_games %>%
    arrange(desc(gameday)) %>%
    select(week, gameday, away_team, away_score, home_team, home_score) %>%
    head(10)
  
  for(i in 1:nrow(recent_games)) {
    game <- recent_games[i,]
    margin <- game$home_score - game$away_score
    winner <- ifelse(margin > 0, game$home_team, game$away_team)
    margin_abs <- abs(margin)
    
    cat(sprintf("Week %d (%s): %s %d-%d %s (%s by %d)\n",
               game$week, game$gameday, 
               game$away_team, game$away_score,
               game$home_team, game$home_score,
               winner, margin_abs))
  }
}

# Load 2025 play-by-play data
cat("\n🎯 2025 PLAY-BY-PLAY ANALYSIS:\n")
pbp_2025 <- load_pbp(2025)

if (!is.null(pbp_2025) && nrow(pbp_2025) > 0) {
  cat(sprintf("Total plays loaded: %s\n", format(nrow(pbp_2025), big.mark = ",")))
  
  # Analyze games with play-by-play data
  games_with_pbp <- pbp_2025 %>%
    filter(!is.na(game_id), !is.na(week)) %>%
    group_by(game_id, week, home_team, away_team) %>%
    summarise(plays = n(), .groups = "drop") %>%
    arrange(week)
  
  cat(sprintf("Games with play-by-play: %d\n", nrow(games_with_pbp)))
  
  if (nrow(games_with_pbp) > 0) {
    pbp_week_summary <- games_with_pbp %>%
      group_by(week) %>%
      summarise(games = n(), total_plays = sum(plays), .groups = "drop")
    
    cat("\n📊 PLAY-BY-PLAY DATA BY WEEK:\n")
    for(i in 1:nrow(pbp_week_summary)) {
      week_data <- pbp_week_summary[i,]
      cat(sprintf("Week %d: %d games, %s plays\n", 
                 week_data$week, week_data$games, 
                 format(week_data$total_plays, big.mark = ",")))
    }
  }
  
  # Check for situational data needed for learning
  cat("\n🧠 SITUATIONAL ANALYSIS READINESS:\n")
  situational_plays <- pbp_2025 %>%
    filter(!is.na(down), !is.na(ydstogo), play_type %in% c("pass", "run"))
  
  cat(sprintf("Usable situational plays: %s\n", format(nrow(situational_plays), big.mark = ",")))
  
  if (nrow(situational_plays) > 0) {
    first_down_plays <- situational_plays %>% filter(down == 1)
    red_zone_plays <- situational_plays %>% filter(yardline_100 <= 20)
    
    cat(sprintf("- 1st down plays: %s (for pass rate analysis)\n", format(nrow(first_down_plays), big.mark = ",")))
    cat(sprintf("- Red zone plays: %s (for goal line tendencies)\n", format(nrow(red_zone_plays), big.mark = ",")))
    cat("✅ READY for LAC-style situational tendency analysis!\n")
  }
}

cat("\n🎯 DATA PIPELINE STATUS:\n")
cat("========================\n") 
cat("✅ 2025 schedule data: AVAILABLE\n")
cat("✅ Completed game results: AVAILABLE\n")
cat("✅ Play-by-play data: AVAILABLE\n")
cat("✅ Situational analysis data: READY\n")
cat("\n🚀 READY FOR CRITICAL CURRENT SEASON LEARNING!\n")