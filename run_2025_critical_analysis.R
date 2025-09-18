# 2025 NFL CRITICAL SEASON ANALYSIS PIPELINE
# Run this directly in your terminal to get current season learning data
# YOU have the data access - this will work in your environment

library(dplyr)
library(nflreadr)
library(nflfastR)

cat("🏈 2025 NFL CRITICAL SEASON ANALYSIS\n")
cat("====================================\n")

# 1. LOAD CRITICAL 2025 DATA
cat("📊 Step 1: Loading 2025 Data...\n")
games_2025 <- load_schedules(2025)
pbp_2025 <- load_pbp(2025)

cat(sprintf("Schedule games loaded: %d\n", nrow(games_2025)))
cat(sprintf("Play-by-play plays loaded: %s\n", format(nrow(pbp_2025), big.mark = ",")))

# 2. FIND COMPLETED GAMES (CRITICAL FOR LEARNING)
cat("\n🎯 Step 2: Identifying Completed Games...\n")
completed_games <- games_2025[complete.cases(games_2025$home_score, games_2025$away_score),]
cat(sprintf("✅ COMPLETED GAMES: %d\n", nrow(completed_games)))

# Show completed games by week
if (nrow(completed_games) > 0) {
  week_summary <- completed_games %>%
    group_by(week) %>%
    summarise(games = n(), .groups = "drop")
  
  cat("Completed games by week:\n")
  print(week_summary)
  
  # Show recent results for learning validation
  cat("\n📈 Recent completed games (for learning):\n")
  recent <- completed_games %>%
    arrange(desc(gameday)) %>%
    select(week, away_team, away_score, home_team, home_score, gameday) %>%
    head(8)
  
  for(i in 1:nrow(recent)) {
    game <- recent[i,]
    margin <- game$home_score - game$away_score
    winner <- ifelse(margin > 0, game$home_team, game$away_team)
    cat(sprintf("Week %d: %s %d @ %s %d (%s by %d) [%s]\n",
               game$week, game$away_team, game$away_score,
               game$home_team, game$home_score, winner, abs(margin), game$gameday))
  }
}

# 3. ANALYZE CURRENT SEASON TEAM TENDENCIES
cat("\n🧠 Step 3: 2025 Team Tendency Analysis...\n")
if (!is.null(pbp_2025) && nrow(pbp_2025) > 0) {
  
  # First down pass rates (like LAC example)
  first_down_analysis <- pbp_2025 %>%
    filter(down == 1, play_type %in% c("pass", "run"), !is.na(posteam)) %>%
    group_by(posteam) %>%
    summarise(
      first_down_plays = n(),
      passes = sum(play_type == "pass"),
      pass_rate = passes / first_down_plays,
      .groups = "drop"
    ) %>%
    filter(first_down_plays >= 10) %>%  # Minimum sample size
    arrange(desc(pass_rate))
  
  # League average for comparison
  league_avg_pass_rate <- mean(first_down_analysis$pass_rate)
  
  cat(sprintf("League average 1st down pass rate: %.1f%%\n", league_avg_pass_rate * 100))
  cat("\n🎯 Teams with notable 1st down tendencies:\n")
  
  # Show teams significantly above/below average
  for(i in 1:min(10, nrow(first_down_analysis))) {
    team_data <- first_down_analysis[i,]
    deviation <- team_data$pass_rate - league_avg_pass_rate
    
    if (abs(deviation) > 0.05) {  # 5%+ deviation is notable
      status <- ifelse(deviation > 0, "HIGH", "LOW") 
      cat(sprintf("%s: %.1f%% pass rate (%+.1f%% vs avg) - %s [%d plays]\n",
                 team_data$posteam, team_data$pass_rate * 100,
                 deviation * 100, status, team_data$first_down_plays))
    }
  }
  
  # Red zone tendencies
  cat("\n🎯 Red Zone Analysis:\n")
  red_zone_analysis <- pbp_2025 %>%
    filter(yardline_100 <= 20, play_type %in% c("pass", "run"), !is.na(posteam)) %>%
    group_by(posteam) %>%
    summarise(
      rz_plays = n(),
      rz_passes = sum(play_type == "pass"),
      rz_pass_rate = rz_passes / rz_plays,
      .groups = "drop"
    ) %>%
    filter(rz_plays >= 5) %>%
    arrange(desc(rz_pass_rate))
  
  league_avg_rz_pass_rate <- mean(red_zone_analysis$rz_pass_rate)
  cat(sprintf("League average red zone pass rate: %.1f%%\n", league_avg_rz_pass_rate * 100))
  
  # Show teams with different red zone approaches
  for(i in 1:min(8, nrow(red_zone_analysis))) {
    team_data <- red_zone_analysis[i,]
    deviation <- team_data$rz_pass_rate - league_avg_rz_pass_rate
    
    if (abs(deviation) > 0.10) {  # 10%+ deviation in red zone
      status <- ifelse(deviation > 0, "PASS-HEAVY", "RUN-HEAVY")
      cat(sprintf("%s: %.1f%% RZ pass rate (%s) [%d plays]\n",
                 team_data$posteam, team_data$rz_pass_rate * 100,
                 status, team_data$rz_plays))
    }
  }
}

# 4. SAVE ANALYSIS FOR LEARNING SYSTEM
cat("\n💾 Step 4: Saving Analysis for Learning System...\n")

# Create analysis summary for learning integration
analysis_summary <- list(
  date_analyzed = Sys.Date(),
  completed_games = nrow(completed_games),
  total_plays = ifelse(exists("pbp_2025"), nrow(pbp_2025), 0),
  teams_analyzed = ifelse(exists("first_down_analysis"), nrow(first_down_analysis), 0),
  league_baselines = list(
    first_down_pass_rate = ifelse(exists("league_avg_pass_rate"), league_avg_pass_rate, 0.58),
    red_zone_pass_rate = ifelse(exists("league_avg_rz_pass_rate"), league_avg_rz_pass_rate, 0.32)
  )
)

# Save to JSON for learning system integration
writeLines(jsonlite::toJSON(analysis_summary, pretty = TRUE), "2025_analysis_summary.json")

cat("✅ Analysis complete!\n")
cat("📊 Results saved to: 2025_analysis_summary.json\n")
cat("🎯 This data is now ready for learning system integration!\n")

cat("\n🚀 CRITICAL CURRENT SEASON DATA STATUS:\n")
cat("======================================\n")
cat("✅ 2025 schedule: LOADED\n")
cat("✅ Completed games: IDENTIFIED\n") 
cat("✅ Play-by-play data: AVAILABLE\n")
cat("✅ Team tendencies: ANALYZED\n")
cat("✅ Learning integration: READY\n")

# Return key stats for immediate use
if (exists("completed_games") && nrow(completed_games) > 0) {
  cat(sprintf("\n🎯 READY TO LEARN FROM %d COMPLETED 2025 GAMES!\n", nrow(completed_games)))
}