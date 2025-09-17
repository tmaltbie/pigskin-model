# NFL Spread Prediction System
# Predicts team performance against the spread

library(nflreadr)
library(data.table)  # For fast data manipulation

# Function to calculate team efficiency metrics
calculate_team_metrics <- function(pbp_data, schedule_data) {
  
  library(dplyr)
  
  # Calculate offensive and defensive efficiency per team
  team_stats <- pbp_data %>%
    filter(!is.na(posteam)) %>%
    group_by(posteam, season) %>%
    summarise(
      total_plays = n(),
      total_yards = sum(yards_gained, na.rm = TRUE),
      total_points = sum(ifelse(touchdown == 1, 6, 0) + 
                        ifelse(field_goal_result == "made", 3, 0), na.rm = TRUE),
      turnovers = sum(interception == 1 | fumble_lost == 1, na.rm = TRUE),
      third_down_conversions = sum(third_down_converted == 1, na.rm = TRUE),
      third_down_attempts = sum(down == 3, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      yards_per_play = total_yards / total_plays,
      points_per_game = total_points / 17,  # Assuming 17 games
      turnover_rate = turnovers / total_plays,
      third_down_pct = third_down_conversions / third_down_attempts
    )
  
  return(team_stats)
}

# Function to get recent team performance
get_recent_form <- function(schedule_data, team, weeks_back = 4) {
  
  library(dplyr)
  
  recent_games <- schedule_data %>%
    filter((home_team == team | away_team == team) & !is.na(result)) %>%
    select(game_id, week, home_team, away_team, home_score, away_score, result) %>%
    arrange(desc(week)) %>%
    slice_head(n = min(weeks_back, nrow(.)))
  
  if(nrow(recent_games) == 0) {
    return(list(win_rate = 0, avg_margin = 0, games_played = 0))
  }
  
  # Calculate win rate and average margin
  wins <- sum(
    (recent_games$home_team == team & recent_games$result > 0) |
    (recent_games$away_team == team & recent_games$result < 0)
  )
  
  avg_margin <- mean(
    ifelse(recent_games$home_team == team, 
           recent_games$result, 
           -recent_games$result),
    na.rm = TRUE
  )
  
  return(list(
    win_rate = wins / nrow(recent_games),
    avg_margin = avg_margin,
    games_played = nrow(recent_games)
  ))
}

# Main prediction function
predict_game_spread <- function(home_team, away_team, vegas_spread, 
                               team_metrics, schedule_data) {
  
  # Get team metrics
  home_metrics <- team_metrics %>% filter(posteam == home_team)
  away_metrics <- team_metrics %>% filter(posteam == away_team)
  
  if(nrow(home_metrics) == 0 || nrow(away_metrics) == 0) {
    # If no metrics available, return neutral prediction
    return(list(
      home_team = home_team,
      away_team = away_team,
      predicted_margin = 0,
      vegas_spread = vegas_spread,
      spread_difference = 0,
      recommendation = "PASS",
      confidence = 0
    ))
  }
  
  # Get recent form
  home_form <- get_recent_form(schedule_data, home_team)
  away_form <- get_recent_form(schedule_data, away_team)
  
  # Calculate predicted point differential
  # Simple model: combine efficiency metrics with recent form
  home_strength <- (home_metrics$yards_per_play * 10) + 
                   (home_metrics$points_per_game * 0.5) - 
                   (home_metrics$turnover_rate * 50) +
                   (home_form$avg_margin * 0.3) + 
                   3  # Home field advantage
  
  away_strength <- (away_metrics$yards_per_play * 10) + 
                   (away_metrics$points_per_game * 0.5) - 
                   (away_metrics$turnover_rate * 50) +
                   (away_form$avg_margin * 0.3)
  
  predicted_margin <- home_strength - away_strength
  
  # Compare to Vegas spread
  spread_difference <- predicted_margin - (-vegas_spread)  # Vegas spread is usually negative for favorite
  
  # Recommendation
  recommendation <- ifelse(
    abs(spread_difference) < 3, "PASS",
    ifelse(spread_difference > 3, "BET HOME", "BET AWAY")
  )
  
  return(list(
    home_team = home_team,
    away_team = away_team,
    predicted_margin = round(predicted_margin, 1),
    vegas_spread = vegas_spread,
    spread_difference = round(spread_difference, 1),
    recommendation = recommendation,
    confidence = min(abs(spread_difference) / 3, 1)  # Confidence score 0-1
  ))
}

cat("NFL Spread Prediction System loaded!\n")
cat("Next: Load data and define this week's games with spreads\n")