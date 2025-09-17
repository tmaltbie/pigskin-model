# Advanced EPA-Based Team Metrics System
# Using nflfastR's sophisticated analytics for superior predictions

library(nflfastR)
library(dplyr)

# ============= EPA-BASED TEAM METRICS =============

calculate_advanced_epa_metrics <- function(pbp_data, team) {
  
  cat(sprintf("Calculating advanced EPA metrics for %s...\n", team))
  
  # Get team's offensive plays (when they have the ball)
  team_offense <- pbp_data %>% 
    filter(posteam == team, !is.na(epa), !is.na(play_type))
  
  # Get team's defensive plays (when opponent has the ball)  
  team_defense <- pbp_data %>%
    filter(defteam == team, !is.na(epa), !is.na(play_type))
  
  if(nrow(team_offense) == 0 || nrow(team_defense) == 0) {
    return(list(
      team = team,
      off_epa_play = 0, def_epa_play = 0, 
      pass_epa_play = 0, rush_epa_play = 0,
      red_zone_epa = 0, third_down_epa = 0,
      cpoe = 0, success_rate = 0,
      explosive_epa = 0, turnover_epa = 0
    ))
  }
  
  # CORE EPA METRICS
  off_epa_per_play <- mean(team_offense$epa, na.rm = TRUE)
  def_epa_per_play <- mean(team_defense$epa, na.rm = TRUE)  # Lower is better for defense
  
  # PASS vs RUSH EPA
  pass_plays <- team_offense %>% filter(play_type == "pass")
  rush_plays <- team_offense %>% filter(play_type == "run")
  
  pass_epa_per_play <- ifelse(nrow(pass_plays) > 0, mean(pass_plays$epa, na.rm = TRUE), 0)
  rush_epa_per_play <- ifelse(nrow(rush_plays) > 0, mean(rush_plays$epa, na.rm = TRUE), 0)
  
  # SITUATIONAL EPA
  # Red zone EPA (inside 20 yard line)
  red_zone_plays <- team_offense %>% filter(yardline_100 <= 20)
  red_zone_epa <- ifelse(nrow(red_zone_plays) > 0, mean(red_zone_plays$epa, na.rm = TRUE), 0)
  
  # Third down EPA
  third_down_plays <- team_offense %>% filter(down == 3)
  third_down_epa <- ifelse(nrow(third_down_plays) > 0, mean(third_down_plays$epa, na.rm = TRUE), 0)
  
  # QUARTERBACK METRICS
  qb_plays <- team_offense %>% filter(play_type == "pass", !is.na(cpoe))
  avg_cpoe <- ifelse(nrow(qb_plays) > 0, mean(qb_plays$cpoe, na.rm = TRUE), 0)
  
  # SUCCESS RATE (positive EPA plays)
  success_rate <- mean(team_offense$epa > 0, na.rm = TRUE)
  
  # EXPLOSIVE PLAY EPA (plays with EPA > 1.0)
  explosive_plays <- team_offense %>% filter(epa > 1.0)
  explosive_epa_rate <- nrow(explosive_plays) / nrow(team_offense)
  
  # TURNOVER EPA IMPACT
  turnovers <- team_offense %>% filter(interception == 1 | fumble_lost == 1)
  turnover_epa <- ifelse(nrow(turnovers) > 0, mean(turnovers$epa, na.rm = TRUE), 0)
  
  return(list(
    team = team,
    off_epa_play = round(off_epa_per_play, 3),
    def_epa_play = round(def_epa_per_play, 3),
    pass_epa_play = round(pass_epa_per_play, 3),
    rush_epa_play = round(rush_epa_per_play, 3),
    red_zone_epa = round(red_zone_epa, 3),
    third_down_epa = round(third_down_epa, 3),
    cpoe = round(avg_cpoe, 3),
    success_rate = round(success_rate, 3),
    explosive_epa_rate = round(explosive_epa_rate, 3),
    turnover_epa = round(turnover_epa, 3),
    total_plays = nrow(team_offense)
  ))
}

# Calculate EPA metrics for all teams
calculate_all_team_epa_metrics <- function(pbp_data) {
  
  cat("=== CALCULATING EPA METRICS FOR ALL TEAMS ===\n")
  
  # Get all unique teams
  teams <- unique(c(pbp_data$posteam, pbp_data$defteam))
  teams <- teams[!is.na(teams)]
  
  epa_metrics <- list()
  
  for(team in teams) {
    team_metrics <- calculate_advanced_epa_metrics(pbp_data, team)
    epa_metrics[[team]] <- team_metrics
  }
  
  # Convert to data frame
  epa_df <- do.call(rbind, lapply(epa_metrics, function(x) as.data.frame(x)))
  
  cat(sprintf("✅ EPA metrics calculated for %d teams\n", nrow(epa_df)))
  return(epa_df)
}

# ============= EPA-BASED PREDICTION MODEL =============

predict_game_with_epa <- function(home_team, away_team, vegas_spread, epa_metrics, 
                                  injury_impact = 0, rest_impact = 0, weather_impact = 0) {
  
  # Get EPA metrics for both teams
  home_metrics <- epa_metrics[epa_metrics$team == home_team, ]
  away_metrics <- epa_metrics[epa_metrics$team == away_team, ]
  
  if(nrow(home_metrics) == 0 || nrow(away_metrics) == 0) {
    return(list(
      predicted_margin = 0,
      confidence = 0,
      factors = "Missing EPA data"
    ))
  }
  
  # EPA-BASED TEAM STRENGTH CALCULATION
  # Much more sophisticated than basic yards/points
  
  # Offensive EPA strength (higher is better)
  home_off_strength <- (home_metrics$off_epa_play * 30) +        # Core EPA impact
                       (home_metrics$success_rate * 10) +        # Consistency
                       (home_metrics$explosive_epa_rate * 15) +   # Big plays
                       (home_metrics$red_zone_epa * 8) +         # Scoring efficiency
                       (home_metrics$third_down_epa * 6) +       # Situational
                       (home_metrics$cpoe * 5)                   # QB accuracy
  
  away_off_strength <- (away_metrics$off_epa_play * 30) +
                       (away_metrics$success_rate * 10) +
                       (away_metrics$explosive_epa_rate * 15) +
                       (away_metrics$red_zone_epa * 8) +
                       (away_metrics$third_down_epa * 6) +
                       (away_metrics$cpoe * 5)
  
  # Defensive EPA strength (LOWER epa allowed is better)
  home_def_strength <- -(away_metrics$def_epa_play * 25)         # Defense stops opponent
  away_def_strength <- -(home_metrics$def_epa_play * 25)
  
  # Total team strength
  home_total_strength <- home_off_strength + home_def_strength + 3  # Home field advantage
  away_total_strength <- away_off_strength + away_def_strength
  
  # Predicted margin
  base_margin <- home_total_strength - away_total_strength
  
  # Add situational factors
  total_margin <- base_margin + injury_impact + rest_impact + weather_impact
  
  # Compare to Vegas spread
  spread_difference <- total_margin - (-vegas_spread)
  
  # Recommendation
  recommendation <- if(abs(spread_difference) < 2.5) {
    "PASS"
  } else if(spread_difference > 2.5) {
    "BET HOME"  
  } else {
    "BET AWAY"
  }
  
  # Confidence based on EPA data quality and spread difference
  data_confidence <- min(home_metrics$total_plays, away_metrics$total_plays) / 100
  spread_confidence <- min(abs(spread_difference) / 3, 1)
  total_confidence <- (data_confidence + spread_confidence) / 2
  
  return(list(
    predicted_margin = round(total_margin, 1),
    vegas_spread = vegas_spread,
    spread_difference = round(spread_difference, 1),
    recommendation = recommendation,
    confidence = round(total_confidence, 2),
    breakdown = list(
      home_off_epa = round(home_metrics$off_epa_play, 3),
      away_off_epa = round(away_metrics$off_epa_play, 3),
      home_def_epa = round(home_metrics$def_epa_play, 3),
      away_def_epa = round(away_metrics$def_epa_play, 3),
      home_success_rate = round(home_metrics$success_rate, 3),
      away_success_rate = round(away_metrics$success_rate, 3)
    )
  ))
}

# ============= COMPREHENSIVE EPA SYSTEM TEST =============

test_epa_system <- function(season = 2024) {
  
  cat("=== TESTING EPA PREDICTION SYSTEM ===\n")
  
  # Load data
  cat(sprintf("Loading %d play-by-play data...\n", season))
  pbp_data <- load_pbp(season)
  
  cat(sprintf("Loaded %d plays\n", nrow(pbp_data)))
  
  # Calculate EPA metrics
  epa_metrics <- calculate_all_team_epa_metrics(pbp_data)
  
  # Show top 5 teams by offensive EPA
  cat("\nTOP 5 OFFENSIVE EPA TEAMS:\n")
  top_offense <- epa_metrics[order(-epa_metrics$off_epa_play), ][1:5, ]
  for(i in 1:nrow(top_offense)) {
    team <- top_offense[i, ]
    cat(sprintf("%d. %s: %.3f EPA/play (Success: %.1f%%, Explosive: %.1f%%)\n",
                i, team$team, team$off_epa_play, 
                team$success_rate * 100, team$explosive_epa_rate * 100))
  }
  
  # Show top 5 defenses (LOWEST EPA allowed)
  cat("\nTOP 5 DEFENSIVE EPA TEAMS (lowest EPA allowed):\n")
  top_defense <- epa_metrics[order(epa_metrics$def_epa_play), ][1:5, ]
  for(i in 1:nrow(top_defense)) {
    team <- top_defense[i, ]
    cat(sprintf("%d. %s: %.3f EPA/play allowed\n",
                i, team$team, team$def_epa_play))
  }
  
  return(epa_metrics)
}

cat("Advanced EPA-Based Prediction System loaded! 🚀\n")
cat("\nKey Features:\n")
cat("✅ Offensive/Defensive EPA per play\n")
cat("✅ Pass vs Rush EPA efficiency\n") 
cat("✅ Red zone and third down EPA\n")
cat("✅ QB Completion % Over Expected (CPOE)\n")
cat("✅ Success rate and explosive play rate\n")
cat("✅ Turnover EPA impact\n")

cat("\nFunctions available:\n")
cat("- test_epa_system(): Test EPA system on 2024 data\n")
cat("- calculate_all_team_epa_metrics(): Get EPA for all teams\n")
cat("- predict_game_with_epa(): Make EPA-based predictions\n")

cat("\nTo test: test_epa_system()\n")