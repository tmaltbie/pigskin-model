# Enhanced NFL ML Feature Engineering
# Leverages nflfastR advanced metrics for ML-based spread prediction

library(nflreadr)
library(dplyr)
library(data.table)
library(tidyr)
library(caret)
library(randomForest)
library(xgboost)
library(glmnet)

# ============= ADVANCED FEATURE ENGINEERING =============

# Function to calculate comprehensive team features using nflfastR metrics
calculate_ml_features <- function(pbp_data, season_start = 1999, season_end = 2024) {
  
  cat("Building ML features from nflfastR data...\n")
  cat(sprintf("Processing seasons %d-%d\n", season_start, season_end))
  
  # Filter to relevant seasons
  pbp_filtered <- pbp_data %>%
    filter(season >= season_start & season <= season_end,
           !is.na(posteam), 
           !is.na(defteam),
           play_type %in% c("pass", "run"))
  
  cat(sprintf("Processing %d plays\n", nrow(pbp_filtered)))
  
  # Calculate team-level features by season and week
  team_features <- pbp_filtered %>%
    arrange(game_id, play_id) %>%
    group_by(season, week, posteam) %>%
    summarise(
      # EPA Metrics
      mean_epa = mean(epa, na.rm = TRUE),
      epa_per_play = mean(epa, na.rm = TRUE),
      epa_variance = var(epa, na.rm = TRUE),
      success_rate = mean(epa > 0, na.rm = TRUE),
      
      # Passing Metrics
      cpoe = mean(cpoe, na.rm = TRUE),              # Completion % Over Expected
      passing_epa = mean(epa[play_type == "pass"], na.rm = TRUE),
      air_epa = mean(air_epa, na.rm = TRUE),
      yac_epa = mean(yac_epa, na.rm = TRUE),
      xyac_epa = mean(xyac_epa, na.rm = TRUE),      # Expected YAC EPA
      comp_air_epa = mean(comp_air_epa, na.rm = TRUE),
      comp_yac_epa = mean(comp_yac_epa, na.rm = TRUE),
      
      # Rushing Metrics  
      rushing_epa = mean(epa[play_type == "run"], na.rm = TRUE),
      
      # Win Probability Metrics
      avg_wp = mean(wp, na.rm = TRUE),              # Average Win Probability
      wp_added = mean(wpa, na.rm = TRUE),           # Win Probability Added
      
      # Situational Metrics
      first_down_epa = mean(epa[down == 1], na.rm = TRUE),
      second_down_epa = mean(epa[down == 2], na.rm = TRUE),
      third_down_epa = mean(epa[down == 3], na.rm = TRUE),
      fourth_down_epa = mean(epa[down == 4], na.rm = TRUE),
      
      # Red Zone Performance
      rz_epa = mean(epa[yardline_100 <= 20], na.rm = TRUE),
      rz_success_rate = mean((epa > 0)[yardline_100 <= 20], na.rm = TRUE),
      
      # Drive Metrics
      avg_drive_start = mean(yardline_100[drive_play_count == 1], na.rm = TRUE),
      
      # Explosive Play Rate
      explosive_rate = mean(yards_gained >= 20, na.rm = TRUE),
      
      # Turnover Metrics
      interception_rate = mean(interception == 1, na.rm = TRUE),
      fumble_rate = mean(fumble_lost == 1, na.rm = TRUE),
      
      # Play calling tendencies
      pass_rate_early_down = mean(play_type == "pass"[down %in% c(1,2)], na.rm = TRUE),
      pass_rate_neutral = mean(play_type == "pass"[wp >= 0.2 & wp <= 0.8], na.rm = TRUE),
      
      # Game script metrics
      avg_score_diff = mean(score_differential, na.rm = TRUE),
      
      # Efficiency metrics
      plays_run = n(),
      total_yards = sum(yards_gained, na.rm = TRUE),
      yards_per_play = mean(yards_gained, na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    # Add rolling averages for recent form
    arrange(season, week) %>%
    group_by(posteam) %>%
    mutate(
      # 4-game rolling averages
      epa_l4 = zoo::rollmean(epa_per_play, k = 4, fill = NA, align = "right"),
      success_rate_l4 = zoo::rollmean(success_rate, k = 4, fill = NA, align = "right"),
      cpoe_l4 = zoo::rollmean(cpoe, k = 4, fill = NA, align = "right"),
      wp_added_l4 = zoo::rollmean(wp_added, k = 4, fill = NA, align = "right"),
      
      # Season trends (slope of EPA over time)
      epa_trend = calculate_trend(epa_per_play, week),
      wp_trend = calculate_trend(wp_added, week)
    ) %>%
    ungroup()
  
  return(team_features)
}

# Helper function to calculate trend
calculate_trend <- function(values, time_var) {
  if(length(values) < 3 || all(is.na(values))) return(0)
  
  valid_idx <- !is.na(values)
  if(sum(valid_idx) < 3) return(0)
  
  tryCatch({
    lm_result <- lm(values[valid_idx] ~ time_var[valid_idx])
    return(coef(lm_result)[2])
  }, error = function(e) {
    return(0)
  })
}

# Function to create defensive features
calculate_defensive_features <- function(pbp_data, season_start = 1999, season_end = 2024) {
  
  cat("Calculating defensive features...\n")
  
  # Calculate defensive metrics (team allowing these stats)
  def_features <- pbp_data %>%
    filter(season >= season_start & season <= season_end,
           !is.na(posteam), 
           !is.na(defteam),
           play_type %in% c("pass", "run")) %>%
    group_by(season, week, defteam) %>%
    summarise(
      # Defensive EPA (EPA allowed)
      def_epa_allowed = mean(epa, na.rm = TRUE),
      def_success_rate_allowed = mean(epa > 0, na.rm = TRUE),
      
      # Pass defense
      def_cpoe_allowed = mean(cpoe, na.rm = TRUE),
      def_passing_epa_allowed = mean(epa[play_type == "pass"], na.rm = TRUE),
      def_yac_epa_allowed = mean(yac_epa, na.rm = TRUE),
      
      # Run defense  
      def_rushing_epa_allowed = mean(epa[play_type == "run"], na.rm = TRUE),
      
      # Pressure metrics (if available)
      def_pressure_rate = mean(qb_hit == 1 | sack == 1, na.rm = TRUE),
      
      # Turnover creation
      def_int_created = mean(interception == 1, na.rm = TRUE),
      def_fumble_created = mean(fumble_forced == 1, na.rm = TRUE),
      
      # Red zone defense
      def_rz_epa_allowed = mean(epa[yardline_100 <= 20], na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    rename(posteam = defteam) %>%  # Rename for joining
    rename_with(~paste0("def_", .), -c(season, week, posteam))
  
  return(def_features)
}

# Function to create matchup-specific features
create_matchup_features <- function(home_features, away_features, home_def, away_def) {
  
  # Calculate differentials and matchup advantages
  matchup_data <- data.frame(
    # EPA differentials
    epa_diff = home_features$epa_per_play - away_features$epa_per_play,
    success_rate_diff = home_features$success_rate - away_features$success_rate,
    
    # Offensive vs Defensive matchups
    home_off_vs_away_def_epa = home_features$epa_per_play - away_def$def_def_epa_allowed,
    away_off_vs_home_def_epa = away_features$epa_per_play - home_def$def_def_epa_allowed,
    
    # Passing matchups
    home_cpoe_vs_away_def = home_features$cpoe - away_def$def_def_cpoe_allowed,
    away_cpoe_vs_home_def = away_features$cpoe - home_def$def_def_cpoe_allowed,
    
    # Rushing matchups
    home_rush_vs_away_def = home_features$rushing_epa - away_def$def_def_rushing_epa_allowed,
    away_rush_vs_home_def = away_features$rushing_epa - home_def$def_def_rushing_epa_allowed,
    
    # Win probability differentials
    wp_added_diff = home_features$wp_added - away_features$wp_added,
    
    # Form differentials (recent 4 games)
    epa_l4_diff = home_features$epa_l4 - away_features$epa_l4,
    
    # Trend differentials
    epa_trend_diff = home_features$epa_trend - away_features$epa_trend,
    wp_trend_diff = home_features$wp_trend - away_features$wp_trend,
    
    # Style matchups
    home_pass_rate = home_features$pass_rate_neutral,
    away_pass_rate = away_features$pass_rate_neutral,
    style_mismatch = abs(home_features$pass_rate_neutral - away_features$pass_rate_neutral)
  )
  
  return(matchup_data)
}

# Function to add situational features
add_situational_features <- function(features_df, game_info) {
  
  situational <- features_df %>%
    mutate(
      # Home field advantage variations
      dome_game = ifelse(!is.null(game_info$dome), game_info$dome, FALSE),
      division_game = ifelse(!is.null(game_info$division), game_info$division, FALSE),
      
      # Rest days impact
      home_rest = ifelse(!is.null(game_info$home_rest), game_info$home_rest, 7),
      away_rest = ifelse(!is.null(game_info$away_rest), game_info$away_rest, 7),
      rest_advantage = home_rest - away_rest,
      
      # Weather impact (for outdoor games)
      weather_impact = ifelse(!dome_game & !is.null(game_info$weather), 
                             calculate_weather_score(game_info$weather), 0),
      
      # Prime time factors
      primetime = ifelse(!is.null(game_info$primetime), game_info$primetime, FALSE),
      
      # Season timing
      week_in_season = week,
      early_season = week <= 4,
      late_season = week >= 14,
      
      # Playoff implications (would need standings data)
      playoff_race = ifelse(!is.null(game_info$playoff_implications), 
                           game_info$playoff_implications, FALSE)
    )
  
  return(situational)
}

# Helper function to calculate weather impact score
calculate_weather_score <- function(weather) {
  score <- 0
  
  if(!is.null(weather$temperature)) {
    if(weather$temperature < 32) score <- score - 2
    if(weather$temperature < 20) score <- score - 1
    if(weather$temperature > 90) score := score - 1
  }
  
  if(!is.null(weather$wind_speed)) {
    if(weather$wind_speed > 15) score <- score - 1
    if(weather$wind_speed > 25) score <- score - 2
  }
  
  if(!is.null(weather$precipitation)) {
    if(weather$precipitation == "Light") score <- score - 0.5
    if(weather$precipitation == "Heavy") score <- score - 1.5
  }
  
  return(score)
}

# Main function to build complete feature set for a game
build_game_features <- function(home_team, away_team, season, week, 
                               pbp_data, additional_info = NULL) {
  
  cat(sprintf("Building features for %s @ %s (Week %d, %d)\n", 
              away_team, home_team, week, season))
  
  # Get team features up to this point in the season
  team_features <- calculate_ml_features(pbp_data, season_start = 1999, season_end = season)
  def_features <- calculate_defensive_features(pbp_data, season_start = 1999, season_end = season)
  
  # Filter to current point in season (avoid look-ahead bias)
  current_features <- team_features %>%
    filter(season < !!season | (season == !!season & week < !!week))
  
  current_def <- def_features %>%
    filter(season < !!season | (season == !!season & week < !!week))
  
  # Get most recent features for each team
  home_recent <- current_features %>% 
    filter(posteam == home_team) %>%
    arrange(season, week) %>%
    tail(1)
  
  away_recent <- current_features %>% 
    filter(posteam == away_team) %>%
    arrange(season, week) %>%
    tail(1)
  
  home_def_recent <- current_def %>% 
    filter(posteam == home_team) %>%
    arrange(season, week) %>%
    tail(1)
  
  away_def_recent <- current_def %>% 
    filter(posteam == away_team) %>%
    arrange(season, week) %>%
    tail(1)
  
  if(nrow(home_recent) == 0 || nrow(away_recent) == 0) {
    cat("Warning: Insufficient data for one or both teams\n")
    return(NULL)
  }
  
  # Create matchup features
  matchup_features <- create_matchup_features(home_recent, away_recent, 
                                            home_def_recent, away_def_recent)
  
  # Add home field advantage
  matchup_features$home_field_advantage <- 3.0  # Base HFA
  
  # Add situational features if provided
  if(!is.null(additional_info)) {
    final_features <- add_situational_features(matchup_features, additional_info)
  } else {
    final_features <- matchup_features
  }
  
  # Add meta features
  final_features$season <- season
  final_features$week <- week
  final_features$home_team <- home_team
  final_features$away_team <- away_team
  
  return(final_features)
}

cat("Enhanced ML Feature Engineering loaded!\n")
cat("Features include:\n")
cat("✅ EPA metrics (mean, variance, success rate)\n")
cat("✅ CPOE (Completion Percentage Over Expected)\n") 
cat("✅ YAC EPA and XYAC EPA\n")
cat("✅ Win Probability and WPA\n")
cat("✅ Situational EPA (down, distance, field position)\n")
cat("✅ Defensive metrics (EPA allowed, pressure rate)\n")
cat("✅ Rolling averages and trends\n")
cat("✅ Matchup-specific features\n")
cat("✅ Weather, rest, and situational factors\n")
cat("\nNext: Build ML models using these features\n")