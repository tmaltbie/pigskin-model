# Advanced NFL Prediction Model Architecture
# Incorporates injuries, coaching, situational factors, weather, and more

library(httr)
library(jsonlite)
library(dplyr)

# ============= INJURY IMPACT SYSTEM =============

# Player impact ratings by position
get_position_impact <- function() {
  return(list(
    "QB" = 10.0,   # Quarterback has massive impact
    "RB" = 4.0,    # Running back significant impact
    "WR" = 3.5,    # Wide receiver notable impact
    "TE" = 2.5,    # Tight end moderate impact
    "OL" = 3.0,    # Offensive line important
    "DE" = 3.5,    # Pass rushers key
    "LB" = 2.5,    # Linebackers moderate
    "CB" = 3.0,    # Cornerbacks important
    "S" = 2.0,     # Safeties moderate
    "K" = 1.0,     # Kicker minimal
    "P" = 0.5      # Punter minimal
  ))
}

# Injury status weights
get_injury_weights <- function() {
  return(list(
    "Out" = 1.0,           # Full impact
    "Doubtful" = 0.8,      # 80% likely out
    "Questionable" = 0.4,  # 40% impact
    "Probable" = 0.1,      # Minimal impact
    "Healthy" = 0.0        # No impact
  ))
}

# Function to calculate team injury impact
calculate_injury_impact <- function(team_injuries) {
  if(nrow(team_injuries) == 0) return(0)
  
  position_impact <- get_position_impact()
  injury_weights <- get_injury_weights()
  
  total_impact <- 0
  
  for(i in 1:nrow(team_injuries)) {
    injury <- team_injuries[i,]
    
    # Safely get position impact
    pos_impact <- position_impact[[injury$position]]
    if(is.null(pos_impact) || length(pos_impact) == 0) pos_impact <- 1.0
    
    # Safely get injury weight
    injury_weight <- injury_weights[[injury$status]]
    if(is.null(injury_weight) || length(injury_weight) == 0) injury_weight <- 0.4
    
    # Player talent multiplier (1.0 = average, 1.5 = star, 0.8 = backup)
    talent_mult <- ifelse(is.na(injury$talent_rating), 1.0, injury$talent_rating)
    
    impact <- pos_impact * injury_weight * talent_mult
    total_impact <- total_impact + impact
  }
  
  return(total_impact)
}

# ============= SITUATIONAL FACTORS =============

# Days rest impact
calculate_rest_impact <- function(days_rest) {
  if(days_rest <= 3) return(-2.0)      # Short week penalty
  if(days_rest == 4) return(-1.0)      # Thursday game
  if(days_rest <= 6) return(0.0)       # Normal week
  if(days_rest <= 9) return(0.5)       # Mini bye
  return(1.0)                          # Full bye week
}

# Prime time game adjustments
get_primetime_adjustments <- function() {
  return(list(
    "MNF" = list(home_boost = 1.0, public_pressure = 0.5),
    "SNF" = list(home_boost = 0.8, public_pressure = 0.8),
    "TNF" = list(home_boost = -0.5, fatigue_factor = -1.0),  # Short week
    "Regular" = list(home_boost = 0.0, public_pressure = 0.0)
  ))
}

# Coaching tendencies and matchups
calculate_coaching_edge <- function(home_coach, away_coach, situation) {
  # This would be populated with actual coaching data
  coaching_data <- list(
    "Andy Reid" = list(
      ats_record = 0.52,
      playoff_bonus = 2.0,
      road_penalty = -0.5,
      primetime_boost = 1.0
    ),
    "Bill Belichick" = list(
      ats_record = 0.58,
      playoff_bonus = 3.0,
      road_bonus = 1.0,
      underdog_boost = 2.0
    )
    # Add all 32 coaches...
  )
  
  home_edge <- 0
  away_edge <- 0
  
  if(!is.null(coaching_data[[home_coach]])) {
    coach_data <- coaching_data[[home_coach]]
    home_edge <- (coach_data$ats_record - 0.5) * 10  # Convert to point impact
  }
  
  if(!is.null(coaching_data[[away_coach]])) {
    coach_data <- coaching_data[[away_coach]]
    away_edge <- (coach_data$ats_record - 0.5) * 10
  }
  
  return(home_edge - away_edge)
}

# ============= WEATHER AND VENUE FACTORS =============

# Stadium information
get_stadium_info <- function() {
  return(list(
    "BUF" = list(type = "outdoor", altitude = 600, surface = "turf"),
    "MIA" = list(type = "outdoor", altitude = 10, surface = "grass"),
    "DEN" = list(type = "outdoor", altitude = 5280, surface = "grass"),  # Mile high
    "DET" = list(type = "dome", altitude = 600, surface = "turf"),
    "NO" = list(type = "dome", altitude = 10, surface = "turf"),
    "MIN" = list(type = "dome", altitude = 830, surface = "turf")
    # Add all 32 stadiums...
  ))
}

# Weather impact calculation
calculate_weather_impact <- function(weather_conditions, stadium_type) {
  if(stadium_type == "dome") return(0)  # No weather impact in domes
  
  impact <- 0
  
  # Temperature effects
  if(weather_conditions$temp < 32) impact <- impact - 1.0  # Freezing
  if(weather_conditions$temp < 20) impact <- impact - 1.5  # Bitter cold
  if(weather_conditions$temp > 95) impact <- impact - 0.5  # Extreme heat
  
  # Wind effects (affects passing and kicking)
  if(weather_conditions$wind_speed > 15) impact <- impact - 1.0
  if(weather_conditions$wind_speed > 25) impact <- impact - 2.0
  
  # Precipitation effects
  if(weather_conditions$precipitation == "Light") impact <- impact - 0.5
  if(weather_conditions$precipitation == "Heavy") impact <- impact - 1.5
  if(weather_conditions$precipitation == "Snow") impact <- impact - 1.0
  
  return(impact)
}

# ============= ADVANCED TEAM METRICS =============

# Calculate advanced metrics beyond basic stats
calculate_advanced_metrics <- function(pbp_data, schedule_data, team) {
  
  team_pbp <- pbp_data %>% filter(posteam == team)
  
  if(nrow(team_pbp) == 0) {
    return(list(
      rz_efficiency = 0,
      third_down_pct = 0,
      turnover_diff = 0,
      time_of_poss = 0,
      explosive_rate = 0
    ))
  }
  
  # Red zone efficiency
  rz_attempts <- team_pbp %>% filter(yardline_100 <= 20) %>% nrow()
  rz_tds <- team_pbp %>% filter(yardline_100 <= 20, touchdown == 1) %>% nrow()
  rz_efficiency <- ifelse(rz_attempts > 0, rz_tds / rz_attempts, 0)
  
  # Third down efficiency  
  third_down_att <- team_pbp %>% filter(down == 3) %>% nrow()
  third_down_conv <- team_pbp %>% filter(down == 3, first_down == 1) %>% nrow()
  third_down_pct <- ifelse(third_down_att > 0, third_down_conv / third_down_att, 0)
  
  # Turnover differential (simplified - only what we can measure)
  turnovers_lost <- team_pbp %>% filter(interception == 1 | fumble_lost == 1) %>% nrow()
  
  # For turnovers gained, check if fumble_recovered column exists
  if("fumble_recovered" %in% colnames(pbp_data)) {
    turnovers_gained <- pbp_data %>% 
      filter(defteam == team, interception == 1 | fumble_recovered == 1) %>% nrow()
  } else {
    # Use a simplified approach if column doesn't exist
    turnovers_gained <- pbp_data %>% 
      filter(defteam == team, interception == 1) %>% nrow()
  }
  
  turnover_diff <- turnovers_gained - turnovers_lost
  
  # Time of possession (approximation)
  plays_run <- nrow(team_pbp)
  avg_play_time <- 6  # seconds per play approximation
  time_of_poss <- (plays_run * avg_play_time) / 60  # minutes
  
  # Explosive play rate (20+ yard gains)
  explosive_plays <- team_pbp %>% filter(yards_gained >= 20) %>% nrow()
  explosive_rate <- ifelse(plays_run > 0, explosive_plays / plays_run, 0)
  
  return(list(
    rz_efficiency = rz_efficiency,
    third_down_pct = third_down_pct,
    turnover_diff = turnover_diff,
    time_of_poss = time_of_poss,
    explosive_rate = explosive_rate
  ))
}

# ============= COMPREHENSIVE PREDICTION MODEL =============

advanced_predict_game <- function(home_team, away_team, game_info, 
                                 team_metrics, pbp_data, schedule_data) {
  
  # Base team strength (from existing model)
  home_metrics <- team_metrics %>% filter(posteam == home_team)
  away_metrics <- team_metrics %>% filter(posteam == away_team)
  
  if(nrow(home_metrics) == 0 || nrow(away_metrics) == 0) {
    return(list(predicted_margin = 0, confidence = 0, factors = "Missing team data"))
  }
  
  # Base prediction
  home_base <- (home_metrics$yards_per_play * 10) + (home_metrics$points_per_game * 0.5)
  away_base <- (away_metrics$yards_per_play * 10) + (away_metrics$points_per_game * 0.5)
  
  base_margin <- home_base - away_base + 3  # Home field advantage
  
  # INJURY ADJUSTMENTS
  injury_impact <- 0
  if(!is.null(game_info$injuries)) {
    home_injuries <- game_info$injuries %>% filter(team == home_team)
    away_injuries <- game_info$injuries %>% filter(team == away_team)
    
    home_injury_impact <- calculate_injury_impact(home_injuries)
    away_injury_impact <- calculate_injury_impact(away_injuries)
    
    injury_impact <- away_injury_impact - home_injury_impact
  }
  
  # SITUATIONAL ADJUSTMENTS
  rest_impact <- 0
  if(!is.null(game_info$days_rest)) {
    home_rest <- calculate_rest_impact(game_info$days_rest$home)
    away_rest <- calculate_rest_impact(game_info$days_rest$away)
    rest_impact <- home_rest - away_rest
  }
  
  # PRIME TIME ADJUSTMENTS
  primetime_impact <- 0
  if(!is.null(game_info$game_type)) {
    pt_adj <- get_primetime_adjustments()[[game_info$game_type]]
    if(!is.null(pt_adj)) {
      primetime_impact <- pt_adj$home_boost
    }
  }
  
  # WEATHER ADJUSTMENTS
  weather_impact <- 0
  stadium_info <- get_stadium_info()
  if(!is.null(game_info$weather) && !is.null(stadium_info[[home_team]])) {
    weather_impact <- calculate_weather_impact(
      game_info$weather, 
      stadium_info[[home_team]]$type
    )
  }
  
  # COACHING ADJUSTMENTS
  coaching_impact <- 0
  if(!is.null(game_info$coaches)) {
    coaching_impact <- calculate_coaching_edge(
      game_info$coaches$home,
      game_info$coaches$away,
      game_info$situation
    )
  }
  
  # ADVANCED METRICS
  advanced_impact <- 0
  home_advanced <- calculate_advanced_metrics(pbp_data, schedule_data, home_team)
  away_advanced <- calculate_advanced_metrics(pbp_data, schedule_data, away_team)
  
  # Weight advanced metrics
  advanced_impact <- (home_advanced$rz_efficiency - away_advanced$rz_efficiency) * 5 +
                    (home_advanced$turnover_diff - away_advanced$turnover_diff) * 1.5 +
                    (home_advanced$explosive_rate - away_advanced$explosive_rate) * 8
  
  # FINAL PREDICTION
  final_margin <- base_margin + injury_impact + rest_impact + 
                  primetime_impact + weather_impact + coaching_impact + advanced_impact
  
  # Calculate confidence based on data availability
  confidence_factors <- c(
    !is.null(game_info$injuries),
    !is.null(game_info$weather),
    !is.null(game_info$coaches),
    !is.null(game_info$days_rest)
  )
  confidence <- (sum(confidence_factors) + 2) / 6  # Base confidence of 2/6
  
  return(list(
    predicted_margin = round(final_margin, 1),
    confidence = confidence,
    breakdown = list(
      base = round(base_margin, 1),
      injuries = round(injury_impact, 1),
      rest = round(rest_impact, 1),
      primetime = round(primetime_impact, 1),
      weather = round(weather_impact, 1),
      coaching = round(coaching_impact, 1),
      advanced = round(advanced_impact, 1)
    )
  ))
}

cat("Advanced NFL Prediction Model loaded!\n")
cat("Factors included:\n")
cat("✅ Injury impact by position and severity\n")
cat("✅ Rest days and short week penalties\n") 
cat("✅ Prime time game adjustments\n")
cat("✅ Weather and venue factors\n")
cat("✅ Coaching tendencies and edges\n")
cat("✅ Advanced team metrics (red zone, turnover diff, etc.)\n")
cat("\nNext: Integrate data sources for all factors\n")