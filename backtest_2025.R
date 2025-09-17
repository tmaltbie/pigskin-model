# NFL 2025 Week 1-2 Backtesting System
# Tests advanced model against actual results

source("advanced_model.R")
source("enhanced_predictions.R")

# Function to get historical injury data for specific weeks
get_historical_injuries <- function(week, season = 2025) {
  # This would integrate with actual injury report sources
  # For now, creating sample data based on typical injury patterns
  
  sample_injuries <- data.frame(
    team = c("BUF", "MIA", "DAL", "GB", "KC", "SF"),
    player = c("Von Miller", "Tua Tagovailoa", "Dak Prescott", 
               "Aaron Rodgers", "Travis Kelce", "Christian McCaffrey"),
    position = c("LB", "QB", "QB", "QB", "TE", "RB"),
    status = c("Out", "Questionable", "Probable", "Out", "Questionable", "Out"),
    talent_rating = c(1.2, 1.8, 1.6, 1.9, 1.4, 1.7),  # Star player ratings
    week = week,
    season = season,
    stringsAsFactors = FALSE
  )
  
  cat(sprintf("NOTE: Using sample injury data for Week %d\n", week))
  return(sample_injuries)
}

# Function to get historical weather for completed games
get_historical_weather <- function(games_df) {
  # This would integrate with weather APIs
  weather_data <- games_df %>%
    mutate(
      temperature = sample(35:85, n(), replace = TRUE),
      wind_speed = sample(0:20, n(), replace = TRUE), 
      precipitation = sample(c("None", "Light", "Heavy"), n(), replace = TRUE, prob = c(0.8, 0.15, 0.05)),
      humidity = sample(40:90, n(), replace = TRUE)
    )
  
  cat("NOTE: Using sample weather data\n")
  return(weather_data)
}

# Function to calculate days rest between games
calculate_days_rest <- function(schedule_data, team, game_week) {
  
  # Get previous game for the team
  prev_games <- schedule_data %>%
    filter((home_team == team | away_team == team) & week < game_week) %>%
    arrange(desc(week))
  
  if(nrow(prev_games) == 0) {
    return(7)  # First game of season, assume normal rest
  }
  
  # Calculate days between games (simplified)
  weeks_diff <- game_week - prev_games$week[1]
  days_rest <- weeks_diff * 7
  
  # Adjust for Thursday/Monday games (simplified)
  if(game_week %% 17 == 3) days_rest <- days_rest - 3  # Thursday game
  if(game_week %% 19 == 0) days_rest <- days_rest + 1  # Monday game
  
  return(days_rest)
}

# Function to identify prime time games
identify_game_type <- function(game_info) {
  # This would use actual game schedule data
  # For now, simulate based on patterns
  
  if(grepl("Thu", game_info$game_day, ignore.case = TRUE)) return("TNF")
  if(grepl("Sun.*Night", game_info$game_time, ignore.case = TRUE)) return("SNF") 
  if(grepl("Mon", game_info$game_day, ignore.case = TRUE)) return("MNF")
  return("Regular")
}

# Main backtesting function for Week 1-2
backtest_weeks_1_2 <- function() {
  
  cat("=== BACKTESTING NFL 2025 WEEKS 1-2 ===\n\n")
  
  # Load 2025 data
  pbp_2025 <- load_pbp(2025)
  schedule_2025 <- load_schedules(2025)
  team_metrics <- calculate_team_metrics(pbp_2025, schedule_2025)
  
  # Get completed games from Weeks 1-2
  completed_games <- schedule_2025 %>%
    filter(week %in% c(1, 2) & !is.na(result)) %>%
    arrange(week, game_id)
  
  cat(sprintf("Found %d completed games in Weeks 1-2\n\n", nrow(completed_games)))
  
  if(nrow(completed_games) == 0) {
    cat("No completed games found for backtesting\n")
    return(NULL)
  }
  
  # Backtesting results
  results <- data.frame()
  
  for(i in 1:nrow(completed_games)) {
    game <- completed_games[i,]
    
    cat(sprintf("Week %d: %s @ %s\n", game$week, game$away_team, game$home_team))
    
    # Gather all contextual information for this game
    game_info <- list(
      injuries = get_historical_injuries(game$week) %>% 
        filter(team %in% c(game$home_team, game$away_team)),
      
      days_rest = list(
        home = calculate_days_rest(schedule_2025, game$home_team, game$week),
        away = calculate_days_rest(schedule_2025, game$away_team, game$week)
      ),
      
      game_type = identify_game_type(list(
        game_day = "Sunday",  # Simplified
        game_time = "1:00 PM"
      )),
      
      weather = list(
        temp = sample(65:75, 1),
        wind_speed = sample(5:15, 1),
        precipitation = "None"
      ),
      
      coaches = list(
        home = "Unknown",  # Would need coaching database
        away = "Unknown"
      )
    )
    
    # Make prediction using advanced model
    prediction <- advanced_predict_game(
      home_team = game$home_team,
      away_team = game$away_team, 
      game_info = game_info,
      team_metrics = team_metrics,
      pbp_data = pbp_2025,
      schedule_data = schedule_2025
    )
    
    # Calculate actual result
    actual_margin <- game$home_score - game$away_score
    prediction_error <- abs(actual_margin - prediction$predicted_margin)
    
    # Store results
    game_result <- data.frame(
      week = game$week,
      game = paste(game$away_team, "@", game$home_team),
      predicted_margin = prediction$predicted_margin,
      actual_margin = actual_margin,
      error = prediction_error,
      confidence = prediction$confidence,
      within_3 = prediction_error <= 3,
      within_7 = prediction_error <= 7,
      
      # Breakdown of factors
      base_prediction = prediction$breakdown$base,
      injury_impact = prediction$breakdown$injuries,
      rest_impact = prediction$breakdown$rest,
      weather_impact = prediction$breakdown$weather,
      
      stringsAsFactors = FALSE
    )
    
    results <- rbind(results, game_result)
    
    # Print result
    cat(sprintf("  Predicted: %s by %.1f\n", game$home_team, prediction$predicted_margin))
    cat(sprintf("  Actual: %s by %.1f\n", game$home_team, actual_margin))
    cat(sprintf("  Error: %.1f points\n", prediction_error))
    cat(sprintf("  Breakdown - Base: %.1f, Injuries: %.1f, Rest: %.1f\n", 
                prediction$breakdown$base, prediction$breakdown$injuries, prediction$breakdown$rest))
    cat("  ----------------------------------------\n")
  }
  
  # Analysis of results
  cat("\n=== BACKTESTING RESULTS ANALYSIS ===\n")
  cat(sprintf("Total games analyzed: %d\n", nrow(results)))
  cat(sprintf("Average prediction error: %.2f points\n", mean(results$error)))
  cat(sprintf("Predictions within 3 points: %.1f%%\n", mean(results$within_3) * 100))
  cat(sprintf("Predictions within 7 points: %.1f%%\n", mean(results$within_7) * 100))
  cat(sprintf("Average confidence: %.2f\n", mean(results$confidence)))
  
  # Best and worst predictions
  cat("\nBEST PREDICTIONS:\n")
  best <- results[order(results$error)[1:3], ]
  for(i in 1:nrow(best)) {
    cat(sprintf("%s: Predicted %.1f, Actual %.1f (Error: %.1f)\n",
                best$game[i], best$predicted_margin[i], best$actual_margin[i], best$error[i]))
  }
  
  cat("\nWORST PREDICTIONS:\n") 
  worst <- results[order(-results$error)[1:3], ]
  for(i in 1:nrow(worst)) {
    cat(sprintf("%s: Predicted %.1f, Actual %.1f (Error: %.1f)\n",
                worst$game[i], worst$predicted_margin[i], worst$actual_margin[i], worst$error[i]))
  }
  
  # Factor impact analysis
  cat("\nFACTOR IMPACT ANALYSIS:\n")
  cat(sprintf("Average injury impact: %.2f points\n", mean(abs(results$injury_impact))))
  cat(sprintf("Average rest impact: %.2f points\n", mean(abs(results$rest_impact))))
  cat(sprintf("Average weather impact: %.2f points\n", mean(abs(results$weather_impact))))
  
  return(results)
}

# Simplified backtest with current basic model for comparison
backtest_basic_model <- function() {
  
  cat("=== BASIC MODEL BACKTEST (FOR COMPARISON) ===\n\n")
  
  pbp_2025 <- load_pbp(2025)
  schedule_2025 <- load_schedules(2025)
  team_metrics <- calculate_team_metrics(pbp_2025, schedule_2025)
  
  completed_games <- schedule_2025 %>%
    filter(week %in% c(1, 2) & !is.na(result))
  
  basic_results <- data.frame()
  
  for(i in 1:nrow(completed_games)) {
    game <- completed_games[i,]
    
    # Use basic prediction model (current system)
    pred <- predict_game_spread(
      home_team = game$home_team,
      away_team = game$away_team,
      vegas_spread = 0,  # Not using spread for backtest
      team_metrics = team_metrics,
      schedule_data = schedule_2025
    )
    
    actual_margin <- game$home_score - game$away_score
    error <- abs(actual_margin - pred$predicted_margin)
    
    basic_results <- rbind(basic_results, data.frame(
      game = paste(game$away_team, "@", game$home_team),
      predicted = pred$predicted_margin,
      actual = actual_margin,
      error = error,
      within_7 = error <= 7
    ))
  }
  
  cat(sprintf("Basic Model - Average Error: %.2f points\n", mean(basic_results$error)))
  cat(sprintf("Basic Model - Within 7 points: %.1f%%\n", mean(basic_results$within_7) * 100))
  
  return(basic_results)
}

cat("NFL 2025 Backtesting System loaded!\n")
cat("Functions available:\n")
cat("- backtest_weeks_1_2(): Test advanced model on Weeks 1-2\n") 
cat("- backtest_basic_model(): Test basic model for comparison\n")
cat("\nTo run: backtest_weeks_1_2()\n")