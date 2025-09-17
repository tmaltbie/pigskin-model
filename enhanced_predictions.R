# Complete NFL Prediction System with Real Data
# Combines 2025 NFL data with live betting odds

source("spread_predictor.R")
source("odds_api.R")
source("team_mapping.R")

# Enhanced prediction system using 2025 data
run_2025_predictions <- function(target_week = 3) {
  
  cat("=== NFL 2025 PREDICTION SYSTEM ===\n\n")
  
  # Step 1: Load 2025 NFL data
  cat("Loading 2025 NFL data...\n")
  pbp_2025 <- load_pbp(2025)
  schedule_2025 <- load_schedules(2025)
  
  cat(sprintf("✅ Loaded %d plays and %d games from 2025\n", 
              nrow(pbp_2025), nrow(schedule_2025)))
  
  # Step 2: Get live betting odds
  cat("\nFetching live betting odds...\n")
  odds_data <- get_nfl_odds()
  
  if(!is.null(odds_data)) {
    parsed_odds <- parse_nfl_odds(odds_data)
    consensus_spreads <- get_consensus_spreads(parsed_odds)
    consensus_spreads <- convert_team_names(consensus_spreads)  # Add team mapping
    
    cat(sprintf("✅ Retrieved odds for %d games\n", nrow(consensus_spreads)))
  } else {
    cat("❌ Could not retrieve betting odds\n")
    return(NULL)
  }
  
  # Step 3: Calculate team metrics using 2025 data
  cat("\nCalculating team metrics from 2025 season...\n")
  team_metrics_2025 <- calculate_team_metrics(pbp_2025, schedule_2025)
  
  # Step 4: Run predictions for upcoming games
  cat(sprintf("\n=== PREDICTIONS FOR UPCOMING GAMES ===\n"))
  
  predictions <- list()
  
  for(i in 1:nrow(consensus_spreads)) {
    game <- consensus_spreads[i,]
    
    # Skip games that already happened
    game_time <- as.POSIXct(game$commence_time, format="%Y-%m-%dT%H:%M:%SZ")
    if(game_time < Sys.time()) {
      cat(sprintf("Skipping completed game: %s @ %s\n", 
                  game$away_team, game$home_team))
      next
    }
    
    pred <- predict_game_spread(
      home_team = game$home_team_abbr,  # Use abbreviated names
      away_team = game$away_team_abbr,  # Use abbreviated names
      vegas_spread = game$avg_home_spread,
      team_metrics = team_metrics_2025,
      schedule_data = schedule_2025
    )
    
    predictions[[length(predictions) + 1]] <- pred
    
    # Print prediction
    cat(sprintf("\n%s @ %s\n", pred$away_team, pred$home_team))
    cat(sprintf("Vegas Spread: %s %+.1f (from %d books)\n", 
                pred$home_team, -game$avg_home_spread, game$num_books))
    cat(sprintf("Predicted Margin: %s by %.1f\n", 
                pred$home_team, pred$predicted_margin))
    cat(sprintf("Recommendation: %s (Confidence: %.0f%%)\n", 
                pred$recommendation, pred$confidence * 100))
    cat("----------------------------------------\n")
  }
  
  # Step 5: Summary
  if(length(predictions) > 0) {
    strong_bets <- predictions[sapply(predictions, function(x) x$confidence > 0.6)]
    
    cat("\n=== BETTING RECOMMENDATIONS ===\n")
    if(length(strong_bets) > 0) {
      cat("STRONG PLAYS:\n")
      for(bet in strong_bets) {
        cat(sprintf("• %s @ %s: %s\n", 
                    bet$away_team, bet$home_team, bet$recommendation))
      }
    } else {
      cat("No strong plays found this week.\n")
    }
  }
  
  return(list(
    predictions = predictions,
    odds_data = consensus_spreads,
    team_metrics = team_metrics_2025
  ))
}

# Function to backtest using 2025 completed games
backtest_2025_season <- function() {
  
  cat("=== BACKTESTING 2025 SEASON ===\n\n")
  
  # Load 2025 data
  schedule_2025 <- load_schedules(2025)
  pbp_2025 <- load_pbp(2025)
  
  # Get completed games only
  completed_games <- schedule_2025[!is.na(schedule_2025$result), ]
  
  cat(sprintf("Found %d completed games in 2025\n", nrow(completed_games)))
  
  if(nrow(completed_games) == 0) {
    cat("No completed games found for backtesting\n")
    return(NULL)
  }
  
  # For backtesting, we need historical spreads
  # This would ideally come from archived betting data
  # For now, we'll simulate some spreads based on team strength
  
  cat("Note: Using simulated spreads for backtesting.\n")
  cat("Integrate historical betting data for accurate backtest.\n")
  
  # Calculate team metrics
  team_metrics <- calculate_team_metrics(pbp_2025, schedule_2025)
  
  # Simple backtest on completed games
  correct_predictions <- 0
  total_predictions <- 0
  
  for(i in 1:min(10, nrow(completed_games))) {  # Test first 10 games
    game <- completed_games[i,]
    
    # Simulate a spread (normally would come from historical data)
    simulated_spread <- rnorm(1, mean = 0, sd = 6)
    
    actual_margin <- game$home_score - game$away_score
    
    pred <- predict_game_spread(
      home_team = game$home_team,
      away_team = game$away_team,
      vegas_spread = simulated_spread,
      team_metrics = team_metrics,
      schedule_data = schedule_2025
    )
    
    prediction_error <- abs(actual_margin - pred$predicted_margin)
    
    if(prediction_error < 7) correct_predictions <- correct_predictions + 1
    total_predictions <- total_predictions + 1
    
    cat(sprintf("%s @ %s: Predicted %.1f, Actual %.1f (Error: %.1f)\n",
                game$away_team, game$home_team, 
                pred$predicted_margin, actual_margin, prediction_error))
  }
  
  accuracy <- correct_predictions / total_predictions * 100
  cat(sprintf("\nBacktest Accuracy (within 7 points): %.1f%%\n", accuracy))
  
  return(list(
    accuracy = accuracy,
    correct_predictions = correct_predictions,
    total_predictions = total_predictions
  ))
}

cat("Enhanced NFL Prediction System loaded!\n")
cat("Functions available:\n")
cat("- run_2025_predictions(): Get predictions for upcoming games\n")
cat("- backtest_2025_season(): Test accuracy on completed 2025 games\n")
cat("\nTo run: run_2025_predictions()\n")