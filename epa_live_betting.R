# EPA-Based Live Betting System
# Combines advanced EPA metrics with real-time odds

source('epa_prediction_system.R')
source('odds_api.R')
source('team_mapping.R')

# Function to get EPA predictions for current games
get_epa_betting_predictions <- function() {
  
  cat("=== EPA-BASED LIVE BETTING PREDICTIONS ===\n")
  
  # Get current odds
  cat("Getting live betting odds...\n")
  raw_odds <- get_nfl_odds()
  
  if(is.null(raw_odds) || length(raw_odds) == 0) {
    cat("No odds data available\n")
    return(NULL)
  }
  
  # Parse the raw odds data
  cat("Parsing odds data...\n")
  parsed_odds <- parse_nfl_odds(raw_odds)
  
  if(nrow(parsed_odds) == 0) {
    cat("No parsed odds data available\n")
    return(NULL)
  }
  
  # Get consensus spreads
  odds_data <- get_consensus_spreads(parsed_odds)
  
  if(nrow(odds_data) == 0) {
    cat("No consensus spreads available\n")
    return(NULL)
  }
  
  # Get 2025 EPA metrics
  cat("Loading 2025 EPA metrics...\n")
  pbp_2025 <- load_pbp(2025)
  epa_metrics <- calculate_all_team_epa_metrics(pbp_2025)
  
  predictions <- list()
  
  for(i in 1:nrow(odds_data)) {
    game <- odds_data[i,]
    
    # Convert team names to abbreviations
    home_abbr <- convert_team_name(game$home_team)
    away_abbr <- convert_team_name(game$away_team)
    
    if(is.null(home_abbr) || is.null(away_abbr)) {
      cat(sprintf("Could not map teams: %s vs %s\n", game$away_team, game$home_team))
      next
    }
    
    # Use home spread from consensus data
    vegas_spread <- game$avg_home_spread
    
    if(is.na(vegas_spread)) {
      cat(sprintf("No spread available for %s vs %s\n", game$away_team, game$home_team))
      next
    }
    
    # Get EPA prediction
    epa_pred <- predict_game_with_epa(
      home_team = home_abbr,
      away_team = away_abbr, 
      vegas_spread = vegas_spread,
      epa_metrics = epa_metrics
    )
    
    # Calculate betting recommendation
    spread_diff <- epa_pred$predicted_margin - (-vegas_spread)
    
    recommendation <- if(abs(spread_diff) < 2.0) {
      "PASS - Close to market"
    } else if(spread_diff > 2.0) {
      sprintf("BET %s - EPA says %+.1f vs market %+.1f", 
              home_abbr, epa_pred$predicted_margin, -vegas_spread)
    } else {
      sprintf("BET %s - EPA says %+.1f vs market %+.1f",
              away_abbr, epa_pred$predicted_margin, -vegas_spread)
    }
    
    predictions[[i]] <- list(
      game = sprintf("%s @ %s", away_abbr, home_abbr),
      epa_prediction = epa_pred$predicted_margin,
      vegas_spread = -vegas_spread,
      difference = spread_diff,
      recommendation = recommendation,
      confidence = epa_pred$confidence,
      epa_breakdown = epa_pred$breakdown
    )
    
    cat(sprintf("\n%s @ %s:\n", away_abbr, home_abbr))
    cat(sprintf("  EPA Prediction: %s by %.1f\n", 
                ifelse(epa_pred$predicted_margin > 0, home_abbr, away_abbr),
                abs(epa_pred$predicted_margin)))
    cat(sprintf("  Vegas Line: %s %.1f\n", 
                ifelse(vegas_spread > 0, away_abbr, home_abbr),
                abs(vegas_spread)))
    cat(sprintf("  Books: %d\n", game$num_books))
    cat(sprintf("  Difference: %.1f points\n", abs(spread_diff)))
    cat(sprintf("  %s\n", recommendation))
    if(!is.null(epa_pred$breakdown)) {
      cat(sprintf("  EPA Breakdown - Home Off: %.3f, Away Off: %.3f\n", 
                  epa_pred$breakdown$home_off_epa, epa_pred$breakdown$away_off_epa))
    }
  }
  
  cat(sprintf("\n✅ Analyzed %d games with EPA model\n", length(predictions)))
  return(predictions)
}

# Function to show top EPA betting opportunities
show_epa_betting_opportunities <- function(predictions) {
  
  if(is.null(predictions) || length(predictions) == 0) {
    cat("No predictions available\n")
    return()
  }
  
  cat("\n=== TOP EPA BETTING OPPORTUNITIES ===\n")
  
  # Sort by absolute difference from market
  differences <- sapply(predictions, function(x) abs(x$difference))
  sorted_indices <- order(differences, decreasing = TRUE)
  
  top_opportunities <- predictions[sorted_indices[1:min(5, length(predictions))]]
  
  for(i in 1:length(top_opportunities)) {
    opp <- top_opportunities[[i]]
    if(abs(opp$difference) >= 2.0) {
      cat(sprintf("%d. %s\n", i, opp$game))
      cat(sprintf("   %s\n", opp$recommendation))
      cat(sprintf("   Edge: %.1f points\n", abs(opp$difference)))
      cat(sprintf("   Confidence: %.2f\n\n", opp$confidence))
    }
  }
  
  strong_bets <- sum(sapply(predictions, function(x) abs(x$difference) >= 3.0))
  medium_bets <- sum(sapply(predictions, function(x) abs(x$difference) >= 2.0 && abs(x$difference) < 3.0))
  
  cat(sprintf("Strong Bets (3+ point edge): %d\n", strong_bets))
  cat(sprintf("Medium Bets (2-3 point edge): %d\n", medium_bets))
}

cat("EPA Live Betting System loaded!\n")
cat("Functions available:\n")
cat("- get_epa_betting_predictions(): Get EPA predictions vs current odds\n")
cat("- show_epa_betting_opportunities(): Show top betting opportunities\n")
cat("\nTo run: predictions <- get_epa_betting_predictions(); show_epa_betting_opportunities(predictions)\n")