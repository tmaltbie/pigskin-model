# Week 3 2025 Predictions with Win Probabilities & EV Analysis
# Matching the format from your screenshot

source('complete_week3_enhanced.R')

# Function to convert spread to win probability
spread_to_win_prob <- function(spread) {
  # Using standard -110 juice and normal distribution
  # Positive spread = team is underdog, negative = favorite
  prob <- pnorm(-spread / 13.86)  # 13.86 is standard NFL spread-to-prob conversion
  return(round(prob * 100, 1))
}

# Function to calculate expected value for betting
calculate_ev <- function(our_prob, market_odds) {
  # Convert American odds to decimal
  if (market_odds > 0) {
    decimal_odds <- (market_odds / 100) + 1
  } else {
    decimal_odds <- (100 / abs(market_odds)) + 1
  }
  
  # Calculate implied probability from market odds
  implied_prob <- 1 / decimal_odds
  
  # EV = (our_prob * (decimal_odds - 1)) - ((1 - our_prob) * 1)
  ev <- (our_prob * (decimal_odds - 1)) - (1 - our_prob)
  return(ev)
}

# Generate Week 3 predictions with probabilities
generate_week3_probabilities <- function() {
  
  cat("🏈 WEEK 3 2025 NFL PREDICTIONS WITH WIN PROBABILITIES\n")
  cat("=====================================================\n\n")
  
  # Get predictions from our enhanced system
  predictions <- generate_complete_week3_predictions()
  
  # Real Week 3 games with estimated market odds (American format)
  week3_detailed <- data.frame(
    away_team = c("MIA", "ATL", "GB", "HOU", "CIN", "PIT", "LAR", "NYJ", "IND", "LV", 
                  "DEN", "NO", "DAL", "ARI", "KC", "DET"),
    home_team = c("BUF", "CAR", "CLE", "JAX", "MIN", "NE", "PHI", "TB", "TEN", "WAS",
                  "LAC", "SEA", "CHI", "SF", "NYG", "BAL"),
    our_spread = c(-4.9, +1.4, -1.0, -3.5, -0.3, -1.8, -2.1, -2.4, -0.0, -3.1,
                   -2.4, -2.1, -0.7, -1.8, +0.7, -0.0),  # From our system
    vegas_spread = c(-6.5, -3.0, -2.5, -3.0, -3.5, -4.0, -3.5, -2.5, -2.0, -1.5,
                     -3.0, -4.5, -3.5, -7.0, -3.0, -3.5),  # Market spreads
    # Estimated moneyline odds (American format)
    away_ml = c(+265, -125, +115, +125, +140, +165, +140, +115, +100, +70,
                +125, +185, +140, +280, +125, +140),
    home_ml = c(-320, +105, -135, -145, -160, -185, -160, -135, -120, -90,
                -145, -220, -160, -340, -145, -160),
    stringsAsFactors = FALSE
  )
  
  cat("Week 3 Win Probability Estimates (Realistic Strengths)\n")
  cat("-------------------------------------------------------\n")
  
  # Display initial probabilities
  for (i in 1:nrow(week3_detailed)) {
    away <- week3_detailed$away_team[i]
    home <- week3_detailed$home_team[i]
    
    # Calculate win probabilities from our spreads
    home_prob <- spread_to_win_prob(-week3_detailed$our_spread[i])  # Negative because home team perspective
    away_prob <- 100 - home_prob
    
    cat(sprintf("%s @ %s: %s win chance = %.1f%% | %s win chance = %.1f%%\n",
                away, home, home, home_prob, away, away_prob))
  }
  
  cat("\n7:20 PM    adjusted strengths\n")
  cat("Week 3 Win Probability Estimates (Realistic Strengths)\n")
  cat("-------------------------------------------------------\n")
  
  # Display with more detailed breakdown and EV analysis
  ev_bets <- data.frame()
  
  for (i in 1:nrow(week3_detailed)) {
    away <- week3_detailed$away_team[i]
    home <- week3_detailed$home_team[i]
    
    # Our probabilities
    home_prob <- spread_to_win_prob(-week3_detailed$our_spread[i]) / 100
    away_prob <- 1 - home_prob
    
    # Market implied probabilities from moneyline
    away_market_prob <- if(week3_detailed$away_ml[i] > 0) {
      100 / (week3_detailed$away_ml[i] + 100)
    } else {
      abs(week3_detailed$away_ml[i]) / (abs(week3_detailed$away_ml[i]) + 100)
    }
    
    home_market_prob <- if(week3_detailed$home_ml[i] > 0) {
      100 / (week3_detailed$home_ml[i] + 100)
    } else {
      abs(week3_detailed$home_ml[i]) / (abs(week3_detailed$home_ml[i]) + 100)
    }
    
    # Calculate EVs
    away_ev <- calculate_ev(away_prob, week3_detailed$away_ml[i])
    home_ev <- calculate_ev(home_prob, week3_detailed$home_ml[i])
    
    # Color coding for display (simulate the red/green from screenshot)
    away_color <- if(away_prob > away_market_prob) "💚" else "🔴"
    home_color <- if(home_prob > home_market_prob) "💚" else "🔴"
    
    cat(sprintf("%s @ %s: %s win chance = %s%.1f%% | %s win chance = %s%.1f%%\n",
                away, home, home, home_color, home_prob * 100, 
                away, away_color, away_prob * 100))
    
    # Store EV data for betting recommendations
    if (away_ev > 0.05) {  # 5% EV threshold
      ev_bets <- rbind(ev_bets, data.frame(
        team = away, opponent = home, location = "away",
        bet_type = "ML", ev = away_ev, odds = week3_detailed$away_ml[i],
        our_prob = away_prob, market_prob = away_market_prob
      ))
    }
    
    if (home_ev > 0.05) {
      ev_bets <- rbind(ev_bets, data.frame(
        team = home, opponent = away, location = "home", 
        bet_type = "ML", ev = home_ev, odds = week3_detailed$home_ml[i],
        our_prob = home_prob, market_prob = home_market_prob
      ))
    }
  }
  
  return(list(
    detailed_games = week3_detailed,
    ev_opportunities = ev_bets
  ))
}

# Generate the output
cat("Generating Week 3 probability analysis...\n")
week3_analysis <- generate_week3_probabilities()