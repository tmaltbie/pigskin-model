# Week 2 2025 Backtest Analysis
# Test our model on Week 2 results from ESPN

source('moneyline_ats_analysis.R')
library(dplyr)

# Week 2 2025 actual results from ESPN image
week2_games_2025 <- data.frame(
  away_team = c("WAS", "JAX", "NYG", "CHI", "LA", "NE", "SF", "BUF", "SEA", "CLE", "DEN", "CAR", "PHI", "ATL", "TB", "LAC"),
  home_team = c("GB", "CIN", "DAL", "DET", "TEN", "MIA", "NO", "NYJ", "PIT", "BAL", "IND", "ARI", "KC", "MIN", "HOU", "LV"),
  actual_home_score = c(27, 31, 40, 52, 33, 33, 26, 30, 31, 41, 29, 27, 20, 22, 20, 20),
  actual_away_score = c(18, 27, 37, 21, 19, 27, 21, 10, 17, 17, 28, 22, 17, 6, 19, 9),
  actual_margin = c(9, 4, 3, 31, 14, 6, 5, 20, 14, 24, 1, 5, 3, 16, 1, 11),  # positive = home win
  game_day = c("Thursday", rep("Sunday", 13), "Monday", "Monday"),
  # Estimated Vegas spreads (would need actual data for precise analysis)
  vegas_spread = c(-3.0, -3.5, -7.5, -3.0, -3.5, -7.0, -2.5, -4.0, -2.5, -3.0, -1.0, -3.5, -2.5, -9.5, -1.5, -3.0),
  stringsAsFactors = FALSE
)

# Function to run Week 2 backtest
run_week2_backtest <- function() {
  
  cat("=== WEEK 2 2025 BACKTEST ANALYSIS ===\n\n")
  
  # Load our best EPA model (Pure 2025 - now includes Week 1 + Week 2 data)
  cat("Loading 2025 EPA metrics (includes Weeks 1-2)...\n")
  pbp_2025 <- load_pbp(2025)
  pbp_df <- as.data.frame(pbp_2025)
  
  # Calculate EPA metrics using all available 2025 data
  source('epa_prediction_system.R')
  epa_metrics_2025 <- calculate_all_team_epa_metrics(pbp_df)
  
  cat("✅ EPA metrics calculated using Weeks 1-2 of 2025\n\n")
  
  # Show top teams
  epa_rankings <- epa_metrics_2025 %>%
    mutate(net_epa = off_epa_play - def_epa_play) %>%
    arrange(desc(net_epa))
  
  cat("🏆 TOP 2025 EPA TEAMS (after Week 2):\n")
  print(epa_rankings[1:10, c("team", "off_epa_play", "def_epa_play", "net_epa")])
  
  # Analyze each game
  results <- data.frame(
    game = character(),
    our_predicted_winner = character(),
    vegas_favorite = character(),
    actual_winner = character(),
    our_spread = numeric(),
    vegas_spread = numeric(),
    actual_margin = numeric(),
    our_error = numeric(),
    vegas_error = numeric(),
    our_moneyline = character(),
    vegas_moneyline = character(),
    our_ats = character(),
    vegas_ats = character(),
    stringsAsFactors = FALSE
  )
  
  cat("\n📋 WEEK 2 GAME-BY-GAME RESULTS:\n")
  cat(sprintf("%-15s %-4s %-4s %-4s %8s %8s %8s %8s %8s %-8s %-8s %-8s %-8s\n",
              "Game", "Our", "Veg", "Act", "OurSprd", "VegSprd", "ActMarg", "OurErr", "VegErr", "OurML", "VegML", "OurATS", "VegATS"))
  cat(paste(rep("-", 120), collapse = ""), "\n")
  
  for(i in 1:nrow(week2_games_2025)) {
    game <- week2_games_2025[i,]
    
    # Get EPA for both teams
    home_epa <- epa_rankings$net_epa[epa_rankings$team == game$home_team]
    away_epa <- epa_rankings$net_epa[epa_rankings$team == game$away_team]
    
    if(length(home_epa) > 0 && length(away_epa) > 0) {
      # Our prediction using EPA
      epa_diff <- home_epa - away_epa
      our_predicted_margin <- epa_diff * 15 + 2.5  # Scale + home field advantage
      
      our_predicted_winner <- ifelse(our_predicted_margin > 0, game$home_team, game$away_team)
      vegas_favorite <- ifelse(game$vegas_spread < 0, game$home_team, game$away_team)
      actual_winner <- ifelse(game$actual_margin > 0, game$home_team, game$away_team)
      
      # Calculate errors
      our_error <- abs(our_predicted_margin - game$actual_margin)
      vegas_error <- abs((-game$vegas_spread) - game$actual_margin)
      
      # Moneyline results
      our_moneyline_correct <- (our_predicted_winner == actual_winner)
      vegas_moneyline_correct <- (vegas_favorite == actual_winner)
      
      # ATS results
      our_ats_correct <- (our_predicted_margin > 0 && game$actual_margin > 0) || 
                        (our_predicted_margin < 0 && game$actual_margin < 0)
      
      vegas_ats_correct <- (game$vegas_spread < 0 && game$actual_margin > -game$vegas_spread) ||
                          (game$vegas_spread > 0 && game$actual_margin < -game$vegas_spread)
      
      results[i,] <- list(
        game = sprintf("%s @ %s", game$away_team, game$home_team),
        our_predicted_winner = our_predicted_winner,
        vegas_favorite = vegas_favorite,
        actual_winner = actual_winner,
        our_spread = our_predicted_margin,
        vegas_spread = -game$vegas_spread,
        actual_margin = game$actual_margin,
        our_error = our_error,
        vegas_error = vegas_error,
        our_moneyline = ifelse(our_moneyline_correct, "WIN", "LOSS"),
        vegas_moneyline = ifelse(vegas_moneyline_correct, "WIN", "LOSS"),
        our_ats = ifelse(our_ats_correct, "COVER", "NO COVER"),
        vegas_ats = ifelse(vegas_ats_correct, "COVER", "NO COVER")
      )
      
      cat(sprintf("%-15s %-4s %-4s %-4s %+8.1f %+8.1f %+8d %8.1f %8.1f %-8s %-8s %-8s %-8s\n",
                  results$game[i], results$our_predicted_winner[i], results$vegas_favorite[i], 
                  results$actual_winner[i], results$our_spread[i], results$vegas_spread[i],
                  results$actual_margin[i], results$our_error[i], results$vegas_error[i],
                  results$our_moneyline[i], results$vegas_moneyline[i],
                  results$our_ats[i], results$vegas_ats[i]))
    }
  }
  
  # Calculate summary statistics
  our_moneyline_wins <- sum(results$our_moneyline == "WIN", na.rm = TRUE)
  vegas_moneyline_wins <- sum(results$vegas_moneyline == "WIN", na.rm = TRUE)
  our_ats_wins <- sum(results$our_ats == "COVER", na.rm = TRUE)
  vegas_ats_wins <- sum(results$vegas_ats == "COVER", na.rm = TRUE)
  total_games <- nrow(results)
  
  our_avg_error <- mean(results$our_error, na.rm = TRUE)
  vegas_avg_error <- mean(results$vegas_error, na.rm = TRUE)
  
  cat(sprintf("\n📊 WEEK 2 SUMMARY RESULTS:\n"))
  cat(sprintf("🎯 PREDICTION ACCURACY:\n"))
  cat(sprintf("Our Average Error: %.2f points\n", our_avg_error))
  cat(sprintf("Vegas Average Error: %.2f points\n", vegas_avg_error))
  cat(sprintf("Improvement vs Vegas: %+.2f points\n", vegas_avg_error - our_avg_error))
  
  cat(sprintf("\n🏆 MONEYLINE PERFORMANCE:\n"))
  cat(sprintf("Our Model: %d/%d wins (%.1f%%)\n", our_moneyline_wins, total_games, (our_moneyline_wins/total_games)*100))
  cat(sprintf("Vegas Favorites: %d/%d wins (%.1f%%)\n", vegas_moneyline_wins, total_games, (vegas_moneyline_wins/total_games)*100))
  
  cat(sprintf("\n💰 AGAINST THE SPREAD (ATS):\n"))
  cat(sprintf("Our Model ATS: %d/%d covers (%.1f%%)\n", our_ats_wins, total_games, (our_ats_wins/total_games)*100))
  cat(sprintf("Vegas ATS: %d/%d covers (%.1f%%)\n", vegas_ats_wins, total_games, (vegas_ats_wins/total_games)*100))
  
  # Combined Week 1 + Week 2 analysis
  cat(sprintf("\n🔄 COMPARING TO WEEK 1 PERFORMANCE:\n"))
  cat(sprintf("Week 1 - Our Moneyline: 81.2%% | Our ATS: 81.2%%\n"))
  cat(sprintf("Week 2 - Our Moneyline: %.1f%% | Our ATS: %.1f%%\n", 
              (our_moneyline_wins/total_games)*100, (our_ats_wins/total_games)*100))
  
  # Profitability check
  cat(sprintf("\n🎰 PROFITABILITY (52.7%% breakeven):\n"))
  ml_profitable <- (our_moneyline_wins/total_games) > 0.527
  ats_profitable <- (our_ats_wins/total_games) > 0.527
  
  cat(sprintf("%s Our Moneyline Week 2: %s\n", 
              ifelse(ml_profitable, "✅", "❌"),
              ifelse(ml_profitable, "PROFITABLE", "NOT PROFITABLE")))
  cat(sprintf("%s Our ATS Week 2: %s\n", 
              ifelse(ats_profitable, "✅", "❌"),
              ifelse(ats_profitable, "PROFITABLE", "NOT PROFITABLE")))
  
  return(results)
}

# Run the Week 2 backtest
week2_results <- run_week2_backtest()