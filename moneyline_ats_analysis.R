# Moneyline and ATS Performance Analysis for Week 1 2025
# Using our best model: Pure 2025 EPA (100%/0%)

source('weighted_epa_system.R')
library(dplyr)

# Week 1 2025 results with our predictions
analyze_moneyline_ats <- function() {
  
  cat("=== WEEK 1 2025 MONEYLINE & ATS ANALYSIS ===\n\n")
  
  # Load our best EPA model (Pure 2025)
  weighted_epa <- create_weighted_epa_metrics(1.0, 0.0)  # 100% 2025, 0% 2024
  
  # Week 1 games with actual results
  week1_games <- data.frame(
    away_team = c("DAL", "KC", "TB", "CIN", "MIA", "LV", "ARI", "PIT", "NYG", "CAR", "TEN", "SF", "DET", "HOU", "BAL", "MIN"),
    home_team = c("PHI", "LAC", "ATL", "CLE", "IND", "NE", "NO", "NYJ", "WAS", "JAX", "DEN", "SEA", "GB", "LAR", "BUF", "CHI"),
    vegas_spread = c(-6.5, -4.0, -2.5, -4.5, -7.0, -6.5, -2.5, -1.5, -1.0, -4.0, -3.0, -3.5, -2.5, -3.0, -2.5, -1.5),
    actual_margin = c(4, 6, 3, 1, 25, 7, 7, 2, 3, 16, 8, 4, 14, 5, 1, 3),
    actual_winner = c("PHI", "LAC", "ATL", "CLE", "IND", "NE", "NO", "NYJ", "WAS", "JAX", "DEN", "SEA", "GB", "LAR", "BUF", "CHI"),
    stringsAsFactors = FALSE
  )
  
  # Calculate our predictions
  results <- data.frame(
    game = character(),
    our_predicted_winner = character(),
    vegas_favorite = character(),
    actual_winner = character(),
    our_spread = numeric(),
    vegas_spread = numeric(),
    actual_margin = numeric(),
    our_moneyline = character(),
    vegas_moneyline = character(),
    our_ats = character(),
    vegas_ats = character(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:nrow(week1_games)) {
    game <- week1_games[i,]
    
    # Get EPA for both teams
    home_epa <- weighted_epa$net_epa[weighted_epa$team == game$home_team]
    away_epa <- weighted_epa$net_epa[weighted_epa$team == game$away_team]
    
    if(length(home_epa) > 0 && length(away_epa) > 0) {
      # Our prediction
      epa_diff <- home_epa - away_epa
      our_predicted_margin <- epa_diff * 15 + 2.5  # Scale + home field advantage
      
      our_predicted_winner <- ifelse(our_predicted_margin > 0, game$home_team, game$away_team)
      vegas_favorite <- ifelse(game$vegas_spread < 0, game$home_team, game$away_team)
      
      # Moneyline results
      our_moneyline_correct <- (our_predicted_winner == game$actual_winner)
      vegas_moneyline_correct <- (vegas_favorite == game$actual_winner)
      
      # ATS results
      # For us: did our predicted winner cover our predicted spread?
      our_ats_correct <- (our_predicted_margin > 0 && game$actual_margin > 0) || 
                        (our_predicted_margin < 0 && game$actual_margin < 0)
      
      # For Vegas: did the favorite cover the spread?
      vegas_ats_correct <- (game$vegas_spread < 0 && game$actual_margin > -game$vegas_spread) ||
                          (game$vegas_spread > 0 && game$actual_margin < -game$vegas_spread)
      
      results[i,] <- list(
        game = sprintf("%s @ %s", game$away_team, game$home_team),
        our_predicted_winner = our_predicted_winner,
        vegas_favorite = vegas_favorite,
        actual_winner = game$actual_winner,
        our_spread = our_predicted_margin,
        vegas_spread = -game$vegas_spread,
        actual_margin = game$actual_margin,
        our_moneyline = ifelse(our_moneyline_correct, "WIN", "LOSS"),
        vegas_moneyline = ifelse(vegas_moneyline_correct, "WIN", "LOSS"),
        our_ats = ifelse(our_ats_correct, "COVER", "NO COVER"),
        vegas_ats = ifelse(vegas_ats_correct, "COVER", "NO COVER")
      )
    }
  }
  
  # Print detailed results
  cat("📋 DETAILED GAME RESULTS:\n")
  cat(sprintf("%-15s %-4s %-4s %-4s %8s %8s %8s %-8s %-8s %-8s %-8s\n",
              "Game", "Our", "Veg", "Act", "OurSprd", "VegSprd", "ActMarg", "OurML", "VegML", "OurATS", "VegATS"))
  cat(paste(rep("-", 100), collapse = ""), "\n")
  
  for(i in 1:nrow(results)) {
    cat(sprintf("%-15s %-4s %-4s %-4s %+8.1f %+8.1f %+8d %-8s %-8s %-8s %-8s\n",
                results$game[i], results$our_predicted_winner[i], results$vegas_favorite[i], 
                results$actual_winner[i], results$our_spread[i], results$vegas_spread[i],
                results$actual_margin[i], results$our_moneyline[i], results$vegas_moneyline[i],
                results$our_ats[i], results$vegas_ats[i]))
  }
  
  # Calculate summary statistics
  our_moneyline_wins <- sum(results$our_moneyline == "WIN", na.rm = TRUE)
  vegas_moneyline_wins <- sum(results$vegas_moneyline == "WIN", na.rm = TRUE)
  our_ats_wins <- sum(results$our_ats == "COVER", na.rm = TRUE)
  vegas_ats_wins <- sum(results$vegas_ats == "COVER", na.rm = TRUE)
  total_games <- nrow(results)
  
  cat(sprintf("\n📊 SUMMARY RESULTS:\n"))
  cat(sprintf("🏆 MONEYLINE PERFORMANCE:\n"))
  cat(sprintf("Our Model: %d/%d wins (%.1f%%)\n", our_moneyline_wins, total_games, (our_moneyline_wins/total_games)*100))
  cat(sprintf("Vegas Favorites: %d/%d wins (%.1f%%)\n", vegas_moneyline_wins, total_games, (vegas_moneyline_wins/total_games)*100))
  
  cat(sprintf("\n💰 AGAINST THE SPREAD (ATS):\n"))
  cat(sprintf("Our Model ATS: %d/%d covers (%.1f%%)\n", our_ats_wins, total_games, (our_ats_wins/total_games)*100))
  cat(sprintf("Vegas ATS: %d/%d covers (%.1f%%)\n", vegas_ats_wins, total_games, (vegas_ats_wins/total_games)*100))
  
  # Betting analysis
  cat(sprintf("\n🎰 BETTING ANALYSIS:\n"))
  if(our_moneyline_wins/total_games > 0.527) {  # Break-even point for -110 odds
    cat(sprintf("✅ Our Moneyline: PROFITABLE (%.1f%% > 52.7%% breakeven)\n", (our_moneyline_wins/total_games)*100))
  } else {
    cat(sprintf("❌ Our Moneyline: NOT PROFITABLE (%.1f%% < 52.7%% breakeven)\n", (our_moneyline_wins/total_games)*100))
  }
  
  if(our_ats_wins/total_games > 0.527) {
    cat(sprintf("✅ Our ATS: PROFITABLE (%.1f%% > 52.7%% breakeven)\n", (our_ats_wins/total_games)*100))
  } else {
    cat(sprintf("❌ Our ATS: NOT PROFITABLE (%.1f%% < 52.7%% breakeven)\n", (our_ats_wins/total_games)*100))
  }
  
  return(results)
}

# Run the analysis
results <- analyze_moneyline_ats()