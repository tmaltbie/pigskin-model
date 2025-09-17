# Weighted EPA System: Blend 2024 Season + 2025 Current Data
# Weight recent 2025 games more heavily while using 2024 as baseline

source('epa_prediction_system.R')
library(dplyr)

# Function to create weighted EPA metrics
create_weighted_epa_metrics <- function(weight_2025 = 0.70, weight_2024 = 0.30) {
  
  cat("=== CREATING WEIGHTED EPA METRICS ===\n")
  cat(sprintf("2025 Weight: %.0f%% | 2024 Weight: %.0f%%\n", weight_2025*100, weight_2024*100))
  
  # Load both seasons
  cat("Loading 2024 season data...\n")
  pbp_2024 <- load_pbp(2024)
  epa_2024 <- calculate_all_team_epa_metrics(as.data.frame(pbp_2024))
  
  cat("Loading 2025 current data...\n")
  pbp_2025 <- load_pbp(2025)
  epa_2025 <- calculate_all_team_epa_metrics(as.data.frame(pbp_2025))
  
  cat("Blending EPA metrics...\n")
  
  # Create weighted combination
  weighted_epa <- epa_2024 %>%
    left_join(epa_2025, by = "team", suffix = c("_2024", "_2025")) %>%
    mutate(
      # Weighted offensive EPA
      off_epa_play = (off_epa_play_2024 * weight_2024) + (off_epa_play_2025 * weight_2025),
      
      # Weighted defensive EPA  
      def_epa_play = (def_epa_play_2024 * weight_2024) + (def_epa_play_2025 * weight_2025),
      
      # Weighted passing EPA
      pass_epa_play = (pass_epa_play_2024 * weight_2024) + (pass_epa_play_2025 * weight_2025),
      
      # Weighted rushing EPA
      rush_epa_play = (rush_epa_play_2024 * weight_2024) + (rush_epa_play_2025 * weight_2025),
      
      # Weighted other metrics
      success_rate = (success_rate_2024 * weight_2024) + (success_rate_2025 * weight_2025),
      cpoe = (cpoe_2024 * weight_2024) + (cpoe_2025 * weight_2025),
      
      # Net EPA for ranking
      net_epa = off_epa_play - def_epa_play
    ) %>%
    select(team, off_epa_play, def_epa_play, pass_epa_play, rush_epa_play, 
           success_rate, cpoe, net_epa) %>%
    arrange(desc(net_epa))
  
  cat(sprintf("✅ Weighted EPA metrics created for %d teams\n\n", nrow(weighted_epa)))
  
  # Show comparison of top teams
  cat("🏆 TOP TEAMS BY WEIGHTED EPA:\n")
  print(weighted_epa[1:10, c("team", "off_epa_play", "def_epa_play", "net_epa")])
  
  return(weighted_epa)
}

# Function to test different weighting schemes
test_weighting_schemes <- function() {
  
  cat("\n=== TESTING DIFFERENT WEIGHTING SCHEMES ===\n")
  
  # Week 1 games for testing
  week1_games <- data.frame(
    away_team = c("DAL", "KC", "TB", "CIN", "MIA", "LV", "ARI", "PIT", "NYG", "CAR", "TEN", "SF", "DET", "HOU", "BAL", "MIN"),
    home_team = c("PHI", "LAC", "ATL", "CLE", "IND", "NE", "NO", "NYJ", "WAS", "JAX", "DEN", "SEA", "GB", "LAR", "BUF", "CHI"),
    vegas_spread = c(-6.5, -4.0, -2.5, -4.5, -7.0, -6.5, -2.5, -1.5, -1.0, -4.0, -3.0, -3.5, -2.5, -3.0, -2.5, -1.5),
    actual_margin = c(4, 6, 3, 1, 25, 7, 7, 2, 3, 16, 8, 4, 14, 5, 1, 3),
    stringsAsFactors = FALSE
  )
  
  # Test different weighting schemes
  schemes <- list(
    "Pure 2025 (100%/0%)" = c(1.0, 0.0),
    "Heavy 2025 (80%/20%)" = c(0.8, 0.2),
    "Moderate 2025 (70%/30%)" = c(0.7, 0.3),
    "Balanced (60%/40%)" = c(0.6, 0.4),
    "Conservative (50%/50%)" = c(0.5, 0.5),
    "Pure 2024 (0%/100%)" = c(0.0, 1.0)
  )
  
  results_summary <- data.frame(
    scheme = character(),
    avg_error = numeric(),
    vegas_error = numeric(),
    improvement = numeric(),
    better_games = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:length(schemes)) {
    scheme_name <- names(schemes)[i]
    weights <- schemes[[i]]
    
    cat(sprintf("\n--- Testing %s ---\n", scheme_name))
    
    # Create weighted EPA for this scheme
    weighted_epa <- create_weighted_epa_metrics(weights[1], weights[2])
    
    # Test predictions
    errors <- c()
    for(j in 1:nrow(week1_games)) {
      game <- week1_games[j,]
      
      # Simple prediction using weighted EPA
      home_epa <- weighted_epa$net_epa[weighted_epa$team == game$home_team]
      away_epa <- weighted_epa$net_epa[weighted_epa$team == game$away_team]
      
      if(length(home_epa) > 0 && length(away_epa) > 0) {
        epa_diff <- home_epa - away_epa
        predicted_margin <- epa_diff * 15 + 2.5  # Scale EPA to points + home field
        
        error <- abs(predicted_margin - game$actual_margin)
        errors <- c(errors, error)
      }
    }
    
    avg_error <- mean(errors, na.rm = TRUE)
    vegas_error <- mean(abs((-week1_games$vegas_spread) - week1_games$actual_margin))
    improvement <- vegas_error - avg_error
    better_games <- sum(errors < abs((-week1_games$vegas_spread) - week1_games$actual_margin), na.rm = TRUE)
    
    results_summary[i,] <- list(
      scheme = scheme_name,
      avg_error = avg_error,
      vegas_error = vegas_error,
      improvement = improvement,
      better_games = better_games
    )
    
    cat(sprintf("Average Error: %.2f points\n", avg_error))
    cat(sprintf("vs Vegas (%.2f): %+.2f points\n", vegas_error, improvement))
  }
  
  cat("\n📊 WEIGHTING SCHEME COMPARISON:\n")
  print(results_summary)
  
  # Find best scheme
  best_scheme_idx <- which.max(results_summary$improvement)
  best_scheme <- results_summary[best_scheme_idx,]
  
  cat(sprintf("\n🏆 BEST SCHEME: %s\n", best_scheme$scheme))
  cat(sprintf("Improvement over Vegas: %+.2f points\n", best_scheme$improvement))
  
  return(results_summary)
}

cat("Weighted EPA System loaded! 🎯\n\n")
cat("Functions available:\n")
cat("- create_weighted_epa_metrics(weight_2025, weight_2024): Create blended EPA\n")
cat("- test_weighting_schemes(): Test different 2024/2025 combinations\n\n")
cat("To find optimal weights: test_weighting_schemes()\n")