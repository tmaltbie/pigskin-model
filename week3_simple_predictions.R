# Week 3 NFL Predictions - Simple Winners + Confidence
# Using 2025 current season data for enhanced accuracy

library(dplyr)

cat("🏈 WEEK 3 NFL PREDICTIONS - WINNERS + CONFIDENCE\n")
cat("===============================================\n")

# Load existing Week 3 predictions from learning system
if (file.exists("learning_system/predictions_tracking.csv")) {
  predictions <- read.csv("learning_system/predictions_tracking.csv", stringsAsFactors = FALSE)
  week3_games <- predictions[predictions$week == 3 & predictions$result_processed == FALSE,]
  
  cat(sprintf("Week 3 games to predict: %d\n", nrow(week3_games)))
  
  if (nrow(week3_games) > 0) {
    cat("\n🎯 WEEK 3 PREDICTIONS (SIMPLE FORMAT):\n")
    cat("====================================\n")
    
    for (i in 1:nrow(week3_games)) {
      game <- week3_games[i,]
      
      # Determine winner and confidence
      if (game$predicted_margin > 0) {
        predicted_winner <- game$home_team
        margin <- abs(game$predicted_margin)
      } else {
        predicted_winner <- game$away_team  
        margin <- abs(game$predicted_margin)
      }
      
      # Convert confidence to percentage
      confidence_pct <- round(game$confidence * 100)
      
      # Add confidence category
      if (confidence_pct >= 75) {
        confidence_level <- "HIGH"
      } else if (confidence_pct >= 65) {
        confidence_level <- "MEDIUM"  
      } else {
        confidence_level <- "LOW"
      }
      
      # Simple prediction format
      cat(sprintf("%-15s beats %-15s (%d%% confidence - %s)\n",
                 predicted_winner, 
                 ifelse(predicted_winner == game$home_team, game$away_team, game$home_team),
                 confidence_pct,
                 confidence_level))
    }
    
    cat("\n📊 SUMMARY:\n")
    cat("==========\n")
    
    # Calculate summary stats
    avg_confidence <- round(mean(week3_games$confidence) * 100)
    high_confidence <- sum(week3_games$confidence >= 0.75)
    medium_confidence <- sum(week3_games$confidence >= 0.65 & week3_games$confidence < 0.75)
    low_confidence <- sum(week3_games$confidence < 0.65)
    
    cat(sprintf("Average confidence: %d%%\n", avg_confidence))
    cat(sprintf("High confidence games (75+ percent): %d\n", high_confidence))  
    cat(sprintf("Medium confidence games (65-74 percent): %d\n", medium_confidence))
    cat(sprintf("Low confidence games (under 65 percent): %d\n", low_confidence))
    
    # Show most confident picks
    cat("\n🎯 MOST CONFIDENT PICKS:\n")
    cat("======================\n")
    
    top_picks <- week3_games[order(-week3_games$confidence),][1:min(5, nrow(week3_games)),]
    
    for (i in 1:nrow(top_picks)) {
      game <- top_picks[i,]
      winner <- ifelse(game$predicted_margin > 0, game$home_team, game$away_team)
      confidence <- round(game$confidence * 100)
      
      cat(sprintf("%d. %s (%d%% confidence)\n", i, winner, confidence))
    }
    
  } else {
    cat("No Week 3 games found in predictions database\n")
  }
  
} else {
  cat("No predictions file found - generating quick Week 3 predictions\n")
  
  # Quick Week 3 matchups (if no existing predictions)
  week3_matchups <- data.frame(
    away_team = c("CAR", "CHI", "DEN", "HOU", "NE", "PHI", "WAS", "CLE", "MIA", "LAC", "ARI", "JAX", "TEN", "SF", "ATL", "DAL"),
    home_team = c("LV", "IND", "TB", "MIN", "NYJ", "NO", "CIN", "NYG", "SEA", "PIT", "DET", "BUF", "GB", "LAR", "KC", "BAL"),
    stringsAsFactors = FALSE
  )
  
  cat("\n🏈 QUICK WEEK 3 PREDICTIONS:\n")
  cat("===========================\n")
  
  # Apply basic predictions (would be enhanced with EPA data)
  for (i in 1:nrow(week3_matchups)) {
    matchup <- week3_matchups[i,]
    
    # Simple prediction logic (placeholder - would use EPA + tendency data)
    # This is a basic example - real system uses comprehensive analysis
    
    if (matchup$home_team %in% c("KC", "BUF", "BAL", "DET", "SF")) {
      # Strong home teams
      predicted_winner <- matchup$home_team
      confidence <- 72
    } else if (matchup$away_team %in% c("LAC", "PHI", "ARI")) {
      # Strong away teams  
      predicted_winner <- matchup$away_team
      confidence <- 68
    } else {
      # Closer games - home field advantage
      predicted_winner <- matchup$home_team
      confidence <- 58
    }
    
    confidence_level <- ifelse(confidence >= 70, "HIGH", ifelse(confidence >= 60, "MEDIUM", "LOW"))
    
    cat(sprintf("%-15s beats %-15s (%d%% confidence - %s)\n",
               predicted_winner,
               ifelse(predicted_winner == matchup$home_team, matchup$away_team, matchup$home_team), 
               confidence,
               confidence_level))
  }
  
  cat("\n⚠️  Note: These are basic predictions. Run full system for EPA-enhanced accuracy.\n")
}

cat("\n✅ Week 3 predictions complete!\n")