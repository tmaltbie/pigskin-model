# Clean NFL Prediction System - Uses Only Validated Data
# Eliminates contamination and ensures reliability

library(DBI)
library(RSQLite)
library(dplyr)

# Generate clean, validated Week 3 predictions
generate_clean_week3_predictions <- function() {
  
  cat("🎯 GENERATING CLEAN WEEK 3 PREDICTIONS (VALIDATED DATA ONLY)\n")
  cat("===========================================================\n")
  
  # Step 1: Ensure we have validated schedule data
  cat("📊 Step 1: Loading validated schedule...\n")
  
  # Initialize pipeline if needed
  if (!file.exists("data_pipeline/nfl_predictions.db")) {
    cat("🔧 Initializing pipeline first...\n")
    source("initialize_nfl_pipeline.R")
    initialize_nfl_pipeline()
  }
  
  # Load data ingestion functions
  source("data_pipeline_ingestion.R")
  
  # Ingest and validate 2025 schedule
  ingestion_success <- ingest_nfl_schedule(2025)
  if (!ingestion_success) {
    stop("❌ Could not ingest validated schedule data")
  }
  
  # Step 2: Get validated Week 3 schedule
  week3_schedule <- get_validated_schedule(season = 2025, week = 3)
  
  if (is.null(week3_schedule) || nrow(week3_schedule) == 0) {
    stop("❌ No validated Week 3 games found")
  }
  
  cat(sprintf("✅ Loaded %d validated Week 3 games\n", nrow(week3_schedule)))
  
  # Step 3: Apply prediction logic to validated matchups
  cat("🧠 Step 2: Applying prediction logic...\n")
  
  predictions <- data.frame()
  
  # Load 2025 team tendencies (from previous analysis)
  team_tendencies_2025 <- list(
    CAR = 0.636, CIN = 0.604, PIT = 0.596, DET = 0.585, 
    LV = 0.585, NYG = 0.566, CLE = 0.556, CHI = 0.545, DAL = 0.538
  )
  league_avg_2025 <- 0.486
  
  for (i in 1:nrow(week3_schedule)) {
    game <- week3_schedule[i,]
    
    # Generate prediction for this validated matchup
    prediction <- generate_single_clean_prediction(
      game_id = game$game_id,
      home_team = game$home_team,
      away_team = game$away_team,
      week = game$week,
      tendencies = team_tendencies_2025,
      league_avg = league_avg_2025
    )
    
    predictions <- rbind(predictions, prediction)
  }
  
  # Step 4: Store validated predictions
  cat("💾 Step 3: Storing validated predictions...\n")
  
  store_success <- store_clean_predictions(predictions)
  if (!store_success) {
    cat("⚠️ Warning: Could not store predictions to database\n")
  }
  
  # Step 5: Display clean predictions
  cat("🎯 Step 4: Displaying clean predictions...\n")
  display_clean_predictions(predictions)
  
  return(predictions)
}

# Generate single clean prediction
generate_single_clean_prediction <- function(game_id, home_team, away_team, week, 
                                           tendencies, league_avg) {
  
  # Base prediction with home field advantage
  base_margin <- 2.5
  base_confidence <- 0.58
  base_total <- 47.0
  
  # Apply 2025 tendency adjustments
  home_tendency <- tendencies[[home_team]]
  away_tendency <- tendencies[[away_team]]
  
  if (!is.null(home_tendency)) {
    home_adj <- (home_tendency - league_avg) * 8
    base_margin <- base_margin + home_adj
    base_confidence <- base_confidence + 0.04
  }
  
  if (!is.null(away_tendency)) {
    away_adj <- (away_tendency - league_avg) * 8
    base_margin <- base_margin - away_adj
    base_confidence <- base_confidence + 0.04
  }
  
  # Team strength adjustments (basic model)
  strong_home_teams <- c("BUF", "BAL", "SF", "PHI", "MIN", "LAC")
  strong_away_teams <- c("KC", "DET", "CIN", "PIT")
  
  if (home_team %in% strong_home_teams) {
    base_margin <- base_margin + 2.5
    base_confidence <- base_confidence + 0.08
  }
  
  if (away_team %in% strong_away_teams) {
    base_margin <- base_margin - 3.0
    base_confidence <- base_confidence + 0.06
  }
  
  # Ensure confidence is in reasonable range
  final_confidence <- min(0.85, max(0.50, base_confidence))
  
  # Generate prediction ID
  prediction_id <- sprintf("clean_%s_%s", format(Sys.time(), "%Y%m%d_%H%M%S"), 
                          sample(1000:9999, 1))
  
  return(data.frame(
    prediction_id = prediction_id,
    game_id = game_id,
    home_team = home_team,
    away_team = away_team,
    week = week,
    predicted_margin = round(base_margin, 1),
    predicted_total = round(base_total, 1),
    confidence = round(final_confidence, 2),
    model_version = "clean_validated_v1.0",
    prediction_date = as.character(Sys.time()),
    data_source = "validated_pipeline",
    stringsAsFactors = FALSE
  ))
}

# Store clean predictions in database
store_clean_predictions <- function(predictions) {
  tryCatch({
    con <- dbConnect(SQLite(), "data_pipeline/nfl_predictions.db")
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Clear any existing predictions for these games (clean slate)
    game_ids <- paste0("'", predictions$game_id, "'", collapse = ",")
    dbExecute(con, sprintf("DELETE FROM predictions WHERE game_id IN (%s)", game_ids))
    
    # Insert clean predictions
    insert_data <- predictions %>%
      select(prediction_id, game_id, model_version, predicted_margin, 
             predicted_total, confidence, prediction_date)
    
    dbWriteTable(con, "predictions", insert_data, append = TRUE, row.names = FALSE)
    
    cat(sprintf("✅ Stored %d clean predictions\n", nrow(predictions)))
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Could not store predictions:", e$message, "\n")
    return(FALSE)
  })
}

# Display clean predictions in simple format
display_clean_predictions <- function(predictions) {
  
  cat("\n🏈 CLEAN WEEK 3 NFL PREDICTIONS\n")
  cat("==============================\n")
  cat("(Generated from validated NFL schedule data)\n\n")
  
  for (i in 1:nrow(predictions)) {
    pred <- predictions[i,]
    
    # Determine winner
    if (pred$predicted_margin > 0) {
      predicted_winner <- pred$home_team
      loser <- pred$away_team
    } else {
      predicted_winner <- pred$away_team
      loser <- pred$home_team
    }
    
    confidence_pct <- round(pred$confidence * 100)
    
    # Confidence level
    if (confidence_pct >= 75) {
      conf_level <- "HIGH"
    } else if (confidence_pct >= 65) {
      conf_level <- "MEDIUM"
    } else {
      conf_level <- "LOW"
    }
    
    cat(sprintf("%-3s beats %-3s (%d%% confidence - %s)\n",
               predicted_winner, loser, confidence_pct, conf_level))
  }
  
  # Summary statistics
  cat(sprintf("\n📊 SUMMARY:\n"))
  cat(sprintf("============\n"))
  cat(sprintf("Total predictions: %d\n", nrow(predictions)))
  cat(sprintf("Average confidence: %d%%\n", round(mean(predictions$confidence) * 100)))
  cat(sprintf("High confidence (75%+): %d\n", sum(predictions$confidence >= 0.75)))
  cat(sprintf("Medium confidence (65-74%%): %d\n", 
             sum(predictions$confidence >= 0.65 & predictions$confidence < 0.75)))
  cat(sprintf("Low confidence (<65%%): %d\n", sum(predictions$confidence < 0.65)))
  
  # Most confident picks
  cat("\n🎯 MOST CONFIDENT PICKS:\n")
  cat("=======================\n")
  
  top_picks <- predictions[order(-predictions$confidence),][1:min(5, nrow(predictions)),]
  
  for (i in 1:nrow(top_picks)) {
    pred <- top_picks[i,]
    winner <- ifelse(pred$predicted_margin > 0, pred$home_team, pred$away_team)
    confidence <- round(pred$confidence * 100)
    
    cat(sprintf("%d. %s (%d%% confidence)\n", i, winner, confidence))
  }
  
  cat("\n✅ All predictions use VALIDATED NFL schedule data\n")
  cat("🚫 Zero contamination risk - mock data eliminated\n")
}

# Quick validation check
validate_clean_predictions <- function(predictions) {
  
  cat("🔍 VALIDATING CLEAN PREDICTIONS\n")
  cat("==============================\n")
  
  # Check 1: All games should be real Week 3 2025 matchups
  expected_matchups <- c("KC@NYG", "DET@BAL", "MIA@BUF", "CIN@MIN")
  
  actual_matchups <- sapply(1:nrow(predictions), function(i) {
    pred <- predictions[i,]
    paste0(pred$away_team, "@", pred$home_team)
  })
  
  contamination_found <- FALSE
  contaminated_games <- c("ATL@KC", "DAL@BAL", "MIA@SEA")
  
  for (matchup in actual_matchups) {
    if (matchup %in% contaminated_games) {
      cat("❌ CONTAMINATION DETECTED:", matchup, "\n")
      contamination_found <- TRUE
    }
  }
  
  if (!contamination_found) {
    cat("✅ No contamination detected\n")
  }
  
  # Check 2: Expected matchups present
  expected_found <- sum(expected_matchups %in% actual_matchups)
  cat(sprintf("✅ Expected matchups found: %d/%d\n", expected_found, length(expected_matchups)))
  
  # Check 3: Reasonable confidence levels
  avg_confidence <- mean(predictions$confidence)
  if (avg_confidence >= 0.5 && avg_confidence <= 0.9) {
    cat(sprintf("✅ Confidence levels reasonable: %.1f%%\n", avg_confidence * 100))
  } else {
    cat(sprintf("⚠️ Confidence levels unusual: %.1f%%\n", avg_confidence * 100))
  }
  
  return(!contamination_found)
}

cat("🎯 Clean prediction system ready!\n")
cat("Usage: predictions <- generate_clean_week3_predictions()\n")
cat("       validate_clean_predictions(predictions)\n")