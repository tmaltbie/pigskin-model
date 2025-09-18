# Advanced ML Model Validation and Backtesting Framework
# Comprehensive validation system for NFL spread prediction models

library(caret)
library(pROC)
library(MLmetrics)
library(ggplot2)
library(dplyr)

source("ml_models.R")
source("ml_feature_engineering.R")

# ============= TIME-SERIES CROSS VALIDATION =============

# Walk-forward validation respecting temporal order
time_series_cv <- function(training_df, n_folds = 5, min_train_seasons = 10) {
  
  cat("Performing time-series cross validation...\n")
  
  # Sort by season and week
  training_df <- training_df %>% arrange(season, week)
  
  # Determine fold boundaries
  seasons <- unique(training_df$season)
  total_seasons <- length(seasons)
  
  if(total_seasons < min_train_seasons + n_folds) {
    stop("Insufficient data for time-series CV")
  }
  
  cv_results <- list()
  
  for(fold in 1:n_folds) {
    cat(sprintf("\nFold %d/%d\n", fold, n_folds))
    
    # Define train/test split
    test_season_start <- seasons[min_train_seasons + fold]
    test_season_end <- min(test_season_start + 1, max(seasons))
    
    train_data <- training_df %>% 
      filter(season < test_season_start)
    
    test_data <- training_df %>% 
      filter(season >= test_season_start & season <= test_season_end)
    
    cat(sprintf("Train: %d-%d (%d games), Test: %d-%d (%d games)\n",
                min(train_data$season), max(train_data$season), nrow(train_data),
                min(test_data$season), max(test_data$season), nrow(test_data)))
    
    if(nrow(train_data) < 100 || nrow(test_data) < 10) {
      cat("Insufficient data for this fold, skipping...\n")
      next
    }
    
    # Train models on this fold's training data
    fold_models <- train_ensemble_models(train_data)
    
    # Evaluate on test data
    fold_results <- evaluate_models_on_test(fold_models, test_data, training_df)
    fold_results$fold <- fold
    fold_results$train_seasons <- paste(min(train_data$season), max(train_data$season), sep="-")
    fold_results$test_seasons <- paste(min(test_data$season), max(test_data$season), sep="-")
    
    cv_results[[fold]] <- fold_results
  }
  
  return(cv_results)
}

# Function to evaluate models on test set
evaluate_models_on_test <- function(models, test_data, full_training_data) {
  
  cat("Evaluating models on test set...\n")
  
  predictions <- list()
  
  for(i in 1:nrow(test_data)) {
    game <- test_data[i, ]
    
    # Build features for this game
    game_features <- build_game_features(
      home_team = game$home_team,
      away_team = game$away_team,
      season = game$season,
      week = game$week,
      pbp_data = full_training_data  # Use full data for feature building
    )
    
    if(is.null(game_features)) next
    
    # Make predictions
    ensemble_pred <- predict_with_ensemble(models, game_features)
    
    # Store results
    predictions[[i]] <- list(
      game_id = paste(game$season, game$week, game$away_team, game$home_team, sep="_"),
      actual_margin = game$actual_margin,
      vegas_spread = game$vegas_spread,
      actual_cover = game$cover_binary,
      
      # Individual model predictions
      xgb_margin = ensemble_pred$individual$xgb_margin,
      glmnet_margin = ensemble_pred$individual$glmnet_margin,
      rf_cover_prob = ensemble_pred$individual$rf_cover_prob,
      svm_cover_prob = ensemble_pred$individual$svm_cover_prob,
      
      # Ensemble predictions
      ensemble_margin = ensemble_pred$ensemble_margin,
      ensemble_cover_prob = ensemble_pred$ensemble_cover_prob,
      confidence = ensemble_pred$confidence
    )
  }
  
  # Convert to dataframe
  pred_df <- do.call(rbind, lapply(predictions, function(x) {
    data.frame(x, stringsAsFactors = FALSE)
  }))
  
  # Calculate metrics
  metrics <- calculate_prediction_metrics(pred_df)
  
  return(list(
    predictions = pred_df,
    metrics = metrics
  ))
}

# ============= COMPREHENSIVE METRICS CALCULATION =============

calculate_prediction_metrics <- function(pred_df) {
  
  cat("Calculating prediction metrics...\n")
  
  metrics <- list()
  
  # Margin prediction metrics
  if(!all(is.na(pred_df$ensemble_margin))) {
    metrics$margin_mae = mean(abs(pred_df$actual_margin - pred_df$ensemble_margin), na.rm = TRUE)
    metrics$margin_rmse = sqrt(mean((pred_df$actual_margin - pred_df$ensemble_margin)^2, na.rm = TRUE))
    metrics$margin_r2 = cor(pred_df$actual_margin, pred_df$ensemble_margin, use = "complete.obs")^2
  }
  
  # Spread beating metrics (vs Vegas)
  vegas_margin <- -pred_df$vegas_spread  # Convert to home team margin
  if(!all(is.na(pred_df$ensemble_margin))) {
    metrics$beat_vegas_mae = mean(abs((pred_df$actual_margin - vegas_margin) - 
                                    (pred_df$ensemble_margin - vegas_margin)), na.rm = TRUE)
    metrics$beat_vegas_accuracy = mean(sign(pred_df$actual_margin - vegas_margin) == 
                                     sign(pred_df$ensemble_margin - vegas_margin), na.rm = TRUE)
  }
  
  # Cover prediction metrics
  if(!all(is.na(pred_df$ensemble_cover_prob))) {
    actual_cover_numeric <- ifelse(pred_df$actual_cover == "Cover", 1, 0)
    
    # AUC
    if(length(unique(actual_cover_numeric)) > 1) {
      roc_obj <- roc(actual_cover_numeric, pred_df$ensemble_cover_prob, quiet = TRUE)
      metrics$cover_auc = as.numeric(auc(roc_obj))
    }
    
    # Accuracy at 0.5 threshold
    pred_cover <- ifelse(pred_df$ensemble_cover_prob > 0.5, 1, 0)
    metrics$cover_accuracy = mean(actual_cover_numeric == pred_cover, na.rm = TRUE)
    
    # Log loss
    # Clip probabilities to avoid log(0)
    probs_clipped <- pmax(pmin(pred_df$ensemble_cover_prob, 0.999), 0.001)
    metrics$cover_logloss = -mean(actual_cover_numeric * log(probs_clipped) + 
                                (1 - actual_cover_numeric) * log(1 - probs_clipped), na.rm = TRUE)
  }
  
  # Betting simulation metrics
  betting_results <- simulate_betting_strategy(pred_df)
  metrics <- c(metrics, betting_results)
  
  return(metrics)
}

# ============= BETTING STRATEGY SIMULATION =============

simulate_betting_strategy <- function(pred_df, strategies = c("conservative", "aggressive", "kelly")) {
  
  cat("Simulating betting strategies...\n")
  
  results <- list()
  
  for(strategy in strategies) {
    
    # Determine bet sizes and selections based on strategy
    bet_decisions <- apply_betting_strategy(pred_df, strategy)
    
    # Calculate results
    total_bets <- sum(bet_decisions$bet_amount > 0)
    total_wagered <- sum(bet_decisions$bet_amount)
    
    if(total_bets > 0) {
      # Calculate wins/losses (assuming -110 odds)
      wins <- sum(bet_decisions$bet_result == "WIN")
      losses <- sum(bet_decisions$bet_result == "LOSS")
      pushes <- sum(bet_decisions$bet_result == "PUSH")
      
      # Profit calculation
      profit <- (wins * 0.909) - (losses * 1.0)  # Win $0.909 per $1 bet, lose $1
      roi <- (profit / total_wagered) * 100
      win_rate <- wins / (wins + losses) * 100
      
      results[[paste0(strategy, "_total_bets")]] <- total_bets
      results[[paste0(strategy, "_win_rate")]] <- win_rate
      results[[paste0(strategy, "_roi")]] <- roi
      results[[paste0(strategy, "_profit_per_bet")]] <- profit / total_bets
      results[[paste0(strategy, "_units_wagered")]] <- total_wagered
    }
  }
  
  return(results)
}

# Apply different betting strategies
apply_betting_strategy <- function(pred_df, strategy) {
  
  pred_df$bet_amount <- 0
  pred_df$bet_side <- "NONE"
  pred_df$bet_result <- "NO_BET"
  
  for(i in 1:nrow(pred_df)) {
    game <- pred_df[i, ]
    
    if(is.na(game$ensemble_margin) || is.na(game$confidence)) next
    
    # Calculate edge vs Vegas
    vegas_margin <- -game$vegas_spread
    our_edge <- game$ensemble_margin - vegas_margin
    
    # Determine bet based on strategy
    if(strategy == "conservative") {
      # Only bet with high confidence and significant edge
      if(game$confidence > 0.7 && abs(our_edge) > 3) {
        pred_df$bet_amount[i] <- 1.0  # 1 unit
        pred_df$bet_side[i] <- ifelse(our_edge > 0, "HOME", "AWAY")
      }
      
    } else if(strategy == "aggressive") {
      # Bet with lower confidence threshold
      if(game$confidence > 0.5 && abs(our_edge) > 2) {
        pred_df$bet_amount[i] <- ifelse(game$confidence > 0.8, 1.5, 1.0)
        pred_df$bet_side[i] <- ifelse(our_edge > 0, "HOME", "AWAY")
      }
      
    } else if(strategy == "kelly") {
      # Kelly criterion betting
      win_prob <- pmax(pmin(game$ensemble_cover_prob, 0.95), 0.05)  # Clip probabilities
      
      if(our_edge > 0) {
        # Betting on home team to cover
        kelly_fraction <- (win_prob * 1.909 - (1 - win_prob)) / 1.909
      } else {
        # Betting on away team to cover  
        kelly_fraction <- ((1 - win_prob) * 1.909 - win_prob) / 1.909
      }
      
      # Scale down Kelly and apply minimum edge requirement
      if(kelly_fraction > 0.02 && abs(our_edge) > 1.5) {
        pred_df$bet_amount[i] <- min(kelly_fraction * 5, 2.0)  # Scale and cap
        pred_df$bet_side[i] <- ifelse(our_edge > 0, "HOME", "AWAY")
      }
    }
    
    # Determine bet result
    if(pred_df$bet_amount[i] > 0) {
      actual_margin <- game$actual_margin
      spread <- game$vegas_spread
      
      if(pred_df$bet_side[i] == "HOME") {
        # Bet on home team to cover
        covered <- (actual_margin + spread) > 0
      } else {
        # Bet on away team to cover
        covered <- (actual_margin + spread) < 0
      }
      
      if(abs(actual_margin + spread) < 0.5) {
        pred_df$bet_result[i] <- "PUSH"
      } else {
        pred_df$bet_result[i] <- ifelse(covered, "WIN", "LOSS")
      }
    }
  }
  
  return(pred_df)
}

# ============= VALIDATION PIPELINE =============

# Main validation function
run_complete_validation <- function(pbp_data, schedule_data, spreads_data, 
                                  seasons = 1999:2023) {
  
  cat("=== COMPREHENSIVE MODEL VALIDATION ===\n\n")
  
  # Prepare training data
  training_data <- prepare_training_data(pbp_data, schedule_data, spreads_data, seasons)
  training_clean <- prepare_model_features(training_data)
  
  cat(sprintf("Final training dataset: %d games\n\n", nrow(training_clean)))
  
  # Run time-series cross validation
  cv_results <- time_series_cv(training_clean)
  
  # Aggregate results across folds
  validation_summary <- aggregate_cv_results(cv_results)
  
  # Print summary
  print_validation_summary(validation_summary)
  
  # Save results
  validation_results <- list(
    cv_results = cv_results,
    summary = validation_summary,
    training_data = training_clean
  )
  
  saveRDS(validation_results, "nfl_validation_results.rds")
  cat("\nValidation results saved to nfl_validation_results.rds\n")
  
  return(validation_results)
}

# Aggregate cross-validation results
aggregate_cv_results <- function(cv_results) {
  
  if(length(cv_results) == 0) return(NULL)
  
  # Extract metrics from each fold
  all_metrics <- lapply(cv_results, function(fold) fold$metrics)
  
  # Calculate averages
  metric_names <- unique(unlist(lapply(all_metrics, names)))
  
  summary <- list()
  for(metric in metric_names) {
    values <- sapply(all_metrics, function(m) m[[metric]])
    values <- values[!is.na(values)]
    
    if(length(values) > 0) {
      summary[[metric]] <- list(
        mean = mean(values),
        sd = sd(values),
        min = min(values),
        max = max(values)
      )
    }
  }
  
  return(summary)
}

# Print validation summary
print_validation_summary <- function(summary) {
  
  cat("\n=== VALIDATION RESULTS SUMMARY ===\n\n")
  
  # Margin prediction performance
  if("margin_mae" %in% names(summary)) {
    cat("MARGIN PREDICTION:\n")
    cat(sprintf("MAE: %.2f ± %.2f points\n", 
                summary$margin_mae$mean, summary$margin_mae$sd))
    cat(sprintf("RMSE: %.2f ± %.2f points\n", 
                summary$margin_rmse$mean, summary$margin_rmse$sd))
    cat(sprintf("R²: %.3f ± %.3f\n", 
                summary$margin_r2$mean, summary$margin_r2$sd))
  }
  
  # Beat Vegas performance
  if("beat_vegas_mae" %in% names(summary)) {
    cat("\nBEAT VEGAS PERFORMANCE:\n")
    cat(sprintf("MAE vs Vegas: %.2f ± %.2f points\n", 
                summary$beat_vegas_mae$mean, summary$beat_vegas_mae$sd))
    cat(sprintf("Directional Accuracy: %.1f%% ± %.1f%%\n", 
                summary$beat_vegas_accuracy$mean * 100, 
                summary$beat_vegas_accuracy$sd * 100))
  }
  
  # Cover prediction performance
  if("cover_auc" %in% names(summary)) {
    cat("\nCOVER PREDICTION:\n")
    cat(sprintf("AUC: %.3f ± %.3f\n", 
                summary$cover_auc$mean, summary$cover_auc$sd))
    cat(sprintf("Accuracy: %.1f%% ± %.1f%%\n", 
                summary$cover_accuracy$mean * 100, 
                summary$cover_accuracy$sd * 100))
  }
  
  # Betting performance
  if("conservative_roi" %in% names(summary)) {
    cat("\nBETTING STRATEGY PERFORMANCE:\n")
    
    strategies <- c("conservative", "aggressive", "kelly")
    for(strategy in strategies) {
      roi_key <- paste0(strategy, "_roi")
      if(roi_key %in% names(summary)) {
        cat(sprintf("%s: %.1f%% ROI ± %.1f%%\n", 
                    toupper(strategy),
                    summary[[roi_key]]$mean, 
                    summary[[roi_key]]$sd))
      }
    }
  }
}

cat("Advanced Model Validation Framework loaded!\n")
cat("Features:\n")
cat("✅ Time-series cross validation (respects temporal order)\n")
cat("✅ Walk-forward validation methodology\n")
cat("✅ Comprehensive metrics (MAE, RMSE, AUC, ROI)\n")
cat("✅ Multiple betting strategy simulation\n")
cat("✅ Beat Vegas accuracy measurement\n")
cat("✅ Kelly criterion betting optimization\n")
cat("\nTo run: run_complete_validation(pbp_data, schedule_data, spreads_data)\n")