# Advanced ML Models for NFL Spread Prediction
# Implements multiple ML algorithms trained on historical nflfastR data

library(caret)
library(xgboost)
library(randomForest)
library(glmnet)
library(ranger)
library(e1071)
library(caretEnsemble)

source("ml_feature_engineering.R")

# ============= MODEL TRAINING PIPELINE =============

# Function to prepare training data from historical games
prepare_training_data <- function(pbp_data, schedule_data, spreads_data, 
                                 seasons = 1999:2023) {
  
  cat("Preparing training data...\n")
  cat(sprintf("Processing seasons %d-%d\n", min(seasons), max(seasons)))
  
  training_games <- list()
  
  for(season in seasons) {
    cat(sprintf("Processing season %d...\n", season))
    
    # Get completed games for this season
    season_schedule <- schedule_data %>%
      filter(season == !!season, !is.na(result))
    
    if(nrow(season_schedule) == 0) {
      cat(sprintf("No completed games found for season %d\n", season))
      next
    }
    
    for(i in 1:nrow(season_schedule)) {
      game <- season_schedule[i,]
      
      # Skip if no spread data available
      spread_info <- spreads_data %>%
        filter(season == !!season, week == game$week,
               home_team == game$home_team, away_team == game$away_team)
      
      if(nrow(spread_info) == 0) next
      
      # Build features for this game (using data up to this point)
      game_features <- build_game_features(
        home_team = game$home_team,
        away_team = game$away_team,
        season = season,
        week = game$week,
        pbp_data = pbp_data
      )
      
      if(is.null(game_features)) next
      
      # Add target variables
      actual_margin <- game$home_score - game$away_score
      vegas_spread <- spread_info$spread[1]  # Negative means home team favored
      
      game_features$actual_margin <- actual_margin
      game_features$vegas_spread <- vegas_spread
      game_features$spread_cover <- actual_margin > (-vegas_spread)  # Did home team cover?
      game_features$spread_error <- actual_margin - (-vegas_spread)  # How far off was Vegas?
      
      # Our prediction targets
      game_features$margin_vs_vegas <- actual_margin - (-vegas_spread)  # Beat the spread by this much
      game_features$cover_binary <- as.factor(ifelse(game_features$spread_cover, "Cover", "No_Cover"))
      
      training_games[[length(training_games) + 1]] <- game_features
    }
  }
  
  if(length(training_games) == 0) {
    stop("No training games found!")
  }
  
  # Combine all games into training dataset
  training_df <- do.call(rbind, training_games)
  
  cat(sprintf("Training data prepared: %d games\n", nrow(training_df)))
  
  return(training_df)
}

# Function to clean and prepare features for modeling
prepare_model_features <- function(training_df) {
  
  cat("Preparing features for modeling...\n")
  
  # Remove rows with too many missing values
  completeness <- rowSums(!is.na(training_df)) / ncol(training_df)
  training_clean <- training_df[completeness > 0.7, ]
  
  cat(sprintf("Kept %d/%d games after completeness filter\n", 
              nrow(training_clean), nrow(training_df)))
  
  # Handle remaining missing values
  numeric_cols <- sapply(training_clean, is.numeric)
  
  # Impute numeric columns with median
  for(col in names(training_clean)[numeric_cols]) {
    if(any(is.na(training_clean[[col]]))) {
      median_val <- median(training_clean[[col]], na.rm = TRUE)
      training_clean[[col]][is.na(training_clean[[col]])] <- median_val
    }
  }
  
  # Remove highly correlated features
  numeric_features <- training_clean[, numeric_cols]
  numeric_features <- numeric_features[, !names(numeric_features) %in% 
                                      c("actual_margin", "spread_error", "margin_vs_vegas")]
  
  correlation_matrix <- cor(numeric_features, use = "complete.obs")
  high_corr <- findCorrelation(correlation_matrix, cutoff = 0.9)
  
  if(length(high_corr) > 0) {
    features_to_remove <- names(numeric_features)[high_corr]
    cat(sprintf("Removing %d highly correlated features\n", length(features_to_remove)))
    training_clean <- training_clean[, !names(training_clean) %in% features_to_remove]
  }
  
  return(training_clean)
}

# ============= INDIVIDUAL MODEL IMPLEMENTATIONS =============

# XGBoost model for margin prediction
train_xgboost_margin <- function(training_df, target = "margin_vs_vegas") {
  
  cat("Training XGBoost margin model...\n")
  
  # Prepare features
  feature_cols <- setdiff(names(training_df), 
                         c("actual_margin", "vegas_spread", "spread_cover", 
                           "spread_error", "margin_vs_vegas", "cover_binary",
                           "season", "week", "home_team", "away_team"))
  
  X <- training_df[, feature_cols]
  y <- training_df[[target]]
  
  # Remove any remaining non-numeric columns
  numeric_cols <- sapply(X, is.numeric)
  X <- X[, numeric_cols]
  
  # Create train/validation split
  set.seed(42)
  train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
  
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_val <- X[-train_idx, ]
  y_val <- y[-train_idx]
  
  # XGBoost parameters
  params <- list(
    objective = "reg:squarederror",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    gamma = 0
  )
  
  # Convert to DMatrix
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dval <- xgb.DMatrix(data = as.matrix(X_val), label = y_val)
  
  # Train model
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 500,
    early_stopping_rounds = 50,
    watchlist = list(train = dtrain, val = dval),
    verbose = 1
  )
  
  # Get feature importance
  importance <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)
  cat("Top 10 features:\n")
  print(importance[1:min(10, nrow(importance)), ])
  
  return(list(
    model = xgb_model,
    features = colnames(X_train),
    importance = importance,
    validation_rmse = min(xgb_model$evaluation_log$val_rmse)
  ))
}

# Random Forest for spread cover classification
train_rf_cover <- function(training_df) {
  
  cat("Training Random Forest cover model...\n")
  
  # Prepare features
  feature_cols <- setdiff(names(training_df), 
                         c("actual_margin", "vegas_spread", "spread_cover", 
                           "spread_error", "margin_vs_vegas", "cover_binary",
                           "season", "week", "home_team", "away_team"))
  
  X <- training_df[, feature_cols]
  y <- training_df$cover_binary
  
  # Remove any remaining non-numeric columns
  numeric_cols <- sapply(X, is.numeric)
  X <- X[, numeric_cols]
  
  # Combine for caret
  rf_data <- cbind(X, cover = y)
  
  # Train with cross-validation
  set.seed(42)
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  rf_model <- train(
    cover ~ .,
    data = rf_data,
    method = "rf",
    trControl = ctrl,
    metric = "ROC",
    ntree = 500,
    importance = TRUE
  )
  
  cat(sprintf("Random Forest CV ROC: %.3f\n", max(rf_model$results$ROC)))
  
  return(list(
    model = rf_model,
    features = colnames(X),
    cv_roc = max(rf_model$results$ROC)
  ))
}

# Elastic Net model for margin prediction
train_elastic_net <- function(training_df, target = "margin_vs_vegas") {
  
  cat("Training Elastic Net model...\n")
  
  # Prepare features
  feature_cols <- setdiff(names(training_df), 
                         c("actual_margin", "vegas_spread", "spread_cover", 
                           "spread_error", "margin_vs_vegas", "cover_binary",
                           "season", "week", "home_team", "away_team"))
  
  X <- training_df[, feature_cols]
  y <- training_df[[target]]
  
  # Remove any remaining non-numeric columns
  numeric_cols <- sapply(X, is.numeric)
  X <- X[, numeric_cols]
  
  # Standardize features
  X_scaled <- scale(X)
  
  # Combine for caret
  glmnet_data <- data.frame(X_scaled, target = y)
  
  # Train with cross-validation
  set.seed(42)
  ctrl <- trainControl(method = "cv", number = 5)
  
  glmnet_model <- train(
    target ~ .,
    data = glmnet_data,
    method = "glmnet",
    trControl = ctrl,
    tuneLength = 10
  )
  
  cat(sprintf("Elastic Net CV RMSE: %.3f\n", min(glmnet_model$results$RMSE)))
  
  return(list(
    model = glmnet_model,
    features = colnames(X),
    scaling_center = attr(X_scaled, "scaled:center"),
    scaling_scale = attr(X_scaled, "scaled:scale"),
    cv_rmse = min(glmnet_model$results$RMSE)
  ))
}

# Support Vector Machine for classification
train_svm_cover <- function(training_df) {
  
  cat("Training SVM cover model...\n")
  
  # Prepare features (smaller subset for SVM efficiency)
  feature_cols <- setdiff(names(training_df), 
                         c("actual_margin", "vegas_spread", "spread_cover", 
                           "spread_error", "margin_vs_vegas", "cover_binary",
                           "season", "week", "home_team", "away_team"))
  
  X <- training_df[, feature_cols]
  y <- training_df$cover_binary
  
  # Remove any remaining non-numeric columns
  numeric_cols <- sapply(X, is.numeric)
  X <- X[, numeric_cols]
  
  # Feature selection for efficiency
  if(ncol(X) > 20) {
    importance_scores <- apply(X, 2, function(col) abs(cor(col, as.numeric(y == "Cover"), use = "complete.obs")))
    top_features <- names(sort(importance_scores, decreasing = TRUE)[1:20])
    X <- X[, top_features]
  }
  
  # Combine for caret
  svm_data <- cbind(X, cover = y)
  
  # Train with cross-validation (smaller grid for efficiency)
  set.seed(42)
  ctrl <- trainControl(
    method = "cv",
    number = 3,  # Fewer folds for speed
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  svm_model <- train(
    cover ~ .,
    data = svm_data,
    method = "svmRadial",
    trControl = ctrl,
    metric = "ROC",
    tuneLength = 5  # Smaller grid
  )
  
  cat(sprintf("SVM CV ROC: %.3f\n", max(svm_model$results$ROC)))
  
  return(list(
    model = svm_model,
    features = colnames(X),
    cv_roc = max(svm_model$results$ROC)
  ))
}

# ============= MODEL ENSEMBLE =============

# Train ensemble of all models
train_ensemble_models <- function(training_df) {
  
  cat("Training ensemble of ML models...\n")
  
  models <- list()
  
  # Train individual models
  tryCatch({
    models$xgb_margin <- train_xgboost_margin(training_df)
    cat("✅ XGBoost margin model trained\n")
  }, error = function(e) {
    cat("❌ XGBoost failed:", e$message, "\n")
  })
  
  tryCatch({
    models$rf_cover <- train_rf_cover(training_df)
    cat("✅ Random Forest cover model trained\n")
  }, error = function(e) {
    cat("❌ Random Forest failed:", e$message, "\n")
  })
  
  tryCatch({
    models$glmnet_margin <- train_elastic_net(training_df)
    cat("✅ Elastic Net margin model trained\n")
  }, error = function(e) {
    cat("❌ Elastic Net failed:", e$message, "\n")
  })
  
  tryCatch({
    models$svm_cover <- train_svm_cover(training_df)
    cat("✅ SVM cover model trained\n")
  }, error = function(e) {
    cat("❌ SVM failed:", e$message, "\n")
  })
  
  # Save models
  saveRDS(models, "nfl_ml_models.rds")
  cat("Models saved to nfl_ml_models.rds\n")
  
  return(models)
}

# Function to make predictions with ensemble
predict_with_ensemble <- function(models, game_features) {
  
  predictions <- list()
  
  # XGBoost margin prediction
  if("xgb_margin" %in% names(models)) {
    xgb_features <- game_features[, models$xgb_margin$features]
    xgb_features <- xgb_features[, sapply(xgb_features, is.numeric)]
    
    if(ncol(xgb_features) > 0) {
      xgb_pred <- predict(models$xgb_margin$model, as.matrix(xgb_features))
      predictions$xgb_margin <- xgb_pred[1]
    }
  }
  
  # Random Forest cover prediction
  if("rf_cover" %in% names(models)) {
    rf_features <- game_features[, models$rf_cover$features]
    rf_features <- rf_features[, sapply(rf_features, is.numeric)]
    
    if(ncol(rf_features) > 0) {
      rf_pred <- predict(models$rf_cover$model, rf_features, type = "prob")
      predictions$rf_cover_prob <- rf_pred$Cover[1]
    }
  }
  
  # Elastic Net margin prediction
  if("glmnet_margin" %in% names(models)) {
    glm_features <- game_features[, models$glmnet_margin$features]
    glm_features <- glm_features[, sapply(glm_features, is.numeric)]
    
    if(ncol(glm_features) > 0) {
      # Apply same scaling as training
      glm_scaled <- scale(glm_features, 
                         center = models$glmnet_margin$scaling_center,
                         scale = models$glmnet_margin$scaling_scale)
      glm_pred <- predict(models$glmnet_margin$model, data.frame(glm_scaled))
      predictions$glmnet_margin <- glm_pred[1]
    }
  }
  
  # SVM cover prediction
  if("svm_cover" %in% names(models)) {
    svm_features <- game_features[, models$svm_cover$features]
    svm_features <- svm_features[, sapply(svm_features, is.numeric)]
    
    if(ncol(svm_features) > 0) {
      svm_pred <- predict(models$svm_cover$model, svm_features, type = "prob")
      predictions$svm_cover_prob <- svm_pred$Cover[1]
    }
  }
  
  # Ensemble predictions
  margin_preds <- c(predictions$xgb_margin, predictions$glmnet_margin)
  cover_probs <- c(predictions$rf_cover_prob, predictions$svm_cover_prob)
  
  ensemble_margin <- mean(margin_preds, na.rm = TRUE)
  ensemble_cover_prob <- mean(cover_probs, na.rm = TRUE)
  
  return(list(
    individual = predictions,
    ensemble_margin = ensemble_margin,
    ensemble_cover_prob = ensemble_cover_prob,
    confidence = 1 - sd(margin_preds, na.rm = TRUE) / abs(ensemble_margin)
  ))
}

cat("Advanced ML Models loaded!\n")
cat("Models available:\n")
cat("✅ XGBoost for margin prediction\n")
cat("✅ Random Forest for spread cover classification\n")
cat("✅ Elastic Net regularized regression\n")
cat("✅ SVM for classification\n")
cat("✅ Ensemble prediction combining all models\n")
cat("\nFeatures used:\n")
cat("- EPA metrics and advanced nflfastR stats\n")
cat("- CPOE, XYAC_EPA, Win Probability metrics\n")
cat("- Matchup-specific differentials\n")
cat("- Rolling averages and trends\n")
cat("- Situational and environmental factors\n")
cat("\nNext: Implement validation framework\n")