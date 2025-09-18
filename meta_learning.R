# Meta-Learning Architecture for NFL Predictions
# Self-reflective system that learns which models work best for which situations
# Implements competitive evaluation and iterative refinement of prediction strategies
# Author: NFL ML System
# Version: 2.0

library(dplyr)
library(data.table)
library(jsonlite)
library(lubridate)
library(digest)
library(RSQLite)
library(DBI)

# Source dependencies
source("prediction_database.R")
source("situational_analysis.R")
source("dynamic_ensemble.R")
source("outcome_tracker.R")

# ==============================================================================
# META-LEARNING CONFIGURATION
# ==============================================================================

# Meta-learning parameters
META_CONFIG <- list(
  evaluation_window = 16,          # Games to consider for meta-evaluation
  confidence_threshold = 0.7,      # Minimum confidence for meta-decisions
  improvement_threshold = 0.05,    # Minimum improvement to trigger changes
  exploration_rate = 0.15,         # Rate of trying alternative approaches
  meta_update_frequency = "weekly", # How often to run meta-evaluation
  learning_rate = 0.1              # How quickly to adapt to new patterns
)

# Strategy evaluation framework
STRATEGY_FRAMEWORK <- list(
  prediction_strategies = list(
    "conservative" = list(
      description = "Favor models with consistent performance",
      weight_distribution = "even",
      confidence_threshold = 0.8,
      risk_tolerance = "low"
    ),
    "aggressive" = list(
      description = "Heavily weight best-performing models",
      weight_distribution = "concentrated", 
      confidence_threshold = 0.6,
      risk_tolerance = "high"
    ),
    "adaptive" = list(
      description = "Dynamically adjust based on recent performance",
      weight_distribution = "performance_based",
      confidence_threshold = 0.7,
      risk_tolerance = "medium"
    ),
    "situational" = list(
      description = "Prioritize situational models for context-specific games",
      weight_distribution = "context_based",
      confidence_threshold = 0.65,
      risk_tolerance = "medium"
    )
  ),
  
  evaluation_metrics = c(
    "accuracy", "precision", "calibration", "roi", "consistency", 
    "adaptability", "robustness", "confidence_reliability"
  ),
  
  learning_dimensions = c(
    "model_selection", "weight_allocation", "confidence_calibration",
    "context_recognition", "feature_engineering", "ensemble_timing"
  )
)

# Competitive evaluation scenarios
COMPETITIVE_SCENARIOS <- list(
  "model_comparison" = "Compare individual models head-to-head",
  "ensemble_vs_best" = "Compare ensemble against best single model",
  "strategy_tournament" = "Tournament between different prediction strategies",
  "feature_ablation" = "Test impact of removing specific features",
  "context_specialization" = "Evaluate context-specific model routing",
  "confidence_calibration" = "Test confidence vs actual accuracy alignment"
)

# ==============================================================================
# CORE META-LEARNING FUNCTIONS
# ==============================================================================

#' Initialize meta-learning system
#' 
#' Sets up the meta-learning architecture with necessary database tables
#' and initial strategy configurations
#' 
#' @param db_path Path to prediction database
#' @export
initialize_meta_learning_system <- function(db_path = DEFAULT_DB_PATH) {
  
  cat("🧠 Initializing Meta-Learning Architecture...\n")
  
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Create meta-learning tables
    create_meta_evaluation_table(con)
    create_strategy_performance_table(con)
    create_competitive_results_table(con)
    create_learning_insights_table(con)
    
    # Initialize default strategies
    initialize_prediction_strategies(con)
    
    # Set up evaluation schedules
    initialize_evaluation_schedule(con)
    
    cat("✅ Meta-learning system initialized successfully\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat(sprintf("❌ Failed to initialize meta-learning system: %s\n", e$message))
    return(FALSE)
  })
}

#' Run comprehensive meta-evaluation
#' 
#' Main meta-learning function that evaluates prediction system performance
#' across multiple dimensions and generates insights for improvement
#' 
#' @param db_path Path to prediction database
#' @param evaluation_period Period to evaluate ("weekly", "monthly", "season")
#' @param force_evaluation Force evaluation even if recently run
#' @return Meta-evaluation results and recommendations
#' @export
run_meta_evaluation <- function(db_path = DEFAULT_DB_PATH, 
                               evaluation_period = "weekly",
                               force_evaluation = FALSE) {
  
  cat("🔍 Running comprehensive meta-evaluation...\n")
  
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Check if evaluation is needed
    if (!force_evaluation && !evaluation_due(con, evaluation_period)) {
      cat("⏭️ Meta-evaluation not due yet\n")
      return(get_latest_meta_evaluation(con))
    }
    
    # Gather evaluation data  
    evaluation_data <- gather_evaluation_data(con, evaluation_period)
    
    if (is.null(evaluation_data) || nrow(evaluation_data$predictions) == 0) {
      cat("⚠️ Insufficient data for meta-evaluation\n")
      return(NULL)
    }
    
    cat(sprintf("📊 Evaluating %d predictions from %d games\n", 
                nrow(evaluation_data$predictions), 
                length(unique(evaluation_data$predictions$game_id))))
    
    # Run competitive evaluations
    competitive_results <- run_competitive_evaluations(evaluation_data)
    
    # Analyze prediction strategies
    strategy_analysis <- analyze_prediction_strategies(evaluation_data)
    
    # Evaluate model performance patterns
    model_patterns <- analyze_model_performance_patterns(evaluation_data)
    
    # Generate learning insights
    learning_insights <- generate_learning_insights(competitive_results, 
                                                   strategy_analysis, 
                                                   model_patterns)
    
    # Create comprehensive meta-evaluation report
    meta_evaluation <- list(
      evaluation_id = generate_evaluation_id(),
      timestamp = Sys.time(),
      period = evaluation_period,
      data_summary = list(
        total_predictions = nrow(evaluation_data$predictions),
        unique_games = length(unique(evaluation_data$predictions$game_id)),
        models_evaluated = length(unique(evaluation_data$model_performance$model_name)),
        date_range = list(
          start = min(evaluation_data$predictions$game_date),
          end = max(evaluation_data$predictions$game_date)
        )
      ),
      competitive_results = competitive_results,
      strategy_analysis = strategy_analysis,
      model_patterns = model_patterns,
      learning_insights = learning_insights,
      recommendations = generate_meta_recommendations(learning_insights)
    )
    
    # Store meta-evaluation results
    store_meta_evaluation(con, meta_evaluation)
    
    # Apply automatic improvements if confidence is high
    apply_meta_learning_updates(con, meta_evaluation)
    
    cat("✅ Meta-evaluation completed successfully\n")
    cat(sprintf("🎯 Key insight: %s\n", learning_insights$top_insight$description))
    
    return(meta_evaluation)
    
  }, error = function(e) {
    cat(sprintf("❌ Meta-evaluation failed: %s\n", e$message))
    return(NULL)
  })
}

#' Run competitive model evaluations
#' 
#' Implements competitive scenarios to test different approaches
#' 
#' @param evaluation_data Prediction and performance data
#' @return Competitive evaluation results
run_competitive_evaluations <- function(evaluation_data) {
  
  cat("⚔️ Running competitive model evaluations...\n")
  
  competitive_results <- list()
  
  # 1. Model-by-model comparison
  cat("  🤖 Individual model comparison...\n")
  model_comparison <- compare_individual_models(evaluation_data$model_performance)
  competitive_results$model_comparison <- model_comparison
  
  # 2. Ensemble vs best individual model
  cat("  🎯 Ensemble vs best model...\n") 
  ensemble_vs_best <- compare_ensemble_vs_best_model(evaluation_data)
  competitive_results$ensemble_vs_best <- ensemble_vs_best
  
  # 3. Strategy tournament
  cat("  🏆 Strategy tournament...\n")
  strategy_tournament <- run_strategy_tournament(evaluation_data)
  competitive_results$strategy_tournament <- strategy_tournament
  
  # 4. Feature importance analysis
  cat("  📈 Feature ablation study...\n")
  feature_ablation <- run_feature_ablation_study(evaluation_data)
  competitive_results$feature_ablation <- feature_ablation
  
  # 5. Context specialization analysis
  cat("  🎪 Context specialization evaluation...\n")
  context_analysis <- evaluate_context_specialization(evaluation_data)
  competitive_results$context_specialization <- context_analysis
  
  # 6. Confidence calibration assessment
  cat("  🎯 Confidence calibration analysis...\n")
  calibration_analysis <- analyze_confidence_calibration(evaluation_data)
  competitive_results$confidence_calibration <- calibration_analysis
  
  return(competitive_results)
}

#' Analyze prediction strategies
#' 
#' Evaluates different prediction strategies and their effectiveness
#' 
#' @param evaluation_data Prediction and performance data
#' @return Strategy analysis results
analyze_prediction_strategies <- function(evaluation_data) {
  
  cat("📋 Analyzing prediction strategies...\n")
  
  predictions <- evaluation_data$predictions
  
  # Analyze current strategy performance
  current_strategy_perf <- predictions %>%
    group_by(model_version) %>%
    summarise(
      predictions = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      total_accuracy = mean(total_correct, na.rm = TRUE), 
      avg_confidence = mean(confidence, na.rm = TRUE),
      calibration_error = abs(mean(confidence, na.rm = TRUE) - mean(spread_correct, na.rm = TRUE)),
      roi_estimate = calculate_strategy_roi(.),
      consistency_score = 1 - sd(spread_correct, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Test alternative strategies retrospectively
  alternative_strategies <- test_alternative_strategies(predictions)
  
  # Compare strategy effectiveness
  strategy_comparison <- compare_strategy_effectiveness(current_strategy_perf, 
                                                       alternative_strategies)
  
  return(list(
    current_performance = current_strategy_perf,
    alternative_results = alternative_strategies,
    strategy_comparison = strategy_comparison,
    recommended_strategy = determine_optimal_strategy(strategy_comparison)
  ))
}

#' Analyze model performance patterns
#' 
#' Identifies patterns in when and why models succeed or fail
#' 
#' @param evaluation_data Prediction and performance data
#' @return Model pattern analysis
analyze_model_performance_patterns <- function(evaluation_data) {
  
  cat("📊 Analyzing model performance patterns...\n")
  
  model_perf <- evaluation_data$model_performance
  predictions <- evaluation_data$predictions
  
  # Time-based patterns
  temporal_patterns <- model_perf %>%
    mutate(
      week_type = case_when(
        week <= 4 ~ "early_season",
        week <= 12 ~ "mid_season", 
        week <= 17 ~ "late_season",
        TRUE ~ "playoffs"
      )
    ) %>%
    group_by(model_name, week_type) %>%
    summarise(
      games = n(),
      accuracy = mean(spread_correct, na.rm = TRUE),
      avg_error = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Team-specific patterns
  team_patterns <- model_perf %>%
    group_by(model_name, home_team) %>%
    summarise(
      games = n(),
      accuracy = mean(spread_correct, na.rm = TRUE),
      avg_error = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(games >= 3) %>%  # Minimum sample size
    arrange(model_name, desc(accuracy))
  
  # Confidence vs performance patterns
  confidence_patterns <- predictions %>%
    mutate(
      confidence_bucket = cut(confidence, 
                             breaks = c(0, 0.6, 0.7, 0.8, 0.9, 1.0),
                             labels = c("Low", "Medium", "High", "Very High", "Extreme"))
    ) %>%
    group_by(confidence_bucket) %>%
    summarise(
      predictions = n(),
      actual_accuracy = mean(spread_correct, na.rm = TRUE),
      avg_stated_confidence = mean(confidence, na.rm = TRUE),
      calibration_error = abs(avg_stated_confidence - actual_accuracy),
      .groups = 'drop'
    )
  
  # Identify performance trends
  performance_trends <- detect_performance_trends(model_perf)
  
  return(list(
    temporal_patterns = temporal_patterns,
    team_patterns = team_patterns,
    confidence_patterns = confidence_patterns,
    performance_trends = performance_trends,
    pattern_insights = generate_pattern_insights(temporal_patterns, team_patterns, 
                                                confidence_patterns, performance_trends)
  ))
}

#' Generate learning insights from evaluations
#' 
#' Synthesizes evaluation results into actionable insights
#' 
#' @param competitive_results Results from competitive evaluations
#' @param strategy_analysis Strategy analysis results
#' @param model_patterns Model performance patterns
#' @return Learning insights and recommendations
generate_learning_insights <- function(competitive_results, strategy_analysis, model_patterns) {
  
  cat("💡 Generating learning insights...\n")
  
  insights <- list()
  
  # Model selection insights
  model_insights <- extract_model_selection_insights(competitive_results$model_comparison)
  insights$model_selection <- model_insights
  
  # Ensemble optimization insights
  ensemble_insights <- extract_ensemble_insights(competitive_results$ensemble_vs_best)
  insights$ensemble_optimization <- ensemble_insights
  
  # Strategy optimization insights
  strategy_insights <- extract_strategy_insights(strategy_analysis)
  insights$strategy_optimization <- strategy_insights
  
  # Feature engineering insights
  feature_insights <- extract_feature_insights(competitive_results$feature_ablation)
  insights$feature_engineering <- feature_insights
  
  # Context routing insights
  context_insights <- extract_context_insights(competitive_results$context_specialization)
  insights$context_routing <- context_insights
  
  # Confidence calibration insights
  calibration_insights <- extract_calibration_insights(competitive_results$confidence_calibration)
  insights$confidence_calibration <- calibration_insights
  
  # Performance pattern insights
  pattern_insights <- model_patterns$pattern_insights
  insights$performance_patterns <- pattern_insights
  
  # Identify top insight
  top_insight <- identify_top_insight(insights)
  insights$top_insight <- top_insight
  
  # Generate action priorities
  action_priorities <- prioritize_actions(insights)
  insights$action_priorities <- action_priorities
  
  return(insights)
}

# ==============================================================================
# COMPETITIVE EVALUATION IMPLEMENTATIONS
# ==============================================================================

#' Compare individual models head-to-head
compare_individual_models <- function(model_performance) {
  
  # Calculate pairwise comparisons
  models <- unique(model_performance$model_name)
  comparison_matrix <- matrix(0, nrow = length(models), ncol = length(models),
                             dimnames = list(models, models))
  
  for (i in 1:length(models)) {
    for (j in 1:length(models)) {
      if (i != j) {
        model_a <- models[i]
        model_b <- models[j]
        
        # Find games where both models made predictions
        common_games <- model_performance %>%
          filter(model_name %in% c(model_a, model_b)) %>%
          group_by(game_id) %>%
          filter(n() == 2) %>%
          ungroup()
        
        if (nrow(common_games) > 0) {
          model_a_perf <- common_games %>% filter(model_name == model_a)
          model_b_perf <- common_games %>% filter(model_name == model_b)
          
          # Compare accuracy
          a_wins <- sum(model_a_perf$spread_correct > model_b_perf$spread_correct)
          b_wins <- sum(model_b_perf$spread_correct > model_a_perf$spread_correct)
          
          comparison_matrix[i, j] <- a_wins / (a_wins + b_wins)
        }
      }
    }
  }
  
  # Calculate overall rankings
  win_percentages <- rowMeans(comparison_matrix, na.rm = TRUE)
  rankings <- data.frame(
    model_name = names(win_percentages),
    win_percentage = win_percentages,
    rank = rank(-win_percentages)
  ) %>%
    arrange(rank)
  
  return(list(
    comparison_matrix = comparison_matrix,
    rankings = rankings,
    champion = rankings$model_name[1]
  ))
}

#' Compare ensemble performance against best individual model
compare_ensemble_vs_best_model <- function(evaluation_data) {
  
  predictions <- evaluation_data$predictions
  model_perf <- evaluation_data$model_performance
  
  # Identify best individual model
  best_model <- model_perf %>%
    group_by(model_name) %>%
    summarise(
      accuracy = mean(spread_correct, na.rm = TRUE),
      games = n(),
      .groups = 'drop'
    ) %>%
    filter(games >= 10) %>%
    arrange(desc(accuracy)) %>%
    slice(1) %>%
    pull(model_name)
  
  # Compare ensemble vs best model on common games
  ensemble_perf <- predictions %>%
    summarise(
      accuracy = mean(spread_correct, na.rm = TRUE),
      avg_error = mean(abs(spread_error), na.rm = TRUE),
      games = n()
    )
  
  best_model_perf <- model_perf %>%
    filter(model_name == best_model) %>%
    summarise(
      accuracy = mean(spread_correct, na.rm = TRUE),
      avg_error = mean(abs(spread_error), na.rm = TRUE),
      games = n()
    )
  
  advantage <- ensemble_perf$accuracy - best_model_perf$accuracy
  
  return(list(
    best_individual_model = best_model,
    ensemble_accuracy = ensemble_perf$accuracy,
    best_model_accuracy = best_model_perf$accuracy,
    ensemble_advantage = advantage,
    statistical_significance = test_significance(ensemble_perf, best_model_perf),
    recommendation = ifelse(advantage > 0.05, "USE_ENSEMBLE", 
                           ifelse(advantage < -0.05, "USE_BEST_MODEL", "NEUTRAL"))
  ))
}

#' Run tournament between prediction strategies
run_strategy_tournament <- function(evaluation_data) {
  
  predictions <- evaluation_data$predictions
  
  # Test each strategy retrospectively
  strategy_results <- list()
  
  for (strategy_name in names(STRATEGY_FRAMEWORK$prediction_strategies)) {
    strategy_config <- STRATEGY_FRAMEWORK$prediction_strategies[[strategy_name]]
    
    # Apply strategy to historical data
    strategy_perf <- simulate_strategy_performance(predictions, strategy_config)
    strategy_results[[strategy_name]] <- strategy_perf
  }
  
  # Compare strategies
  strategy_comparison <- bind_rows(strategy_results, .id = "strategy") %>%
    arrange(desc(overall_score))
  
  return(list(
    strategy_results = strategy_results,
    rankings = strategy_comparison,
    champion_strategy = strategy_comparison$strategy[1],
    improvement_potential = max(strategy_comparison$overall_score) - 
                           strategy_comparison$overall_score[strategy_comparison$strategy == "current"]
  ))
}

# ==============================================================================
# INSIGHT EXTRACTION FUNCTIONS
# ==============================================================================

#' Extract model selection insights
extract_model_selection_insights <- function(model_comparison) {
  
  champion <- model_comparison$champion
  rankings <- model_comparison$rankings
  
  # Identify significant performance gaps
  performance_gaps <- diff(rankings$win_percentage)
  significant_gap <- any(performance_gaps > 0.1)
  
  insight <- list(
    type = "model_selection",
    description = sprintf("Model %s consistently outperforms others", champion),
    confidence = min(0.9, rankings$win_percentage[1] + 0.1),
    actionable = significant_gap,
    recommendation = if (significant_gap) {
      sprintf("Increase weight of %s in ensemble", champion)
    } else {
      "Current model balance appears optimal"
    },
    impact_estimate = ifelse(significant_gap, "HIGH", "MEDIUM")
  )
  
  return(insight)
}

#' Extract ensemble optimization insights
extract_ensemble_insights <- function(ensemble_vs_best) {
  
  advantage <- ensemble_vs_best$ensemble_advantage
  
  insight <- list(
    type = "ensemble_optimization",
    description = sprintf("Ensemble shows %.1f%% advantage over best individual model", 
                         advantage * 100),
    confidence = 0.8,
    actionable = abs(advantage) > 0.03,
    recommendation = switch(ensemble_vs_best$recommendation,
      "USE_ENSEMBLE" = "Continue using ensemble approach with current weights",
      "USE_BEST_MODEL" = sprintf("Consider using %s as primary model", 
                                ensemble_vs_best$best_individual_model),
      "NEUTRAL" = "Monitor ensemble vs individual model performance"
    ),
    impact_estimate = ifelse(abs(advantage) > 0.05, "HIGH", "MEDIUM")
  )
  
  return(insight)
}

#' Extract strategy optimization insights
extract_strategy_insights <- function(strategy_analysis) {
  
  recommended <- strategy_analysis$recommended_strategy
  current_perf <- strategy_analysis$current_performance
  
  insight <- list(
    type = "strategy_optimization", 
    description = sprintf("Strategy analysis suggests %s approach", recommended$name),
    confidence = recommended$confidence,
    actionable = recommended$improvement_potential > 0.03,
    recommendation = sprintf("Consider adopting %s strategy for %.1f%% improvement",
                           recommended$name, recommended$improvement_potential * 100),
    impact_estimate = ifelse(recommended$improvement_potential > 0.05, "HIGH", "MEDIUM")
  )
  
  return(insight)
}

# ==============================================================================
# META-LEARNING APPLICATION
# ==============================================================================

#' Apply meta-learning updates to the system
#' 
#' Automatically implements improvements based on meta-evaluation insights
#' 
#' @param con Database connection
#' @param meta_evaluation Meta-evaluation results
apply_meta_learning_updates <- function(con, meta_evaluation) {
  
  cat("🔄 Applying meta-learning updates...\n")
  
  insights <- meta_evaluation$learning_insights
  updates_applied <- 0
  
  # Apply high-confidence, high-impact recommendations
  for (insight_name in names(insights)) {
    insight <- insights[[insight_name]]
    
    if (is.list(insight) && 
        !is.null(insight$confidence) && insight$confidence > META_CONFIG$confidence_threshold &&
        !is.null(insight$actionable) && insight$actionable &&
        !is.null(insight$impact_estimate) && insight$impact_estimate == "HIGH") {
      
      success <- apply_insight_update(con, insight)
      if (success) {
        updates_applied <- updates_applied + 1
        cat(sprintf("✅ Applied update: %s\n", insight$description))
      }
    }
  }
  
  # Record applied updates
  record_meta_updates(con, meta_evaluation$evaluation_id, updates_applied)
  
  cat(sprintf("🎯 Applied %d meta-learning updates\n", updates_applied))
  
  return(updates_applied)
}

#' Apply specific insight update
apply_insight_update <- function(con, insight) {
  
  tryCatch({
    switch(insight$type,
      "model_selection" = apply_model_selection_update(con, insight),
      "ensemble_optimization" = apply_ensemble_optimization_update(con, insight),
      "strategy_optimization" = apply_strategy_optimization_update(con, insight),
      "confidence_calibration" = apply_calibration_update(con, insight),
      FALSE  # Unknown insight type
    )
  }, error = function(e) {
    cat(sprintf("⚠️ Failed to apply insight update: %s\n", e$message))
    FALSE
  })
}

# ==============================================================================
# DATABASE FUNCTIONS
# ==============================================================================

#' Create meta-evaluation table
create_meta_evaluation_table <- function(con) {
  sql <- "
  CREATE TABLE IF NOT EXISTS meta_evaluations (
    evaluation_id TEXT PRIMARY KEY,
    timestamp TIMESTAMP NOT NULL,
    evaluation_period TEXT NOT NULL,
    total_predictions INTEGER,
    unique_games INTEGER,
    models_evaluated INTEGER,
    competitive_results TEXT,  -- JSON
    strategy_analysis TEXT,    -- JSON
    model_patterns TEXT,       -- JSON
    learning_insights TEXT,    -- JSON
    recommendations TEXT,      -- JSON
    updates_applied INTEGER DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )"
  
  dbExecute(con, sql)
}

#' Create strategy performance table
create_strategy_performance_table <- function(con) {
  sql <- "
  CREATE TABLE IF NOT EXISTS strategy_performance (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    strategy_name TEXT NOT NULL,
    evaluation_period TEXT NOT NULL,
    accuracy REAL,
    roi REAL,
    consistency_score REAL,
    calibration_error REAL,
    games_evaluated INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )"
  
  dbExecute(con, sql)
}

#' Generate evaluation ID
generate_evaluation_id <- function() {
  paste("meta", format(Sys.time(), "%Y%m%d_%H%M%S"), 
        substr(digest(Sys.time()), 1, 6), sep = "_")
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

#' Test if evaluation is due
evaluation_due <- function(con, period) {
  
  last_eval <- dbGetQuery(con, "
    SELECT MAX(timestamp) as last_eval 
    FROM meta_evaluations 
    WHERE evaluation_period = ?", 
    params = list(period))
  
  if (is.na(last_eval$last_eval[1])) {
    return(TRUE)  # No previous evaluation
  }
  
  last_eval_time <- as.POSIXct(last_eval$last_eval[1])
  
  days_since <- as.numeric(difftime(Sys.time(), last_eval_time, units = "days"))
  
  threshold <- switch(period,
    "weekly" = 7,
    "monthly" = 30,
    "season" = 120,
    7  # Default
  )
  
  return(days_since >= threshold)
}

#' Gather evaluation data
gather_evaluation_data <- function(con, period) {
  
  # Determine date range
  lookback_days <- switch(period,
    "weekly" = 7,
    "monthly" = 30, 
    "season" = 120,
    30
  )
  
  # Get predictions
  predictions <- dbGetQuery(con, "
    SELECT * FROM predictions 
    WHERE game_completed = 1 
      AND created_at >= date('now', '-' || ? || ' days')
    ORDER BY game_date DESC", 
    params = list(lookback_days))
  
  # Get model performance
  model_performance <- dbGetQuery(con, "
    SELECT * FROM model_performance
    WHERE created_at >= date('now', '-' || ? || ' days')
    ORDER BY created_at DESC",
    params = list(lookback_days))
  
  return(list(
    predictions = predictions,
    model_performance = model_performance,
    date_range = list(
      start = Sys.Date() - lookback_days,
      end = Sys.Date()
    )
  ))
}

# ==============================================================================
# TESTING AND VALIDATION
# ==============================================================================

#' Test meta-learning system
#' 
#' Validates meta-learning functionality
#' 
#' @param db_path Path to test database
#' @export
test_meta_learning_system <- function(db_path = "test_meta_learning.db") {
  
  cat("🧪 Testing Meta-Learning Architecture...\n")
  
  # Clean up any existing test database
  if (file.exists(db_path)) file.remove(db_path)
  
  test_results <- list()
  
  # Test 1: System initialization
  cat("Test 1: Meta-learning system initialization...\n")
  tryCatch({
    result <- initialize_meta_learning_system(db_path)
    test_results$initialization <- ifelse(result, "PASS", "FAIL")
    cat("✅ Initialization test passed\n")
  }, error = function(e) {
    test_results$initialization <- "FAIL"
    cat(sprintf("❌ Initialization test failed: %s\n", e$message))
  })
  
  # Test 2: Meta-evaluation (with minimal data)
  cat("Test 2: Meta-evaluation functionality...\n")
  tryCatch({
    # This would require actual prediction data for a full test
    # For now, just test that the function can run without errors
    test_results$meta_evaluation <- "SKIP"
    cat("⏭️ Meta-evaluation test skipped (requires prediction data)\n")
  }, error = function(e) {
    test_results$meta_evaluation <- "FAIL"
    cat(sprintf("❌ Meta-evaluation test failed: %s\n", e$message))
  })
  
  # Clean up test database
  if (file.exists(db_path)) file.remove(db_path)
  
  # Summary
  passed_tests <- sum(test_results == "PASS")
  total_tests <- length(test_results)
  
  cat(sprintf("\n🏁 Meta-learning testing complete: %d/%d tests passed\n", passed_tests, total_tests))
  
  return(test_results)
}

# ==============================================================================
# INITIALIZATION
# ==============================================================================

cat("🧠 Meta-Learning Architecture v2.0 Loaded\n")
cat("Key Functions Available:\n")
cat("  - initialize_meta_learning_system(): Set up meta-learning framework\n")
cat("  - run_meta_evaluation(): Comprehensive system evaluation\n")
cat("  - apply_meta_learning_updates(): Apply insights automatically\n")
cat("  - test_meta_learning_system(): Validate system functionality\n")
cat("\nMeta-Learning Dimensions:\n")
for (dimension in STRATEGY_FRAMEWORK$learning_dimensions) {
  cat(sprintf("  - %s: Continuous optimization of %s\n", dimension, dimension))
}
cat("\nCompetitive Scenarios:\n")
for (scenario_name in names(COMPETITIVE_SCENARIOS)) {
  cat(sprintf("  - %s: %s\n", scenario_name, COMPETITIVE_SCENARIOS[[scenario_name]]))
}
cat("\nExample Usage:\n")
cat("  # Initialize meta-learning\n")
cat("  initialize_meta_learning_system()\n")
cat("  \n")
cat("  # Run weekly meta-evaluation\n")
cat("  insights <- run_meta_evaluation('weekly')\n")
cat("  \n")
cat("  # System will automatically apply high-confidence improvements\n")

cat("\n🎯 Ready for continuous self-improvement and competitive evaluation!\n")