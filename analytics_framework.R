# Analytics Framework for NFL Predictions
# Statistical validation, performance metrics, and comprehensive analysis
# Integrates with learning system for continuous improvement

library(dplyr)

# Source dependencies
source('learning_system/situational_analysis.R')
source('learning_system/outcome_tracker_csv.R')
source('learning_system/unified_data_access.R')

# ==============================================================================
# ANALYTICS CONFIGURATION
# ==============================================================================

ANALYTICS_CONFIG <- list(
  metrics = c("accuracy", "precision", "recall", "f1", "mse", "mae", "calibration"),
  validation_methods = c("holdout", "cross_validation", "bootstrap"),
  confidence_levels = c(0.90, 0.95, 0.99),
  statistical_tests = c("t_test", "wilcoxon", "chi_square", "ks_test"),
  performance_windows = c(8, 16, 32),  # Games to analyze
  significance_threshold = 0.05
)

# Performance benchmarks
BENCHMARKS <- list(
  spread_accuracy = list(
    excellent = 0.55,
    good = 0.52,
    average = 0.50,
    poor = 0.48
  ),
  total_accuracy = list(
    excellent = 0.55,
    good = 0.52, 
    average = 0.50,
    poor = 0.48
  ),
  confidence_calibration = list(
    excellent = 0.05,  # Low calibration error
    good = 0.10,
    average = 0.15,
    poor = 0.25
  )
)

# ==============================================================================
# COMPREHENSIVE ANALYTICS FUNCTIONS
# ==============================================================================

#' Generate comprehensive analytics report
#' 
#' Creates detailed performance analysis with statistical validation
#' 
#' @param start_date Start date for analysis (optional)
#' @param end_date End date for analysis (optional) 
#' @param model_versions Specific model versions to analyze (optional)
#' @return Comprehensive analytics report
#' @export
generate_analytics_report <- function(start_date = NULL, end_date = NULL, model_versions = NULL) {
  
  cat("📊 Generating Comprehensive Analytics Report...\n")
  cat("=" %R% 60, "\n")
  
  # Load prediction data
  predictions_data <- load_predictions_for_analysis(start_date, end_date, model_versions)
  
  if (nrow(predictions_data) == 0) {
    cat("⚠️ No prediction data available for analysis\n")
    return(NULL)
  }
  
  cat(sprintf("📈 Analyzing %d predictions across %d games\n", 
              nrow(predictions_data), 
              length(unique(predictions_data$game_id))))
  
  # Core performance metrics
  cat("\n📊 Computing Core Performance Metrics...\n")
  core_metrics <- calculate_core_performance_metrics(predictions_data)
  
  # Statistical significance tests
  cat("📈 Running Statistical Significance Tests...\n")
  significance_tests <- run_statistical_significance_tests(predictions_data)
  
  # Confidence calibration analysis
  cat("🎯 Analyzing Confidence Calibration...\n")
  calibration_analysis <- analyze_confidence_calibration(predictions_data)
  
  # Performance trends over time
  cat("📈 Computing Performance Trends...\n") 
  trend_analysis <- analyze_performance_trends(predictions_data)
  
  # Model comparison analysis
  cat("🤖 Comparing Model Performance...\n")
  model_comparison <- compare_model_performance(predictions_data)
  
  # Situational performance analysis
  cat("🎪 Analyzing Situational Performance...\n")
  situational_performance <- analyze_situational_performance(predictions_data)
  
  # Generate final report
  analytics_report <- list(
    metadata = list(
      report_date = Sys.time(),
      data_period = list(start = start_date, end = end_date),
      total_predictions = nrow(predictions_data),
      unique_games = length(unique(predictions_data$game_id)),
      model_versions = unique(predictions_data$model_version)
    ),
    
    core_metrics = core_metrics,
    significance_tests = significance_tests,
    calibration_analysis = calibration_analysis,
    trend_analysis = trend_analysis,
    model_comparison = model_comparison,
    situational_performance = situational_performance,
    
    summary_insights = generate_summary_insights(core_metrics, significance_tests, 
                                                calibration_analysis, trend_analysis),
    
    recommendations = generate_improvement_recommendations(core_metrics, model_comparison, 
                                                         calibration_analysis)
  )
  
  # Display report summary
  display_analytics_summary(analytics_report)
  
  cat("\n✅ Analytics report generation complete!\n")
  cat("=" %R% 60, "\n")
  
  return(analytics_report)
}

#' Calculate core performance metrics
calculate_core_performance_metrics <- function(predictions_data) {
  
  # Overall accuracy metrics
  overall_metrics <- predictions_data %>%
    summarise(
      total_predictions = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      total_accuracy = mean(total_correct, na.rm = TRUE),
      
      # Error metrics
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      spread_rmse = sqrt(mean(spread_error^2, na.rm = TRUE)),
      total_mae = mean(abs(total_error), na.rm = TRUE),
      total_rmse = sqrt(mean(total_error^2, na.rm = TRUE)),
      
      # Confidence metrics
      avg_confidence = mean(confidence, na.rm = TRUE),
      confidence_std = sd(confidence, na.rm = TRUE),
      
      # Calibration error (preliminary)
      calibration_error = abs(mean(confidence, na.rm = TRUE) - mean(spread_correct, na.rm = TRUE)),
      
      .groups = 'drop'
    )
  
  # Performance by confidence buckets
  confidence_buckets <- predictions_data %>%
    mutate(
      confidence_bucket = cut(confidence, 
                             breaks = c(0, 0.6, 0.7, 0.8, 0.9, 1.0),
                             labels = c("Low", "Medium", "High", "Very High", "Extreme"),
                             include.lowest = TRUE)
    ) %>%
    filter(!is.na(confidence_bucket)) %>%
    group_by(confidence_bucket) %>%
    summarise(
      predictions = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      total_accuracy = mean(total_correct, na.rm = TRUE),
      avg_confidence = mean(confidence, na.rm = TRUE),
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Recent performance (last 16 games)
  recent_predictions <- predictions_data %>%
    arrange(desc(prediction_date)) %>%
    head(16)
  
  if (nrow(recent_predictions) > 0) {
    recent_metrics <- recent_predictions %>%
      summarise(
        recent_spread_accuracy = mean(spread_correct, na.rm = TRUE),
        recent_total_accuracy = mean(total_correct, na.rm = TRUE),
        recent_confidence = mean(confidence, na.rm = TRUE),
        recent_spread_mae = mean(abs(spread_error), na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    recent_metrics <- data.frame(
      recent_spread_accuracy = NA,
      recent_total_accuracy = NA,
      recent_confidence = NA,
      recent_spread_mae = NA
    )
  }
  
  return(list(
    overall = overall_metrics,
    by_confidence = confidence_buckets,
    recent = recent_metrics,
    benchmarks = assess_performance_vs_benchmarks(overall_metrics)
  ))
}

#' Run statistical significance tests
run_statistical_significance_tests <- function(predictions_data) {
  
  tests_results <- list()
  
  # Test 1: Is accuracy significantly different from 50%?
  if (nrow(predictions_data) > 30) {
    spread_accuracy <- mean(predictions_data$spread_correct, na.rm = TRUE)
    
    # Binomial test for accuracy vs 50%
    binom_test <- binom.test(
      sum(predictions_data$spread_correct, na.rm = TRUE),
      sum(!is.na(predictions_data$spread_correct)),
      p = 0.5,
      alternative = "two.sided"
    )
    
    tests_results$accuracy_vs_chance <- list(
      test_statistic = binom_test$statistic,
      p_value = binom_test$p.value,
      confidence_interval = binom_test$conf.int,
      significant = binom_test$p.value < ANALYTICS_CONFIG$significance_threshold,
      interpretation = ifelse(binom_test$p.value < 0.05,
                             ifelse(spread_accuracy > 0.5, "Significantly better than chance", "Significantly worse than chance"),
                             "Not significantly different from chance")
    )
  }
  
  # Test 2: Confidence calibration
  if (nrow(predictions_data) > 50) {
    calibration_test <- run_calibration_test(predictions_data)
    tests_results$calibration_test <- calibration_test
  }
  
  # Test 3: Model version comparison (if multiple versions)
  model_versions <- unique(predictions_data$model_version)
  if (length(model_versions) > 1) {
    version_comparison <- compare_model_versions_statistically(predictions_data)
    tests_results$model_version_comparison <- version_comparison
  }
  
  return(tests_results)
}

#' Analyze confidence calibration
analyze_confidence_calibration <- function(predictions_data) {
  
  # Reliability diagram data
  calibration_buckets <- predictions_data %>%
    mutate(
      confidence_bin = round(confidence * 10) / 10  # Round to nearest 0.1
    ) %>%
    group_by(confidence_bin) %>%
    summarise(
      predictions = n(),
      actual_accuracy = mean(spread_correct, na.rm = TRUE),
      avg_stated_confidence = mean(confidence, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(predictions >= 5) %>%  # Minimum sample size per bin
    mutate(
      calibration_error = abs(avg_stated_confidence - actual_accuracy)
    )
  
  # Overall calibration metrics
  overall_calibration <- list(
    mean_calibration_error = mean(calibration_buckets$calibration_error, na.rm = TRUE),
    max_calibration_error = max(calibration_buckets$calibration_error, na.rm = TRUE),
    calibration_slope = calculate_calibration_slope(predictions_data),
    brier_score = calculate_brier_score(predictions_data)
  )
  
  # Calibration assessment
  calibration_quality <- assess_calibration_quality(overall_calibration$mean_calibration_error)
  
  return(list(
    reliability_diagram = calibration_buckets,
    overall_metrics = overall_calibration,
    quality_assessment = calibration_quality
  ))
}

#' Analyze performance trends over time
analyze_performance_trends <- function(predictions_data) {
  
  # Performance by week
  weekly_performance <- predictions_data %>%
    group_by(season, week) %>%
    summarise(
      predictions = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      total_accuracy = mean(total_correct, na.rm = TRUE),
      avg_confidence = mean(confidence, na.rm = TRUE),
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(season, week)
  
  # Rolling performance (8-game windows)
  if (nrow(predictions_data) > 16) {
    rolling_performance <- calculate_rolling_performance(predictions_data, window = 8)
  } else {
    rolling_performance <- NULL
  }
  
  # Trend analysis
  if (nrow(weekly_performance) > 4) {
    trend_analysis <- list(
      accuracy_trend = calculate_trend(weekly_performance$spread_accuracy),
      confidence_trend = calculate_trend(weekly_performance$avg_confidence),
      error_trend = calculate_trend(weekly_performance$spread_mae)
    )
  } else {
    trend_analysis <- NULL
  }
  
  return(list(
    weekly = weekly_performance,
    rolling = rolling_performance,
    trends = trend_analysis
  ))
}

#' Compare performance across different model versions
compare_model_performance <- function(predictions_data) {
  
  model_comparison <- predictions_data %>%
    group_by(model_version) %>%
    summarise(
      predictions = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      total_accuracy = mean(total_correct, na.rm = TRUE),
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      total_mae = mean(abs(total_error), na.rm = TRUE),
      avg_confidence = mean(confidence, na.rm = TRUE),
      calibration_error = abs(mean(confidence, na.rm = TRUE) - mean(spread_correct, na.rm = TRUE)),
      .groups = 'drop'
    ) %>%
    arrange(desc(spread_accuracy))
  
  # Best performing model
  if (nrow(model_comparison) > 0) {
    best_model <- model_comparison[1, ]
    
    # Performance gaps
    if (nrow(model_comparison) > 1) {
      accuracy_gap <- best_model$spread_accuracy - model_comparison$spread_accuracy[2]
      error_gap <- model_comparison$spread_mae[2] - best_model$spread_mae
    } else {
      accuracy_gap <- NA
      error_gap <- NA
    }
  } else {
    best_model <- NULL
    accuracy_gap <- NA
    error_gap <- NA
  }
  
  return(list(
    comparison_table = model_comparison,
    best_model = best_model,
    performance_gaps = list(
      accuracy_gap = accuracy_gap,
      error_gap = error_gap
    )
  ))
}

#' Analyze performance in different situational contexts
analyze_situational_performance <- function(predictions_data) {
  
  situational_analysis <- list()
  
  # Performance by team (home vs away)
  if ("home_team" %in% colnames(predictions_data) && "away_team" %in% colnames(predictions_data)) {
    team_performance <- predictions_data %>%
      group_by(home_team, away_team) %>%
      summarise(
        games = n(),
        spread_accuracy = mean(spread_correct, na.rm = TRUE),
        spread_mae = mean(abs(spread_error), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(games >= 2) %>%
      arrange(desc(spread_accuracy))
    
    situational_analysis$team_matchups = head(team_performance, 10)
  }
  
  # Performance by confidence level
  confidence_performance <- predictions_data %>%
    mutate(
      high_confidence = confidence >= 0.7
    ) %>%
    group_by(high_confidence) %>%
    summarise(
      predictions = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      total_accuracy = mean(total_correct, na.rm = TRUE),
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    )
  
  situational_analysis$confidence_levels = confidence_performance
  
  # Performance by prediction magnitude
  margin_performance <- predictions_data %>%
    mutate(
      large_margin = abs(predicted_margin) >= 7
    ) %>%
    group_by(large_margin) %>%
    summarise(
      predictions = n(),
      spread_accuracy = mean(spread_correct, na.rm = TRUE),
      spread_mae = mean(abs(spread_error), na.rm = TRUE),
      .groups = 'drop'
    )
  
  situational_analysis$prediction_magnitude = margin_performance
  
  return(situational_analysis)
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

#' Load predictions data for analysis
load_predictions_for_analysis <- function(start_date = NULL, end_date = NULL, model_versions = NULL) {
  
  tryCatch({
    # Load from CSV tracking system
    predictions_file <- "learning_system/predictions_tracking.csv"
    
    if (!file.exists(predictions_file)) {
      cat("⚠️ No predictions file found\n")
      return(data.frame())
    }
    
    predictions_data <- read.csv(predictions_file, stringsAsFactors = FALSE)
    
    # Filter by date if specified
    if (!is.null(start_date) || !is.null(end_date)) {
      predictions_data$prediction_date <- as.Date(predictions_data$prediction_date)
      
      if (!is.null(start_date)) {
        predictions_data <- predictions_data %>% filter(prediction_date >= as.Date(start_date))
      }
      
      if (!is.null(end_date)) {
        predictions_data <- predictions_data %>% filter(prediction_date <= as.Date(end_date))
      }
    }
    
    # Filter by model version if specified
    if (!is.null(model_versions)) {
      predictions_data <- predictions_data %>% filter(model_version %in% model_versions)
    }
    
    # Only include processed predictions
    predictions_data <- predictions_data %>% filter(result_processed == TRUE)
    
    return(predictions_data)
    
  }, error = function(e) {
    cat(sprintf("❌ Error loading predictions data: %s\n", e$message))
    return(data.frame())
  })
}

#' Assess performance against benchmarks
assess_performance_vs_benchmarks <- function(metrics) {
  
  spread_rating <- case_when(
    metrics$spread_accuracy >= BENCHMARKS$spread_accuracy$excellent ~ "Excellent",
    metrics$spread_accuracy >= BENCHMARKS$spread_accuracy$good ~ "Good", 
    metrics$spread_accuracy >= BENCHMARKS$spread_accuracy$average ~ "Average",
    TRUE ~ "Poor"
  )
  
  total_rating <- case_when(
    metrics$total_accuracy >= BENCHMARKS$total_accuracy$excellent ~ "Excellent",
    metrics$total_accuracy >= BENCHMARKS$total_accuracy$good ~ "Good",
    metrics$total_accuracy >= BENCHMARKS$total_accuracy$average ~ "Average", 
    TRUE ~ "Poor"
  )
  
  calibration_rating <- case_when(
    metrics$calibration_error <= BENCHMARKS$confidence_calibration$excellent ~ "Excellent",
    metrics$calibration_error <= BENCHMARKS$confidence_calibration$good ~ "Good",
    metrics$calibration_error <= BENCHMARKS$confidence_calibration$average ~ "Average",
    TRUE ~ "Poor"
  )
  
  return(list(
    spread_accuracy_rating = spread_rating,
    total_accuracy_rating = total_rating,
    calibration_rating = calibration_rating,
    overall_rating = determine_overall_rating(spread_rating, total_rating, calibration_rating)
  ))
}

#' Calculate calibration slope
calculate_calibration_slope <- function(predictions_data) {
  
  tryCatch({
    # Fit linear model: actual_accuracy ~ stated_confidence
    model <- lm(spread_correct ~ confidence, data = predictions_data)
    return(coef(model)[2])  # Slope coefficient
  }, error = function(e) {
    return(NA)
  })
}

#' Calculate Brier score
calculate_brier_score <- function(predictions_data) {
  
  tryCatch({
    # Brier score for probability forecasts
    brier <- mean((predictions_data$confidence - predictions_data$spread_correct)^2, na.rm = TRUE)
    return(brier)
  }, error = function(e) {
    return(NA)
  })
}

#' Calculate rolling performance
calculate_rolling_performance <- function(predictions_data, window = 8) {
  
  # Sort by date
  predictions_data <- predictions_data %>%
    arrange(prediction_date)
  
  rolling_results <- data.frame()
  
  for (i in window:nrow(predictions_data)) {
    window_data <- predictions_data[(i - window + 1):i, ]
    
    window_metrics <- data.frame(
      end_date = window_data$prediction_date[nrow(window_data)],
      window_size = window,
      spread_accuracy = mean(window_data$spread_correct, na.rm = TRUE),
      total_accuracy = mean(window_data$total_correct, na.rm = TRUE),
      avg_confidence = mean(window_data$confidence, na.rm = TRUE),
      spread_mae = mean(abs(window_data$spread_error), na.rm = TRUE)
    )
    
    rolling_results <- rbind(rolling_results, window_metrics)
  }
  
  return(rolling_results)
}

#' Display analytics summary
display_analytics_summary <- function(analytics_report) {
  
  cat("\n📋 ANALYTICS SUMMARY\n")
  cat("=" %R% 40, "\n")
  
  overall <- analytics_report$core_metrics$overall
  benchmarks <- analytics_report$core_metrics$benchmarks
  
  cat(sprintf("📊 Total Predictions: %d\n", overall$total_predictions))
  cat(sprintf("🎯 Spread Accuracy: %.1f%% (%s)\n", 
              overall$spread_accuracy * 100, benchmarks$spread_accuracy_rating))
  cat(sprintf("🎯 Total Accuracy: %.1f%% (%s)\n", 
              overall$total_accuracy * 100, benchmarks$total_accuracy_rating))
  cat(sprintf("📈 Spread MAE: %.2f points\n", overall$spread_mae))
  cat(sprintf("🎪 Avg Confidence: %.1f%%\n", overall$avg_confidence * 100))
  cat(sprintf("⚖️ Calibration Error: %.3f (%s)\n", 
              overall$calibration_error, benchmarks$calibration_rating))
  
  # Significance tests
  if (!is.null(analytics_report$significance_tests$accuracy_vs_chance)) {
    acc_test <- analytics_report$significance_tests$accuracy_vs_chance
    significance_icon <- ifelse(acc_test$significant, "✅", "⚠️")
    cat(sprintf("%s Statistical Significance: %s\n", 
                significance_icon, acc_test$interpretation))
  }
  
  # Recent performance
  if (!is.null(analytics_report$core_metrics$recent)) {
    recent <- analytics_report$core_metrics$recent
    if (!is.na(recent$recent_spread_accuracy)) {
      cat(sprintf("📈 Recent Performance: %.1f%% accuracy (last 16 games)\n", 
                  recent$recent_spread_accuracy * 100))
    }
  }
  
  cat("=" %R% 40, "\n")
}

#' Generate summary insights
generate_summary_insights <- function(core_metrics, significance_tests, calibration_analysis, trend_analysis) {
  
  insights <- list()
  
  # Performance insight
  overall <- core_metrics$overall
  
  if (overall$spread_accuracy > 0.52) {
    insights$performance <- "System is performing above average with strong predictive accuracy"
  } else if (overall$spread_accuracy > 0.48) {
    insights$performance <- "System is performing at average levels, room for improvement"
  } else {
    insights$performance <- "System is underperforming, significant improvements needed"
  }
  
  # Calibration insight
  cal_error <- calibration_analysis$overall_metrics$mean_calibration_error
  if (cal_error < 0.05) {
    insights$calibration <- "Excellent confidence calibration - stated confidence matches actual performance"
  } else if (cal_error < 0.15) {
    insights$calibration <- "Good confidence calibration with minor adjustments needed"
  } else {
    insights$calibration <- "Poor confidence calibration - significant recalibration required"
  }
  
  # Trend insight
  if (!is.null(trend_analysis$trends)) {
    if (trend_analysis$trends$accuracy_trend > 0.01) {
      insights$trend <- "Positive trend: Accuracy is improving over time"
    } else if (trend_analysis$trends$accuracy_trend < -0.01) {
      insights$trend <- "Negative trend: Accuracy is declining over time"
    } else {
      insights$trend <- "Stable performance: No significant trend in accuracy"
    }
  }
  
  return(insights)
}

# Helper functions
`%R%` <- function(x, n) paste(rep(x, n), collapse = "")
calculate_trend <- function(values) {
  if (length(values) < 3) return(0)
  model <- lm(values ~ seq_along(values))
  return(coef(model)[2])
}

determine_overall_rating <- function(spread, total, calibration) {
  ratings <- c(spread, total, calibration)
  if (all(ratings %in% c("Excellent", "Good"))) return("Good")
  if (any(ratings == "Poor")) return("Poor")
  return("Average")
}

assess_calibration_quality <- function(cal_error) {
  case_when(
    cal_error <= 0.05 ~ "Excellent",
    cal_error <= 0.10 ~ "Good", 
    cal_error <= 0.20 ~ "Fair",
    TRUE ~ "Poor"
  )
}

run_calibration_test <- function(predictions_data) {
  # Hosmer-Lemeshow-style test for calibration
  tryCatch({
    # Group predictions into deciles
    predictions_data$decile <- cut(predictions_data$confidence, 
                                  breaks = quantile(predictions_data$confidence, 
                                                   probs = seq(0, 1, 0.1), na.rm = TRUE),
                                  include.lowest = TRUE)
    
    # Calculate expected vs observed in each decile
    decile_stats <- predictions_data %>%
      group_by(decile) %>%
      summarise(
        n = n(),
        expected = sum(confidence, na.rm = TRUE),
        observed = sum(spread_correct, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(n > 0)
    
    # Chi-square statistic
    chi_sq <- sum((decile_stats$observed - decile_stats$expected)^2 / decile_stats$expected)
    p_value <- pchisq(chi_sq, df = nrow(decile_stats) - 2, lower.tail = FALSE)
    
    return(list(
      test_statistic = chi_sq,
      p_value = p_value,
      significant = p_value < 0.05,
      interpretation = ifelse(p_value < 0.05, "Poorly calibrated", "Well calibrated")
    ))
  }, error = function(e) {
    return(list(error = e$message))
  })
}

compare_model_versions_statistically <- function(predictions_data) {
  # Compare accuracy between model versions using chi-square test
  tryCatch({
    contingency_table <- table(predictions_data$model_version, predictions_data$spread_correct)
    
    if (all(dim(contingency_table) >= 2)) {
      chi_test <- chisq.test(contingency_table)
      
      return(list(
        test_statistic = chi_test$statistic,
        p_value = chi_test$p.value,
        significant = chi_test$p.value < 0.05,
        interpretation = ifelse(chi_test$p.value < 0.05, 
                               "Significant difference between model versions",
                               "No significant difference between model versions")
      ))
    } else {
      return(list(error = "Insufficient data for comparison"))
    }
  }, error = function(e) {
    return(list(error = e$message))
  })
}

generate_improvement_recommendations <- function(core_metrics, model_comparison, calibration_analysis) {
  
  recommendations <- list()
  
  # Accuracy recommendations
  overall <- core_metrics$overall
  if (overall$spread_accuracy < 0.52) {
    recommendations$accuracy <- "Consider enhancing feature engineering, adding new data sources, or adjusting model parameters"
  }
  
  # Calibration recommendations
  cal_error <- calibration_analysis$overall_metrics$mean_calibration_error
  if (cal_error > 0.10) {
    recommendations$calibration <- "Implement confidence recalibration using Platt scaling or isotonic regression"
  }
  
  # Model recommendations
  if (!is.null(model_comparison$performance_gaps$accuracy_gap) && 
      model_comparison$performance_gaps$accuracy_gap > 0.03) {
    recommendations$model <- sprintf("Focus development on %s model as it shows %.1f%% accuracy advantage",
                                    model_comparison$best_model$model_version,
                                    model_comparison$performance_gaps$accuracy_gap * 100)
  }
  
  return(recommendations)
}

cat("📊 Analytics Framework for NFL Predictions loaded! 📈\n\n")
cat("Available functions:\n")
cat("- generate_analytics_report(): Comprehensive performance analysis\n")
cat("- calculate_core_performance_metrics(): Basic metrics calculation\n")
cat("- analyze_confidence_calibration(): Confidence vs accuracy analysis\n")
cat("- analyze_performance_trends(): Time-based trend analysis\n")
cat("\n🎯 Quick example:\n")
cat("analytics_report <- generate_analytics_report()\n")