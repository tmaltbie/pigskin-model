# NFL Data Pipeline Migration Scripts
# Handles migration from CSV-based system to database-backed pipeline
# Includes data cleaning, validation, and seamless transition

library(dplyr)
library(lubridate)
library(RSQLite)

# Load pipeline components
source("data_pipeline/error_handling.R")
source("data_pipeline/storage_layer.R")
source("data_pipeline/validation_layer.R")
source("data_pipeline/api_layer.R")

# Migration configuration
MIGRATION_CONFIG <- list(
  backup_directory = "migration_backups",
  csv_files = list(
    predictions = "learning_system/predictions_tracking.csv",
    performance = "learning_system/performance_results.csv"
  ),
  validation_required = TRUE,
  create_backups = TRUE,
  test_migration = TRUE
)

#' Complete Pipeline Migration
#' 
#' Performs complete migration from CSV system to database pipeline
#' This is the main function to execute the transition
#' 
#' @param dry_run Perform dry run without actual changes
#' @param force_reset Reset existing database (destructive)
#' @return Migration report
migrate_to_database_pipeline <- function(dry_run = FALSE, force_reset = FALSE) {
  
  cat("🚀 STARTING NFL DATA PIPELINE MIGRATION\n")
  cat("======================================\n")
  
  migration_report <- list(
    start_time = Sys.time(),
    dry_run = dry_run,
    force_reset = force_reset,
    steps_completed = list(),
    errors = list(),
    warnings = list(),
    data_summary = list()
  )
  
  # Step 1: Create backups
  with_error_handling(
    func = function() {
      if (MIGRATION_CONFIG$create_backups && !dry_run) {
        create_migration_backups()
      }
    },
    operation_name = "Create Migration Backups",
    critical = FALSE
  )
  migration_report$steps_completed <- append(migration_report$steps_completed, "backups_created")
  
  # Step 2: Initialize pipeline components
  init_result <- with_error_handling(
    func = function() {
      # Initialize error handling and data sources
      source_init <- initialize_data_sources()
      if (!source_init$success) {
        stop("Failed to initialize data sources")
      }
      
      # Initialize database
      con <- initialize_database(reset_db = force_reset)
      create_database_schema(con)
      
      return(list(
        sources = source_init,
        database_initialized = TRUE
      ))
    },
    operation_name = "Initialize Pipeline Components",
    critical = TRUE
  )
  
  if (init_result$metadata$status != "success") {
    migration_report$errors <- append(migration_report$errors, 
                                     list(step = "initialization", error = init_result$error))
    return(migration_report)
  }
  migration_report$steps_completed <- append(migration_report$steps_completed, "pipeline_initialized")
  
  # Step 3: Load and validate official schedule
  schedule_result <- with_error_handling(
    func = function() {
      cat("📅 Loading official 2025 NFL schedule...\n")
      
      # Load official schedule for 2025
      schedule_2025 <- load_reliable_schedule(seasons = 2025, force_refresh = TRUE)
      
      if (!dry_run) {
        store_official_schedule(schedule_2025, 2025, validate_data = TRUE)
      }
      
      return(list(
        games_loaded = nrow(schedule_2025),
        weeks_covered = length(unique(schedule_2025$week)),
        completed_games = sum(!is.na(schedule_2025$home_score) & !is.na(schedule_2025$away_score))
      ))
    },
    operation_name = "Load Official Schedule",
    critical = TRUE
  )
  
  if (schedule_result$metadata$status == "success") {
    migration_report$data_summary$official_schedule <- schedule_result$data
    migration_report$steps_completed <- append(migration_report$steps_completed, "official_schedule_loaded")
  } else {
    migration_report$errors <- append(migration_report$errors,
                                     list(step = "official_schedule", error = schedule_result$error))
  }
  
  # Step 4: Migrate and clean CSV prediction data
  prediction_result <- with_error_handling(
    func = function() {
      migrate_prediction_data(dry_run = dry_run)
    },
    operation_name = "Migrate Prediction Data",
    critical = FALSE
  )
  
  if (prediction_result$metadata$status == "success") {
    migration_report$data_summary$predictions <- prediction_result$data
    migration_report$steps_completed <- append(migration_report$steps_completed, "predictions_migrated")
  } else {
    migration_report$warnings <- append(migration_report$warnings,
                                       list(step = "predictions", warning = prediction_result$error))
  }
  
  # Step 5: Validate Week 3 2025 data specifically
  week3_result <- with_validation_error_handling(
    validation_func = function() {
      validate_week3_2025_migration()
    },
    data_description = "Week 3 2025 Schedule Validation",
    auto_clean = TRUE
  )
  
  if (week3_result$metadata$status == "success") {
    migration_report$data_summary$week3_validation <- week3_result$data
    migration_report$steps_completed <- append(migration_report$steps_completed, "week3_validated")
  } else {
    migration_report$errors <- append(migration_report$errors,
                                     list(step = "week3_validation", error = week3_result$error))
  }
  
  # Step 6: Test API functionality
  if (MIGRATION_CONFIG$test_migration) {
    test_result <- with_error_handling(
      func = function() {
        test_migrated_api()
      },
      operation_name = "Test Migrated API",
      critical = FALSE
    )
    
    if (test_result$metadata$status == "success") {
      migration_report$data_summary$api_tests <- test_result$data
      migration_report$steps_completed <- append(migration_report$steps_completed, "api_tested")
    } else {
      migration_report$warnings <- append(migration_report$warnings,
                                         list(step = "api_testing", warning = test_result$error))
    }
  }
  
  # Step 7: Generate migration summary
  migration_report$end_time <- Sys.time()
  migration_report$duration <- as.numeric(migration_report$end_time - migration_report$start_time, units = "secs")
  migration_report$success <- length(migration_report$errors) == 0
  
  # Print migration summary
  print_migration_summary(migration_report)
  
  return(migration_report)
}

#' Migrate Prediction Data from CSV
#' 
#' Migrates and cleans prediction data from CSV files
#' 
#' @param dry_run Perform dry run without actual database changes
#' @return Migration result for prediction data
migrate_prediction_data <- function(dry_run = FALSE) {
  
  cat("🔮 Migrating prediction data from CSV...\n")
  
  csv_path <- MIGRATION_CONFIG$csv_files$predictions
  
  if (!file.exists(csv_path)) {
    return(list(
      migrated_records = 0,
      message = "No CSV file found to migrate"
    ))
  }
  
  # Read CSV data
  csv_data <- read.csv(csv_path, stringsAsFactors = FALSE)
  cat(sprintf("📊 Found %d prediction records in CSV\n", nrow(csv_data)))
  
  migration_result <- list(
    original_records = nrow(csv_data),
    cleaned_records = 0,
    contaminated_records = 0,
    corrected_records = 0,
    migrated_records = 0,
    validation_errors = list()
  )
  
  if (nrow(csv_data) == 0) {
    return(migration_result)
  }
  
  # Identify and clean contaminated Week 3 data
  week3_data <- csv_data %>% filter(week == 3, season == 2025)
  
  if (nrow(week3_data) > 0) {
    cat(sprintf("🧹 Found %d Week 3 2025 predictions to validate and clean...\n", nrow(week3_data)))
    
    # Validate Week 3 data
    validation_result <- validate_game_schedule(week3_data, 2025, strict_validation = TRUE)
    
    if (!validation_result$valid) {
      cat("❌ Week 3 data contamination detected. Attempting to clean...\n")
      
      cleaning_result <- clean_contaminated_data(week3_data, 2025)
      
      # Replace contaminated data with cleaned data
      csv_data <- csv_data %>%
        filter(!(week == 3 & season == 2025)) %>%
        bind_rows(cleaning_result$cleaned_data)
      
      migration_result$contaminated_records <- nrow(week3_data)
      migration_result$corrected_records <- cleaning_result$report$corrected_count
      migration_result$cleaned_records <- nrow(cleaning_result$cleaned_data)
      
      cat(sprintf("✅ Cleaned Week 3 data: %d corrected, %d cleaned records\n",
                 cleaning_result$report$corrected_count,
                 nrow(cleaning_result$cleaned_data)))
    } else {
      cat("✅ Week 3 data validation passed\n")
      migration_result$cleaned_records <- nrow(week3_data)
    }
  }
  
  # Migrate cleaned data to database
  if (!dry_run && nrow(csv_data) > 0) {
    
    # Ensure required columns exist
    required_columns <- c("prediction_id", "game_id", "prediction_date", "model_version",
                         "season", "week", "away_team", "home_team", "predicted_margin",
                         "predicted_total", "home_win_prob", "confidence")
    
    missing_columns <- setdiff(required_columns, names(csv_data))
    
    if (length(missing_columns) > 0) {
      # Add missing columns with defaults
      for (col in missing_columns) {
        if (col == "prediction_id") {
          csv_data[[col]] <- paste("migrated", format(Sys.time(), "%Y%m%d_%H%M%S"),
                                  1:nrow(csv_data), sep = "_")
        } else if (col == "model_version") {
          csv_data[[col]] <- "migrated_v1.0"
        } else if (col == "prediction_date") {
          csv_data[[col]] <- Sys.time()
        } else {
          csv_data[[col]] <- NA
        }
      }
    }
    
    # Store in database
    tryCatch({
      stored_count <- store_predictions(csv_data, validate_against_schedule = TRUE)
      migration_result$migrated_records <- stored_count
      cat(sprintf("💾 Successfully migrated %d predictions to database\n", stored_count))
    }, error = function(e) {
      migration_result$validation_errors <- append(migration_result$validation_errors, e$message)
      cat(sprintf("❌ Migration failed: %s\n", e$message))
    })
  }
  
  return(migration_result)
}

#' Validate Week 3 2025 Migration
#' 
#' Specific validation for Week 3 2025 to ensure contamination is fixed
#' 
#' @return Week 3 validation results
validate_week3_2025_migration <- function() {
  
  cat("🎯 Validating Week 3 2025 migration...\n")
  
  # Get official Week 3 schedule
  official_week3 <- load_official_schedule(2025) %>%
    filter(week == 3) %>%
    arrange(gameday, away_team)
  
  # Check database for Week 3 predictions
  con <- get_db_connection()
  stored_week3 <- dbGetQuery(con, "
    SELECT * FROM predictions 
    WHERE season = 2025 AND week = 3 
    ORDER BY away_team")
  
  validation_result <- list(
    official_games = nrow(official_week3),
    stored_predictions = nrow(stored_week3),
    matchups_validated = 0,
    contamination_detected = FALSE,
    issues = list()
  )
  
  if (nrow(stored_week3) > 0) {
    # Validate each stored prediction against official schedule
    for (i in 1:nrow(stored_week3)) {
      pred <- stored_week3[i, ]
      
      # Find matching official game
      official_match <- official_week3 %>%
        filter(away_team == pred$away_team, home_team == pred$home_team)
      
      if (nrow(official_match) == 0) {
        # Check for swapped teams (sign of contamination)
        swapped_match <- official_week3 %>%
          filter(away_team == pred$home_team, home_team == pred$away_team)
        
        if (nrow(swapped_match) > 0) {
          validation_result$contamination_detected <- TRUE
          validation_result$issues <- append(validation_result$issues,
            sprintf("Swapped teams detected: %s @ %s should be %s @ %s",
                   pred$away_team, pred$home_team,
                   swapped_match$away_team, swapped_match$home_team))
        } else {
          validation_result$issues <- append(validation_result$issues,
            sprintf("Invalid matchup: %s @ %s not found in official schedule",
                   pred$away_team, pred$home_team))
        }
      } else {
        validation_result$matchups_validated <- validation_result$matchups_validated + 1
      }
    }
  }
  
  # Display official Week 3 matchups for reference
  cat("\n🏈 OFFICIAL WEEK 3 2025 MATCHUPS:\n")
  for (i in 1:nrow(official_week3)) {
    game <- official_week3[i, ]
    cat(sprintf("   %s @ %s (%s)\n", 
               game$away_team, game$home_team, 
               format(as.Date(game$gameday), "%a %b %d")))
  }
  
  if (validation_result$contamination_detected) {
    cat("\n❌ CONTAMINATION STILL DETECTED:\n")
    for (issue in validation_result$issues) {
      cat(sprintf("   • %s\n", issue))
    }
  } else {
    cat("\n✅ Week 3 2025 validation passed - no contamination detected\n")
  }
  
  return(validation_result)
}

#' Test Migrated API
#' 
#' Tests the API functionality after migration
#' 
#' @return API test results
test_migrated_api <- function() {
  
  cat("🧪 Testing migrated API functionality...\n")
  
  test_results <- list(
    tests_run = 0,
    tests_passed = 0,
    tests_failed = 0,
    test_details = list()
  )
  
  # Test 1: Get schedule data
  test_results$tests_run <- test_results$tests_run + 1
  tryCatch({
    schedule_response <- nfl_api_get_schedule(season = 2025, week = 3)
    if (schedule_response$status == "success" && nrow(schedule_response$data) > 0) {
      test_results$tests_passed <- test_results$tests_passed + 1
      test_results$test_details$schedule_api <- "PASSED"
      cat("✅ Schedule API test passed\n")
    } else {
      test_results$tests_failed <- test_results$tests_failed + 1
      test_results$test_details$schedule_api <- "FAILED - No data returned"
    }
  }, error = function(e) {
    test_results$tests_failed <- test_results$tests_failed + 1
    test_results$test_details$schedule_api <- paste("FAILED -", e$message)
    cat(sprintf("❌ Schedule API test failed: %s\n", e$message))
  })
  
  # Test 2: Get prediction data
  test_results$tests_run <- test_results$tests_run + 1
  tryCatch({
    prediction_response <- nfl_api_get_prediction_data(season = 2025, week = 3)
    if (prediction_response$status == "success") {
      test_results$tests_passed <- test_results$tests_passed + 1
      test_results$test_details$prediction_api <- "PASSED"
      cat("✅ Prediction data API test passed\n")
    } else {
      test_results$tests_failed <- test_results$tests_failed + 1
      test_results$test_details$prediction_api <- "FAILED - Error response"
    }
  }, error = function(e) {
    test_results$tests_failed <- test_results$tests_failed + 1
    test_results$test_details$prediction_api <- paste("FAILED -", e$message)
    cat(sprintf("❌ Prediction data API test failed: %s\n", e$message))
  })
  
  # Test 3: Health check
  test_results$tests_run <- test_results$tests_run + 1
  tryCatch({
    health_response <- nfl_api_get_health_status()
    if (health_response$status == "success") {
      test_results$tests_passed <- test_results$tests_passed + 1
      test_results$test_details$health_api <- "PASSED"
      cat("✅ Health status API test passed\n")
    } else {
      test_results$tests_failed <- test_results$tests_failed + 1
      test_results$test_details$health_api <- "FAILED - Error response"
    }
  }, error = function(e) {
    test_results$tests_failed <- test_results$tests_failed + 1
    test_results$test_details$health_api <- paste("FAILED -", e$message)
    cat(sprintf("❌ Health status API test failed: %s\n", e$message))
  })
  
  test_results$success_rate <- test_results$tests_passed / test_results$tests_run
  
  cat(sprintf("🧪 API Tests: %d/%d passed (%.1f%%)\n", 
             test_results$tests_passed, test_results$tests_run, 
             test_results$success_rate * 100))
  
  return(test_results)
}

#' Create Migration Backups
#' 
#' Creates backups of existing CSV files and database
#' 
#' @return Backup file paths
create_migration_backups <- function() {
  
  cat("💾 Creating migration backups...\n")
  
  # Ensure backup directory exists
  if (!dir.exists(MIGRATION_CONFIG$backup_directory)) {
    dir.create(MIGRATION_CONFIG$backup_directory, recursive = TRUE)
  }
  
  backup_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_paths <- list()
  
  # Backup CSV files
  for (csv_name in names(MIGRATION_CONFIG$csv_files)) {
    csv_path <- MIGRATION_CONFIG$csv_files[[csv_name]]
    
    if (file.exists(csv_path)) {
      backup_path <- file.path(MIGRATION_CONFIG$backup_directory,
                              paste0(csv_name, "_backup_", backup_timestamp, ".csv"))
      file.copy(csv_path, backup_path)
      backup_paths[[csv_name]] <- backup_path
      cat(sprintf("📁 Backed up %s to %s\n", csv_path, backup_path))
    }
  }
  
  # Backup existing database if it exists
  if (file.exists(DB_CONFIG$db_path)) {
    db_backup_path <- file.path(MIGRATION_CONFIG$backup_directory,
                               paste0("nfl_data_backup_", backup_timestamp, ".sqlite"))
    file.copy(DB_CONFIG$db_path, db_backup_path)
    backup_paths$database <- db_backup_path
    cat(sprintf("🗄️  Backed up database to %s\n", db_backup_path))
  }
  
  return(backup_paths)
}

#' Print Migration Summary
#' 
#' Prints a comprehensive migration summary
#' 
#' @param migration_report Migration report object
print_migration_summary <- function(migration_report) {
  
  cat("\n" %R% rep("=", 60) %R% "\n")
  cat("🚀 NFL DATA PIPELINE MIGRATION SUMMARY\n")
  cat(rep("=", 60), "\n")
  
  # Overall status
  status_emoji <- if (migration_report$success) "✅" else "❌"
  cat(sprintf("%s Migration Status: %s\n", status_emoji, 
             if (migration_report$success) "SUCCESS" else "FAILED"))
  
  cat(sprintf("📅 Duration: %.1f seconds\n", migration_report$duration))
  cat(sprintf("🔧 Dry Run: %s\n", if (migration_report$dry_run) "YES" else "NO"))
  
  # Steps completed
  cat(sprintf("\n📋 Steps Completed (%d):\n", length(migration_report$steps_completed)))
  for (step in migration_report$steps_completed) {
    cat(sprintf("   ✅ %s\n", gsub("_", " ", toupper(step))))
  }
  
  # Data summary
  if (length(migration_report$data_summary) > 0) {
    cat("\n📊 Data Summary:\n")
    
    if (!is.null(migration_report$data_summary$official_schedule)) {
      sched <- migration_report$data_summary$official_schedule
      cat(sprintf("   🏈 Official Schedule: %d games, %d weeks, %d completed\n",
                 sched$games_loaded, sched$weeks_covered, sched$completed_games))
    }
    
    if (!is.null(migration_report$data_summary$predictions)) {
      pred <- migration_report$data_summary$predictions
      cat(sprintf("   🔮 Predictions: %d original → %d migrated (%d contaminated, %d corrected)\n",
                 pred$original_records, pred$migrated_records,
                 pred$contaminated_records, pred$corrected_records))
    }
    
    if (!is.null(migration_report$data_summary$week3_validation)) {
      w3 <- migration_report$data_summary$week3_validation
      cat(sprintf("   🎯 Week 3 Validation: %d official games, %d stored predictions, contamination: %s\n",
                 w3$official_games, w3$stored_predictions,
                 if (w3$contamination_detected) "DETECTED" else "NONE"))
    }
    
    if (!is.null(migration_report$data_summary$api_tests)) {
      api <- migration_report$data_summary$api_tests
      cat(sprintf("   🧪 API Tests: %d/%d passed (%.1f%%)\n",
                 api$tests_passed, api$tests_run, api$success_rate * 100))
    }
  }
  
  # Warnings
  if (length(migration_report$warnings) > 0) {
    cat(sprintf("\n⚠️  Warnings (%d):\n", length(migration_report$warnings)))
    for (warning in migration_report$warnings) {
      cat(sprintf("   • %s: %s\n", warning$step, warning$warning$error$message))
    }
  }
  
  # Errors
  if (length(migration_report$errors) > 0) {
    cat(sprintf("\n❌ Errors (%d):\n", length(migration_report$errors)))
    for (error in migration_report$errors) {
      cat(sprintf("   • %s: %s\n", error$step, error$error$error$message))
    }
  }
  
  cat("\n" %R% rep("=", 60) %R% "\n")
  
  if (migration_report$success) {
    cat("🎉 Migration completed successfully!\n")
    cat("   Next steps:\n")
    cat("   1. Update prediction system to use nfl_api_get_prediction_data()\n")
    cat("   2. Test predictions with validated Week 3 data\n")
    cat("   3. Monitor system health with nfl_api_get_health_status()\n")
  } else {
    cat("💥 Migration encountered errors. Please review and retry.\n")
  }
}

# String concatenation operator for cleaner code
`%R%` <- function(x, y) paste0(x, y)

cat("🔄 Migration Scripts loaded successfully\n")
cat("Use migrate_to_database_pipeline() to start migration\n")