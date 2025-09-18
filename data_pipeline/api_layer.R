# NFL Data API Layer - Clean Interface for Prediction System
# Provides a clean, validated data access interface that replaces
# direct CSV access with database-backed, validated data operations

library(dplyr)
library(lubridate)
library(jsonlite)

# Load required layers
source("data_pipeline/storage_layer.R")
source("data_pipeline/validation_layer.R")
source("data_pipeline/data_source_layer.R")

# API configuration
API_CONFIG <- list(
  cache_duration = 300,        # 5 minutes cache for API responses
  max_results = 1000,          # Maximum results per query
  default_season = 2025,       # Default season for queries
  enable_caching = TRUE,       # Enable response caching
  validate_requests = TRUE     # Validate all requests
)

# API response cache
.api_env <- new.env()
.api_env$cache <- list()
.api_env$request_log <- list()

#' NFL Data API - Get Current Season Schedule
#' 
#' Returns the current season's complete schedule with validation
#' 
#' @param season Season to retrieve (default: current season)
#' @param week Specific week to filter (optional)
#' @param include_results Include actual game results if available
#' @param force_refresh Force refresh from source
#' @return List with schedule data and metadata
nfl_api_get_schedule <- function(season = API_CONFIG$default_season, 
                                week = NULL, 
                                include_results = TRUE,
                                force_refresh = FALSE) {
  
  # Generate cache key
  cache_key <- paste("schedule", season, paste(week, collapse="-"), include_results, sep="_")
  
  # Check cache
  if (!force_refresh && API_CONFIG$enable_caching && cache_key %in% names(.api_env$cache)) {
    cache_entry <- .api_env$cache[[cache_key]]
    if (as.numeric(Sys.time()) - cache_entry$timestamp < API_CONFIG$cache_duration) {
      cache_entry$data$metadata$cached <- TRUE
      return(cache_entry$data)
    }
  }
  
  cat(sprintf("🏈 API: Getting schedule for season %d%s\n", 
             season, if (!is.null(week)) sprintf(", week %s", paste(week, collapse=",")) else ""))
  
  tryCatch({
    
    # Ensure database is initialized and populated
    ensure_schedule_available(season, force_refresh)
    
    # Query database
    con <- get_db_connection()
    
    query <- "SELECT * FROM official_schedule WHERE season = ?"
    params <- list(season)
    
    if (!is.null(week)) {
      query <- paste(query, "AND week IN (", paste(rep("?", length(week)), collapse=","), ")")
      params <- append(params, as.list(week))
    }
    
    query <- paste(query, "ORDER BY week, gameday, away_team")
    
    schedule_data <- dbGetQuery(con, query, params = params)
    
    if (nrow(schedule_data) == 0) {
      return(create_error_response("No schedule data found", 
                                   list(season = season, week = week)))
    }
    
    # Format response
    response <- list(
      data = schedule_data,
      metadata = list(
        season = season,
        week = week,
        total_games = nrow(schedule_data),
        completed_games = sum(schedule_data$game_completed, na.rm = TRUE),
        last_updated = max(schedule_data$updated_timestamp, na.rm = TRUE),
        data_source = "database",
        cached = FALSE,
        api_version = "1.0"
      ),
      status = "success"
    )
    
    # Cache response
    if (API_CONFIG$enable_caching) {
      .api_env$cache[[cache_key]] <- list(
        data = response,
        timestamp = as.numeric(Sys.time())
      )
    }
    
    return(response)
    
  }, error = function(e) {
    return(create_error_response(sprintf("Failed to get schedule: %s", e$message),
                                list(season = season, week = week)))
  })
}

#' NFL Data API - Get Validated Game Data for Predictions
#' 
#' Returns validated game data suitable for making predictions
#' This is the replacement for CSV-based data access
#' 
#' @param season Season to retrieve
#' @param week Week to retrieve
#' @param validate_data Ensure data passes validation
#' @return List with validated game data
nfl_api_get_prediction_data <- function(season = API_CONFIG$default_season,
                                       week = NULL,
                                       validate_data = TRUE) {
  
  cat(sprintf("🔮 API: Getting prediction data for season %d%s\n", 
             season, if (!is.null(week)) sprintf(", week %s", paste(week, collapse=",")) else ""))
  
  tryCatch({
    
    # Get schedule data
    schedule_response <- nfl_api_get_schedule(season, week, include_results = TRUE)
    
    if (schedule_response$status != "success") {
      return(schedule_response)
    }
    
    schedule_data <- schedule_response$data
    
    # Validate data if requested
    if (validate_data) {
      validation_result <- validate_game_schedule(schedule_data, season)
      
      if (!validation_result$valid) {
        return(create_error_response("Data validation failed", 
                                    list(
                                      validation_errors = validation_result$errors,
                                      season = season,
                                      week = week
                                    )))
      }
      
      # Log successful validation
      log_validation_result(season, week, validation_result)
    }
    
    # Format for prediction system (compatible with existing code)
    prediction_data <- schedule_data %>%
      select(game_id, season, week, gameday, away_team, home_team, 
             away_score, home_score, game_completed) %>%
      mutate(
        # Add computed fields that prediction system expects
        matchup = paste(away_team, "@", home_team),
        week_year = paste(season, week, sep="_"),
        is_completed = game_completed
      ) %>%
      arrange(week, gameday, away_team)
    
    response <- list(
      data = prediction_data,
      metadata = list(
        season = season,
        week = week,
        total_games = nrow(prediction_data),
        available_for_prediction = sum(!prediction_data$game_completed, na.rm = TRUE),
        data_validated = validate_data,
        validation_passed = if (validate_data) validation_result$valid else NULL,
        api_version = "1.0"
      ),
      status = "success"
    )
    
    return(response)
    
  }, error = function(e) {
    return(create_error_response(sprintf("Failed to get prediction data: %s", e$message),
                                list(season = season, week = week)))
  })
}

#' NFL Data API - Store Predictions
#' 
#' Stores predictions with validation and returns confirmation
#' 
#' @param predictions_data Data frame with predictions
#' @param model_version Model version identifier
#' @param validate_against_schedule Validate against official schedule
#' @return Response with storage status
nfl_api_store_predictions <- function(predictions_data, 
                                     model_version,
                                     validate_against_schedule = TRUE) {
  
  cat(sprintf("💾 API: Storing %d predictions from model %s\n", 
             nrow(predictions_data), model_version))
  
  tryCatch({
    
    # Add required fields if missing
    if (!"prediction_id" %in% names(predictions_data)) {
      predictions_data$prediction_id <- paste("pred", 
                                              format(Sys.time(), "%Y%m%d_%H%M%S"),
                                              sample(1000:9999, nrow(predictions_data)),
                                              sep = "_")
    }
    
    if (!"model_version" %in% names(predictions_data)) {
      predictions_data$model_version <- model_version
    }
    
    if (!"prediction_date" %in% names(predictions_data)) {
      predictions_data$prediction_date <- Sys.time()
    }
    
    # Store predictions
    stored_count <- store_predictions(predictions_data, validate_against_schedule)
    
    response <- list(
      data = list(
        stored_predictions = stored_count,
        model_version = model_version,
        validation_enabled = validate_against_schedule
      ),
      metadata = list(
        operation = "store_predictions",
        timestamp = Sys.time(),
        api_version = "1.0"
      ),
      status = "success"
    )
    
    return(response)
    
  }, error = function(e) {
    return(create_error_response(sprintf("Failed to store predictions: %s", e$message),
                                list(model_version = model_version, 
                                     prediction_count = nrow(predictions_data))))
  })
}

#' NFL Data API - Get Prediction Performance
#' 
#' Returns prediction accuracy and performance metrics
#' 
#' @param season Season to analyze (optional)
#' @param model_version Model version to filter (optional)
#' @param week Week to filter (optional)
#' @return Performance metrics and analysis
nfl_api_get_prediction_performance <- function(season = NULL, 
                                              model_version = NULL,
                                              week = NULL) {
  
  cat("📊 API: Getting prediction performance metrics\n")
  
  tryCatch({
    
    # Get predictions with results
    predictions <- get_predictions_with_results(season, week, model_version)
    
    if (nrow(predictions) == 0) {
      return(create_error_response("No predictions found for analysis",
                                  list(season = season, model_version = model_version, week = week)))
    }
    
    # Calculate performance metrics
    completed_predictions <- predictions %>% filter(result_processed == TRUE)
    
    if (nrow(completed_predictions) == 0) {
      return(list(
        data = list(
          total_predictions = nrow(predictions),
          completed_predictions = 0,
          message = "No completed predictions available for analysis"
        ),
        status = "success"
      ))
    }
    
    performance_metrics <- list(
      total_predictions = nrow(predictions),
      completed_predictions = nrow(completed_predictions),
      
      # Spread accuracy
      avg_spread_error = mean(abs(completed_predictions$spread_error), na.rm = TRUE),
      median_spread_error = median(abs(completed_predictions$spread_error), na.rm = TRUE),
      spread_rmse = sqrt(mean(completed_predictions$spread_error^2, na.rm = TRUE)),
      
      # Total accuracy  
      avg_total_error = mean(abs(completed_predictions$total_error), na.rm = TRUE),
      median_total_error = median(abs(completed_predictions$total_error), na.rm = TRUE),
      total_rmse = sqrt(mean(completed_predictions$total_error^2, na.rm = TRUE)),
      
      # Direction accuracy
      direction_accuracy = mean(completed_predictions$correct_direction, na.rm = TRUE),
      
      # Confidence calibration
      avg_confidence = mean(completed_predictions$confidence, na.rm = TRUE),
      
      # By model version (if multiple)
      by_model = completed_predictions %>%
        group_by(model_version) %>%
        summarise(
          predictions = n(),
          avg_spread_error = mean(abs(spread_error), na.rm = TRUE),
          direction_accuracy = mean(correct_direction, na.rm = TRUE),
          avg_confidence = mean(confidence, na.rm = TRUE),
          .groups = "drop"
        )
    )
    
    response <- list(
      data = performance_metrics,
      metadata = list(
        analysis_date = Sys.time(),
        filters = list(season = season, model_version = model_version, week = week),
        api_version = "1.0"
      ),
      status = "success"
    )
    
    return(response)
    
  }, error = function(e) {
    return(create_error_response(sprintf("Failed to analyze performance: %s", e$message),
                                list(season = season, model_version = model_version)))
  })
}

#' NFL Data API - Update Game Results  
#' 
#' Updates stored predictions with actual game results
#' 
#' @param game_results Data frame with actual game results
#' @return Update status and summary
nfl_api_update_results <- function(game_results) {
  
  cat(sprintf("📊 API: Updating results for %d games\n", nrow(game_results)))
  
  tryCatch({
    
    updated_count <- update_prediction_results(game_results)
    
    response <- list(
      data = list(
        games_processed = nrow(game_results),
        predictions_updated = updated_count
      ),
      metadata = list(
        operation = "update_results",
        timestamp = Sys.time(),
        api_version = "1.0"
      ),
      status = "success"
    )
    
    return(response)
    
  }, error = function(e) {
    return(create_error_response(sprintf("Failed to update results: %s", e$message),
                                list(games_count = nrow(game_results))))
  })
}

#' NFL Data API - Get Data Health Status
#' 
#' Returns comprehensive status of data pipeline health
#' 
#' @return System health and data quality status
nfl_api_get_health_status <- function() {
  
  cat("🏥 API: Checking data pipeline health\n")
  
  tryCatch({
    
    # Check data source availability
    source_status <- get_source_status()
    
    # Check database status
    con <- get_db_connection()
    db_tables <- dbListTables(con)
    
    # Check recent data
    recent_schedule <- dbGetQuery(con, "
      SELECT season, COUNT(*) as games 
      FROM official_schedule 
      WHERE season >= ? 
      GROUP BY season 
      ORDER BY season DESC", 
      params = list(2024))
    
    recent_predictions <- dbGetQuery(con, "
      SELECT model_version, COUNT(*) as predictions,
             MAX(prediction_date) as last_prediction
      FROM predictions 
      WHERE prediction_date >= date('now', '-7 days')
      GROUP BY model_version")
    
    # Check validation history
    validation_history <- get_validation_history()
    recent_validations <- length(validation_history)
    
    health_status <- list(
      system_status = "operational",
      data_sources = source_status,
      database = list(
        connected = dbIsValid(con),
        tables = length(db_tables),
        required_tables = all(c("official_schedule", "predictions", "prediction_results") %in% db_tables)
      ),
      data_availability = list(
        seasons_available = recent_schedule,
        recent_predictions = recent_predictions,
        total_predictions = dbGetQuery(con, "SELECT COUNT(*) as count FROM predictions")$count
      ),
      validation = list(
        recent_validations = recent_validations,
        last_validation = if (recent_validations > 0) validation_history[[recent_validations]]$timestamp else NULL
      ),
      cache_status = list(
        api_cache_entries = length(.api_env$cache),
        source_cache_entries = length(.data_source_env$cache)
      )
    )
    
    response <- list(
      data = health_status,
      metadata = list(
        check_time = Sys.time(),
        api_version = "1.0"
      ),
      status = "success"
    )
    
    return(response)
    
  }, error = function(e) {
    return(create_error_response(sprintf("Health check failed: %s", e$message), list()))
  })
}

# Helper Functions

#' Ensure Schedule is Available
#' 
#' Ensures official schedule is loaded and available in database
#' 
#' @param season Season to ensure
#' @param force_refresh Force refresh from source
ensure_schedule_available <- function(season, force_refresh = FALSE) {
  
  con <- get_db_connection()
  
  # Check if schedule exists
  existing <- dbGetQuery(con, "SELECT COUNT(*) as count FROM official_schedule WHERE season = ?",
                        params = list(season))
  
  if (existing$count == 0 || force_refresh) {
    cat(sprintf("📥 Loading official schedule for season %d...\n", season))
    
    # Initialize data sources if needed
    source_init <- initialize_data_sources()
    if (!source_init$success) {
      stop("No data sources available")
    }
    
    # Load and store official schedule
    schedule_data <- load_reliable_schedule(seasons = season, force_refresh = force_refresh)
    store_official_schedule(schedule_data, season)
  }
}

#' Create Error Response
#' 
#' Creates standardized error response
#' 
#' @param message Error message
#' @param details Additional error details
#' @return Standardized error response
create_error_response <- function(message, details = list()) {
  return(list(
    data = NULL,
    error = list(
      message = message,
      details = details,
      timestamp = Sys.time()
    ),
    metadata = list(
      api_version = "1.0"
    ),
    status = "error"
  ))
}

#' Log Validation Result
#' 
#' Logs validation results to database
#' 
#' @param season Season validated
#' @param week Week validated (optional)
#' @param validation_result Validation result object
log_validation_result <- function(season, week, validation_result) {
  
  con <- get_db_connection()
  
  log_id <- paste("val", format(Sys.time(), "%Y%m%d_%H%M%S"), 
                  sample(1000:9999, 1), sep = "_")
  
  status <- if (validation_result$valid) "success" else if (length(validation_result$warnings) > 0) "warning" else "error"
  
  dbExecute(con, "
    INSERT INTO validation_log 
    (log_id, validation_type, season, week, total_records, valid_records, 
     invalid_records, warnings_count, errors_count, validation_details, status)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      log_id, "schedule_validation", season, week,
      validation_result$total_games, validation_result$valid_games,
      validation_result$invalid_games, length(validation_result$warnings),
      length(validation_result$errors), toJSON(validation_result), status
    ))
}

#' Clear API Cache
#' 
#' Clears API response cache
clear_api_cache <- function() {
  .api_env$cache <- list()
  cat("🧹 API cache cleared\n")
}

cat("🌐 NFL Data API Layer loaded successfully\n")
cat("Use nfl_api_get_prediction_data() to replace CSV access\n")