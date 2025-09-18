# NFL Data Pipeline Error Handling Framework
# Comprehensive error handling and recovery for data pipeline operations
# Provides graceful degradation and recovery mechanisms

library(dplyr)
library(lubridate)
library(jsonlite)

# Error handling configuration
ERROR_CONFIG <- list(
  max_retries = 3,              # Maximum retry attempts
  retry_delay = c(1, 2, 5),     # Retry delays in seconds
  enable_fallbacks = TRUE,      # Enable fallback mechanisms
  log_errors = TRUE,            # Log all errors
  enable_recovery = TRUE,       # Enable automatic recovery
  notification_threshold = 5    # Error threshold for notifications
)

# Error tracking and logging
.error_env <- new.env()
.error_env$error_log <- list()
.error_env$recovery_actions <- list()
.error_env$system_health <- list(
  status = "unknown",
  last_check = NULL,
  error_count = 0,
  recovery_count = 0
)

#' Enhanced Error Handling Wrapper
#' 
#' Wraps functions with comprehensive error handling, retries, and fallbacks
#' 
#' @param func Function to execute
#' @param operation_name Name of the operation for logging
#' @param max_retries Maximum retry attempts (overrides global config)
#' @param fallback_func Fallback function to try if main function fails
#' @param critical Whether this is a critical operation
#' @return Result of function execution or error information
with_error_handling <- function(func, 
                               operation_name,
                               max_retries = ERROR_CONFIG$max_retries,
                               fallback_func = NULL,
                               critical = FALSE) {
  
  operation_id <- paste("op", format(Sys.time(), "%Y%m%d_%H%M%S"), 
                       sample(1000:9999, 1), sep = "_")
  
  cat(sprintf("🔄 Starting operation: %s [ID: %s]\n", operation_name, operation_id))
  
  last_error <- NULL
  
  # Main function execution with retries
  for (attempt in 1:(max_retries + 1)) {
    
    if (attempt > 1) {
      delay <- ERROR_CONFIG$retry_delay[min(attempt - 1, length(ERROR_CONFIG$retry_delay))]
      cat(sprintf("⏳ Retry attempt %d/%d in %d seconds...\n", 
                 attempt - 1, max_retries, delay))
      Sys.sleep(delay)
    }
    
    tryCatch({
      
      result <- func()
      
      if (attempt > 1) {
        cat(sprintf("✅ Operation recovered on attempt %d: %s\n", attempt, operation_name))
        log_recovery_event(operation_name, attempt, last_error)
      }
      
      return(create_success_response(result, operation_name, operation_id))
      
    }, error = function(e) {
      last_error <- e
      
      if (attempt <= max_retries) {
        cat(sprintf("❌ Attempt %d failed: %s\n", attempt, e$message))
        log_error_event(operation_name, e, attempt, operation_id, recoverable = TRUE)
      }
    })
  }
  
  # All retries failed, try fallback if available
  if (!is.null(fallback_func) && ERROR_CONFIG$enable_fallbacks) {
    cat(sprintf("🔀 Trying fallback for operation: %s\n", operation_name))
    
    tryCatch({
      result <- fallback_func()
      cat(sprintf("✅ Fallback succeeded for: %s\n", operation_name))
      log_recovery_event(paste(operation_name, "fallback"), 1, last_error)
      
      return(create_success_response(result, paste(operation_name, "(fallback)"), operation_id))
      
    }, error = function(e) {
      cat(sprintf("❌ Fallback also failed: %s\n", e$message))
      log_error_event(paste(operation_name, "fallback"), e, 1, operation_id, recoverable = FALSE)
    })
  }
  
  # Complete failure
  final_error <- create_error_response(
    sprintf("Operation failed after %d attempts: %s", max_retries + 1, last_error$message),
    operation_name,
    operation_id,
    critical,
    list(
      attempts = max_retries + 1,
      final_error = last_error$message,
      fallback_attempted = !is.null(fallback_func)
    )
  )
  
  log_error_event(operation_name, last_error, max_retries + 1, operation_id, recoverable = FALSE)
  
  # Handle critical failures
  if (critical) {
    handle_critical_failure(operation_name, final_error)
  }
  
  return(final_error)
}

#' Network Operation Error Handler
#' 
#' Specialized error handling for network operations (API calls, data downloads)
#' 
#' @param network_func Network function to execute
#' @param operation_name Operation name for logging
#' @param timeout_seconds Operation timeout
#' @return Network operation result or error
with_network_error_handling <- function(network_func, 
                                       operation_name, 
                                       timeout_seconds = 30) {
  
  # Network-specific fallback function
  network_fallback <- function() {
    cat("🌐 Attempting network recovery...\n")
    
    # Check network connectivity
    if (!check_network_connectivity()) {
      stop("Network connectivity lost")
    }
    
    # Try with reduced timeout
    network_func()
  }
  
  return(with_error_handling(
    func = network_func,
    operation_name = paste("Network:", operation_name),
    fallback_func = network_fallback,
    critical = FALSE
  ))
}

#' Database Operation Error Handler
#' 
#' Specialized error handling for database operations
#' 
#' @param db_func Database function to execute
#' @param operation_name Operation name for logging
#' @param transaction Whether operation is part of a transaction
#' @return Database operation result or error
with_database_error_handling <- function(db_func, 
                                        operation_name,
                                        transaction = FALSE) {
  
  # Database-specific fallback
  db_fallback <- function() {
    cat("🗄️  Attempting database recovery...\n")
    
    # Try to reconnect to database
    con <- get_db_connection()
    if (!dbIsValid(con)) {
      con <- initialize_database()
    }
    
    db_func()
  }
  
  return(with_error_handling(
    func = db_func,
    operation_name = paste("Database:", operation_name),
    fallback_func = db_fallback,
    critical = transaction
  ))
}

#' Data Validation Error Handler
#' 
#' Specialized error handling for data validation operations
#' 
#' @param validation_func Validation function to execute
#' @param data_description Description of data being validated
#' @param auto_clean Attempt automatic data cleaning on validation failure
#' @return Validation result or error with cleaning suggestions
with_validation_error_handling <- function(validation_func, 
                                          data_description,
                                          auto_clean = TRUE) {
  
  # Validation-specific fallback
  validation_fallback <- function() {
    if (auto_clean) {
      cat("🧹 Attempting automatic data cleaning...\n")
      # This would integrate with the validation layer's cleaning functions
      stop("Auto-cleaning not implemented for this validation type")
    } else {
      stop("Validation failed and auto-cleaning disabled")
    }
  }
  
  return(with_error_handling(
    func = validation_func,
    operation_name = paste("Validation:", data_description),
    fallback_func = if (auto_clean) validation_fallback else NULL,
    critical = FALSE
  ))
}

#' System Health Checker
#' 
#' Performs comprehensive system health checks and reports status
#' 
#' @param quick_check Perform quick check only
#' @return System health status
check_system_health <- function(quick_check = FALSE) {
  
  cat("🏥 Performing system health check...\n")
  
  health_status <- list(
    timestamp = Sys.time(),
    overall_status = "unknown",
    components = list(),
    errors = list(),
    recommendations = list()
  )
  
  # Check data sources
  tryCatch({
    source_init <- initialize_data_sources()
    health_status$components$data_sources <- list(
      status = if (source_init$success) "healthy" else "degraded",
      available_sources = source_init$sources,
      primary_source = source_init$primary
    )
  }, error = function(e) {
    health_status$components$data_sources <- list(status = "failed", error = e$message)
    health_status$errors <- append(health_status$errors, list(component = "data_sources", error = e$message))
  })
  
  # Check database
  tryCatch({
    con <- get_db_connection()
    db_valid <- dbIsValid(con)
    
    if (db_valid && !quick_check) {
      # Perform database integrity checks
      table_count <- length(dbListTables(con))
      row_counts <- list(
        official_schedule = dbGetQuery(con, "SELECT COUNT(*) as count FROM official_schedule")$count,
        predictions = dbGetQuery(con, "SELECT COUNT(*) as count FROM predictions")$count
      )
    }
    
    health_status$components$database <- list(
      status = if (db_valid) "healthy" else "failed",
      connected = db_valid,
      tables = if (!quick_check) table_count else NULL,
      row_counts = if (!quick_check) row_counts else NULL
    )
  }, error = function(e) {
    health_status$components$database <- list(status = "failed", error = e$message)
    health_status$errors <- append(health_status$errors, list(component = "database", error = e$message))
  })
  
  # Check network connectivity (if not quick check)
  if (!quick_check) {
    health_status$components$network <- list(
      status = if (check_network_connectivity()) "healthy" else "degraded",
      connectivity = check_network_connectivity()
    )
  }
  
  # Determine overall status
  component_statuses <- sapply(health_status$components, function(x) x$status)
  
  if (all(component_statuses == "healthy")) {
    health_status$overall_status <- "healthy"
  } else if (any(component_statuses == "failed")) {
    health_status$overall_status <- "critical"
  } else {
    health_status$overall_status <- "degraded"
  }
  
  # Generate recommendations
  if (health_status$overall_status != "healthy") {
    health_status$recommendations <- generate_health_recommendations(health_status)
  }
  
  # Update system health tracking
  .error_env$system_health <- health_status
  
  cat(sprintf("🏥 System health: %s\n", toupper(health_status$overall_status)))
  
  return(health_status)
}

#' Check Network Connectivity
#' 
#' Tests network connectivity to external resources
#' 
#' @return TRUE if network is accessible, FALSE otherwise
check_network_connectivity <- function() {
  
  tryCatch({
    # Try to resolve a reliable DNS name
    nslookup_result <- system("nslookup google.com", intern = TRUE, ignore.stderr = TRUE)
    return(length(nslookup_result) > 0)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Generate Health Recommendations
#' 
#' Generates actionable recommendations based on health status
#' 
#' @param health_status Current health status
#' @return List of recommendations
generate_health_recommendations <- function(health_status) {
  
  recommendations <- list()
  
  # Data source recommendations
  if (health_status$components$data_sources$status != "healthy") {
    recommendations <- append(recommendations, 
      "Check network connectivity and nflverse package installations")
  }
  
  # Database recommendations
  if (health_status$components$database$status != "healthy") {
    recommendations <- append(recommendations,
      "Reinitialize database connection or recreate database schema")
  }
  
  # Network recommendations
  if (!is.null(health_status$components$network) && 
      health_status$components$network$status != "healthy") {
    recommendations <- append(recommendations,
      "Check internet connectivity and firewall settings")
  }
  
  return(recommendations)
}

#' Handle Critical Failures
#' 
#' Special handling for critical system failures
#' 
#' @param operation_name Name of failed operation
#' @param error_response Error response object
handle_critical_failure <- function(operation_name, error_response) {
  
  cat(sprintf("🚨 CRITICAL FAILURE: %s\n", operation_name))
  
  # Log critical failure
  log_error_event(operation_name, 
                  list(message = error_response$error$message), 
                  0, 
                  error_response$metadata$operation_id, 
                  recoverable = FALSE, 
                  critical = TRUE)
  
  # Attempt system recovery
  if (ERROR_CONFIG$enable_recovery) {
    cat("🔧 Attempting system recovery...\n")
    attempt_system_recovery(operation_name, error_response)
  }
  
  # Update error count
  .error_env$system_health$error_count <- .error_env$system_health$error_count + 1
  
  # Check if notification threshold reached
  if (.error_env$system_health$error_count >= ERROR_CONFIG$notification_threshold) {
    send_error_notification(sprintf("Critical failure threshold reached: %d errors", 
                                   .error_env$system_health$error_count))
  }
}

#' Attempt System Recovery
#' 
#' Automated system recovery procedures
#' 
#' @param failed_operation Name of operation that failed
#' @param error_response Error response
attempt_system_recovery <- function(failed_operation, error_response) {
  
  recovery_actions <- list()
  
  # Database recovery
  if (grepl("database", failed_operation, ignore.case = TRUE)) {
    tryCatch({
      con <- initialize_database()
      create_database_schema(con)
      recovery_actions <- append(recovery_actions, "Database reinitialized")
    }, error = function(e) {
      recovery_actions <- append(recovery_actions, paste("Database recovery failed:", e$message))
    })
  }
  
  # Data source recovery
  if (grepl("data|network", failed_operation, ignore.case = TRUE)) {
    tryCatch({
      clear_data_cache()
      source_init <- initialize_data_sources()
      recovery_actions <- append(recovery_actions, "Data sources reinitialized")
    }, error = function(e) {
      recovery_actions <- append(recovery_actions, paste("Data source recovery failed:", e$message))
    })
  }
  
  # Log recovery actions
  .error_env$recovery_actions[[length(.error_env$recovery_actions) + 1]] <- list(
    timestamp = Sys.time(),
    failed_operation = failed_operation,
    actions_taken = recovery_actions
  )
  
  .error_env$system_health$recovery_count <- .error_env$system_health$recovery_count + 1
  
  cat(sprintf("🔧 Recovery attempted. Actions taken: %s\n", 
             paste(recovery_actions, collapse = ", ")))
}

#' Send Error Notification
#' 
#' Placeholder for error notification system
#' 
#' @param message Notification message
send_error_notification <- function(message) {
  # This would integrate with actual notification systems
  cat(sprintf("📢 NOTIFICATION: %s\n", message))
  
  # Log notification
  .error_env$error_log[[length(.error_env$error_log) + 1]] <- list(
    timestamp = Sys.time(),
    type = "notification",
    message = message
  )
}

# Helper functions for response creation

#' Create Success Response
#' 
#' Creates standardized success response
#' 
#' @param result Operation result
#' @param operation_name Operation name
#' @param operation_id Operation ID
#' @return Standardized success response
create_success_response <- function(result, operation_name, operation_id) {
  return(list(
    data = result,
    metadata = list(
      operation = operation_name,
      operation_id = operation_id,
      timestamp = Sys.time(),
      status = "success"
    ),
    error = NULL
  ))
}

#' Create Error Response
#' 
#' Creates standardized error response
#' 
#' @param message Error message
#' @param operation_name Operation name
#' @param operation_id Operation ID
#' @param critical Whether error is critical
#' @param details Additional error details
#' @return Standardized error response
create_error_response <- function(message, operation_name, operation_id, critical = FALSE, details = list()) {
  return(list(
    data = NULL,
    error = list(
      message = message,
      operation = operation_name,
      critical = critical,
      details = details,
      timestamp = Sys.time()
    ),
    metadata = list(
      operation = operation_name,
      operation_id = operation_id,
      status = "error"
    )
  ))
}

#' Log Error Event
#' 
#' Logs error events for analysis and monitoring
#' 
#' @param operation_name Operation name
#' @param error Error object
#' @param attempt Attempt number
#' @param operation_id Operation ID
#' @param recoverable Whether error is recoverable
#' @param critical Whether error is critical
log_error_event <- function(operation_name, error, attempt, operation_id, 
                           recoverable = TRUE, critical = FALSE) {
  
  if (!ERROR_CONFIG$log_errors) return()
  
  error_entry <- list(
    timestamp = Sys.time(),
    operation = operation_name,
    operation_id = operation_id,
    attempt = attempt,
    error_message = error$message,
    recoverable = recoverable,
    critical = critical,
    call_stack = if (!is.null(error$call)) as.character(error$call) else NULL
  )
  
  .error_env$error_log[[length(.error_env$error_log) + 1]] <- error_entry
}

#' Log Recovery Event
#' 
#' Logs successful recovery events
#' 
#' @param operation_name Operation name
#' @param attempt Attempt number that succeeded
#' @param original_error Original error that was recovered from
log_recovery_event <- function(operation_name, attempt, original_error) {
  
  recovery_entry <- list(
    timestamp = Sys.time(),
    operation = operation_name,
    recovery_attempt = attempt,
    original_error = original_error$message
  )
  
  .error_env$recovery_actions[[length(.error_env$recovery_actions) + 1]] <- recovery_entry
}

#' Get Error Log
#' 
#' Returns recent error log entries
#' 
#' @param limit Number of recent entries to return
#' @return List of error log entries
get_error_log <- function(limit = 50) {
  log_length <- length(.error_env$error_log)
  if (log_length == 0) return(list())
  
  start_index <- max(1, log_length - limit + 1)
  return(.error_env$error_log[start_index:log_length])
}

#' Get Recovery Log
#' 
#' Returns recent recovery events
#' 
#' @param limit Number of recent entries to return  
#' @return List of recovery events
get_recovery_log <- function(limit = 20) {
  log_length <- length(.error_env$recovery_actions)
  if (log_length == 0) return(list())
  
  start_index <- max(1, log_length - limit + 1)
  return(.error_env$recovery_actions[start_index:log_length])
}

#' Clear Error Logs
#' 
#' Clears error and recovery logs
clear_error_logs <- function() {
  .error_env$error_log <- list()
  .error_env$recovery_actions <- list()
  .error_env$system_health$error_count <- 0
  .error_env$system_health$recovery_count <- 0
  cat("🧹 Error logs cleared\n")
}

cat("🛡️  Error Handling Framework loaded successfully\n")
cat("Use with_error_handling() to wrap critical operations\n")