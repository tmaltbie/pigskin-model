# NFL Data Source Layer - Reliable Data Access with Fallbacks
# Production-ready module for accessing NFL data from multiple sources
# Handles network issues, missing packages, and data validation

library(dplyr)
library(lubridate)
library(RSQLite)

# Data source configuration
DATA_SOURCE_CONFIG <- list(
  primary = "nflreadr",    # Primary data source
  fallback = "nflfastR",   # Fallback data source  
  cache_duration = 3600,   # Cache duration in seconds (1 hour)
  max_retries = 3,         # Maximum retry attempts
  timeout = 30             # Request timeout in seconds
)

# Global environment for caching and status tracking
.data_source_env <- new.env()
.data_source_env$cache <- list()
.data_source_env$source_status <- list()
.data_source_env$last_validation <- NULL

#' Initialize Data Source Layer
#' 
#' Sets up the data source layer by checking package availability,
#' testing connections, and initializing cache.
#' 
#' @return List with initialization status and available sources
initialize_data_sources <- function() {
  cat("🚀 Initializing NFL Data Source Layer\n")
  cat("=====================================\n")
  
  sources <- list(
    nflreadr = FALSE,
    nflfastR = FALSE,
    local_cache = FALSE
  )
  
  # Test nflreadr availability
  tryCatch({
    if (!requireNamespace("nflreadr", quietly = TRUE)) {
      stop("Package not available")
    }
    library(nflreadr, warn.conflicts = FALSE)
    
    # Test connection with a simple query
    test_data <- nflreadr::load_schedules(2024, limit = 1)
    if (nrow(test_data) > 0) {
      sources$nflreadr <- TRUE
      cat("✅ nflreadr: Available and tested\n")
    }
  }, error = function(e) {
    cat("❌ nflreadr: Not available -", e$message, "\n")
  })
  
  # Test nflfastR availability
  tryCatch({
    if (!requireNamespace("nflfastR", quietly = TRUE)) {
      stop("Package not available")
    }
    library(nflfastR, warn.conflicts = FALSE)
    sources$nflfastR <- TRUE
    cat("✅ nflfastR: Available\n")
  }, error = function(e) {
    cat("❌ nflfastR: Not available -", e$message, "\n")
  })
  
  # Check local cache directory
  cache_dir <- "data_cache"
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  sources$local_cache <- dir.exists(cache_dir)
  
  if (sources$local_cache) {
    cat("✅ Local cache: Available at", cache_dir, "\n")
  }
  
  # Store source status
  .data_source_env$source_status <- sources
  
  # Determine primary source
  primary_source <- if (sources$nflreadr) {
    "nflreadr"
  } else if (sources$nflfastR) {
    "nflfastR"
  } else {
    "local_cache"
  }
  
  cat(sprintf("🎯 Primary data source: %s\n", primary_source))
  
  return(list(
    success = any(unlist(sources)),
    sources = sources,
    primary = primary_source
  ))
}

#' Load NFL Schedule with Reliability
#' 
#' Loads NFL schedule data with automatic fallbacks and caching.
#' Validates data quality and handles network/package issues gracefully.
#' 
#' @param seasons Vector of seasons to load
#' @param weeks Vector of weeks to filter (optional)
#' @param force_refresh Force refresh of cached data
#' @return Data frame with validated schedule data
load_reliable_schedule <- function(seasons = 2025, weeks = NULL, force_refresh = FALSE) {
  
  cache_key <- paste("schedule", paste(seasons, collapse="-"), 
                     paste(weeks, collapse="-"), sep="_")
  
  # Check cache first (unless forcing refresh)
  if (!force_refresh && cache_key %in% names(.data_source_env$cache)) {
    cache_entry <- .data_source_env$cache[[cache_key]]
    if (as.numeric(Sys.time()) - cache_entry$timestamp < DATA_SOURCE_CONFIG$cache_duration) {
      cat("📋 Using cached schedule data\n")
      return(cache_entry$data)
    }
  }
  
  cat(sprintf("🏈 Loading schedule for seasons: %s\n", paste(seasons, collapse=", ")))
  
  schedule_data <- NULL
  last_error <- NULL
  
  # Try primary source (nflreadr)
  if (.data_source_env$source_status$nflreadr) {
    tryCatch({
      cat("📡 Attempting nflreadr...\n")
      schedule_data <- nflreadr::load_schedules(seasons)
      
      if (!is.null(weeks)) {
        schedule_data <- schedule_data %>% filter(week %in% weeks)
      }
      
      cat(sprintf("✅ Successfully loaded %d games from nflreadr\n", nrow(schedule_data)))
      
    }, error = function(e) {
      last_error <- e$message
      cat(sprintf("❌ nflreadr failed: %s\n", e$message))
    })
  }
  
  # Try fallback source (nflfastR) if primary failed
  if (is.null(schedule_data) && .data_source_env$source_status$nflfastR) {
    tryCatch({
      cat("📡 Attempting nflfastR fallback...\n")
      # Note: nflfastR doesn't have load_schedules, so we'd need to construct from games
      # This would require loading play-by-play data and extracting game info
      cat("⚠️ nflfastR schedule loading not yet implemented\n")
      
    }, error = function(e) {
      last_error <- e$message
      cat(sprintf("❌ nflfastR fallback failed: %s\n", e$message))
    })
  }
  
  # Try local cache as final fallback
  if (is.null(schedule_data)) {
    cache_file <- sprintf("data_cache/schedule_%s.rds", paste(seasons, collapse="-"))
    if (file.exists(cache_file)) {
      tryCatch({
        cat("💾 Loading from local cache...\n")
        schedule_data <- readRDS(cache_file)
        cat(sprintf("✅ Loaded %d games from cache\n", nrow(schedule_data)))
      }, error = function(e) {
        cat(sprintf("❌ Cache load failed: %s\n", e$message))
      })
    }
  }
  
  # Validate data if we got something
  if (!is.null(schedule_data)) {
    validation_result <- validate_schedule_data(schedule_data, seasons, weeks)
    
    if (validation_result$valid) {
      # Cache the successful result
      .data_source_env$cache[[cache_key]] <- list(
        data = schedule_data,
        timestamp = as.numeric(Sys.time())
      )
      
      # Save to local cache for future fallback
      cache_file <- sprintf("data_cache/schedule_%s.rds", paste(seasons, collapse="-"))
      saveRDS(schedule_data, cache_file)
      cat(sprintf("💾 Cached to %s\n", cache_file))
      
      return(schedule_data)
    } else {
      cat("❌ Data validation failed:", validation_result$message, "\n")
      schedule_data <- NULL
    }
  }
  
  # If all sources failed
  if (is.null(schedule_data)) {
    stop(sprintf("Failed to load schedule data. Last error: %s", 
                 last_error %||% "No data sources available"))
  }
}

#' Load Game Results with Reliability
#' 
#' Loads completed game results with validation and fallbacks
#' 
#' @param seasons Vector of seasons to load
#' @param weeks Vector of weeks to filter (optional) 
#' @param completed_only Only return games with final scores
#' @param force_refresh Force refresh of cached data
#' @return Data frame with validated game results
load_reliable_results <- function(seasons = 2025, weeks = NULL, 
                                  completed_only = TRUE, force_refresh = FALSE) {
  
  cache_key <- paste("results", paste(seasons, collapse="-"), 
                     paste(weeks, collapse="-"), completed_only, sep="_")
  
  # Check cache first
  if (!force_refresh && cache_key %in% names(.data_source_env$cache)) {
    cache_entry <- .data_source_env$cache[[cache_key]]
    if (as.numeric(Sys.time()) - cache_entry$timestamp < DATA_SOURCE_CONFIG$cache_duration) {
      cat("📊 Using cached results data\n")
      return(cache_entry$data)
    }
  }
  
  cat(sprintf("🏆 Loading results for seasons: %s\n", paste(seasons, collapse=", ")))
  
  results_data <- NULL
  
  # Try to get results from schedule data (which includes scores)
  tryCatch({
    schedule_data <- load_reliable_schedule(seasons, weeks, force_refresh)
    
    if (completed_only) {
      results_data <- schedule_data %>%
        filter(!is.na(home_score) & !is.na(away_score)) %>%
        mutate(
          total_score = home_score + away_score,
          home_margin = home_score - away_score,
          home_win = home_score > away_score
        )
    } else {
      results_data <- schedule_data
    }
    
    cat(sprintf("✅ Successfully processed %d games\n", nrow(results_data)))
    
  }, error = function(e) {
    cat(sprintf("❌ Results loading failed: %s\n", e$message))
  })
  
  # Validate and cache
  if (!is.null(results_data)) {
    # Cache the result
    .data_source_env$cache[[cache_key]] <- list(
      data = results_data,
      timestamp = as.numeric(Sys.time())
    )
    
    return(results_data)
  }
  
  stop("Failed to load game results from any source")
}

#' Validate Schedule Data Quality
#' 
#' Validates that schedule data meets quality requirements
#' 
#' @param data Schedule data frame to validate
#' @param expected_seasons Expected seasons
#' @param expected_weeks Expected weeks (optional)
#' @return List with validation status and message
validate_schedule_data <- function(data, expected_seasons = NULL, expected_weeks = NULL) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "No data provided"))
  }
  
  required_columns <- c("game_id", "season", "week", "away_team", "home_team", "gameday")
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    return(list(valid = FALSE, 
                message = paste("Missing columns:", paste(missing_columns, collapse=", "))))
  }
  
  # Check for expected seasons
  if (!is.null(expected_seasons)) {
    actual_seasons <- unique(data$season)
    missing_seasons <- setdiff(expected_seasons, actual_seasons)
    if (length(missing_seasons) > 0) {
      return(list(valid = FALSE,
                  message = paste("Missing seasons:", paste(missing_seasons, collapse=", "))))
    }
  }
  
  # Check for expected weeks
  if (!is.null(expected_weeks)) {
    actual_weeks <- unique(data$week)
    missing_weeks <- setdiff(expected_weeks, actual_weeks)
    if (length(missing_weeks) > 0) {
      return(list(valid = FALSE,
                  message = paste("Missing weeks:", paste(missing_weeks, collapse=", "))))
    }
  }
  
  # Check for data quality issues
  invalid_teams <- data %>%
    filter(is.na(away_team) | is.na(home_team) | away_team == home_team) %>%
    nrow()
  
  if (invalid_teams > 0) {
    return(list(valid = FALSE,
                message = sprintf("%d games with invalid team assignments", invalid_teams)))
  }
  
  return(list(valid = TRUE, message = "Data validation passed"))
}

#' Get Data Source Status
#' 
#' Returns current status of all data sources
#' 
#' @return List with source availability and cache status
get_source_status <- function() {
  return(list(
    sources = .data_source_env$source_status,
    cache_entries = length(.data_source_env$cache),
    last_validation = .data_source_env$last_validation
  ))
}

#' Clear Data Cache
#' 
#' Clears all cached data to force fresh loads
clear_data_cache <- function() {
  .data_source_env$cache <- list()
  cat("🧹 Data cache cleared\n")
}

# Utility function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

cat("📦 Data Source Layer loaded successfully\n")
cat("Use initialize_data_sources() to set up data access\n")