# NFL Data Validation Layer - Prevent Mock Data Contamination
# Comprehensive validation system to ensure data integrity and prevent
# incorrect matchups from contaminating predictions

library(dplyr)
library(lubridate)
library(stringr)

# Load data source layer
source("data_pipeline/data_source_layer.R")

# Official NFL team abbreviations and validation rules
NFL_TEAMS <- c(
  "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", 
  "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", 
  "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG", 
  "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS"
)

# Validation rules configuration
VALIDATION_CONFIG <- list(
  max_games_per_week = 16,           # Maximum regular season games per week
  min_games_per_week = 10,           # Minimum games per week (bye weeks)
  regular_season_weeks = 1:18,       # Valid regular season weeks
  max_score = 100,                   # Maximum reasonable score
  max_total_score = 150,             # Maximum reasonable total score
  require_official_schedule = TRUE   # Require validation against official schedule
)

# Global validation cache
.validation_env <- new.env()
.validation_env$official_schedules <- list()
.validation_env$validation_history <- list()

#' Load and Cache Official NFL Schedule
#' 
#' Loads the official NFL schedule from reliable sources and caches it
#' for validation purposes. This is the source of truth for all matchups.
#' 
#' @param season Season to load official schedule for
#' @param force_refresh Force refresh of cached official schedule
#' @return Data frame with official schedule
load_official_schedule <- function(season = 2025, force_refresh = FALSE) {
  
  cache_key <- paste("official", season, sep="_")
  
  # Check cache first
  if (!force_refresh && cache_key %in% names(.validation_env$official_schedules)) {
    cat(sprintf("📋 Using cached official schedule for %d\n", season))
    return(.validation_env$official_schedules[[cache_key]])
  }
  
  cat(sprintf("🏈 Loading official NFL schedule for %d...\n", season))
  
  tryCatch({
    # Load from reliable source
    official_schedule <- load_reliable_schedule(seasons = season, force_refresh = force_refresh)
    
    if (nrow(official_schedule) == 0) {
      stop("No official schedule data returned")
    }
    
    # Standardize and clean the official schedule
    official_schedule <- official_schedule %>%
      mutate(
        # Ensure consistent date format
        gameday = as.Date(gameday),
        # Create standardized game identifiers
        official_game_key = paste(season, sprintf("%02d", week), away_team, home_team, sep="_"),
        # Add validation metadata
        data_source = "official",
        load_timestamp = Sys.time()
      ) %>%
      select(game_id, season, week, gameday, away_team, home_team, 
             away_score, home_score, official_game_key, data_source, load_timestamp)
    
    # Cache the official schedule
    .validation_env$official_schedules[[cache_key]] <- official_schedule
    
    cat(sprintf("✅ Loaded and cached %d official games for %d\n", 
               nrow(official_schedule), season))
    
    return(official_schedule)
    
  }, error = function(e) {
    stop(sprintf("Failed to load official schedule for %d: %s", season, e$message))
  })
}

#' Validate Game Schedule Against Official Source
#' 
#' Validates game matchups against the official NFL schedule to prevent
#' mock data contamination and incorrect predictions.
#' 
#' @param games_data Data frame with games to validate
#' @param season Season to validate against
#' @param strict_validation Use strict validation rules
#' @return List with validation results and details
validate_game_schedule <- function(games_data, season = 2025, strict_validation = TRUE) {
  
  cat(sprintf("🔍 Validating %d games against official %d schedule...\n", 
             nrow(games_data), season))
  
  validation_result <- list(
    valid = FALSE,
    total_games = nrow(games_data),
    valid_games = 0,
    invalid_games = 0,
    warnings = character(),
    errors = character(),
    details = list()
  )
  
  # Basic data structure validation
  required_columns <- c("away_team", "home_team", "week", "season")
  missing_columns <- setdiff(required_columns, names(games_data))
  
  if (length(missing_columns) > 0) {
    validation_result$errors <- c(validation_result$errors,
                                  paste("Missing required columns:", paste(missing_columns, collapse=", ")))
    return(validation_result)
  }
  
  # Load official schedule
  tryCatch({
    official_schedule <- load_official_schedule(season)
  }, error = function(e) {
    validation_result$errors <- c(validation_result$errors,
                                  paste("Failed to load official schedule:", e$message))
    return(validation_result)
  })
  
  # Validate each game
  game_validations <- list()
  
  for (i in 1:nrow(games_data)) {
    game <- games_data[i, ]
    game_validation <- validate_single_game(game, official_schedule, strict_validation)
    game_validations[[i]] <- game_validation
    
    if (game_validation$valid) {
      validation_result$valid_games <- validation_result$valid_games + 1
    } else {
      validation_result$invalid_games <- validation_result$invalid_games + 1
    }
    
    # Collect warnings and errors
    validation_result$warnings <- c(validation_result$warnings, game_validation$warnings)
    validation_result$errors <- c(validation_result$errors, game_validation$errors)
  }
  
  validation_result$details <- game_validations
  validation_result$valid <- validation_result$invalid_games == 0
  
  # Summary reporting
  cat(sprintf("📊 Validation Results:\n"))
  cat(sprintf("   ✅ Valid games: %d\n", validation_result$valid_games))
  cat(sprintf("   ❌ Invalid games: %d\n", validation_result$invalid_games))
  
  if (length(validation_result$warnings) > 0) {
    cat(sprintf("   ⚠️  Warnings: %d\n", length(validation_result$warnings)))
  }
  
  if (length(validation_result$errors) > 0) {
    cat(sprintf("   🚨 Errors: %d\n", length(validation_result$errors)))
    for (error in validation_result$errors) {
      cat(sprintf("      • %s\n", error))
    }
  }
  
  # Store validation history
  .validation_env$validation_history[[length(.validation_env$validation_history) + 1]] <- list(
    timestamp = Sys.time(),
    season = season,
    result = validation_result
  )
  
  return(validation_result)
}

#' Validate Single Game Against Official Schedule
#' 
#' Validates a single game matchup against official schedule
#' 
#' @param game Single game data
#' @param official_schedule Official schedule data frame
#' @param strict_validation Use strict validation
#' @return Validation result for single game
validate_single_game <- function(game, official_schedule, strict_validation = TRUE) {
  
  result <- list(
    valid = FALSE,
    game_info = paste(game$away_team, "@", game$home_team, "Week", game$week),
    warnings = character(),
    errors = character(),
    official_match = NULL
  )
  
  # Basic team validation
  if (!game$away_team %in% NFL_TEAMS) {
    result$errors <- c(result$errors, 
                       paste("Invalid away team:", game$away_team))
  }
  
  if (!game$home_team %in% NFL_TEAMS) {
    result$errors <- c(result$errors,
                       paste("Invalid home team:", game$home_team))
  }
  
  if (game$away_team == game$home_team) {
    result$errors <- c(result$errors,
                       "Team cannot play against itself")
  }
  
  # Week validation
  if (!game$week %in% VALIDATION_CONFIG$regular_season_weeks) {
    result$errors <- c(result$errors,
                       paste("Invalid week:", game$week))
  }
  
  # Find official match
  official_matches <- official_schedule %>%
    filter(
      season == game$season,
      week == game$week,
      away_team == game$away_team,
      home_team == game$home_team
    )
  
  if (nrow(official_matches) == 0) {
    # Check if teams are swapped (common error)
    swapped_matches <- official_schedule %>%
      filter(
        season == game$season,
        week == game$week,
        away_team == game$home_team,  # Swapped
        home_team == game$away_team   # Swapped
      )
    
    if (nrow(swapped_matches) > 0) {
      result$errors <- c(result$errors,
                         sprintf("Home/Away teams swapped. Official matchup: %s @ %s",
                                swapped_matches$away_team[1], swapped_matches$home_team[1]))
    } else {
      # Check if either team plays in this week
      team_games <- official_schedule %>%
        filter(
          season == game$season,
          week == game$week,
          (away_team %in% c(game$away_team, game$home_team) |
           home_team %in% c(game$away_team, game$home_team))
        )
      
      if (nrow(team_games) > 0) {
        actual_matchups <- team_games %>%
          mutate(matchup = paste(away_team, "@", home_team)) %>%
          pull(matchup)
        
        result$errors <- c(result$errors,
                           sprintf("Incorrect matchup. Teams actually play: %s",
                                  paste(actual_matchups, collapse=", ")))
      } else {
        result$errors <- c(result$errors,
                           "No official game found for these teams in this week")
      }
    }
  } else if (nrow(official_matches) > 1) {
    result$warnings <- c(result$warnings,
                         "Multiple official matches found (duplicate schedule data)")
    result$official_match <- official_matches[1, ]
  } else {
    result$official_match <- official_matches[1, ]
  }
  
  # Score validation (if scores provided)
  if ("home_score" %in% names(game) && !is.na(game$home_score)) {
    if (game$home_score < 0 || game$home_score > VALIDATION_CONFIG$max_score) {
      result$errors <- c(result$errors,
                         paste("Invalid home score:", game$home_score))
    }
  }
  
  if ("away_score" %in% names(game) && !is.na(game$away_score)) {
    if (game$away_score < 0 || game$away_score > VALIDATION_CONFIG$max_score) {
      result$errors <- c(result$errors,
                         paste("Invalid away score:", game$away_score))
    }
  }
  
  # Set overall validity
  result$valid <- length(result$errors) == 0
  
  return(result)
}

#' Clean Contaminated Data
#' 
#' Attempts to fix common data contamination issues by matching
#' against official schedule
#' 
#' @param contaminated_data Data frame with potentially contaminated data
#' @param season Season to validate against
#' @return List with cleaned data and cleaning report
clean_contaminated_data <- function(contaminated_data, season = 2025) {
  
  cat(sprintf("🧹 Cleaning potentially contaminated data (%d records)...\n", 
             nrow(contaminated_data)))
  
  official_schedule <- load_official_schedule(season)
  
  cleaning_report <- list(
    original_count = nrow(contaminated_data),
    cleaned_count = 0,
    removed_count = 0,
    corrected_count = 0,
    corrections = list()
  )
  
  cleaned_data <- contaminated_data
  
  # Track corrections
  for (i in 1:nrow(contaminated_data)) {
    game <- contaminated_data[i, ]
    
    # Try to find correct official matchup
    week_games <- official_schedule %>%
      filter(season == game$season, week == game$week)
    
    # Check if current matchup is valid
    valid_matchup <- week_games %>%
      filter(away_team == game$away_team, home_team == game$home_team)
    
    if (nrow(valid_matchup) == 0) {
      # Try to find correct matchup involving these teams
      correct_matchup <- week_games %>%
        filter(away_team %in% c(game$away_team, game$home_team) |
               home_team %in% c(game$away_team, game$home_team))
      
      if (nrow(correct_matchup) == 1) {
        # Found single correct matchup - apply correction
        old_matchup <- paste(game$away_team, "@", game$home_team)
        new_matchup <- paste(correct_matchup$away_team, "@", correct_matchup$home_team)
        
        cleaned_data[i, "away_team"] <- correct_matchup$away_team
        cleaned_data[i, "home_team"] <- correct_matchup$home_team
        
        cleaning_report$corrections[[length(cleaning_report$corrections) + 1]] <- list(
          row = i,
          old = old_matchup,
          new = new_matchup,
          reason = "Matched to official schedule"
        )
        
        cleaning_report$corrected_count <- cleaning_report$corrected_count + 1
        
        cat(sprintf("🔧 Corrected: %s → %s\n", old_matchup, new_matchup))
      }
    }
  }
  
  # Remove any records that couldn't be corrected
  validation_result <- validate_game_schedule(cleaned_data, season, strict_validation = TRUE)
  
  if (validation_result$invalid_games > 0) {
    # Remove invalid games
    invalid_indices <- which(!sapply(validation_result$details, function(x) x$valid))
    cleaned_data <- cleaned_data[-invalid_indices, ]
    cleaning_report$removed_count <- length(invalid_indices)
    
    cat(sprintf("🗑️  Removed %d uncorrectable records\n", cleaning_report$removed_count))
  }
  
  cleaning_report$cleaned_count <- nrow(cleaned_data)
  
  cat(sprintf("✅ Cleaning complete: %d → %d records (%d corrected, %d removed)\n",
             cleaning_report$original_count, cleaning_report$cleaned_count,
             cleaning_report$corrected_count, cleaning_report$removed_count))
  
  return(list(
    cleaned_data = cleaned_data,
    report = cleaning_report
  ))
}

#' Validate Week 3 2025 Schedule
#' 
#' Specific validation for Week 3 2025 to address the current contamination issue
#' 
#' @return Validation report for Week 3 2025
validate_week3_2025 <- function() {
  
  cat("🎯 VALIDATING WEEK 3 2025 SCHEDULE\n")
  cat("==================================\n")
  
  # Load official Week 3 schedule
  official_week3 <- load_official_schedule(2025) %>%
    filter(week == 3) %>%
    arrange(gameday, away_team)
  
  cat(sprintf("📋 Official Week 3 2025 games: %d\n", nrow(official_week3)))
  
  # Display official matchups
  cat("\n🏈 OFFICIAL WEEK 3 MATCHUPS:\n")
  for (i in 1:nrow(official_week3)) {
    game <- official_week3[i, ]
    cat(sprintf("   %s @ %s (%s)\n", 
               game$away_team, game$home_team, 
               format(game$gameday, "%a %b %d")))
  }
  
  # Check stored predictions
  if (file.exists("learning_system/predictions_tracking.csv")) {
    stored_predictions <- read.csv("learning_system/predictions_tracking.csv", stringsAsFactors = FALSE)
    stored_week3 <- stored_predictions %>% filter(week == 3, season == 2025)
    
    cat(sprintf("\n📊 Stored Week 3 predictions: %d\n", nrow(stored_week3)))
    
    if (nrow(stored_week3) > 0) {
      # Validate stored predictions
      validation_result <- validate_game_schedule(stored_week3, 2025, strict_validation = TRUE)
      
      if (!validation_result$valid) {
        cat("\n❌ CONTAMINATION DETECTED IN STORED DATA:\n")
        for (error in validation_result$errors) {
          cat(sprintf("   • %s\n", error))
        }
        
        # Attempt cleaning
        cat("\n🧹 Attempting to clean contaminated data...\n")
        cleaning_result <- clean_contaminated_data(stored_week3, 2025)
        
        return(list(
          official_schedule = official_week3,
          contaminated_data = stored_week3,
          cleaning_result = cleaning_result,
          validation = validation_result
        ))
      } else {
        cat("\n✅ Stored Week 3 data is valid!\n")
      }
    }
  } else {
    cat("\n⚠️  No stored predictions file found\n")
  }
  
  return(list(
    official_schedule = official_week3,
    status = "validated"
  ))
}

#' Get Validation History
#' 
#' Returns history of validation operations
#' 
#' @return List of validation history entries
get_validation_history <- function() {
  return(.validation_env$validation_history)
}

cat("🛡️  Data Validation Layer loaded successfully\n")
cat("Use validate_week3_2025() to check current data integrity\n")