# Unified NFL Data Access System
# Comprehensive data loading from all available nflverse sources
# Prioritizes actual game results and real 2025 data

library(dplyr)
library(lubridate)

# Try to load nflverse packages if available
nflfastR_available <- FALSE
nflreadr_available <- FALSE

tryCatch({
  library(nflfastR)
  nflfastR_available <- TRUE
  cat("✅ nflfastR loaded\n")
}, error = function(e) {
  cat("⚠️ nflfastR not available\n")
})

tryCatch({
  library(nflreadr) 
  nflreadr_available <- TRUE
  cat("✅ nflreadr loaded\n")
}, error = function(e) {
  cat("⚠️ nflreadr not available\n")
})

# Global data cache for performance
.unified_data_cache <- new.env()

#' Load actual NFL game results from nfldata repository
#' 
#' Loads complete game results including 2025 actual scores from the 
#' nflverse/nfldata repository which contains verified game outcomes.
#' 
#' @param seasons Vector of seasons to load (default: 2020:2025)
#' @param completed_only Load only games with final scores (default: TRUE)
#' @param cache Use cached data if available (default: TRUE)
#' @return Data frame with actual game results
#' 
#' @examples
#' # Load all completed 2025 games
#' results_2025 <- load_actual_game_results(seasons = 2025)
#' 
#' # Load Week 1-2 2025 results (what we have so far)
#' recent_results <- load_actual_game_results(seasons = 2025, completed_only = TRUE)
load_actual_game_results <- function(seasons = 2020:2025, completed_only = TRUE, cache = TRUE) {
  
  cache_key <- paste("game_results", paste(seasons, collapse="-"), completed_only, sep="_")
  
  if (cache && exists(cache_key, envir = .unified_data_cache)) {
    cat("📋 Loading cached game results...\n")
    return(get(cache_key, envir = .unified_data_cache))
  }
  
  cat("🏈 Loading actual NFL game results...\n")
  
  # Try multiple sources for game results
  games_data <- NULL
  
  
  # Source 1: nflfastR/nflreadr (preferred if available)
  if (nflreadr_available) {
    cat("📊 Loading from nflreadr...\n")
    tryCatch({
      games_data <- nflreadr::load_schedules(seasons = seasons)
      if (!is.null(games_data) && nrow(games_data) > 0) {
        cat("✅ nflreadr: Loaded", nrow(games_data), "total games\n")
      }
    }, error = function(e) {
      cat("❌ nflreadr error:", e$message, "\n")
      games_data <- NULL
    })
  }
  
  # Source 2: nfldata repository (fallback)
  if (is.null(games_data)) {
  nfldata_path <- "/Users/trevor/Git/playground/nfldata/data/games.csv"
  if (file.exists(nfldata_path)) {
    cat("📊 Loading from nfldata repository...\n")
    tryCatch({
      games_data <- read.csv(nfldata_path)
      cat("✅ nfldata: Loaded", nrow(games_data), "total games\n")
    }, error = function(e) {
      cat("❌ nfldata error:", e$message, "\n")
    })
  }
  
  # Source 2: nflreadr as fallback
  if (is.null(games_data)) {
    cat("📡 Trying nflreadr as fallback...\n")
    tryCatch({
      games_data <- nflreadr::load_schedules(seasons = seasons)
      if (!is.null(games_data) && nrow(games_data) > 0) {
        cat("✅ nflreadr: Loaded", nrow(games_data), "games\n")
      }
    }, error = function(e) {
      cat("❌ nflreadr error:", e$message, "\n")
    })
  }
  
  if (is.null(games_data) || nrow(games_data) == 0) {
    cat("❌ No game data loaded from any source\n")
    return(NULL)
  }
  
  # Filter to requested seasons
  if (!is.null(seasons)) {
    games_data <- games_data[games_data$season %in% seasons,]
  }
  
  # Filter to completed games if requested
  if (completed_only) {
    # Check for actual scores (non-NA home_score and away_score)
    if ("home_score" %in% names(games_data) && "away_score" %in% names(games_data)) {
      completed_mask <- complete.cases(games_data$home_score, games_data$away_score)
      games_data <- games_data[completed_mask,]
      cat("📊 Filtered to", nrow(games_data), "completed games\n")
    } else {
      cat("⚠️  Score columns not found - returning all scheduled games\n")
    }
  }
  
  # Standardize columns and add computed fields
  if (nrow(games_data) > 0) {
    games_data <- games_data %>%
      mutate(
        # Ensure we have margin calculation
        margin = ifelse(!is.na(home_score) & !is.na(away_score), 
                       home_score - away_score, NA),
        
        # Add total points
        total_points = ifelse(!is.na(home_score) & !is.na(away_score),
                             home_score + away_score, NA),
        
        # Convert gameday to proper date
        game_date = as.Date(gameday),
        
        # Add season info for easy filtering
        season_type = ifelse(week <= 18, "regular", 
                            ifelse(week <= 22, "playoffs", "preseason"))
      ) %>%
      # Sort by date for easy analysis
      arrange(season, week, game_date)
  }
  
  # Cache the results
  if (cache) {
    assign(cache_key, games_data, envir = .unified_data_cache)
  }
  
  # Summary report
  if (nrow(games_data) > 0) {
    seasons_loaded <- sort(unique(games_data$season))
    cat("✅ Game results loaded successfully:\n")
    cat("- Seasons:", paste(seasons_loaded, collapse = ", "), "\n")
    cat("- Total games:", nrow(games_data), "\n")
    
    if (completed_only && any(!is.na(games_data$home_score))) {
      completed_by_season <- games_data %>%
        filter(!is.na(home_score)) %>%
        group_by(season) %>%
        summarise(completed_games = n(), .groups = 'drop')
      
      cat("- Completed games by season:\n")
      for (i in 1:nrow(completed_by_season)) {
        cat(sprintf("  %d: %d games\n", 
                   completed_by_season$season[i], 
                   completed_by_season$completed_games[i]))
      }
      
      # Highlight 2025 data
      if (2025 %in% seasons_loaded) {
        games_2025 <- games_data[games_data$season == 2025 & !is.na(games_data$home_score),]
        if (nrow(games_2025) > 0) {
          weeks_2025 <- sort(unique(games_2025$week))
          cat(sprintf("🎯 2025 Season: %d completed games in weeks %s\n", 
                     nrow(games_2025), paste(weeks_2025, collapse = ", ")))
        }
      }
    }
  }
  
  return(games_data)
}

#' Load comprehensive nflverse play-by-play data
#' 
#' Loads play-by-play data from multiple sources with advanced metrics
#' for machine learning and analysis. Combines local files and nflreadr.
#' 
#' @param seasons Vector of seasons to load
#' @param include_2025_partial Include partial 2025 data if available
#' @return Data frame with play-by-play data including EPA metrics
load_comprehensive_pbp <- function(seasons = 2020:2024, include_2025_partial = TRUE) {
  
  cat("🤖 Loading comprehensive play-by-play data...\n")
  
  all_pbp <- list()
  
  # Load historical data (2020-2024) using existing local system
  for (season in seasons[seasons <= 2024]) {
    cat(sprintf("Loading season %d...\n", season))
    
    # Try local data first (we know 2024 works)
    if (season == 2024) {
      local_file <- "/Users/trevor/Downloads/play_by_play_2024.rds"
      if (file.exists(local_file)) {
        tryCatch({
          source('local_data_access.R')
          pbp_season <- load_local_nflfastr_data(local_file)
          if (!is.null(pbp_season)) {
            all_pbp[[as.character(season)]] <- pbp_season
            cat(sprintf("✅ Season %d: %s plays loaded (local)\n", season, 
                       format(nrow(pbp_season), big.mark = ",")))
          }
        }, error = function(e) {
          cat(sprintf("❌ Local load failed for %d: %s\n", season, e$message))
        })
      }
    }
    
    # Try nflreadr if local failed or for other seasons
    if (!as.character(season) %in% names(all_pbp)) {
      tryCatch({
        pbp_season <- nflreadr::load_pbp(seasons = season)
        if (!is.null(pbp_season) && nrow(pbp_season) > 0) {
          all_pbp[[as.character(season)]] <- pbp_season
          cat(sprintf("✅ Season %d: %s plays loaded (nflreadr)\n", season,
                     format(nrow(pbp_season), big.mark = ",")))
        }
      }, error = function(e) {
        cat(sprintf("❌ nflreadr failed for %d: %s\n", season, e$message))
      })
    }
  }
  
  # Try to load 2025 data if requested
  if (include_2025_partial && 2025 %in% seasons) {
    cat("Attempting to load 2025 partial data...\n")
    tryCatch({
      pbp_2025 <- nflreadr::load_pbp(seasons = 2025)
      if (!is.null(pbp_2025) && nrow(pbp_2025) > 0) {
        all_pbp[["2025"]] <- pbp_2025
        cat(sprintf("✅ 2025: %s plays loaded\n", format(nrow(pbp_2025), big.mark = ",")))
      } else {
        cat("⚠️  2025 play-by-play not yet available\n")
      }
    }, error = function(e) {
      cat("❌ 2025 data not available:", e$message, "\n")
    })
  }
  
  # Combine all loaded data
  if (length(all_pbp) > 0) {
    combined_pbp <- do.call(rbind, all_pbp)
    cat(sprintf("📊 Combined dataset: %s plays across %d seasons\n",
               format(nrow(combined_pbp), big.mark = ","), length(all_pbp)))
    
    return(combined_pbp)
  } else {
    cat("❌ No play-by-play data loaded\n")
    return(NULL)
  }
}

#' Get current NFL season status and data availability
#' 
#' @return List with season info and data availability status
get_nfl_season_status <- function() {
  
  cat("📅 Checking NFL season status...\n")
  
  # Load actual game results to check current status
  current_results <- load_actual_game_results(seasons = 2025, completed_only = TRUE)
  
  status <- list(
    current_season = 2025,
    current_date = Sys.Date(),
    data_sources_available = c("nfldata_repository", "nflreadr_package")
  )
  
  if (!is.null(current_results) && nrow(current_results) > 0) {
    latest_game <- current_results[order(current_results$game_date, decreasing = TRUE),][1,]
    completed_weeks <- sort(unique(current_results$week))
    
    status$games_completed_2025 <- nrow(current_results)
    status$weeks_completed <- completed_weeks
    status$latest_game_date <- latest_game$game_date
    status$latest_game <- sprintf("%s @ %s (%d-%d)", 
                                 latest_game$away_team, latest_game$home_team,
                                 latest_game$away_score, latest_game$home_score)
  } else {
    status$games_completed_2025 <- 0
    status$weeks_completed <- c()
    status$note <- "No completed 2025 games found"
  }
  
  return(status)
}

#' Validate prediction against actual results
#' 
#' Takes a prediction and finds the corresponding actual game result
#' for immediate validation and learning system integration.
#' 
#' @param game_id Game identifier
#' @param home_team Home team abbreviation  
#' @param away_team Away team abbreviation
#' @param predicted_margin Predicted point spread (home team perspective)
#' @param predicted_total Predicted total points
#' @param confidence Model confidence (0-1)
#' @return List with validation results and performance metrics
validate_prediction_vs_actual <- function(game_id, home_team, away_team, 
                                         predicted_margin, predicted_total, confidence) {
  
  cat("🔍 Validating prediction against actual results...\n")
  
  # Load actual results
  actual_results <- load_actual_game_results(seasons = 2025, completed_only = TRUE)
  
  if (is.null(actual_results)) {
    return(list(
      validation_status = "no_data",
      message = "No actual results available for validation"
    ))
  }
  
  # Find matching game
  matching_game <- actual_results[
    actual_results$home_team == home_team & 
    actual_results$away_team == away_team,
  ]
  
  if (nrow(matching_game) == 0) {
    return(list(
      validation_status = "game_not_found",
      message = sprintf("Game %s @ %s not found in results", away_team, home_team)
    ))
  }
  
  if (nrow(matching_game) > 1) {
    # Take most recent if multiple matches
    matching_game <- matching_game[order(matching_game$game_date, decreasing = TRUE),][1,]
  }
  
  # Check if game is completed
  if (is.na(matching_game$home_score) || is.na(matching_game$away_score)) {
    return(list(
      validation_status = "game_not_completed",
      message = sprintf("Game %s @ %s not yet completed", away_team, home_team),
      scheduled_date = matching_game$gameday
    ))
  }
  
  # Calculate performance metrics
  actual_margin <- matching_game$home_score - matching_game$away_score
  actual_total <- matching_game$home_score + matching_game$away_score
  
  spread_error <- abs(predicted_margin - actual_margin)
  total_error <- abs(predicted_total - actual_total)
  
  # Determine prediction quality
  directional_correct <- sign(predicted_margin) == sign(actual_margin)
  
  validation_result <- list(
    validation_status = "completed",
    game_info = list(
      game_id = game_id,
      home_team = home_team,
      away_team = away_team,
      game_date = matching_game$gameday,
      week = matching_game$week
    ),
    predictions = list(
      predicted_margin = predicted_margin,
      predicted_total = predicted_total,
      confidence = confidence
    ),
    actual_results = list(
      home_score = matching_game$home_score,
      away_score = matching_game$away_score,
      actual_margin = actual_margin,
      actual_total = actual_total
    ),
    performance = list(
      spread_error = spread_error,
      total_error = total_error,
      directional_correct = directional_correct,
      confidence_calibration = abs(confidence - ifelse(directional_correct, 1, 0))
    )
  )
  
  # Print summary
  cat(sprintf("✅ Validation complete: %s @ %s\n", away_team, home_team))
  cat(sprintf("Predicted: %+.1f spread, %.1f total\n", predicted_margin, predicted_total))
  cat(sprintf("Actual: %+d spread, %d total\n", actual_margin, actual_total))
  cat(sprintf("Errors: %.1f spread, %.1f total\n", spread_error, total_error))
  cat(sprintf("Direction: %s\n", ifelse(directional_correct, "✅ Correct", "❌ Wrong")))
  
  return(validation_result)
}

# Clear cache function for testing
clear_data_cache <- function() {
  rm(list = ls(.unified_data_cache), envir = .unified_data_cache)
  cat("🗑️  Data cache cleared\n")
}


#' Load comprehensive play-by-play data using nflfastR
#' 
#' Enhanced data loading with nflfastR for detailed situational analysis
#' 
#' @param seasons Vector of seasons to load
#' @param include_epa Include EPA calculations (default: TRUE)
#' @param include_wp Include win probability (default: TRUE)
#' @param cache Use cached data (default: TRUE)
#' @return Enhanced play-by-play data frame
load_enhanced_pbp_data <- function(seasons = 2024:2025, include_epa = TRUE, include_wp = TRUE, cache = TRUE) {
  
  cache_key <- paste("enhanced_pbp", paste(seasons, collapse="-"), include_epa, include_wp, sep="_")
  
  if (cache && exists(cache_key, envir = .unified_data_cache)) {
    cat("📋 Loading cached enhanced PBP data...\n")
    return(get(cache_key, envir = .unified_data_cache))
  }
  
  if (!nflfastR_available) {
    cat("⚠️ nflfastR not available, using fallback data...\n")
    return(load_comprehensive_pbp(seasons = seasons))
  }
  
  cat("🏈 Loading enhanced play-by-play data with nflfastR...\n")
  
  tryCatch({
    # Load play-by-play data with full nflfastR enhancements
    pbp_data <- nflfastR::load_pbp(seasons = seasons)
    
    if (!is.null(pbp_data) && nrow(pbp_data) > 0) {
      cat("✅ nflfastR: Loaded", nrow(pbp_data), "plays\n")
      cat("📊 Enhanced columns available:", length(colnames(pbp_data)), "\n")
      
      # Cache the result
      if (cache) {
        assign(cache_key, pbp_data, envir = .unified_data_cache)
      }
      
      return(pbp_data)
    } else {
      cat("⚠️ No play-by-play data available\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("❌ nflfastR PBP loading failed:", e$message, "\n")
    cat("🔄 Falling back to existing system...\n")
    return(load_comprehensive_pbp(seasons = seasons))
  })
}

#' Get enhanced team statistics using nflfastR data
#' 
#' @param seasons Seasons to analyze
#' @param teams Vector of team abbreviations (optional)
#' @return Enhanced team statistics with EPA metrics
get_enhanced_team_stats <- function(seasons = 2024:2025, teams = NULL) {
  
  if (!nflfastR_available) {
    cat("⚠️ nflfastR not available for enhanced stats\n")
    return(NULL)
  }
  
  cat("📊 Calculating enhanced team statistics...\n")
  
  tryCatch({
    pbp_data <- load_enhanced_pbp_data(seasons = seasons)
    
    if (is.null(pbp_data)) {
      return(NULL)
    }
    
    # Filter to specified teams if provided
    if (!is.null(teams)) {
      pbp_data <- pbp_data %>%
        filter(posteam %in% teams | defteam %in% teams)
    }
    
    # Calculate comprehensive team stats
    team_stats <- pbp_data %>%
      filter(!is.na(epa), !is.na(posteam), play_type %in% c("pass", "run")) %>%
      group_by(posteam) %>%
      summarise(
        # Basic stats
        plays = n(),
        
        # EPA metrics
        epa_per_play = mean(epa, na.rm = TRUE),
        epa_per_pass = mean(epa[play_type == "pass"], na.rm = TRUE),
        epa_per_rush = mean(epa[play_type == "run"], na.rm = TRUE),
        
        # Success rates
        success_rate = mean(success, na.rm = TRUE),
        pass_success_rate = mean(success[play_type == "pass"], na.rm = TRUE),
        rush_success_rate = mean(success[play_type == "run"], na.rm = TRUE),
        
        # Situational metrics
        first_down_pass_rate = mean(play_type == "pass"[down == 1], na.rm = TRUE),
        third_down_conversion_rate = mean(first_down[down == 3], na.rm = TRUE),
        red_zone_success_rate = mean(success[yardline_100 <= 20], na.rm = TRUE),
        
        # Advanced metrics (if available)
        qb_epa = mean(epa[play_type == "pass"], na.rm = TRUE),
        explosive_play_rate = mean((play_type == "pass" & yards_gained >= 20) | 
                                  (play_type == "run" & yards_gained >= 10), na.rm = TRUE),
        
        .groups = 'drop'
      )
    
    # Calculate defensive stats
    def_stats <- pbp_data %>%
      filter(!is.na(epa), !is.na(defteam), play_type %in% c("pass", "run")) %>%
      group_by(defteam) %>%
      summarise(
        def_epa_per_play = mean(epa, na.rm = TRUE),
        def_success_rate_allowed = mean(success, na.rm = TRUE),
        def_explosive_plays_allowed = mean((play_type == "pass" & yards_gained >= 20) | 
                                          (play_type == "run" & yards_gained >= 10), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      rename(posteam = defteam)
    
    # Combine offensive and defensive stats
    enhanced_stats <- team_stats %>%
      left_join(def_stats, by = "posteam", suffix = c("", "_def"))
    
    cat("✅ Enhanced stats calculated for", nrow(enhanced_stats), "teams\n")
    
    return(enhanced_stats)
    
  }, error = function(e) {
    cat("❌ Enhanced stats calculation failed:", e$message, "\n")
    return(NULL)
  })
}
cat("Available functions:\n")
cat("- load_actual_game_results(): Load verified game results with 2025 data\n")
cat("- load_comprehensive_pbp(): Load play-by-play data from all sources\n")
cat("- get_nfl_season_status(): Check current season status and data availability\n")
cat("- validate_prediction_vs_actual(): Validate predictions against real results\n")

cat("\n🎯 Quick validation example:\n")
cat("status <- get_nfl_season_status()\n")
cat("results_2025 <- load_actual_game_results(seasons = 2025)\n")
if (nflfastR_available) {
  cat("enhanced_stats <- get_enhanced_team_stats(seasons = 2024, teams = c('LAC', 'KC'))\n")
}

# Auto-run status check
cat("\n📊 Current Status:\n")
tryCatch({
  status <- get_nfl_season_status()
  if (status$games_completed_2025 > 0) {
    cat(sprintf("✅ %d completed 2025 games available for validation\n", 
               status$games_completed_2025))
    cat(sprintf("Weeks completed: %s\n", paste(status$weeks_completed, collapse = ", ")))
  }
}, error = function(e) {
  cat("Status check failed:", e$message, "\n")
})

cat("\n✅ Unified Data Access System loaded successfully!\n")