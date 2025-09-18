# NFL Data Pipeline - Reliable Data Ingestion & Validation
# Fixes contamination and environment inconsistency issues

library(DBI)
library(RSQLite) 
library(dplyr)

# Reliable data ingestion with validation
ingest_nfl_schedule <- function(season = 2025, force_refresh = FALSE) {
  
  cat("📊 INGESTING NFL SCHEDULE DATA (WITH VALIDATION)\n")
  cat("===============================================\n")
  
  # Connect to database
  con <- dbConnect(SQLite(), "data_pipeline/nfl_predictions.db")
  on.exit(dbDisconnect(con), add = TRUE)
  
  # Check if data already exists (unless force refresh)
  if (!force_refresh) {
    existing_games <- dbGetQuery(con, 
      "SELECT COUNT(*) as count FROM games WHERE season = ?", 
      params = list(season))
    
    if (existing_games$count > 0) {
      cat(sprintf("✅ Schedule data already exists for %d (%d games)\n", 
                 season, existing_games$count))
      return(TRUE)
    }
  }
  
  # Try multiple data sources with fallbacks
  schedule_data <- NULL
  data_source <- NULL
  
  # Source 1: nflreadr (primary)
  if (requireNamespace("nflreadr", quietly = TRUE)) {
    cat("📡 Trying nflreadr...\n")
    tryCatch({
      schedule_data <- nflreadr::load_schedules(seasons = season)
      data_source <- "nflreadr"
      cat("✅ nflreadr: Success\n")
    }, error = function(e) {
      cat("❌ nflreadr failed:", e$message, "\n")
    })
  }
  
  # Source 2: nflfastR (fallback)
  if (is.null(schedule_data) && requireNamespace("nflfastR", quietly = TRUE)) {
    cat("📡 Trying nflfastR fallback...\n")
    tryCatch({
      # nflfastR doesn't have direct schedule loading, would need different approach
      cat("⚠️ nflfastR fallback not implemented\n")
    }, error = function(e) {
      cat("❌ nflfastR fallback failed:", e$message, "\n")
    })
  }
  
  # Source 3: Local cache (ultimate fallback)
  if (is.null(schedule_data)) {
    cat("📁 Trying local cache...\n")
    cache_file <- sprintf("data_pipeline/cache/schedule_%d.rds", season)
    if (file.exists(cache_file)) {
      tryCatch({
        schedule_data <- readRDS(cache_file)
        data_source <- "local_cache"
        cat("✅ Local cache: Success\n")
      }, error = function(e) {
        cat("❌ Local cache failed:", e$message, "\n")
      })
    } else {
      cat("❌ No local cache available\n")
    }
  }
  
  if (is.null(schedule_data)) {
    log_validation("data_ingestion", "failed", "All data sources unavailable")
    return(FALSE)
  }
  
  # VALIDATION LAYER - Prevent contamination
  validation_result <- validate_schedule_data(schedule_data, season)
  if (!validation_result$valid) {
    cat("❌ Schedule validation failed:", validation_result$error, "\n")
    log_validation("schedule_validation", "failed", validation_result$error)
    return(FALSE)
  }
  
  # Insert validated data into database
  insert_result <- insert_schedule_data(con, schedule_data, data_source)
  if (insert_result) {
    # Cache successful data for fallback
    cache_schedule_data(schedule_data, season)
    
    cat(sprintf("✅ Successfully ingested %d games for %d season\n", 
               nrow(schedule_data), season))
    log_validation("data_ingestion", "success", 
                  sprintf("%d games ingested from %s", nrow(schedule_data), data_source))
    return(TRUE)
  } else {
    log_validation("data_ingestion", "failed", "Database insertion failed")
    return(FALSE)
  }
}

# Schedule data validation (prevents contamination)
validate_schedule_data <- function(schedule_data, season) {
  
  # Check 1: Required columns exist
  required_cols <- c("game_id", "season", "week", "away_team", "home_team", "gameday")
  missing_cols <- setdiff(required_cols, names(schedule_data))
  
  if (length(missing_cols) > 0) {
    return(list(valid = FALSE, error = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
  }
  
  # Check 2: Season matches expectation
  if (!season %in% unique(schedule_data$season)) {
    return(list(valid = FALSE, error = paste("Expected season", season, "not found in data")))
  }
  
  # Check 3: Reasonable number of games
  season_games <- schedule_data[schedule_data$season == season, ]
  expected_games <- 272  # 17 weeks * 16 games per week
  
  if (nrow(season_games) < 200 || nrow(season_games) > 300) {
    return(list(valid = FALSE, error = paste("Unreasonable game count:", nrow(season_games))))
  }
  
  # Check 4: Valid team abbreviations
  all_teams <- unique(c(schedule_data$away_team, schedule_data$home_team))
  expected_teams <- 32
  
  if (length(all_teams) < 30 || length(all_teams) > 35) {
    return(list(valid = FALSE, error = paste("Unexpected team count:", length(all_teams))))
  }
  
  # Check 5: Known contaminated matchups (specific to our problem)
  week3_games <- season_games[season_games$week == 3, ]
  if (nrow(week3_games) > 0) {
    contaminated_matchups <- c(
      "ATL@KC",  # Should be KC@NYG
      "DAL@BAL", # Should be DET@BAL
      "MIA@SEA"  # Should be NO@SEA
    )
    
    for (i in 1:nrow(week3_games)) {
      game <- week3_games[i, ]
      matchup <- paste0(game$away_team, "@", game$home_team)
      
      if (matchup %in% contaminated_matchups) {
        return(list(valid = FALSE, error = paste("Contaminated matchup detected:", matchup)))
      }
    }
  }
  
  # Check 6: Validate specific known correct matchups for Week 3 2025
  if (season == 2025 && nrow(week3_games) > 0) {
    expected_week3 <- c("KC@NYG", "DET@BAL", "MIA@BUF", "CIN@MIN")
    
    actual_matchups <- sapply(1:nrow(week3_games), function(i) {
      game <- week3_games[i, ]
      paste0(game$away_team, "@", game$home_team)
    })
    
    found_expected <- sum(expected_week3 %in% actual_matchups)
    if (found_expected < 2) {  # At least 2 expected matchups should be present
      return(list(valid = FALSE, error = "Expected Week 3 2025 matchups not found"))
    }
  }
  
  return(list(valid = TRUE, error = NULL))
}

# Insert validated schedule data
insert_schedule_data <- function(con, schedule_data, data_source) {
  tryCatch({
    # Clear existing data for this season (if force refresh)
    seasons_in_data <- unique(schedule_data$season)
    for (season in seasons_in_data) {
      dbExecute(con, "DELETE FROM games WHERE season = ?", params = list(season))
    }
    
    # Prepare data for insertion
    insert_data <- schedule_data %>%
      select(game_id, season, week, gameday, away_team, home_team, away_score, home_score) %>%
      mutate(
        game_date = as.character(gameday),
        completed = !is.na(away_score) & !is.na(home_score),
        data_source = data_source
      ) %>%
      select(-gameday)  # Remove original gameday column
    
    # Insert into database
    dbWriteTable(con, "games", insert_data, append = TRUE, row.names = FALSE)
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Database insertion error:", e$message, "\n")
    return(FALSE)
  })
}

# Cache schedule data for fallback
cache_schedule_data <- function(schedule_data, season) {
  tryCatch({
    cache_dir <- "data_pipeline/cache"
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    
    cache_file <- file.path(cache_dir, sprintf("schedule_%d.rds", season))
    saveRDS(schedule_data, cache_file)
    
    cat("💾 Schedule data cached for fallback\n")
    
  }, error = function(e) {
    cat("⚠️ Could not cache data:", e$message, "\n")
  })
}

# Get validated schedule from database
get_validated_schedule <- function(season = 2025, week = NULL) {
  tryCatch({
    con <- dbConnect(SQLite(), "data_pipeline/nfl_predictions.db")
    on.exit(dbDisconnect(con), add = TRUE)
    
    query <- "SELECT * FROM games WHERE season = ?"
    params <- list(season)
    
    if (!is.null(week)) {
      query <- paste(query, "AND week = ?")
      params <- append(params, week)
    }
    
    query <- paste(query, "ORDER BY week, game_date")
    
    result <- dbGetQuery(con, query, params = params)
    return(result)
    
  }, error = function(e) {
    cat("❌ Could not retrieve schedule:", e$message, "\n")
    return(NULL)
  })
}

# Log validation results
log_validation <- function(type, status, details = NULL) {
  tryCatch({
    con <- dbConnect(SQLite(), "data_pipeline/nfl_predictions.db")
    on.exit(dbDisconnect(con), add = TRUE)
    
    dbExecute(con, 
      "INSERT INTO validation_log (validation_type, status, details) VALUES (?, ?, ?)",
      params = list(type, status, details))
    
  }, error = function(e) {
    # Silent fail for logging - don't break pipeline
  })
}

cat("📊 Data ingestion and validation layer ready!\n")
cat("Usage: ingest_nfl_schedule(2025) # Loads and validates 2025 schedule\n")
cat("       get_validated_schedule(2025, 3) # Get validated Week 3 games\n")