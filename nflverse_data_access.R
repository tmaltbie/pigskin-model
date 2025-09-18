# nflverse Data Access Configuration
# Properly configure nflreadr for historical play-by-play data from 
# https://github.com/nflverse/nflverse-data/releases/tag/pbp

library(nflreadr)
library(dplyr)

# Configure nflreadr data access
configure_nflverse_data <- function() {
  
  cat("🏈 Configuring nflverse data access...\n")
  
  # Set options for better performance
  options(nflreadr.verbose = TRUE)
  options(nflreadr.cache = TRUE)
  
  # Test basic connectivity
  cat("📡 Testing nflreadr connectivity...\n")
  
  tryCatch({
    # Try to load a small dataset first - use correct nflreadr API
    test_data <- load_schedules(2024)
    cat("✅ nflreadr connectivity successful\n")
    
  }, error = function(e) {
    cat("❌ nflreadr connectivity failed:", e$message, "\n")
    cat("📝 Trying alternative data sources...\n")
  })
}

# Load historical play-by-play data efficiently
load_historical_pbp <- function(seasons = 2020:2024, cache = TRUE) {
  
  cat("📊 Loading historical play-by-play data for seasons:", paste(seasons, collapse = ", "), "\n")
  
  # Initialize list to store data
  pbp_data <- list()
  
  for (season in seasons) {
    cat(sprintf("Loading season %d...\n", season))
    
    tryCatch({
      # Load season data - use correct nflreadr API
      season_data <- load_pbp(seasons = season)
      
      if (nrow(season_data) > 0) {
        pbp_data[[as.character(season)]] <- season_data
        cat(sprintf("✅ Season %d: %s plays loaded\n", season, format(nrow(season_data), big.mark = ",")))
      } else {
        cat(sprintf("⚠️  Season %d: No data available\n", season))
      }
      
    }, error = function(e) {
      cat(sprintf("❌ Season %d failed: %s\n", season, e$message))
    })
    
    # Small delay to be respectful
    Sys.sleep(0.5)
  }
  
  # Combine all seasons
  if (length(pbp_data) > 0) {
    combined_data <- bind_rows(pbp_data)
    cat(sprintf("🎯 Total plays loaded: %s\n", format(nrow(combined_data), big.mark = ",")))
    return(combined_data)
  } else {
    cat("❌ No historical data loaded successfully\n")
    return(NULL)
  }
}

# Load specific advanced metrics for ML enhancement
load_advanced_metrics <- function(seasons = 2020:2024) {
  
  cat("🤖 Loading advanced nflfastR metrics for ML enhancement...\n")
  
  pbp_data <- load_historical_pbp(seasons)
  
  if (is.null(pbp_data)) {
    return(NULL)
  }
  
  # Extract key ML features
  advanced_features <- pbp_data %>%
    filter(!is.na(epa)) %>%
    select(
      # Game identifiers
      season, week, game_id, home_team, away_team,
      
      # Basic play info
      play_type, down, ydstogo, yardline_100, 
      game_seconds_remaining, half_seconds_remaining,
      
      # EPA metrics
      epa, wpa, 
      
      # Advanced metrics for ML
      cpoe,           # Completion Percentage Over Expected
      xyac_epa,       # Expected Yards After Catch EPA  
      xyac_mean_yardage, # Expected YAC yardage
      air_epa,        # Air yards EPA
      yac_epa,        # Yards after catch EPA
      comp_air_epa,   # Complete air EPA
      comp_yac_epa,   # Complete YAC EPA
      
      # Success metrics
      success,        # Successful play indicator
      first_down,     # First down achieved
      
      # Situational
      qtr, goal_to_go, red_zone, two_point_attempt,
      shotgun, no_huddle, 
      
      # Weather (when available)
      temp, wind, weather,
      
      # Score differential
      score_differential, score_differential_post,
      
      # Win probability
      wp, def_wp, home_wp, away_wp,
      
      # Drive information  
      drive, series, series_success, series_result
    )
  
  cat(sprintf("✅ Advanced features extracted: %s plays with %d features\n", 
              format(nrow(advanced_features), big.mark = ","),
              ncol(advanced_features)))
  
  return(advanced_features)
}

# Load roster and schedule data
load_supplemental_data <- function(seasons = 2020:2025) {
  
  cat("📋 Loading supplemental data (rosters, schedules)...\n")
  
  supplemental <- list()
  
  # Load rosters
  tryCatch({
    rosters <- load_rosters(seasons = seasons)
    supplemental$rosters <- rosters
    cat(sprintf("✅ Rosters: %s players loaded\n", format(nrow(rosters), big.mark = ",")))
  }, error = function(e) {
    cat("❌ Roster loading failed:", e$message, "\n")
  })
  
  # Load schedules
  tryCatch({
    schedules <- load_schedules(seasons = seasons)
    supplemental$schedules <- schedules
    cat(sprintf("✅ Schedules: %s games loaded\n", format(nrow(schedules), big.mark = ",")))
  }, error = function(e) {
    cat("❌ Schedule loading failed:", e$message, "\n")
  })
  
  # Load team stats if available
  tryCatch({
    team_stats <- load_team_stats(seasons = seasons)
    supplemental$team_stats <- team_stats
    cat(sprintf("✅ Team stats: %s records loaded\n", format(nrow(team_stats), big.mark = ",")))
  }, error = function(e) {
    cat("⚠️  Team stats not available:", e$message, "\n")
  })
  
  return(supplemental)
}

# Test the complete data pipeline
test_nflverse_pipeline <- function() {
  
  cat("🧪 Testing complete nflverse data pipeline...\n")
  
  # 1. Configure access
  configure_nflverse_data()
  
  # 2. Load recent seasons for testing
  test_seasons <- 2023:2024
  
  # 3. Test advanced metrics loading
  advanced_data <- load_advanced_metrics(seasons = test_seasons)
  
  # 4. Test supplemental data
  supplemental <- load_supplemental_data(seasons = test_seasons)
  
  # 5. Summary report
  if (!is.null(advanced_data)) {
    cat("\n📊 Pipeline Test Results:\n")
    cat(sprintf("- Play-by-play records: %s\n", format(nrow(advanced_data), big.mark = ",")))
    cat(sprintf("- Features available: %d\n", ncol(advanced_data)))
    cat(sprintf("- Seasons covered: %s\n", paste(unique(advanced_data$season), collapse = ", ")))
    
    # Show sample of advanced metrics
    metrics_sample <- advanced_data %>%
      filter(!is.na(cpoe) & !is.na(xyac_epa)) %>%
      head(5) %>%
      select(season, week, home_team, away_team, epa, cpoe, xyac_epa)
    
    cat("\n📋 Sample advanced metrics:\n")
    print(metrics_sample)
    
    return(list(
      pbp_data = advanced_data,
      supplemental = supplemental
    ))
  } else {
    cat("❌ Pipeline test failed - no data loaded\n")
    return(NULL)
  }
}

# Main function to set up everything
setup_nflverse_access <- function() {
  
  cat("🚀 Setting up complete nflverse data access...\n")
  
  # Run the test pipeline
  pipeline_result <- test_nflverse_pipeline()
  
  if (!is.null(pipeline_result)) {
    cat("\n✅ nflverse data access successfully configured!\n")
    cat("📝 Use load_advanced_metrics() and load_supplemental_data() for ML features\n")
    return(TRUE)
  } else {
    cat("\n❌ nflverse setup failed. Check internet connection and try again.\n")
    return(FALSE)
  }
}

# Export key functions
cat("nflverse Data Access loaded! 📊\n\n")
cat("Available functions:\n")
cat("- setup_nflverse_access(): Complete setup and test\n")
cat("- load_advanced_metrics(seasons): Load ML-ready features\n") 
cat("- load_supplemental_data(seasons): Load rosters/schedules\n")
cat("- test_nflverse_pipeline(): Test data access\n")
cat("\nTo get started: setup_nflverse_access()\n")