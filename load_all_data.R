# Comprehensive NFL Data Loading Script
# Load data for 2023, 2024, and 2025 seasons

library(nflreadr)

# Function to safely load data with error handling
safe_load <- function(load_func, season, data_type) {
  tryCatch({
    data <- load_func(season)
    cat(sprintf("%s %d: %d rows\n", data_type, season, nrow(data)))
    return(data)
  }, error = function(e) {
    cat(sprintf("%s %d: Failed to load - %s\n", data_type, season, e$message))
    return(NULL)
  })
}

# Test seasons
seasons <- c(2023, 2024, 2025)

cat("=== NFL DATA AVAILABILITY TEST ===\n\n")

# Test schedules for all seasons
cat("SCHEDULES:\n")
schedules <- list()
for (season in seasons) {
  schedules[[as.character(season)]] <- safe_load(load_schedules, season, "Schedule")
}

cat("\nPLAY-BY-PLAY DATA:\n")
pbp_data <- list()
for (season in seasons) {
  # Only test 2024 and 2025 first (2023 might be large)
  if (season >= 2024) {
    pbp_data[[as.character(season)]] <- safe_load(load_pbp, season, "Play-by-Play")
  } else {
    cat(sprintf("Play-by-Play %d: Skipping large dataset for now\n", season))
  }
}

cat("\nPLAYER STATS:\n")
stats_data <- list()
for (season in seasons) {
  stats_data[[as.character(season)]] <- safe_load(load_player_stats, season, "Player Stats")
}

cat("\nROSTERS:\n")
roster_data <- list()
for (season in seasons) {
  roster_data[[as.character(season)]] <- safe_load(load_rosters, season, "Rosters")
}

cat("\n=== SUMMARY ===\n")
cat("Data successfully loaded for seasons with available data.\n")
cat("Use the loaded data objects: schedules, pbp_data, stats_data, roster_data\n")