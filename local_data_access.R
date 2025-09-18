# Local nflfastR Data Access
# Use locally downloaded nflverse data files

library(dplyr)

# Load local nflfastR data
load_local_nflfastr_data <- function(file_path) {
  
  cat("📂 Loading local nflfastR data from:", file_path, "\n")
  
  tryCatch({
    pbp_data <- readRDS(file_path)
    
    cat(sprintf("✅ Loaded: %s records with %d columns\n", 
                format(nrow(pbp_data), big.mark = ","), ncol(pbp_data)))
    
    # Show available columns related to advanced metrics
    advanced_cols <- names(pbp_data)[grepl("epa|cpoe|xyac|wp|success", names(pbp_data), ignore.case = TRUE)]
    cat("🎯 Advanced metrics available:", paste(head(advanced_cols, 10), collapse = ", "), "\n")
    
    return(pbp_data)
    
  }, error = function(e) {
    cat("❌ Error loading local file:", e$message, "\n")
    return(NULL)
  })
}

# Extract ML-ready features from real nflfastR data
extract_ml_features <- function(pbp_data) {
  
  cat("🤖 Extracting ML features from real nflfastR data...\n")
  
  # Check what columns actually exist
  available_cols <- names(pbp_data)
  
  # Core columns that should exist
  core_cols <- c("season", "week", "game_id", "home_team", "away_team", "play_type")
  
  # Advanced metrics we want (check if they exist)
  desired_advanced <- c("epa", "wpa", "cpoe", "xyac_epa", "xyac_mean_yardage", 
                       "air_epa", "yac_epa", "success", "first_down")
  
  # Situational columns
  desired_situational <- c("down", "ydstogo", "yardline_100", "qtr", "goal_to_go", 
                          "shotgun", "no_huddle", "score_differential")
  
  # Win probability columns
  desired_wp <- c("wp", "def_wp", "home_wp", "away_wp")
  
  # Find which columns actually exist
  available_advanced <- desired_advanced[desired_advanced %in% available_cols]
  available_situational <- desired_situational[desired_situational %in% available_cols]
  available_wp <- desired_wp[desired_wp %in% available_cols]
  
  # Build selection list dynamically
  select_cols <- c(core_cols, available_advanced, available_situational, available_wp)
  select_cols <- select_cols[select_cols %in% available_cols]
  
  cat("📋 Selecting", length(select_cols), "available columns\n")
  cat("🎯 Advanced features:", paste(available_advanced, collapse = ", "), "\n")
  
  # Extract features
  ml_data <- pbp_data %>%
    filter(!is.na(epa)) %>%  # Only plays with EPA data
    select(all_of(select_cols))
  
  cat(sprintf("✅ ML features extracted: %s records with %d features\n",
              format(nrow(ml_data), big.mark = ","), ncol(ml_data)))
  
  return(ml_data)
}

# Test local data loading
test_local_data <- function(file_path = "/Users/trevor/Downloads/play_by_play_2024.rds") {
  
  cat("🧪 Testing local nflfastR data loading...\n")
  
  # Load the data
  pbp_data <- load_local_nflfastr_data(file_path)
  
  if (!is.null(pbp_data)) {
    # Extract ML features
    ml_data <- extract_ml_features(pbp_data)
    
    # Show sample
    if (nrow(ml_data) > 0) {
      cat("\n📊 Sample data:\n")
      sample_data <- ml_data %>% head(3)
      print(sample_data[, 1:min(8, ncol(sample_data))])
      
      # Show advanced metrics distribution
      cat("\n📈 Advanced metrics summary:\n")
      if ("epa" %in% names(ml_data)) {
        cat(sprintf("EPA: mean=%.3f, sd=%.3f\n", mean(ml_data$epa, na.rm=TRUE), sd(ml_data$epa, na.rm=TRUE)))
      }
      if ("cpoe" %in% names(ml_data)) {
        cat(sprintf("CPOE: mean=%.3f, sd=%.3f\n", mean(ml_data$cpoe, na.rm=TRUE), sd(ml_data$cpoe, na.rm=TRUE)))
      }
      
      return(ml_data)
    }
  }
  
  return(NULL)
}

cat("Local nflfastR Data Access loaded! 📂\n\n")
cat("Available functions:\n")
cat("- load_local_nflfastr_data(file_path): Load local .rds file\n")
cat("- extract_ml_features(pbp_data): Extract ML-ready features\n") 
cat("- test_local_data(file_path): Test complete local loading\n")
cat("\nTo test with your file: test_local_data('/Users/trevor/Downloads/play_by_play_2024.rds')\n")