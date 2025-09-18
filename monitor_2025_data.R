# 2025 NFL Data Monitoring System
# Monitor for 2025 play-by-play data availability and integrate when ready

library(nflreadr)
library(dplyr)

# Check 2025 data availability
check_2025_data_availability <- function() {
  
  cat("🔍 Checking 2025 NFL play-by-play data availability...\n")
  
  tryCatch({
    # Try to load 2025 data
    pbp_2025 <- load_pbp(seasons = 2025)
    
    if (!is.null(pbp_2025) && nrow(pbp_2025) > 0) {
      cat(sprintf("✅ 2025 data available: %s plays\n", format(nrow(pbp_2025), big.mark = ",")))
      
      # Show week breakdown
      week_summary <- pbp_2025 %>%
        group_by(week) %>%
        summarise(
          games = length(unique(game_id)),
          plays = n(),
          .groups = 'drop'
        ) %>%
        arrange(week)
      
      cat("\n📊 Week-by-week breakdown:\n")
      print(week_summary)
      
      return(list(
        available = TRUE,
        data = pbp_2025,
        weeks_available = unique(pbp_2025$week)
      ))
    } else {
      cat("⚠️ 2025 data file exists but is empty (pre-season)\n")
      return(list(available = FALSE, data = NULL))
    }
    
  }, error = function(e) {
    cat("❌ Error checking 2025 data:", e$message, "\n")
    return(list(available = FALSE, data = NULL))
  })
}

# Monitor and integrate 2025 data with enhanced ML system
integrate_2025_data <- function() {
  
  cat("🚀 Attempting 2025 data integration...\n")
  
  # Check availability
  check_result <- check_2025_data_availability()
  
  if (check_result$available) {
    cat("📂 Processing 2025 data for ML system...\n")
    
    # Source the local data access functions
    source('local_data_access.R')
    
    # Extract ML features from 2025 data
    ml_features_2025 <- extract_ml_features(check_result$data)
    
    if (!is.null(ml_features_2025) && nrow(ml_features_2025) > 0) {
      
      # Save 2025 ML features locally
      saveRDS(ml_features_2025, "/Users/trevor/Downloads/ml_features_2025.rds")
      cat("💾 2025 ML features saved locally\n")
      
      # Update enhanced ML system
      source('enhanced_ml_system.R')
      
      cat("✅ 2025 data integration complete!\n")
      cat(sprintf("- ML features: %s records with %d columns\n", 
                  format(nrow(ml_features_2025), big.mark = ","), 
                  ncol(ml_features_2025)))
      
      # Test enhanced predictions with 2025 data
      cat("\n🧪 Testing enhanced predictions with 2025 data...\n")
      test_result <- test_ml_enhanced_system()
      
      return(list(
        integrated = TRUE,
        features_2025 = ml_features_2025,
        test_result = test_result
      ))
    }
  } else {
    cat("⏳ 2025 data not yet available. System ready for integration when data arrives.\n")
    cat("\n📝 To monitor manually:\n")
    cat("- Run check_2025_data_availability() regularly\n")
    cat("- Data typically available 15 minutes after games\n")
    cat("- Weekly updates on Wednesday nights\n")
    
    return(list(integrated = FALSE))
  }
}

# Weekly monitoring function (run after Wednesday night updates)
weekly_2025_update <- function() {
  
  cat("📅 Weekly 2025 data update check...\n")
  
  result <- integrate_2025_data()
  
  if (result$integrated) {
    cat("🎯 Enhanced ML predictions now using current 2025 data!\n")
    
    # Show latest week available
    if (!is.null(result$features_2025)) {
      latest_week <- max(result$features_2025$week, na.rm = TRUE)
      cat(sprintf("Latest week available: Week %d\n", latest_week))
    }
  }
  
  return(result)
}

# Setup automated monitoring (future enhancement)
setup_2025_monitoring <- function() {
  
  cat("⚙️ Setting up 2025 data monitoring system...\n")
  
  # Create monitoring configuration
  monitoring_config <- list(
    check_frequency = "daily",
    update_day = "wednesday_night", 
    auto_integrate = TRUE,
    backup_location = "/Users/trevor/Downloads/",
    notification = TRUE
  )
  
  # Save configuration
  saveRDS(monitoring_config, "/Users/trevor/Git/playground/nfl/monitoring_config.rds")
  
  cat("✅ Monitoring system configured\n")
  cat("📋 Manual check commands:\n")
  cat("- check_2025_data_availability(): Check if data is available\n")
  cat("- integrate_2025_data(): Full integration process\n") 
  cat("- weekly_2025_update(): Weekly update check\n")
  
  return(monitoring_config)
}

cat("2025 NFL Data Monitoring System loaded! 📡\n\n")
cat("Available functions:\n")
cat("- check_2025_data_availability(): Check current status\n")
cat("- integrate_2025_data(): Full integration when available\n")
cat("- weekly_2025_update(): Weekly update process\n")
cat("- setup_2025_monitoring(): Configure monitoring\n")
cat("\n🚀 Quick start: integrate_2025_data()\n")

# Note about season timing
cat("\n📅 2025 NFL Season Timeline:\n")
cat("- Regular season: September - January 2026\n")  
cat("- Playoffs: January - February 2026\n")
cat("- Data available 15 minutes after each game\n")
cat("- Best practice: Check Wednesday nights for weekly updates\n")