# Fallback Data Access Strategy for nflfastR
# Alternative approaches when nflreadr has connectivity issues

library(httr)
library(jsonlite)

# Create mock advanced metrics for development
create_mock_advanced_metrics <- function(seasons = 2022:2024) {
  
  cat("🔧 Creating mock advanced metrics for development...\n")
  
  # NFL team abbreviations
  nfl_teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", 
                 "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", 
                 "LV", "LAC", "LAR", "MIA", "MIN", "NE", "NO", "NYG", 
                 "NYJ", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS")
  
  set.seed(42)  # Reproducible mock data
  
  mock_data <- data.frame()
  
  for (season in seasons) {
    cat(sprintf("Generating mock data for %d season...\n", season))
    
    # Generate mock play-by-play records (approximating real volume)
    n_plays <- sample(45000:50000, 1)  # Typical season play count
    
    season_data <- data.frame(
      season = season,
      week = sample(1:18, n_plays, replace = TRUE),
      game_id = sample(1000:9999, n_plays, replace = TRUE),
      home_team = sample(nfl_teams, n_plays, replace = TRUE),
      away_team = sample(nfl_teams, n_plays, replace = TRUE),
      
      # Basic play info
      play_type = sample(c("pass", "run", "punt", "field_goal", "kickoff"), 
                         n_plays, replace = TRUE, prob = c(0.4, 0.4, 0.08, 0.05, 0.07)),
      down = sample(1:4, n_plays, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),
      ydstogo = pmax(1, pmin(20, round(rnorm(n_plays, 8, 4)))),
      yardline_100 = sample(1:99, n_plays, replace = TRUE),
      
      # EPA metrics (realistic distributions based on nflfastR patterns)
      epa = rnorm(n_plays, 0, 2.5),  # EPA typically centers around 0
      wpa = rnorm(n_plays, 0, 0.1),  # Win Probability Added
      
      # Advanced metrics for ML
      cpoe = rnorm(n_plays, 0, 15),  # Completion % Over Expected
      xyac_epa = rnorm(n_plays, 0, 1.2),  # Expected YAC EPA
      xyac_mean_yardage = pmax(0, rnorm(n_plays, 5, 3)),
      air_epa = rnorm(n_plays, 0, 1.8),
      yac_epa = rnorm(n_plays, 0, 1.5),
      
      # Success metrics
      success = rbinom(n_plays, 1, 0.45),  # ~45% success rate
      first_down = rbinom(n_plays, 1, 0.35),
      
      # Situational factors
      qtr = sample(1:4, n_plays, replace = TRUE),
      goal_to_go = rbinom(n_plays, 1, 0.12),
      red_zone = rbinom(n_plays, 1, 0.15),
      shotgun = rbinom(n_plays, 1, 0.6),
      
      # Weather (some missing as in real data)
      temp = ifelse(runif(n_plays) < 0.3, NA, sample(20:85, n_plays, replace = TRUE)),
      wind = ifelse(runif(n_plays) < 0.4, NA, sample(0:25, n_plays, replace = TRUE)),
      
      # Score context
      score_differential = sample(-35:35, n_plays, replace = TRUE),
      
      # Win probability
      wp = pmax(0, pmin(1, rnorm(n_plays, 0.5, 0.25))),
      home_wp = pmax(0, pmin(1, rnorm(n_plays, 0.5, 0.25)))
    )
    
    # Add some realistic correlations
    season_data$away_wp <- 1 - season_data$home_wp
    season_data$def_wp <- 1 - season_data$wp
    
    mock_data <- rbind(mock_data, season_data)
  }
  
  cat(sprintf("✅ Mock data generated: %s plays across %d seasons\n", 
              format(nrow(mock_data), big.mark = ","), length(seasons)))
  
  return(mock_data)
}

# Enhanced mock data with team-specific tendencies
create_enhanced_mock_data <- function() {
  
  cat("🎯 Creating enhanced mock data with team tendencies...\n")
  
  # Get basic mock data
  mock_data <- create_mock_advanced_metrics(2020:2024)
  
  # Add team-specific EPA tendencies (based on general NFL knowledge)
  elite_offenses <- c("KC", "BUF", "SF", "DAL", "MIA", "PHI")
  elite_defenses <- c("SF", "BUF", "DAL", "NE", "BAL", "NYJ")
  
  # Adjust EPA based on team quality
  mock_data$epa <- ifelse(
    mock_data$home_team %in% elite_offenses | mock_data$away_team %in% elite_offenses,
    mock_data$epa + rnorm(nrow(mock_data), 0.2, 0.1),  # Boost for elite offenses
    mock_data$epa
  )
  
  mock_data$epa <- ifelse(
    mock_data$home_team %in% elite_defenses | mock_data$away_team %in% elite_defenses,
    mock_data$epa - rnorm(nrow(mock_data), 0.15, 0.1),  # Penalty for elite defenses
    mock_data$epa
  )
  
  # Add more realistic CPOE for passing plays
  mock_data$cpoe <- ifelse(
    mock_data$play_type == "pass",
    rnorm(nrow(mock_data), 0, 12),  # More realistic CPOE range
    NA
  )
  
  # Add drive success correlation
  mock_data$series_success <- rbinom(nrow(mock_data), 1, 
                                     pmax(0.2, pmin(0.8, 0.4 + mock_data$epa * 0.05)))
  
  cat("✅ Enhanced mock data with realistic team tendencies\n")
  return(mock_data)
}

# Alternative direct download approach
try_direct_download <- function(year = 2024) {
  
  cat("📥 Attempting direct download from nflverse releases...\n")
  
  # Try direct GitHub releases URL
  base_url <- "https://github.com/nflverse/nflverse-data/releases/download/pbp"
  file_url <- sprintf("%s/play_by_play_%d.rds", base_url, year)
  
  tryCatch({
    cat(sprintf("Downloading %d data from: %s\n", year, file_url))
    
    # Try to download the file
    temp_file <- tempfile(fileext = ".rds")
    download.file(file_url, temp_file, mode = "wb", quiet = TRUE)
    
    # Try to read it
    pbp_data <- readRDS(temp_file)
    
    cat(sprintf("✅ Direct download successful: %s plays\n", 
                format(nrow(pbp_data), big.mark = ",")))
    
    unlink(temp_file)
    return(pbp_data)
    
  }, error = function(e) {
    cat(sprintf("❌ Direct download failed: %s\n", e$message))
    return(NULL)
  })
}

# Comprehensive fallback strategy
setup_fallback_data_access <- function() {
  
  cat("🔄 Setting up fallback data access strategy...\n")
  
  # Strategy 1: Try direct download for recent seasons
  real_data <- NULL
  for (year in 2023:2024) {
    year_data <- try_direct_download(year)
    if (!is.null(year_data)) {
      if (is.null(real_data)) {
        real_data <- year_data
      } else {
        real_data <- rbind(real_data, year_data)
      }
    }
  }
  
  # Strategy 2: Use enhanced mock data if needed
  if (is.null(real_data) || nrow(real_data) < 10000) {
    cat("⚠️  Real data unavailable, using enhanced mock data for development\n")
    
    mock_data <- create_enhanced_mock_data()
    
    return(list(
      data = mock_data,
      source = "mock",
      note = "Using mock data due to nflverse connectivity issues"
    ))
  } else {
    cat("✅ Using real nflverse data\n")
    
    return(list(
      data = real_data,
      source = "nflverse_direct",
      note = "Successfully downloaded from nflverse releases"
    ))
  }
}

# Test the fallback system
test_fallback_system <- function() {
  
  cat("🧪 Testing fallback data access system...\n")
  
  result <- setup_fallback_data_access()
  
  if (!is.null(result$data)) {
    data <- result$data
    
    cat(sprintf("\n📊 Fallback System Results (%s):\n", result$source))
    cat(sprintf("- Total records: %s\n", format(nrow(data), big.mark = ",")))
    cat(sprintf("- Seasons: %s\n", paste(sort(unique(data$season)), collapse = ", ")))
    cat(sprintf("- Advanced features: %d\n", sum(!is.na(names(data)[grepl("epa|cpoe|xyac", names(data))]))))
    cat(sprintf("- Note: %s\n", result$note))
    
    # Show feature availability
    key_features <- c("epa", "cpoe", "xyac_epa", "wpa", "success")
    available_features <- key_features[key_features %in% names(data)]
    
    cat(sprintf("\n🎯 Key ML features available: %s\n", 
                paste(available_features, collapse = ", ")))
    
    return(result)
  } else {
    cat("❌ All fallback strategies failed\n")
    return(NULL)
  }
}

cat("Fallback Data Access Strategy loaded! 🔄\n\n")
cat("Available functions:\n")
cat("- setup_fallback_data_access(): Complete fallback setup\n")
cat("- create_enhanced_mock_data(): Enhanced mock data with team tendencies\n")
cat("- try_direct_download(year): Try direct GitHub download\n") 
cat("- test_fallback_system(): Test all fallback approaches\n")
cat("\nTo get started: test_fallback_system()\n")