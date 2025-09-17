# Get Real Week 3 2025 NFL Schedule
# Using ESPN API endpoints from the reference guide

library(httr)
library(jsonlite)
library(dplyr)

get_week3_2025_schedule <- function() {
  cat("🔍 Getting real Week 3 2025 NFL schedule...\n")
  
  # Try ESPN scoreboard API for Week 3 2025
  url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?dates=2025&seasontype=2&week=3"
  
  tryCatch({
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      
      if (length(data$events) > 0) {
        games <- data.frame(
          away_team = character(),
          home_team = character(),
          game_date = character(),
          stringsAsFactors = FALSE
        )
        
        for (i in 1:length(data$events)) {
          event <- data$events[[i]]
          competitors <- event$competitions[[1]]$competitors
          
          # Find away and home teams
          away_idx <- which(competitors$homeAway == "away")
          home_idx <- which(competitors$homeAway == "home")
          
          if (length(away_idx) > 0 && length(home_idx) > 0) {
            away_team <- competitors$team$abbreviation[away_idx]
            home_team <- competitors$team$abbreviation[home_idx]
            game_date <- as.Date(event$date)
            
            games <- rbind(games, data.frame(
              away_team = away_team,
              home_team = home_team,
              game_date = game_date,
              stringsAsFactors = FALSE
            ))
          }
        }
        
        cat("✅ Found", nrow(games), "Week 3 2025 games\n")
        return(games)
      }
    }
    
    cat("❌ ESPN API failed with status:", status_code(response), "\n")
    return(NULL)
    
  }, error = function(e) {
    cat("❌ Error accessing ESPN API:", e$message, "\n")
    return(NULL)
  })
}

# Alternative: Try ESPN events API
get_week3_events <- function() {
  cat("🔍 Trying ESPN events API...\n")
  
  url <- "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/2025/types/2/weeks/3/events"
  
  tryCatch({
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      cat("✅ Events API successful\n")
      print(str(data))
      return(data)
    } else {
      cat("❌ Events API failed with status:", status_code(response), "\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("❌ Events API error:", e$message, "\n")
    return(NULL)
  })
}

# Run both attempts
cat("=== GETTING REAL WEEK 3 2025 SCHEDULE ===\n")

# Try scoreboard first
schedule <- get_week3_2025_schedule()

if (is.null(schedule)) {
  # Try events API as backup
  events <- get_week3_events()
}

if (!is.null(schedule)) {
  cat("\n📋 Week 3 2025 Schedule:\n")
  for (i in 1:nrow(schedule)) {
    cat(sprintf("%s @ %s (%s)\n", 
                schedule$away_team[i], 
                schedule$home_team[i], 
                schedule$game_date[i]))
  }
  
  # Check for KC @ NYJ specifically
  kc_game <- schedule[schedule$away_team == "KC" | schedule$home_team == "KC", ]
  if (nrow(kc_game) > 0) {
    cat("\n🏈 Kansas City game:\n")
    print(kc_game)
  } else {
    cat("\n⚠️  No Kansas City game found in schedule\n")
  }
} else {
  cat("\n❌ Could not retrieve real schedule data\n")
  cat("📝 Will need to manually verify Week 3 matchups\n")
}