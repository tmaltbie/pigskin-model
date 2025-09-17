# ESPN Odds API Integration
# Backup/alternative to The Odds API with real-time sportsbook data

library(httr)
library(jsonlite)
library(dplyr)

# Function to get NFL odds from ESPN
get_espn_nfl_odds <- function() {
  
  url <- "https://site.web.api.espn.com/apis/v3/sports/football/nfl/odds"
  
  tryCatch({
    response <- GET(url)
    
    if(response$status_code == 200) {
      content <- content(response, "parsed")
      cat(sprintf("✅ Retrieved ESPN odds data\n"))
      return(content)
    } else {
      cat(sprintf("❌ ESPN Odds API Error: %d\n", response$status_code))
      return(NULL)
    }
  }, error = function(e) {
    cat(sprintf("❌ Network error: %s\n", e$message))
    return(NULL)
  })
}

# Function to get detailed game odds from ESPN
get_espn_game_odds <- function(event_id) {
  
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/%s/competitions/%s/odds", 
                 event_id, event_id)
  
  tryCatch({
    response <- GET(url)
    
    if(response$status_code == 200) {
      content <- content(response, "parsed")
      return(content)
    } else {
      cat(sprintf("❌ Game odds error for %s: %d\n", event_id, response$status_code))
      return(NULL)
    }
  }, error = function(e) {
    cat(sprintf("❌ Network error for game %s: %s\n", event_id, e$message))
    return(NULL)
  })
}

# Function to parse ESPN odds into standard format
parse_espn_odds <- function(espn_odds_data) {
  
  if(is.null(espn_odds_data) || is.null(espn_odds_data$items) || length(espn_odds_data$items) == 0) {
    cat("No ESPN odds data to parse\n")
    return(data.frame())
  }
  
  games_list <- list()
  
  cat(sprintf("Parsing %d games from ESPN odds...\n", length(espn_odds_data$items)))
  
  for(i in 1:length(espn_odds_data$items)) {
    game_odds <- espn_odds_data$items[[i]]
    
    # Extract basic game info
    game_info <- list(
      source = "ESPN",
      game_date = if(!is.null(game_odds$date)) game_odds$date else NA,
      home_team = "Unknown",
      away_team = "Unknown",
      spread = NA,
      total = NA,
      home_ml = NA,
      away_ml = NA
    )
    
    # Extract team info if available
    if(!is.null(game_odds$competitors) && length(game_odds$competitors) >= 2) {
      for(comp in game_odds$competitors) {
        if(!is.null(comp$homeAway)) {
          team_name <- if(!is.null(comp$team) && !is.null(comp$team$abbreviation)) {
            comp$team$abbreviation
          } else {
            "Unknown"
          }
          
          if(comp$homeAway == "home") {
            game_info$home_team <- team_name
          } else {
            game_info$away_team <- team_name
          }
        }
      }
    }
    
    # Extract odds info
    if(!is.null(game_odds$odds) && length(game_odds$odds) > 0) {
      # Take first available odds (usually the consensus)
      odds_data <- game_odds$odds[[1]]
      
      if(!is.null(odds_data$spread)) {
        game_info$spread <- odds_data$spread
      }
      
      if(!is.null(odds_data$overUnder)) {
        game_info$total <- odds_data$overUnder
      }
      
      if(!is.null(odds_data$homeTeamOdds) && !is.null(odds_data$homeTeamOdds$moneyLine)) {
        game_info$home_ml <- odds_data$homeTeamOdds$moneyLine
      }
      
      if(!is.null(odds_data$awayTeamOdds) && !is.null(odds_data$awayTeamOdds$moneyLine)) {
        game_info$away_ml <- odds_data$awayTeamOdds$moneyLine
      }
    }
    
    games_list[[i]] <- game_info
  }
  
  # Convert to data frame
  if(length(games_list) > 0) {
    games_df <- do.call(rbind, lapply(games_list, function(x) {
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
    
    return(games_df)
  } else {
    return(data.frame())
  }
}

# Function to get current NFL scoreboard with odds
get_nfl_scoreboard_with_odds <- function() {
  
  url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard"
  
  tryCatch({
    response <- GET(url)
    
    if(response$status_code == 200) {
      content <- content(response, "parsed")
      
      if(!is.null(content$events) && length(content$events) > 0) {
        
        games_with_odds <- list()
        
        for(i in 1:length(content$events)) {
          event <- content$events[[i]]
          
          game_info <- list(
            event_id = event$id,
            game_date = event$date,
            status = event$status$type$description,
            home_team = "Unknown",
            away_team = "Unknown",
            home_score = 0,
            away_score = 0,
            spread = NA,
            total = NA
          )
          
          # Extract team info and scores
          if(!is.null(event$competitions) && length(event$competitions) > 0) {
            comp <- event$competitions[[1]]
            
            if(!is.null(comp$competitors) && length(comp$competitors) >= 2) {
              for(team in comp$competitors) {
                team_abbr <- if(!is.null(team$team) && !is.null(team$team$abbreviation)) {
                  team$team$abbreviation
                } else {
                  "UNK"
                }
                
                team_score <- if(!is.null(team$score)) {
                  as.numeric(team$score)
                } else {
                  0
                }
                
                if(!is.null(team$homeAway)) {
                  if(team$homeAway == "home") {
                    game_info$home_team <- team_abbr
                    game_info$home_score <- team_score
                  } else {
                    game_info$away_team <- team_abbr  
                    game_info$away_score <- team_score
                  }
                }
              }
            }
            
            # Extract odds if available
            if(!is.null(comp$odds) && length(comp$odds) > 0) {
              odds <- comp$odds[[1]]
              
              if(!is.null(odds$details)) {
                game_info$spread <- as.numeric(odds$details)
              }
              
              if(!is.null(odds$overUnder)) {
                game_info$total <- as.numeric(odds$overUnder)
              }
            }
          }
          
          games_with_odds[[i]] <- game_info
        }
        
        # Convert to dataframe
        games_df <- do.call(rbind, lapply(games_with_odds, function(x) {
          as.data.frame(x, stringsAsFactors = FALSE)
        }))
        
        cat(sprintf("✅ Retrieved %d games from ESPN scoreboard\n", nrow(games_df)))
        return(games_df)
        
      } else {
        cat("No events found in scoreboard\n")
        return(data.frame())
      }
    } else {
      cat(sprintf("❌ Scoreboard API Error: %d\n", response$status_code))
      return(data.frame())
    }
  }, error = function(e) {
    cat(sprintf("❌ Scoreboard error: %s\n", e$message))
    return(data.frame())
  })
}

# Function to create unified odds system with fallback
get_unified_nfl_odds <- function(prefer_source = "odds_api") {
  
  cat("=== UNIFIED NFL ODDS SYSTEM ===\n")
  
  # Try primary source first
  if(prefer_source == "odds_api") {
    cat("Trying The Odds API first...\n")
    
    # Check if odds_api.R functions are available
    if(exists("get_nfl_odds") && exists("parse_nfl_odds") && exists("get_consensus_spreads")) {
      tryCatch({
        raw_odds <- get_nfl_odds()
        if(!is.null(raw_odds) && length(raw_odds) > 0) {
          parsed_odds <- parse_nfl_odds(raw_odds)
          if(nrow(parsed_odds) > 0) {
            consensus_odds <- get_consensus_spreads(parsed_odds)
            if(nrow(consensus_odds) > 0) {
              cat("✅ Using The Odds API data\n")
              consensus_odds$source <- "The Odds API"
              return(consensus_odds)
            }
          }
        }
      }, error = function(e) {
        cat(sprintf("❌ The Odds API failed: %s\n", e$message))
      })
    }
    
    cat("⚠️ The Odds API unavailable, falling back to ESPN...\n")
  }
  
  # Fallback to ESPN
  cat("Using ESPN Odds API...\n")
  espn_odds <- get_nfl_scoreboard_with_odds()
  
  if(nrow(espn_odds) > 0) {
    cat("✅ Using ESPN odds data\n")
    espn_odds$source <- "ESPN"
    
    # Standardize column names to match The Odds API format
    espn_standard <- espn_odds %>%
      mutate(
        avg_home_spread = ifelse(is.na(spread), 0, spread),
        avg_away_spread = ifelse(is.na(spread), 0, -spread),
        num_books = 1,  # ESPN typically shows consensus
        commence_time = game_date
      ) %>%
      select(event_id, home_team, away_team, commence_time, 
             avg_home_spread, avg_away_spread, num_books, source) %>%
      rename(game_id = event_id)
    
    return(espn_standard)
  }
  
  cat("❌ No odds data available from any source\n")
  return(data.frame())
}

# Test function
test_espn_odds_system <- function() {
  
  cat("=== TESTING ESPN ODDS SYSTEM ===\n\n")
  
  # Test ESPN scoreboard
  cat("1. Testing ESPN Scoreboard with Odds:\n")
  scoreboard_odds <- get_nfl_scoreboard_with_odds()
  
  if(nrow(scoreboard_odds) > 0) {
    cat("Sample games:\n")
    print(head(scoreboard_odds[, c("home_team", "away_team", "spread", "status")], 5))
  }
  
  cat("\n2. Testing Unified Odds System:\n")
  unified_odds <- get_unified_nfl_odds(prefer_source = "espn")
  
  if(nrow(unified_odds) > 0) {
    cat("Unified odds sample:\n")
    print(head(unified_odds, 5))
  }
  
  return(list(
    scoreboard = scoreboard_odds,
    unified = unified_odds
  ))
}

cat("ESPN Odds API Integration loaded! 📊\n\n")
cat("Functions available:\n")
cat("- get_nfl_scoreboard_with_odds(): Get current NFL games with odds\n")
cat("- get_unified_nfl_odds(prefer_source): Unified system with fallback\n")
cat("- test_espn_odds_system(): Test ESPN odds functionality\n\n")
cat("To test: test_espn_odds_system()\n")