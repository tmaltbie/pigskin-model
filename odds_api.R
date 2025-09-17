# The Odds API Integration for NFL Betting Data
# API Documentation: https://the-odds-api.com/liveapi/guides/v4/

library(httr)
library(jsonlite)
library(dplyr)

# API Configuration
ODDS_API_KEY <- "629c3e74c23fee9ad4809b40ea3df597"
BASE_URL <- "https://api.the-odds-api.com/v4"

# Function to test API connection
test_odds_api <- function() {
  
  cat("Testing The Odds API connection...\n")
  
  # Test with sports endpoint
  url <- paste0(BASE_URL, "/sports")
  
  response <- GET(url, 
                  query = list(api_key = ODDS_API_KEY))
  
  if (response$status_code == 200) {
    content <- content(response, "parsed")
    cat("✅ API Connection successful!\n")
    cat(sprintf("Available sports: %d\n", length(content)))
    
    # Find NFL
    nfl_sport <- NULL
    for(sport in content) {
      if(grepl("nfl", sport$key, ignore.case = TRUE)) {
        nfl_sport <- sport
        break
      }
    }
    
    if(!is.null(nfl_sport)) {
      cat(sprintf("✅ NFL found: %s (%s)\n", nfl_sport$title, nfl_sport$key))
      return(nfl_sport$key)
    } else {
      cat("❌ NFL not found in available sports\n")
      return(NULL)
    }
    
  } else {
    cat(sprintf("❌ API Error: %d\n", response$status_code))
    cat(sprintf("Response: %s\n", content(response, "text")))
    return(NULL)
  }
}

# Function to get NFL odds/spreads
get_nfl_odds <- function(sport_key = "americanfootball_nfl", 
                         markets = c("spreads", "totals", "h2h"),
                         bookmakers = c("fanduel", "draftkings", "betmgm")) {
  
  cat("Fetching NFL odds...\n")
  
  url <- paste0(BASE_URL, "/sports/", sport_key, "/odds")
  
  # Convert markets and bookmakers to comma-separated strings
  markets_str <- paste(markets, collapse = ",")
  bookmakers_str <- paste(bookmakers, collapse = ",")
  
  response <- GET(url, 
                  query = list(
                    api_key = ODDS_API_KEY,
                    regions = "us",
                    markets = markets_str,
                    bookmakers = bookmakers_str,
                    oddsFormat = "american"
                  ))
  
  if (response$status_code == 200) {
    content <- content(response, "parsed")
    cat(sprintf("✅ Retrieved odds for %d games\n", length(content)))
    return(content)
  } else {
    cat(sprintf("❌ API Error: %d\n", response$status_code))
    error_content <- content(response, "text")
    cat(sprintf("Error details: %s\n", error_content))
    return(NULL)
  }
}

# Function to parse odds data into clean format
parse_nfl_odds <- function(odds_data) {
  
  if(is.null(odds_data) || length(odds_data) == 0) {
    cat("No odds data to parse\n")
    return(data.frame())
  }
  
  games_list <- list()
  
  for(i in 1:length(odds_data)) {
    game <- odds_data[[i]]
    
    # Basic game info
    game_info <- list(
      game_id = game$id,
      sport_key = game$sport_key,
      commence_time = game$commence_time,
      home_team = game$home_team,
      away_team = game$away_team
    )
    
    # Initialize spread/odds columns
    spread_cols <- c("spread_fanduel_home", "spread_fanduel_away", 
                     "spread_draftkings_home", "spread_draftkings_away",
                     "spread_betmgm_home", "spread_betmgm_away")
    
    for(col in spread_cols) {
      game_info[[col]] <- NA
    }
    
    # Parse bookmaker odds
    if(!is.null(game$bookmakers) && length(game$bookmakers) > 0) {
      
      for(bookmaker in game$bookmakers) {
        
        bookmaker_name <- bookmaker$key
        
        # Parse each market (spreads, totals, h2h)
        if(!is.null(bookmaker$markets) && length(bookmaker$markets) > 0) {
          
          for(market in bookmaker$markets) {
            
            market_key <- market$key
            
            if(market_key == "spreads") {
              # Parse spread data
              for(outcome in market$outcomes) {
                if(outcome$name == game$home_team) {
                  game_info[[paste0("spread_", bookmaker_name, "_home")]] <- outcome$point
                } else {
                  game_info[[paste0("spread_", bookmaker_name, "_away")]] <- outcome$point
                }
              }
            }
          }
        }
      }
    }
    
    games_list[[i]] <- game_info
  }
  
  # Convert list to data frame
  games_df <- do.call(rbind, lapply(games_list, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  
  return(games_df)
}

# Function to get consensus spreads (average across bookmakers)
get_consensus_spreads <- function(parsed_odds) {
  
  if(nrow(parsed_odds) == 0) return(data.frame())
  
  consensus <- parsed_odds %>%
    select(game_id, home_team, away_team, commence_time, 
           starts_with("spread_"), -contains("odds")) %>%
    rowwise() %>%
    mutate(
      # Calculate average spread across bookmakers
      avg_home_spread = mean(c_across(contains("_home")), na.rm = TRUE),
      avg_away_spread = mean(c_across(contains("_away")), na.rm = TRUE),
      # Count how many books have odds
      num_books = sum(!is.na(c_across(contains("_home"))))
    ) %>%
    ungroup() %>%
    select(game_id, home_team, away_team, commence_time, 
           avg_home_spread, avg_away_spread, num_books)
  
  return(consensus)
}

cat("The Odds API integration loaded!\n")
cat("Functions available:\n")
cat("- test_odds_api(): Test API connection\n")
cat("- get_nfl_odds(): Get current NFL odds\n")
cat("- parse_nfl_odds(): Parse raw odds data\n")
cat("- get_consensus_spreads(): Get average spreads\n")
cat("\nTo test: test_odds_api()\n")