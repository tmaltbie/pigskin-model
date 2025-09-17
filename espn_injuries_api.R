# ESPN Injuries API Integration
# Replace 5% placeholder with real injury impact data

library(httr)
library(jsonlite)
library(dplyr)

# Function to get athlete details from reference URL
get_athlete_details <- function(athlete_ref_url) {
  
  tryCatch({
    response <- GET(athlete_ref_url)
    
    if(response$status_code == 200) {
      return(content(response, "parsed"))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Function to get detailed injury data from reference URL
get_injury_details <- function(injury_ref_url) {
  
  tryCatch({
    response <- GET(injury_ref_url)
    
    if(response$status_code == 200) {
      return(content(response, "parsed"))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Function to get team injuries from ESPN
get_team_injuries <- function(team_id) {
  
  url <- sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/teams/%d/injuries", team_id)
  
  tryCatch({
    response <- GET(url)
    
    if(response$status_code == 200) {
      content <- content(response, "parsed")
      return(content)
    } else {
      cat(sprintf("Error fetching injuries for team %d: %d\n", team_id, response$status_code))
      return(NULL)
    }
  }, error = function(e) {
    cat(sprintf("Network error for team %d: %s\n", team_id, e$message))
    return(NULL)
  })
}

# Function to parse injury data into usable format
parse_injury_data <- function(injury_data) {
  
  if(is.null(injury_data) || is.null(injury_data$items) || length(injury_data$items) == 0) {
    return(data.frame(
      athlete_name = character(),
      position = character(),
      status = character(),
      description = character(),
      impact_score = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  injuries <- list()
  
  cat(sprintf("Processing %d injury references...\n", length(injury_data$items)))
  
  for(i in 1:min(length(injury_data$items), 10)) {  # Limit to first 10 to avoid too many API calls
    injury_ref <- injury_data$items[[i]]
    
    # Get the reference URL
    if(!is.null(injury_ref$`$ref`)) {
      injury_details <- get_injury_details(injury_ref$`$ref`)
      
      if(!is.null(injury_details)) {
        # Initialize athlete info
        athlete_name <- "Unknown"
        position <- "UNK"
        
        # Try to get athlete info from injury details first
        if(!is.null(injury_details$athlete)) {
          if(is.list(injury_details$athlete) && !is.null(injury_details$athlete$displayName)) {
            athlete_name <- injury_details$athlete$displayName
            if(!is.null(injury_details$athlete$position) && !is.null(injury_details$athlete$position$abbreviation)) {
              position <- injury_details$athlete$position$abbreviation
            }
          } else if(!is.null(injury_details$athlete$`$ref`)) {
            # Athlete is a reference, fetch details
            athlete_details <- get_athlete_details(injury_details$athlete$`$ref`)
            if(!is.null(athlete_details)) {
              if(!is.null(athlete_details$displayName)) {
                athlete_name <- athlete_details$displayName
              }
              if(!is.null(athlete_details$position) && !is.null(athlete_details$position$abbreviation)) {
                position <- athlete_details$position$abbreviation
              }
            }
          }
        }
        
        # Extract injury status - handle both object and atomic vector formats
        status <- if(!is.null(injury_details$status)) {
          if(is.list(injury_details$status) && !is.null(injury_details$status$abbreviation)) {
            injury_details$status$abbreviation
          } else if(is.character(injury_details$status)) {
            injury_details$status
          } else {
            "Unknown"
          }
        } else {
          "Unknown"
        }
        
        # Extract description
        description <- if(!is.null(injury_details$description)) {
          injury_details$description
        } else {
          "No description"
        }
        
        # Calculate impact score based on position and status
        impact_score <- calculate_injury_impact(position, status)
        
        injuries[[length(injuries) + 1]] <- list(
          athlete_name = athlete_name,
          position = position,
          status = status,
          description = description,
          impact_score = impact_score
        )
        
        cat(sprintf("  %s (%s): %s - %.1f impact\n", athlete_name, position, status, impact_score))
      }
    }
  }
  
  if(length(injuries) == 0) {
    return(data.frame(
      athlete_name = character(),
      position = character(),
      status = character(),
      description = character(),
      impact_score = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Convert to data frame
  injury_df <- do.call(rbind, lapply(injuries, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  
  return(injury_df)
}

# Function to calculate injury impact based on position and status
calculate_injury_impact <- function(position, status) {
  
  # Position importance weights
  position_weights <- list(
    "QB" = 10.0,   # Quarterback - highest impact
    "RB" = 4.0,    # Running back
    "WR" = 3.5,    # Wide receiver  
    "TE" = 2.5,    # Tight end
    "LT" = 3.0,    # Left tackle
    "RT" = 2.0,    # Right tackle
    "LG" = 1.5,    # Left guard
    "RG" = 1.5,    # Right guard
    "C" = 2.0,     # Center
    "DE" = 2.5,    # Defensive end
    "DT" = 2.0,    # Defensive tackle
    "LB" = 2.5,    # Linebacker
    "CB" = 3.0,    # Cornerback
    "S" = 2.5,     # Safety
    "K" = 1.0,     # Kicker
    "P" = 0.5      # Punter
  )
  
  # Status severity multipliers
  status_multipliers <- list(
    "O" = 1.0,     # Out - full impact
    "D" = 0.7,     # Doubtful - 70% impact  
    "Q" = 0.4,     # Questionable - 40% impact
    "P" = 0.1,     # Probable - minimal impact
    "IR" = 1.0,    # Injured Reserve - full impact
    "PUP" = 1.0    # Physically Unable to Perform - full impact
  )
  
  # Get position weight (default to 1.0 if unknown)
  pos_weight <- if(position %in% names(position_weights)) {
    position_weights[[position]]
  } else {
    1.0
  }
  
  # Get status multiplier (default to 0.5 if unknown)
  status_mult <- if(status %in% names(status_multipliers)) {
    status_multipliers[[status]]
  } else {
    0.5
  }
  
  return(pos_weight * status_mult)
}

# Function to get injury adjustment for a team
get_team_injury_adjustment <- function(team_abbr) {
  
  # Team abbreviation to ESPN ID mapping
  team_id_map <- list(
    "ARI" = 22, "ATL" = 1, "BAL" = 33, "BUF" = 2, "CAR" = 29, "CHI" = 3,
    "CIN" = 4, "CLE" = 5, "DAL" = 6, "DEN" = 7, "DET" = 8, "GB" = 9,
    "HOU" = 34, "IND" = 11, "JAX" = 30, "KC" = 12, "LV" = 13, "LAC" = 24,
    "LAR" = 14, "MIA" = 15, "MIN" = 16, "NE" = 17, "NO" = 18, "NYG" = 19,
    "NYJ" = 20, "PHI" = 21, "PIT" = 23, "SEA" = 26, "SF" = 25, "TB" = 27,
    "TEN" = 10, "WAS" = 28
  )
  
  # Handle LA -> LAR mapping
  if(team_abbr == "LA") team_abbr <- "LAR"
  
  team_id <- team_id_map[[team_abbr]]
  
  if(is.null(team_id)) {
    cat(sprintf("Unknown team abbreviation: %s\n", team_abbr))
    return(0)
  }
  
  # Get injury data
  injury_data <- get_team_injuries(team_id)
  injury_df <- parse_injury_data(injury_data)
  
  if(nrow(injury_df) == 0) {
    return(0)  # No injuries
  }
  
  # Calculate total injury impact
  total_impact <- sum(injury_df$impact_score, na.rm = TRUE)
  
  # Scale impact to reasonable point adjustment (cap at 5 points max)
  injury_adjustment <- min(total_impact * 0.2, 5.0)
  
  cat(sprintf("%s injuries: %d players, %.1f total impact, %.1f point adjustment\n", 
              team_abbr, nrow(injury_df), total_impact, injury_adjustment))
  
  return(injury_adjustment)
}

# Function to get injuries for both teams in a matchup  
get_matchup_injury_adjustments <- function(home_team, away_team) {
  
  cat(sprintf("Getting injury data for %s @ %s...\n", away_team, home_team))
  
  home_injury_impact <- get_team_injury_adjustment(home_team)
  away_injury_impact <- get_team_injury_adjustment(away_team)
  
  # Net adjustment favors team with fewer/less severe injuries
  net_adjustment <- away_injury_impact - home_injury_impact
  
  cat(sprintf("Injury adjustment: %s %.1f vs %s %.1f = %+.1f to home team\n",
              away_team, away_injury_impact, home_team, home_injury_impact, net_adjustment))
  
  return(list(
    home_injury_impact = home_injury_impact,
    away_injury_impact = away_injury_impact,
    net_adjustment = net_adjustment,
    total_injuries = home_injury_impact + away_injury_impact
  ))
}

# Test function
test_injury_system <- function() {
  
  cat("=== TESTING ESPN INJURIES API ===\n\n")
  
  # Test a few teams
  test_teams <- c("KC", "BUF", "SF", "PHI")
  
  for(team in test_teams) {
    cat(sprintf("--- %s Injuries ---\n", team))
    adjustment <- get_team_injury_adjustment(team)
    cat(sprintf("Adjustment: %.1f points\n\n", adjustment))
  }
  
  # Test a matchup
  cat("--- Sample Matchup ---\n")
  matchup_data <- get_matchup_injury_adjustments("KC", "BUF")
  
  return(matchup_data)
}

cat("ESPN Injuries API Integration loaded! 🏥\n\n")
cat("Functions available:\n")
cat("- get_team_injury_adjustment(team_abbr): Get injury impact for team\n")
cat("- get_matchup_injury_adjustments(home, away): Get injury data for matchup\n")  
cat("- test_injury_system(): Test injury system on sample teams\n\n")
cat("To test: test_injury_system()\n")