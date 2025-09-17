# Team name mapping for betting odds to NFL abbreviations
team_name_mapping <- function() {
  return(list(
    "Arizona Cardinals" = "ARI",
    "Atlanta Falcons" = "ATL", 
    "Baltimore Ravens" = "BAL",
    "Buffalo Bills" = "BUF",
    "Carolina Panthers" = "CAR",
    "Chicago Bears" = "CHI",
    "Cincinnati Bengals" = "CIN",
    "Cleveland Browns" = "CLE",
    "Dallas Cowboys" = "DAL",
    "Denver Broncos" = "DEN",
    "Detroit Lions" = "DET",
    "Green Bay Packers" = "GB",
    "Houston Texans" = "HOU",
    "Indianapolis Colts" = "IND",
    "Jacksonville Jaguars" = "JAX",
    "Kansas City Chiefs" = "KC",
    "Las Vegas Raiders" = "LV",
    "Los Angeles Chargers" = "LAC",
    "Los Angeles Rams" = "LAR",
    "Miami Dolphins" = "MIA",
    "Minnesota Vikings" = "MIN",
    "New England Patriots" = "NE",
    "New Orleans Saints" = "NO",
    "New York Giants" = "NYG",
    "New York Jets" = "NYJ",
    "Philadelphia Eagles" = "PHI",
    "Pittsburgh Steelers" = "PIT",
    "San Francisco 49ers" = "SF",
    "Seattle Seahawks" = "SEA",
    "Tampa Bay Buccaneers" = "TB",
    "Tennessee Titans" = "TEN",
    "Washington Commanders" = "WAS"
  ))
}

# Function to convert single team name
convert_team_name <- function(team_name) {
  mapping <- team_name_mapping()
  abbr <- mapping[[team_name]]
  if(is.null(abbr)) return(NULL)
  return(abbr)
}

# Function to convert team names
convert_team_names <- function(consensus_spreads) {
  mapping <- team_name_mapping()
  
  consensus_spreads$home_team_abbr <- sapply(consensus_spreads$home_team, function(x) {
    abbr <- mapping[[x]]
    if(is.null(abbr)) return(x)  # Return original if not found
    return(abbr)
  })
  
  consensus_spreads$away_team_abbr <- sapply(consensus_spreads$away_team, function(x) {
    abbr <- mapping[[x]]
    if(is.null(abbr)) return(x)  # Return original if not found  
    return(abbr)
  })
  
  return(consensus_spreads)
}

cat("Team name mapping functions loaded!\n")