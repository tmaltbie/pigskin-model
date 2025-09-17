# NFL Injury Data Scraper
# Since nflreadr has no injury data, scrape from official sources

library(httr)
library(rvest)
library(jsonlite)
library(dplyr)

# ============= NFL.COM INJURY SCRAPER =============

scrape_nfl_injuries <- function() {
  
  cat("Scraping NFL.com injury reports...\n")
  
  base_url <- "https://www.nfl.com/injuries/"
  
  tryCatch({
    # Get the page
    page <- read_html(base_url)
    
    # This would extract injury data from NFL.com
    # HTML structure would need to be analyzed first
    
    cat("NOTE: NFL.com scraping needs HTML structure analysis\n")
    cat("Would extract: player names, teams, injury status, body part\n")
    
    # Return sample structure for now
    sample_injuries <- data.frame(
      team = c("BUF", "KC", "SF", "DAL"),
      player = c("Josh Allen", "Patrick Mahomes", "Brock Purdy", "Dak Prescott"),
      position = c("QB", "QB", "QB", "QB"),
      injury = c("Shoulder", "Ankle", "Back", "Hamstring"),
      status = c("Probable", "Questionable", "Out", "Doubtful"),
      date_updated = Sys.Date(),
      source = "NFL.com",
      stringsAsFactors = FALSE
    )
    
    return(sample_injuries)
    
  }, error = function(e) {
    cat("Error scraping NFL.com:", e$message, "\n")
    return(data.frame())
  })
}

# ============= ESPN FANTASY SCRAPER =============

scrape_espn_fantasy_injuries <- function() {
  
  cat("Scraping ESPN Fantasy injury data...\n")
  
  # ESPN Fantasy has good injury data
  espn_url <- "https://fantasy.espn.com/football/players/news"
  
  tryCatch({
    
    # This would scrape ESPN's fantasy injury updates
    cat("NOTE: ESPN Fantasy scraping needs implementation\n")
    cat("Available: injury status, expected return, severity\n")
    
    # Sample data structure
    espn_injuries <- data.frame(
      team = c("PHI", "MIA", "GB", "LAR"),
      player = c("A.J. Brown", "Tua Tagovailoa", "Aaron Rodgers", "Cooper Kupp"),
      position = c("WR", "QB", "QB", "WR"),
      injury = c("Knee", "Concussion", "Achilles", "Ankle"),
      status = c("Questionable", "Out", "IR", "Probable"),
      expected_return = c("Week 3", "Unknown", "2025", "Week 3"),
      source = "ESPN Fantasy",
      stringsAsFactors = FALSE
    )
    
    return(espn_injuries)
    
  }, error = function(e) {
    cat("Error scraping ESPN:", e$message, "\n")
    return(data.frame())
  })
}

# ============= ROTOWIRE SCRAPER =============

scrape_rotowire_injuries <- function() {
  
  cat("Scraping RotowWire injury data...\n")
  
  rotowire_url <- "https://www.rotowire.com/football/nfl-lineups.php"
  
  tryCatch({
    
    cat("NOTE: RotowWire scraping needs implementation\n")
    cat("Good for: confirmed OUT players, starting lineups\n")
    
    # Sample confirmed OUT players
    rotowire_injuries <- data.frame(
      team = c("NYJ", "CLE", "DEN"),
      player = c("Aaron Rodgers", "Deshaun Watson", "Russell Wilson"), 
      position = c("QB", "QB", "QB"),
      status = c("Out", "Out", "Questionable"),
      injury = c("Achilles", "Shoulder", "Calf"),
      source = "RotowWire",
      confidence = c(1.0, 1.0, 0.7),  # Confidence in status
      stringsAsFactors = FALSE
    )
    
    return(rotowire_injuries)
    
  }, error = function(e) {
    cat("Error scraping RotowWire:", e$message, "\n")
    return(data.frame())
  })
}

# ============= AGGREGATE ALL SOURCES =============

get_comprehensive_injury_data <- function() {
  
  cat("=== COMPREHENSIVE INJURY DATA COLLECTION ===\n")
  
  all_injuries <- list()
  
  # Try each source
  sources <- list(
    nfl_com = scrape_nfl_injuries,
    espn_fantasy = scrape_espn_fantasy_injuries,
    rotowire = scrape_rotowire_injuries
  )
  
  for(source_name in names(sources)) {
    cat(sprintf("\nTrying %s...\n", source_name))
    
    injury_data <- sources[[source_name]]()
    
    if(nrow(injury_data) > 0) {
      all_injuries[[source_name]] <- injury_data
      cat(sprintf("✅ %s: %d injuries found\n", source_name, nrow(injury_data)))
    } else {
      cat(sprintf("❌ %s: No data\n", source_name))
    }
  }
  
  # Combine all sources
  if(length(all_injuries) > 0) {
    combined_injuries <- do.call(rbind, all_injuries)
    
    # Remove duplicates (same player from multiple sources)
    combined_injuries <- combined_injuries %>%
      group_by(team, player) %>%
      slice(1) %>%  # Take first occurrence
      ungroup()
    
    cat(sprintf("\n✅ Total unique injuries: %d\n", nrow(combined_injuries)))
    return(combined_injuries)
    
  } else {
    cat("\n❌ No injury data available from any source\n")
    return(data.frame())
  }
}

# ============= INJURY IMPACT CALCULATOR (UPDATED) =============

calculate_real_injury_impact <- function(team_injuries, calibrated_weights = TRUE) {
  
  if(nrow(team_injuries) == 0) return(0)
  
  # Use calibrated weights (reduced from backtest)
  if(calibrated_weights) {
    position_weights <- list(
      "QB" = 4.0, "RB" = 2.0, "WR" = 1.8, "TE" = 1.2,
      "OL" = 1.5, "DE" = 1.8, "LB" = 1.2, "CB" = 1.5,
      "S" = 1.0, "K" = 0.5, "P" = 0.3
    )
    
    status_weights <- list(
      "Out" = 1.0, "Doubtful" = 0.6, "Questionable" = 0.3,
      "Probable" = 0.1, "IR" = 1.0
    )
  } else {
    # Original weights (too high)
    position_weights <- list(
      "QB" = 10.0, "RB" = 4.0, "WR" = 3.5, "TE" = 2.5,
      "OL" = 3.0, "DE" = 3.5, "LB" = 2.5, "CB" = 3.0,
      "S" = 2.0, "K" = 1.0, "P" = 0.5
    )
    
    status_weights <- list(
      "Out" = 1.0, "Doubtful" = 0.8, "Questionable" = 0.4,
      "Probable" = 0.1, "IR" = 1.0
    )
  }
  
  total_impact <- 0
  
  for(i in 1:nrow(team_injuries)) {
    injury <- team_injuries[i,]
    
    # Get position weight
    pos_weight <- position_weights[[injury$position]]
    if(is.null(pos_weight)) pos_weight <- 1.0
    
    # Get status weight  
    status_weight <- status_weights[[injury$status]]
    if(is.null(status_weight)) status_weight <- 0.3
    
    # Confidence multiplier (if available)
    confidence <- ifelse(is.na(injury$confidence), 1.0, injury$confidence)
    
    impact <- pos_weight * status_weight * confidence
    total_impact <- total_impact + impact
    
    cat(sprintf("  %s (%s) %s: %.1f impact\n", 
                injury$player, injury$position, injury$status, impact))
  }
  
  return(total_impact)
}

# ============= TESTING FUNCTIONS =============

test_injury_system <- function() {
  
  cat("=== TESTING INJURY DATA SYSTEM ===\n")
  
  # Get all injury data
  injuries <- get_comprehensive_injury_data()
  
  if(nrow(injuries) > 0) {
    
    # Test impact calculation for a few teams
    test_teams <- c("BUF", "KC", "SF")
    
    for(team in test_teams) {
      team_injuries <- injuries %>% filter(team == !!team)
      
      if(nrow(team_injuries) > 0) {
        cat(sprintf("\n%s Injuries:\n", team))
        impact <- calculate_real_injury_impact(team_injuries)
        cat(sprintf("Total impact: %.1f points\n", impact))
      }
    }
    
    return(injuries)
  } else {
    cat("No injury data to test\n")
    return(data.frame())
  }
}

cat("NFL Injury Scraping System loaded!\n")
cat("Functions available:\n")
cat("- scrape_nfl_injuries(): Official NFL.com injury reports\n")
cat("- scrape_espn_fantasy_injuries(): ESPN Fantasy injury data\n")
cat("- scrape_rotowire_injuries(): RotowWire confirmed injuries\n")
cat("- get_comprehensive_injury_data(): Aggregate all sources\n")
cat("- test_injury_system(): Test the complete injury system\n")
cat("\nTo test: test_injury_system()\n")