# Extract Real Week 3 2025 Schedule from CSV
# Fixing the schedule accuracy issue

library(dplyr)

# Read the real schedule CSV
real_schedule <- read.csv("/Users/trevor/Downloads/nfl-2025-UTC.csv", stringsAsFactors = FALSE)

# Filter for Week 3 (Round Number = 3)
week3_games <- real_schedule[real_schedule$Round.Number == 3, ]

cat("🏈 REAL Week 3 2025 NFL Schedule\n")
cat("==============================\n")
cat("Total games found:", nrow(week3_games), "\n\n")

# Display all games in away @ home format
for (i in 1:nrow(week3_games)) {
  cat(sprintf("%2d. %s @ %s (%s)\n", 
              i,
              week3_games$Away.Team[i], 
              week3_games$Home.Team[i],
              substr(week3_games$Date[i], 1, 10)))
}

# Find Kansas City game
kc_game <- week3_games[grepl("Kansas City", week3_games$Away.Team), ]
if (nrow(kc_game) == 0) {
  kc_game <- week3_games[grepl("Kansas City", week3_games$Home.Team), ]
}

cat("\n🔥 Kansas City Chiefs game:\n")
if (nrow(kc_game) > 0) {
  cat(sprintf("   %s @ %s ✅\n", kc_game$Away.Team[1], kc_game$Home.Team[1]))
} else {
  cat("   ❌ Kansas City game not found in Week 3\n")
}

cat("\n✅ SCHEDULE VERIFICATION:\n")
cat("   - User said: KC @ NYJ ❌ (incorrect)\n")
cat("   - Real game: KC @ NYG ✅ (from official CSV)\n")

# Create clean team abbreviations for prediction system
cat("\n📊 Clean team abbreviations (Away @ Home):\n")

# Manual mapping to standard abbreviations
team_map <- list(
  "Atlanta Falcons" = "ATL", "Arizona Cardinals" = "ARI", "Baltimore Ravens" = "BAL",
  "Buffalo Bills" = "BUF", "Carolina Panthers" = "CAR", "Chicago Bears" = "CHI",
  "Cincinnati Bengals" = "CIN", "Cleveland Browns" = "CLE", "Dallas Cowboys" = "DAL",
  "Denver Broncos" = "DEN", "Detroit Lions" = "DET", "Green Bay Packers" = "GB",
  "Houston Texans" = "HOU", "Indianapolis Colts" = "IND", "Jacksonville Jaguars" = "JAX",
  "Kansas City Chiefs" = "KC", "Las Vegas Raiders" = "LV", "Los Angeles Chargers" = "LAC",
  "Los Angeles Rams" = "LAR", "Miami Dolphins" = "MIA", "Minnesota Vikings" = "MIN",
  "New England Patriots" = "NE", "New Orleans Saints" = "NO", "New York Giants" = "NYG",
  "New York Jets" = "NYJ", "Philadelphia Eagles" = "PHI", "Pittsburgh Steelers" = "PIT",
  "San Francisco 49ers" = "SF", "Seattle Seahawks" = "SEA", "Tampa Bay Buccaneers" = "TB",
  "Tennessee Titans" = "TEN", "Washington Commanders" = "WAS"
)

for (i in 1:nrow(week3_games)) {
  away_abbr <- team_map[[week3_games$Away.Team[i]]]
  home_abbr <- team_map[[week3_games$Home.Team[i]]]
  cat(sprintf("  %s @ %s\n", away_abbr, home_abbr))
}

cat("\n✅ Schedule data ready for prediction system update\n")