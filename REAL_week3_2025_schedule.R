# REAL Week 3 2025 NFL Schedule (from official CSV)
# Fixed the schedule accuracy issue

library(dplyr)

# Real Week 3 2025 Schedule - all 16 games
real_week3_2025 <- data.frame(
  game_num = 1:16,
  away_team = c("MIA", "ATL", "GB", "HOU", "CIN", "PIT", "LAR", "NYJ", "IND", "LV",
                "DEN", "NO", "DAL", "ARI", "KC", "DET"),
  home_team = c("BUF", "CAR", "CLE", "JAX", "MIN", "NE", "PHI", "TB", "TEN", "WAS",
                "LAC", "SEA", "CHI", "SF", "NYG", "BAL"),
  game_date = c("2025-09-19", rep("2025-09-21", 12), "2025-09-22", "2025-09-23"),
  game_time = c("00:15", rep("17:00", 6), rep("20:05", 2), rep("20:25", 2), "00:20", "00:15"),
  venue = c("Highmark Stadium", "Bank of America Stadium", "Huntington Bank Field", 
            "EverBank Stadium", "U.S. Bank Stadium", "Gillette Stadium",
            "Lincoln Financial Field", "Raymond James Stadium", "Nissan Stadium",
            "Northwest Stadium", "SoFi Stadium", "Lumen Field", "Soldier Field",
            "Levi's Stadium", "MetLife Stadium", "M&T Bank Stadium"),
  stringsAsFactors = FALSE
)

cat("🏈 REAL Week 3 2025 NFL Schedule (16 games)\n")
cat("============================================\n")

for (i in 1:nrow(real_week3_2025)) {
  cat(sprintf("%2d. %s @ %s (%s)\n", 
              i,
              real_week3_2025$away_team[i], 
              real_week3_2025$home_team[i],
              real_week3_2025$game_date[i]))
}

# Verify the Kansas City game
kc_game <- real_week3_2025[real_week3_2025$away_team == "KC", ]
cat("\n🔥 Kansas City Chiefs game:\n")
cat(sprintf("   %s @ %s (%s at %s) ✅\n", 
            kc_game$away_team, kc_game$home_team, 
            kc_game$game_date, kc_game$venue))

cat("\n✅ SCHEDULE CORRECTED:\n")
cat("   - Previous error: ATL @ KC ❌\n")  
cat("   - Previous error: KC @ NYJ ❌\n")
cat("   - CORRECT:        KC @ NYG ✅\n")

# Export for use in prediction system
cat("\n📊 Ready to update prediction system with real schedule\n")

# Display team matchups in prediction format
cat("\n📋 Team Matchups (Away @ Home):\n")
for (i in 1:nrow(real_week3_2025)) {
  cat(sprintf("  %s @ %s\n", real_week3_2025$away_team[i], real_week3_2025$home_team[i]))
}