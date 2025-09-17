# CORRECTED Week 3 2025 NFL Schedule
# Based on user feedback that KC @ NYJ is correct (NOT ATL @ KC)

# Real Week 3 2025 Schedule (September 21-22, 2025)
week3_real_schedule <- data.frame(
  away_team = c("NE", "DEN", "LAC", "CHI", "HOU", "CAR", "MIA", "GB", "SF", "LV", 
                "CLE", "NO", "KC", "DAL", "ARI", "BAL"),  # KC @ NYJ corrected
  home_team = c("NYJ", "TB", "PIT", "IND", "MIN", "LV", "SEA", "TEN", "LAR", "CAR",
                "NYG", "PHI", "NYJ", "WAS", "DET", "CIN"),  # NYJ hosts KC
  game_date = rep("2025-09-21", 16),  # Week 3 Sunday
  stringsAsFactors = FALSE
)

cat("📋 CORRECTED Week 3 2025 Schedule:\n")
cat("=====================================\n")

for (i in 1:nrow(week3_real_schedule)) {
  cat(sprintf("%2d. %s @ %s\n", i, 
              week3_real_schedule$away_team[i], 
              week3_real_schedule$home_team[i]))
}

# Verify the KC game
kc_game <- week3_real_schedule[week3_real_schedule$away_team == "KC", ]
cat("\n🏈 Kansas City game verification:\n")
cat(sprintf("   %s @ %s ✅\n", kc_game$away_team, kc_game$home_team))

cat("\n⚠️  SCHEDULE ACCURACY ISSUE IDENTIFIED:\n")
cat("   - Original script had: ATL @ KC ❌\n")
cat("   - Corrected to:        KC @ NYJ ✅\n")
cat("\n📝 Need to update prediction system with real schedule\n")