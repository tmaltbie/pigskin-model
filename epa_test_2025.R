# Quick EPA Test on 2025 Week 1 Results
# Using the EPA data we already calculated

# EPA data from our test (2025 season through Week 2):
# BAL: 0.220 EPA/play, Success: 51.0%, Explosive: 20.7%
# KC: 0.051 EPA/play, Success: 46.7%, Explosive: 17.3%
# PHI: 0.102 EPA/play, Success: 49.3%, Explosive: 12.5%
# GB: 0.104 EPA/play, Success: 50.0%, Explosive: 20.1%
# BUF: 0.196 EPA/play, Success: 48.4%, Explosive: 17.2%
# ARI: 0.028 EPA/play, Success: 45.9%, Explosive: 18.9%

# Defensive EPA (lower is better):
# KC: 0.125 EPA allowed
# PHI: 0.053 EPA allowed
# BUF: 0.099 EPA allowed
# BAL: 0.004 EPA allowed
# GB: -0.063 EPA allowed  
# ARI: -0.048 EPA allowed

calculate_epa_prediction <- function(home_off_epa, home_def_epa, away_off_epa, away_def_epa, 
                                   home_success, away_success, home_explosive, away_explosive) {
  
  # EPA-based strength calculation (simplified from our model)
  home_off_strength <- (home_off_epa * 30) + (home_success * 10) + (home_explosive * 15)
  away_off_strength <- (away_off_epa * 30) + (away_success * 10) + (away_explosive * 15)
  
  # Defense (lower EPA allowed is better)
  home_def_strength <- -(away_def_epa * 25)  # Defense stops opponent
  away_def_strength <- -(home_def_epa * 25)
  
  # Total strength
  home_total <- home_off_strength + home_def_strength + 3  # Home field advantage
  away_total <- away_off_strength + away_def_strength
  
  return(home_total - away_total)
}

cat("=== EPA PREDICTIONS vs ACTUAL WEEK 1 RESULTS ===\n")

# Game 1: Ravens @ Chiefs (KC won 27-20, so KC +7)
cat("BAL @ KC:\n")
kc_pred <- calculate_epa_prediction(
  home_off_epa = 0.051, home_def_epa = 0.125,   # KC
  away_off_epa = 0.220, away_def_epa = 0.004,   # BAL
  home_success = 0.467, away_success = 0.510,
  home_explosive = 0.173, away_explosive = 0.207
)
cat(sprintf("  EPA Predicted: KC by %.1f\n", kc_pred))
cat("  Actual: KC 27-20 (KC +7)\n")
cat(sprintf("  EPA Error: %.1f points\n", abs(kc_pred - 7)))
cat("  Vegas: KC -2.5\n\n")

# Game 2: Packers @ Eagles (PHI won 34-29, so PHI +5)  
cat("GB @ PHI:\n")
phi_pred <- calculate_epa_prediction(
  home_off_epa = 0.102, home_def_epa = 0.053,   # PHI
  away_off_epa = 0.104, away_def_epa = -0.063,  # GB
  home_success = 0.493, away_success = 0.500,
  home_explosive = 0.125, away_explosive = 0.201
)
cat(sprintf("  EPA Predicted: PHI by %.1f\n", phi_pred))
cat("  Actual: PHI 34-29 (PHI +5)\n")
cat(sprintf("  EPA Error: %.1f points\n", abs(phi_pred - 5)))
cat("  Vegas: PHI -6.0\n\n")

# Game 3: Cardinals @ Bills (BUF won 34-28, so BUF +6)
cat("ARI @ BUF:\n")
buf_pred <- calculate_epa_prediction(
  home_off_epa = 0.196, home_def_epa = 0.099,   # BUF
  away_off_epa = 0.028, away_def_epa = -0.048,  # ARI
  home_success = 0.484, away_success = 0.459,
  home_explosive = 0.172, away_explosive = 0.189
)
cat(sprintf("  EPA Predicted: BUF by %.1f\n", buf_pred))
cat("  Actual: BUF 34-28 (BUF +6)\n")
cat(sprintf("  EPA Error: %.1f points\n", abs(buf_pred - 6)))
cat("  Vegas: BUF -6.5\n\n")

# Calculate average error
errors <- c(abs(kc_pred - 7), abs(phi_pred - 5), abs(buf_pred - 6))
avg_error <- mean(errors)
cat(sprintf("EPA Model Average Error: %.1f points\n", avg_error))