# Check 2025 NFL data availability and build EPA metrics
library(nflreadr)
library(dplyr)

# Load and convert 2025 data
cat("Loading 2025 NFL data...\n")
pbp_2025 <- load_pbp(2025)
pbp_df <- as.data.frame(pbp_2025)

cat("2025 Data Summary:\n")
cat("Total plays:", nrow(pbp_df), "\n")
cat("Columns:", ncol(pbp_df), "\n")

# Check weeks available
weeks_available <- sort(unique(pbp_df$week))
cat("Weeks available:", paste(weeks_available, collapse = ", "), "\n")

# Check teams with data
teams_with_data <- unique(pbp_df$posteam)
teams_clean <- teams_with_data[!is.na(teams_with_data)]
cat("Teams with 2025 data:", length(teams_clean), "\n")
cat("Team list:", paste(sort(teams_clean), collapse = ", "), "\n")

# Check if we have EPA data
has_epa <- "epa" %in% names(pbp_df)
cat("EPA column available:", has_epa, "\n")

if(has_epa) {
  epa_plays <- sum(!is.na(pbp_df$epa))
  cat("Plays with EPA data:", epa_plays, "\n")
  
  # Sample EPA values
  sample_epa <- pbp_df$epa[!is.na(pbp_df$epa)][1:10]
  cat("Sample EPA values:", paste(round(sample_epa, 3), collapse = ", "), "\n")
}