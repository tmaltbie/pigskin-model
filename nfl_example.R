# NFL Data Analysis with nflreadr
# Example script to get started with NFL data

library(nflreadr)

# Load some basic NFL data
# Examples of what you can do:

# 1. Load play-by-play data for a recent season
# pbp <- load_pbp(2023)

# 2. Load schedule data
# schedule <- load_schedules(2023)

# 3. Load team roster data
# roster <- load_rosters(2023)

# 4. Load player stats
# stats <- load_player_stats(2023)

# Print available functions
cat("nflreadr package loaded successfully!\n")
cat("Available data loading functions:\n")
cat("- load_pbp(): Play-by-play data\n")
cat("- load_schedules(): Schedule data\n")
cat("- load_rosters(): Team rosters\n")
cat("- load_player_stats(): Player statistics\n")
cat("- load_contracts(): Player contracts\n")
cat("- load_combine(): NFL Combine data\n")
cat("- load_draft_picks(): Draft pick data\n")