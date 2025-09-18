# Situational Play-by-Play Analysis Module
# Next-generation NFL prediction system with contextual team tendency analysis
# Detects deviations from league norms (like LAC's high 1st down pass rate)
# Author: NFL ML System
# Version: 2.0

library(dplyr)
library(data.table)
library(tidyr)
library(jsonlite)
library(lubridate)

# Source dependencies
source("prediction_database.R")
source("learning_system/unified_data_access.R")

# ==============================================================================
# SITUATIONAL CONTEXT DEFINITIONS
# ==============================================================================

# Define all situational contexts we want to track
SITUATIONAL_CONTEXTS <- list(
  down_distance = list(
    # Traditional down-distance situations
    "1st_and_10" = list(down = 1, ydstogo = 8:12),
    "1st_and_short" = list(down = 1, ydstogo = 1:4),
    "1st_and_long" = list(down = 1, ydstogo = 13:25),
    "2nd_and_short" = list(down = 2, ydstogo = 1:3),
    "2nd_and_medium" = list(down = 2, ydstogo = 4:7),
    "2nd_and_long" = list(down = 2, ydstogo = 8:25),
    "3rd_and_short" = list(down = 3, ydstogo = 1:3),
    "3rd_and_medium" = list(down = 3, ydstogo = 4:7),
    "3rd_and_long" = list(down = 3, ydstogo = 8:25),
    "4th_down" = list(down = 4, ydstogo = 1:25)
  ),
  
  field_position = list(
    "own_territory" = list(yardline_100 = 51:100),
    "midfield" = list(yardline_100 = 35:50),
    "opponent_territory" = list(yardline_100 = 21:34),
    "red_zone" = list(yardline_100 = 1:20),
    "goal_line" = list(yardline_100 = 1:5)
  ),
  
  score_situation = list(
    "leading_by_1_score" = list(score_differential = 1:8),
    "leading_by_2_scores" = list(score_differential = 9:16),
    "leading_big" = list(score_differential = 17:50),
    "tied" = list(score_differential = 0),
    "trailing_by_1_score" = list(score_differential = -8:-1),
    "trailing_by_2_scores" = list(score_differential = -16:-9),
    "trailing_big" = list(score_differential = -50:-17)
  ),
  
  time_situation = list(
    "first_quarter" = list(qtr = 1),
    "second_quarter" = list(qtr = 2),
    "first_half_2min" = list(qtr = 2, game_seconds_remaining = 0:120),
    "third_quarter" = list(qtr = 3),
    "fourth_quarter" = list(qtr = 4),
    "fourth_quarter_2min" = list(qtr = 4, game_seconds_remaining = 0:120),
    "overtime" = list(qtr = 5)
  )
)

# League average baselines (will be updated dynamically)
LEAGUE_BASELINES <- list(
  pass_rate_1st_down = 0.42,
  run_rate_1st_down = 0.58,
  pass_rate_2nd_short = 0.35,
  pass_rate_3rd_long = 0.85,
  success_rate_overall = 0.45,
  epa_per_play = 0.0,
  # Add more as needed
  red_zone_td_rate = 0.58,
  fourth_down_go_rate = 0.65
)

# ==============================================================================
# CORE SITUATIONAL ANALYSIS FUNCTIONS
# ==============================================================================

#' Extract team situational tendencies from play-by-play data
#' 
#' Analyzes team behavior across all defined situational contexts
#' and compares to league averages to identify tendencies and deviations
#' 
#' @param pbp_data Play-by-play data frame
#' @param teams Vector of team abbreviations to analyze
#' @param min_plays Minimum plays required for reliable tendency calculation
#' @return Data frame with team tendencies by situation
#' @export
extract_team_situational_tendencies <- function(pbp_data, teams = NULL, min_plays = 10) {
  
  cat("🔍 Extracting team situational tendencies...\n")
  
  if (is.null(teams)) {
    teams <- unique(c(pbp_data$posteam, pbp_data$defteam))
    teams <- teams[!is.na(teams)]
  }
  
  # Prepare data with situational context
  pbp_enhanced <- pbp_data %>%
    filter(!is.na(posteam), !is.na(play_type)) %>%
    mutate(
      # Score differential from offensive team perspective
      score_differential = ifelse(posteam == home_team, 
                                 posteam_score - defteam_score,
                                 posteam_score - defteam_score),
      
      # Time remaining calculations
      game_seconds_remaining = ifelse(!is.na(game_seconds_remaining), 
                                    game_seconds_remaining,
                                    (4 - qtr) * 900 + ifelse(!is.na(quarter_seconds_remaining), 
                                                            quarter_seconds_remaining, 0)),
      
      # Play type classifications
      is_pass = play_type %in% c("pass", "qb_spike", "qb_kneel"),
      is_run = play_type == "run",
      is_successful = success == 1,
      
      # Situational flags
      is_red_zone = yardline_100 <= 20,
      is_goal_line = yardline_100 <= 5,
      is_two_minute = (qtr == 2 & game_seconds_remaining <= 120) | 
                     (qtr == 4 & game_seconds_remaining <= 120)
    )
  
  # Initialize results list
  all_tendencies <- list()
  
  # Analyze each team
  for (team in teams) {
    team_plays <- pbp_enhanced %>% filter(posteam == team)
    
    if (nrow(team_plays) < min_plays) {
      next  # Skip teams with insufficient data
    }
    
    team_tendencies <- analyze_single_team_tendencies(team_plays, team, min_plays)
    all_tendencies[[team]] <- team_tendencies
  }
  
  # Combine all team tendencies
  tendencies_df <- bind_rows(all_tendencies, .id = "team")
  
  # Calculate deviations from league average
  tendencies_with_deviations <- calculate_tendency_deviations(tendencies_df, pbp_enhanced)
  
  cat(sprintf("✅ Extracted tendencies for %d teams across %d situations\n", 
              length(unique(tendencies_with_deviations$team)),
              length(unique(tendencies_with_deviations$situation))))
  
  return(tendencies_with_deviations)
}

#' Analyze tendencies for a single team
analyze_single_team_tendencies <- function(team_plays, team_name, min_plays) {
  
  tendencies_list <- list()
  
  # Analyze each situational context
  for (context_type in names(SITUATIONAL_CONTEXTS)) {
    contexts <- SITUATIONAL_CONTEXTS[[context_type]]
    
    for (situation_name in names(contexts)) {
      situation_criteria <- contexts[[situation_name]]
      
      # Filter plays matching this situation
      situation_plays <- filter_plays_by_situation(team_plays, situation_criteria)
      
      if (nrow(situation_plays) >= min_plays) {
        tendency_stats <- calculate_situation_statistics(situation_plays, situation_name)
        tendency_stats$context_type <- context_type
        tendency_stats$team <- team_name
        
        tendencies_list[[paste(context_type, situation_name, sep = "_")]] <- tendency_stats
      }
    }
  }
  
  if (length(tendencies_list) > 0) {
    return(bind_rows(tendencies_list))
  } else {
    return(data.frame())  # Return empty if no valid situations
  }
}

#' Filter plays based on situational criteria
filter_plays_by_situation <- function(plays_data, criteria) {
  
  filtered_plays <- plays_data
  
  for (criterion_name in names(criteria)) {
    criterion_values <- criteria[[criterion_name]]
    
    if (criterion_name %in% names(filtered_plays)) {
      if (is.numeric(criterion_values) && length(criterion_values) > 1) {
        # Range filtering (e.g., ydstogo = 8:12)
        filtered_plays <- filtered_plays %>%
          filter(!!sym(criterion_name) %in% criterion_values)
      } else {
        # Exact value filtering
        filtered_plays <- filtered_plays %>%
          filter(!!sym(criterion_name) == criterion_values)
      }
    }
  }
  
  return(filtered_plays)
}

#' Calculate comprehensive statistics for a situation
calculate_situation_statistics <- function(situation_plays, situation_name) {
  
  total_plays <- nrow(situation_plays)
  
  if (total_plays == 0) {
    return(data.frame())
  }
  
  stats <- data.frame(
    situation = situation_name,
    total_plays = total_plays,
    
    # Play calling tendencies
    pass_rate = mean(situation_plays$is_pass, na.rm = TRUE),
    run_rate = mean(situation_plays$is_run, na.rm = TRUE),
    
    # Performance metrics
    success_rate = mean(situation_plays$is_successful, na.rm = TRUE),
    epa_per_play = mean(situation_plays$epa, na.rm = TRUE),
    yards_per_play = mean(situation_plays$yards_gained, na.rm = TRUE),
    
    # Advanced metrics
    explosive_play_rate = mean(situation_plays$yards_gained >= 20, na.rm = TRUE),
    turnover_rate = mean(situation_plays$turnover == 1, na.rm = TRUE),
    sack_rate = mean(situation_plays$sack == 1, na.rm = TRUE),
    
    # Situational-specific metrics
    first_down_rate = mean(situation_plays$first_down == 1, na.rm = TRUE),
    touchdown_rate = mean(situation_plays$touchdown == 1, na.rm = TRUE),
    
    stringsAsFactors = FALSE
  )
  
  return(stats)
}

#' Calculate deviations from league averages
calculate_tendency_deviations <- function(team_tendencies, league_plays) {
  
  # Calculate league averages for each situation
  league_averages <- team_tendencies %>%
    group_by(situation, context_type) %>%
    summarise(
      league_pass_rate = mean(pass_rate, na.rm = TRUE),
      league_success_rate = mean(success_rate, na.rm = TRUE),
      league_epa_per_play = mean(epa_per_play, na.rm = TRUE),
      league_explosive_rate = mean(explosive_play_rate, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Join team tendencies with league averages
  tendencies_with_deviations <- team_tendencies %>%
    left_join(league_averages, by = c("situation", "context_type")) %>%
    mutate(
      # Calculate deviations
      pass_rate_deviation = pass_rate - league_pass_rate,
      success_rate_deviation = success_rate - league_success_rate,
      epa_deviation = epa_per_play - league_epa_per_play,
      explosive_rate_deviation = explosive_play_rate - league_explosive_rate,
      
      # Flag significant deviations (more than 1 standard deviation)
      significant_pass_deviation = abs(pass_rate_deviation) > 0.10,  # 10% threshold
      significant_success_deviation = abs(success_rate_deviation) > 0.08,
      significant_epa_deviation = abs(epa_deviation) > 0.05,
      
      # Overall deviation score
      deviation_score = sqrt(pass_rate_deviation^2 + success_rate_deviation^2 + epa_deviation^2)
    )
  
  return(tendencies_with_deviations)
}

# ==============================================================================
# ADAPTIVE LEARNING AND UPDATING
# ==============================================================================

#' Update team tendencies with new game data
#' 
#' Incrementally updates team tendency models as new games are completed
#' Implements exponential decay to weight recent games more heavily
#' 
#' @param existing_tendencies Current team tendency data
#' @param new_game_data Play-by-play data from new completed games
#' @param decay_factor Weight for historical vs new data (0.9 = 90% historical)
#' @return Updated tendency data
#' @export
update_team_tendencies_incremental <- function(existing_tendencies, new_game_data, 
                                             decay_factor = 0.9) {
  
  cat("🔄 Updating team tendencies with new game data...\n")
  
  # Extract tendencies from new games
  new_tendencies <- extract_team_situational_tendencies(new_game_data)
  
  if (nrow(new_tendencies) == 0) {
    cat("⚠️ No new tendencies extracted from game data\n")
    return(existing_tendencies)
  }
  
  # Combine existing and new tendencies with exponential decay
  updated_tendencies <- existing_tendencies %>%
    full_join(new_tendencies, 
              by = c("team", "situation", "context_type"),
              suffix = c("_old", "_new")) %>%
    mutate(
      # Apply exponential decay to combine old and new values
      total_plays = coalesce(total_plays_old, 0) + coalesce(total_plays_new, 0),
      
      # Weighted averages with more weight on recent data
      pass_rate = ifelse(is.na(pass_rate_new), 
                        pass_rate_old,
                        ifelse(is.na(pass_rate_old),
                               pass_rate_new,
                               decay_factor * pass_rate_old + (1 - decay_factor) * pass_rate_new)),
      
      success_rate = ifelse(is.na(success_rate_new),
                           success_rate_old,
                           ifelse(is.na(success_rate_old),
                                  success_rate_new,
                                  decay_factor * success_rate_old + (1 - decay_factor) * success_rate_new)),
      
      epa_per_play = ifelse(is.na(epa_per_play_new),
                           epa_per_play_old,
                           ifelse(is.na(epa_per_play_old),
                                  epa_per_play_new,
                                  decay_factor * epa_per_play_old + (1 - decay_factor) * epa_per_play_new))
    ) %>%
    # Keep only the combined columns
    select(team, situation, context_type, total_plays, pass_rate, run_rate, 
           success_rate, epa_per_play, explosive_play_rate, turnover_rate, 
           sack_rate, first_down_rate, touchdown_rate)
  
  # Recalculate deviations with updated data
  updated_with_deviations <- calculate_tendency_deviations(updated_tendencies, new_game_data)
  
  cat(sprintf("✅ Updated tendencies for %d teams\n", 
              length(unique(updated_with_deviations$team))))
  
  return(updated_with_deviations)
}

#' Detect significant changes in team tendencies
#' 
#' Identifies when teams significantly change their approach
#' Useful for alerting the prediction system to new patterns
#' 
#' @param old_tendencies Previous tendency data
#' @param new_tendencies Updated tendency data
#' @param change_threshold Minimum change to flag as significant
#' @return Data frame of significant changes
#' @export
detect_tendency_changes <- function(old_tendencies, new_tendencies, 
                                  change_threshold = 0.15) {
  
  cat("🚨 Detecting significant tendency changes...\n")
  
  # Join old and new tendencies
  tendency_comparison <- old_tendencies %>%
    inner_join(new_tendencies, 
               by = c("team", "situation", "context_type"),
               suffix = c("_old", "_new")) %>%
    mutate(
      # Calculate changes
      pass_rate_change = pass_rate_new - pass_rate_old,
      success_rate_change = success_rate_new - success_rate_old,
      epa_change = epa_per_play_new - epa_per_play_old,
      
      # Flag significant changes
      significant_pass_change = abs(pass_rate_change) > change_threshold,
      significant_success_change = abs(success_rate_change) > change_threshold,
      significant_epa_change = abs(epa_change) > 0.1
    )
  
  # Extract significant changes
  significant_changes <- tendency_comparison %>%
    filter(significant_pass_change | significant_success_change | significant_epa_change) %>%
    select(team, situation, context_type, ends_with("_change"), 
           starts_with("significant_")) %>%
    mutate(
      change_magnitude = sqrt(pass_rate_change^2 + success_rate_change^2 + epa_change^2),
      detected_at = Sys.time()
    ) %>%
    arrange(desc(change_magnitude))
  
  if (nrow(significant_changes) > 0) {
    cat(sprintf("🔔 Detected %d significant tendency changes\n", nrow(significant_changes)))
  } else {
    cat("✅ No significant tendency changes detected\n")
  }
  
  return(significant_changes)
}

# ==============================================================================
# MATCHUP-SPECIFIC ANALYSIS
# ==============================================================================

#' Generate situational matchup analysis
#' 
#' Analyzes how team A's tendencies match up against team B's defensive performance
#' in specific situations. Critical for game-specific predictions.
#' 
#' @param team_a Offensive team
#' @param team_b Defensive team  
#' @param team_tendencies Current team tendency data
#' @param situation_filter Optional filter for specific situations
#' @return Matchup analysis data
#' @export
analyze_situational_matchup <- function(team_a, team_b, team_tendencies, 
                                       situation_filter = NULL) {
  
  cat(sprintf("⚔️ Analyzing situational matchup: %s vs %s\n", team_a, team_b))
  
  # Get offensive tendencies for team A
  team_a_offense <- team_tendencies %>%
    filter(team == team_a)
  
  # Get defensive performance for team B (when they were defending)
  # This would require defensive-specific tendency tracking
  team_b_defense <- team_tendencies %>%
    filter(team == team_b) %>%
    # For now, use inverse logic - areas where they struggle defensively
    # In practice, you'd want separate defensive tendency tracking
    mutate(
      def_success_rate_allowed = 1 - success_rate,  # Simplified
      def_epa_allowed = -epa_per_play  # Simplified inverse
    )
  
  # Apply situation filter if provided
  if (!is.null(situation_filter)) {
    team_a_offense <- team_a_offense %>% filter(situation %in% situation_filter)
    team_b_defense <- team_b_defense %>% filter(situation %in% situation_filter)
  }
  
  # Calculate matchup advantages
  matchup_analysis <- team_a_offense %>%
    inner_join(team_b_defense, by = c("situation", "context_type"), 
               suffix = c("_offense", "_defense")) %>%
    mutate(
      # Calculate expected performance based on matchup
      expected_success_rate = (success_rate_offense + def_success_rate_allowed) / 2,
      expected_epa = (epa_per_play_offense - def_epa_allowed) / 2,
      
      # Identify favorable matchups
      favorable_passing = pass_rate_deviation_offense > 0.1 & def_success_rate_allowed > 0.5,
      favorable_running = pass_rate_deviation_offense < -0.1 & def_success_rate_allowed > 0.5,
      
      # Overall matchup rating
      matchup_rating = expected_success_rate * expected_epa,
      
      # Confidence in prediction based on sample sizes
      prediction_confidence = pmin(total_plays_offense, total_plays_defense) / 50
    ) %>%
    arrange(desc(matchup_rating))
  
  # Summarize key insights
  key_insights <- list(
    total_situations_analyzed = nrow(matchup_analysis),
    best_matchup_situations = head(matchup_analysis, 3)$situation,
    worst_matchup_situations = tail(matchup_analysis, 3)$situation,
    overall_matchup_rating = mean(matchup_analysis$matchup_rating, na.rm = TRUE),
    high_confidence_situations = sum(matchup_analysis$prediction_confidence > 0.8)
  )
  
  cat(sprintf("✅ Analyzed %d situations with overall rating: %.3f\n", 
              key_insights$total_situations_analyzed, 
              key_insights$overall_matchup_rating))
  
  return(list(
    detailed_analysis = matchup_analysis,
    key_insights = key_insights,
    team_a = team_a,
    team_b = team_b,
    analysis_timestamp = Sys.time()
  ))
}

# ==============================================================================
# STORAGE AND PERSISTENCE
# ==============================================================================

#' Store team tendencies in database
#' 
#' Persists team tendency analysis for use by the prediction system
#' 
#' @param tendencies Team tendency data frame
#' @param db_path Path to prediction database
#' @param season Season year
#' @param week Week number (optional)
#' @export
store_team_tendencies <- function(tendencies, db_path = DEFAULT_DB_PATH, 
                                season = year(Sys.Date()), week = NULL) {
  
  cat("💾 Storing team tendencies in database...\n")
  
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Create table if it doesn't exist
    create_tendencies_table_sql <- "
    CREATE TABLE IF NOT EXISTS team_tendencies (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      team TEXT NOT NULL,
      season INTEGER NOT NULL,
      week INTEGER,
      situation TEXT NOT NULL,
      context_type TEXT NOT NULL,
      total_plays INTEGER,
      pass_rate REAL,
      run_rate REAL,
      success_rate REAL,
      epa_per_play REAL,
      explosive_play_rate REAL,
      turnover_rate REAL,
      sack_rate REAL,
      first_down_rate REAL,
      touchdown_rate REAL,
      league_pass_rate REAL,
      league_success_rate REAL,
      league_epa_per_play REAL,
      pass_rate_deviation REAL,
      success_rate_deviation REAL,
      epa_deviation REAL,
      deviation_score REAL,
      significant_pass_deviation BOOLEAN,
      significant_success_deviation BOOLEAN,
      significant_epa_deviation BOOLEAN,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(team, season, week, situation, context_type)
    )"
    
    dbExecute(con, create_tendencies_table_sql)
    
    # Prepare data for insertion
    tendencies_to_store <- tendencies %>%
      mutate(
        season = season,
        week = week,
        created_at = Sys.time(),
        updated_at = Sys.time()
      )
    
    # Insert data with UPSERT logic
    for (i in 1:nrow(tendencies_to_store)) {
      row_data <- tendencies_to_store[i,]
      
      # Convert logical columns to integers for SQLite
      row_data$significant_pass_deviation <- as.integer(row_data$significant_pass_deviation)
      row_data$significant_success_deviation <- as.integer(row_data$significant_success_deviation)
      row_data$significant_epa_deviation <- as.integer(row_data$significant_epa_deviation)
      
      insert_sql <- "
      INSERT OR REPLACE INTO team_tendencies (
        team, season, week, situation, context_type, total_plays,
        pass_rate, run_rate, success_rate, epa_per_play, explosive_play_rate,
        turnover_rate, sack_rate, first_down_rate, touchdown_rate,
        league_pass_rate, league_success_rate, league_epa_per_play,
        pass_rate_deviation, success_rate_deviation, epa_deviation, deviation_score,
        significant_pass_deviation, significant_success_deviation, significant_epa_deviation,
        created_at, updated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      
      dbExecute(con, insert_sql, params = as.list(row_data[c(
        "team", "season", "week", "situation", "context_type", "total_plays",
        "pass_rate", "run_rate", "success_rate", "epa_per_play", "explosive_play_rate",
        "turnover_rate", "sack_rate", "first_down_rate", "touchdown_rate",
        "league_pass_rate", "league_success_rate", "league_epa_per_play",
        "pass_rate_deviation", "success_rate_deviation", "epa_deviation", "deviation_score",
        "significant_pass_deviation", "significant_success_deviation", "significant_epa_deviation",
        "created_at", "updated_at"
      )]))
    }
    
    cat(sprintf("✅ Stored %d team tendency records\n", nrow(tendencies_to_store)))
    
  }, error = function(e) {
    cat(sprintf("❌ Error storing tendencies: %s\n", e$message))
  })
}

#' Load team tendencies from database
#' 
#' Retrieves stored team tendency data for use in predictions
#' 
#' @param teams Vector of team names (optional)
#' @param season Season year
#' @param week Week number (optional)
#' @param db_path Path to prediction database
#' @return Team tendency data frame
#' @export
load_team_tendencies <- function(teams = NULL, season = year(Sys.Date()), 
                               week = NULL, db_path = DEFAULT_DB_PATH) {
  
  cat("📖 Loading team tendencies from database...\n")
  
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Build query
    query <- "SELECT * FROM team_tendencies WHERE season = ?"
    params <- list(season)
    
    if (!is.null(week)) {
      query <- paste(query, "AND (week IS NULL OR week <= ?)")
      params <- append(params, week)
    }
    
    if (!is.null(teams)) {
      placeholders <- paste(rep("?", length(teams)), collapse = ",")
      query <- paste(query, sprintf("AND team IN (%s)", placeholders))
      params <- append(params, teams)
    }
    
    query <- paste(query, "ORDER BY team, situation, updated_at DESC")
    
    tendencies <- dbGetQuery(con, query, params = params)
    
    if (nrow(tendencies) > 0) {
      # Convert integer back to logical for consistency
      tendencies$significant_pass_deviation <- as.logical(tendencies$significant_pass_deviation)
      tendencies$significant_success_deviation <- as.logical(tendencies$significant_success_deviation)
      tendencies$significant_epa_deviation <- as.logical(tendencies$significant_epa_deviation)
      
      cat(sprintf("✅ Loaded %d tendency records for %d teams\n", 
                  nrow(tendencies), length(unique(tendencies$team))))
    } else {
      cat("⚠️ No tendency data found for specified criteria\n")
    }
    
    return(tendencies)
    
  }, error = function(e) {
    cat(sprintf("❌ Error loading tendencies: %s\n", e$message))
    return(data.frame())
  })
}

# ==============================================================================
# INTEGRATION WITH PREDICTION SYSTEM
# ==============================================================================

#' Generate situational features for prediction model
#' 
#' Creates features based on team tendencies for a specific matchup
#' These features will be used by the dynamic ensemble system
#' 
#' @param home_team Home team abbreviation
#' @param away_team Away team abbreviation
#' @param tendencies Team tendency data
#' @param game_context Game context (weather, primetime, etc.)
#' @return Feature vector for prediction model
#' @export
generate_situational_features <- function(home_team, away_team, tendencies, 
                                        game_context = list()) {
  
  cat(sprintf("🎯 Generating situational features for %s vs %s\n", away_team, home_team))
  
  # Get team tendencies
  home_tendencies <- tendencies %>% filter(team == home_team)
  away_tendencies <- tendencies %>% filter(team == away_team)
  
  if (nrow(home_tendencies) == 0 || nrow(away_tendencies) == 0) {
    cat("⚠️ Insufficient tendency data for feature generation\n")
    return(list())
  }
  
  # Calculate key situational features
  features <- list(
    # First down tendencies (like the LAC insight)
    home_1st_down_pass_rate = get_tendency_value(home_tendencies, "1st_and_10", "pass_rate"),
    away_1st_down_pass_rate = get_tendency_value(away_tendencies, "1st_and_10", "pass_rate"),
    home_1st_down_pass_deviation = get_tendency_value(home_tendencies, "1st_and_10", "pass_rate_deviation"),
    away_1st_down_pass_deviation = get_tendency_value(away_tendencies, "1st_and_10", "pass_rate_deviation"),
    
    # Red zone efficiency
    home_red_zone_td_rate = get_tendency_value(home_tendencies, "red_zone", "touchdown_rate"),
    away_red_zone_td_rate = get_tendency_value(away_tendencies, "red_zone", "touchdown_rate"),
    
    # Third down performance
    home_3rd_long_success = get_tendency_value(home_tendencies, "3rd_and_long", "success_rate"),
    away_3rd_long_success = get_tendency_value(away_tendencies, "3rd_and_long", "success_rate"),
    
    # Situational EPA
    home_avg_epa_deviation = mean(home_tendencies$epa_deviation, na.rm = TRUE),
    away_avg_epa_deviation = mean(away_tendencies$epa_deviation, na.rm = TRUE),
    
    # Tendency volatility (how different they are from league average)
    home_tendency_volatility = mean(home_tendencies$deviation_score, na.rm = TRUE),
    away_tendency_volatility = mean(away_tendencies$deviation_score, na.rm = TRUE),
    
    # Matchup-specific features
    pass_tendency_mismatch = abs(get_tendency_value(home_tendencies, "1st_and_10", "pass_rate") -
                                get_tendency_value(away_tendencies, "1st_and_10", "pass_rate")),
    
    # Feature metadata
    home_data_quality = nrow(home_tendencies),
    away_data_quality = nrow(away_tendencies),
    feature_generation_time = Sys.time()
  )
  
  # Add game context features if provided
  if (length(game_context) > 0) {
    features <- c(features, game_context)
  }
  
  cat(sprintf("✅ Generated %d situational features\n", length(features)))
  
  return(features)
}

#' Helper function to safely extract tendency values
get_tendency_value <- function(tendencies, situation, metric) {
  value <- tendencies %>%
    filter(situation == !!situation) %>%
    pull(!!metric)
  
  if (length(value) > 0) {
    return(value[1])  # Take first match
  } else {
    return(0)  # Default to neutral value
  }
}

# ==============================================================================
# SYSTEM TESTING AND VALIDATION
# ==============================================================================

#' Test the situational analysis system
#' 
#' Validates system functionality with sample data
#' 
#' @param sample_pbp_data Sample play-by-play data for testing
#' @export
test_situational_analysis_system <- function(sample_pbp_data = NULL) {
  
  cat("🧪 Testing situational analysis system...\n")
  
  # Use sample data if none provided
  if (is.null(sample_pbp_data)) {
    # Create minimal sample data for testing
    sample_pbp_data <- create_sample_pbp_data()
  }
  
  test_results <- list()
  
  # Test 1: Extract tendencies
  cat("Test 1: Extracting team tendencies...\n")
  tryCatch({
    tendencies <- extract_team_situational_tendencies(sample_pbp_data, 
                                                    teams = c("LAC", "KC"), 
                                                    min_plays = 5)
    test_results$tendency_extraction <- ifelse(nrow(tendencies) > 0, "PASS", "FAIL")
    cat("✅ Tendency extraction test passed\n")
  }, error = function(e) {
    test_results$tendency_extraction <- "FAIL"
    cat(sprintf("❌ Tendency extraction test failed: %s\n", e$message))
  })
  
  # Test 2: Matchup analysis
  cat("Test 2: Matchup analysis...\n")
  tryCatch({
    matchup <- analyze_situational_matchup("LAC", "KC", tendencies)
    test_results$matchup_analysis <- ifelse(!is.null(matchup$key_insights), "PASS", "FAIL")
    cat("✅ Matchup analysis test passed\n")
  }, error = function(e) {
    test_results$matchup_analysis <- "FAIL"
    cat(sprintf("❌ Matchup analysis test failed: %s\n", e$message))
  })
  
  # Test 3: Feature generation
  cat("Test 3: Feature generation...\n")
  tryCatch({
    features <- generate_situational_features("LAC", "KC", tendencies)
    test_results$feature_generation <- ifelse(length(features) > 0, "PASS", "FAIL")
    cat("✅ Feature generation test passed\n")
  }, error = function(e) {
    test_results$feature_generation <- "FAIL"
    cat(sprintf("❌ Feature generation test failed: %s\n", e$message))
  })
  
  # Test 4: Database operations (if DB exists)
  cat("Test 4: Database operations...\n")
  test_db_path <- "test_situational.db"
  tryCatch({
    if (exists("tendencies") && nrow(tendencies) > 0) {
      store_team_tendencies(tendencies, test_db_path, 2025, 1)
      loaded_tendencies <- load_team_tendencies(c("LAC", "KC"), 2025, 1, test_db_path)
      test_results$database_operations <- ifelse(nrow(loaded_tendencies) > 0, "PASS", "FAIL")
      
      # Clean up test database
      if (file.exists(test_db_path)) file.remove(test_db_path)
      cat("✅ Database operations test passed\n")
    } else {
      test_results$database_operations <- "SKIP"
      cat("⏭️ Database operations test skipped (no data)\n")
    }
  }, error = function(e) {
    test_results$database_operations <- "FAIL"
    cat(sprintf("❌ Database operations test failed: %s\n", e$message))
  })
  
  # Summary
  passed_tests <- sum(test_results == "PASS")
  total_tests <- length(test_results)
  
  cat(sprintf("\n🏁 Testing complete: %d/%d tests passed\n", passed_tests, total_tests))
  
  return(test_results)
}

#' Create minimal sample data for testing
create_sample_pbp_data <- function() {
  # Create basic sample play-by-play data
  expand.grid(
    posteam = c("LAC", "KC"),
    down = 1:4,
    ydstogo = c(1, 5, 10, 15),
    yardline_100 = c(10, 30, 50, 70),
    qtr = 1:4,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      play_id = 1:n(),
      game_id = paste("2025_01", posteam, ifelse(posteam == "LAC", "KC", "LAC"), sep = "_"),
      season = 2025,
      week = 1,
      play_type = sample(c("pass", "run"), n(), replace = TRUE, prob = c(0.6, 0.4)),
      success = sample(0:1, n(), replace = TRUE, prob = c(0.55, 0.45)),
      epa = rnorm(n(), 0, 1),
      yards_gained = sample(0:20, n(), replace = TRUE),
      turnover = sample(0:1, n(), replace = TRUE, prob = c(0.97, 0.03)),
      touchdown = sample(0:1, n(), replace = TRUE, prob = c(0.95, 0.05)),
      first_down = sample(0:1, n(), replace = TRUE, prob = c(0.65, 0.35)),
      sack = sample(0:1, n(), replace = TRUE, prob = c(0.95, 0.05)),
      home_team = ifelse(posteam == "LAC", "LAC", "KC"),
      away_team = ifelse(posteam == "LAC", "KC", "LAC"),
      defteam = ifelse(posteam == "LAC", "KC", "LAC"),
      posteam_score = sample(0:35, n(), replace = TRUE),
      defteam_score = sample(0:35, n(), replace = TRUE),
      game_seconds_remaining = sample(0:3600, n(), replace = TRUE),
      quarter_seconds_remaining = sample(0:900, n(), replace = TRUE)
    )
}

# ==============================================================================
# INITIALIZATION
# ==============================================================================

cat("🎯 Situational Analysis Module v2.0 Loaded\n")
cat("Key Functions Available:\n")
cat("  - extract_team_situational_tendencies(): Extract team tendencies from play data\n")
cat("  - analyze_situational_matchup(): Analyze team vs team matchups\n")
cat("  - generate_situational_features(): Create prediction features\n")
cat("  - update_team_tendencies_incremental(): Update with new game data\n")
cat("  - detect_tendency_changes(): Identify significant changes\n")
cat("  - store_team_tendencies() / load_team_tendencies(): Database operations\n")
cat("  - test_situational_analysis_system(): Validate system functionality\n")
cat("\nExample Usage:\n")
cat("  # Extract tendencies\n")
cat("  tendencies <- extract_team_situational_tendencies(pbp_data)\n")
cat("  \n")
cat("  # Analyze LAC vs KC matchup\n")
cat("  matchup <- analyze_situational_matchup('LAC', 'KC', tendencies)\n")
cat("  \n")
cat("  # Generate features for prediction\n")
cat("  features <- generate_situational_features('LAC', 'KC', tendencies)\n")

cat("\n🔍 System ready to detect situational tendencies like LAC's high 1st down pass rate!\n")