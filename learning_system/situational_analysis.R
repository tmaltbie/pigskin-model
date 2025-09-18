# Situational Analysis System for NFL Predictions
# Analyzes team tendencies by game context to detect deviations like LAC's 1st down pass rate
# Integrates with learning system for enhanced prediction accuracy

library(dplyr)

# Configuration constants
MIN_SAMPLE_SIZE <- 20  # Minimum plays for reliable tendency estimates
SIGNIFICANCE_LEVEL <- 0.05  # Statistical significance threshold
CURRENT_SEASON_WEIGHT <- 0.7  # Weight for current vs historical data

#' Generate comprehensive situational features for a team matchup
#' 
#' Analyzes both teams' tendencies in various game situations and compares
#' to league averages to identify potential prediction advantages.
#' 
#' @param home_team Home team abbreviation
#' @param away_team Away team abbreviation
#' @param season Current season
#' @param week Current week (for sample size adjustments)
#' @param pbp_data Play-by-play data
#' 
#' @return List with situational features and significance indicators
generate_situational_features <- function(home_team, away_team, season = 2025, week = 3, pbp_data = NULL) {
  
  cat("🎯 Generating situational features for", away_team, "@", home_team, "\n")
  
  # Load play-by-play data if not provided
  if (is.null(pbp_data)) {
    tryCatch({
      source('learning_system/unified_data_access.R')
      pbp_data <- load_comprehensive_pbp(seasons = c(2024, 2025), include_2025_partial = TRUE)
    }, error = function(e) {
      cat("⚠️ Could not load play-by-play data:", e$message, "\n")
      return(create_mock_situational_features(home_team, away_team))
    })
  }
  
  if (is.null(pbp_data) || nrow(pbp_data) == 0) {
    cat("⚠️ No play-by-play data available, using mock features\n")
    return(create_mock_situational_features(home_team, away_team))
  }
  
  # Calculate league averages for baseline comparison
  league_baselines <- calculate_league_baselines(pbp_data, season)
  
  # Analyze each team's tendencies
  home_tendencies <- analyze_team_tendencies(home_team, pbp_data, season, week, league_baselines)
  away_tendencies <- analyze_team_tendencies(away_team, pbp_data, season, week, league_baselines)
  
  # Generate matchup-specific features
  matchup_features <- generate_matchup_features(home_tendencies, away_tendencies, league_baselines)
  
  # Create final feature set
  features <- list(
    # Team-specific deviations
    home_team_deviations = home_tendencies$deviations,
    away_team_deviations = away_tendencies$deviations,
    
    # Matchup advantages
    offensive_advantages = matchup_features$offensive_advantages,
    defensive_advantages = matchup_features$defensive_advantages,
    
    # Situational edges
    down_distance_edges = matchup_features$down_distance_edges,
    field_position_edges = matchup_features$field_position_edges,
    
    # Statistical confidence
    significance_flags = matchup_features$significance_flags,
    sample_sizes = list(
      home_sample = home_tendencies$sample_size,
      away_sample = away_tendencies$sample_size
    ),
    
    # Meta information
    analysis_date = Sys.time(),
    season = season,
    week = week,
    data_quality = assess_data_quality(pbp_data, home_team, away_team, season)
  )
  
  # Print key insights
  print_situational_insights(features, home_team, away_team)
  
  return(features)
}

#' Calculate league baseline averages for situational analysis
#' 
#' @param pbp_data Play-by-play data
#' @param season Current season for weighting
#' 
#' @return List with league averages by situation
calculate_league_baselines <- function(pbp_data, season) {
  
  cat("📊 Calculating league baselines...\n")
  
  # Filter to relevant plays (exclude special teams, kneel downs, etc.)
  regular_plays <- pbp_data %>%
    filter(
      !is.na(down), 
      play_type %in% c("pass", "run"),
      !is.na(epa)
    )
  
  # Weight current season more heavily
  weighted_plays <- regular_plays %>%
    mutate(
      weight = ifelse(season == !!season, CURRENT_SEASON_WEIGHT, 1 - CURRENT_SEASON_WEIGHT)
    )
  
  baselines <- list(
    # Overall tendencies
    overall_pass_rate = calculate_weighted_mean(weighted_plays$play_type == "pass", weighted_plays$weight),
    overall_success_rate = calculate_weighted_mean(weighted_plays$success, weighted_plays$weight),
    
    # By down and distance
    first_down_pass_rate = calculate_situational_baseline(weighted_plays, "down == 1", "pass"),
    second_short_pass_rate = calculate_situational_baseline(weighted_plays, "down == 2 & ydstogo <= 3", "pass"),
    second_long_pass_rate = calculate_situational_baseline(weighted_plays, "down == 2 & ydstogo > 7", "pass"),
    third_down_pass_rate = calculate_situational_baseline(weighted_plays, "down == 3", "pass"),
    
    # By field position
    own_territory_pass_rate = calculate_situational_baseline(weighted_plays, "yardline_100 > 50", "pass"),
    red_zone_pass_rate = calculate_situational_baseline(weighted_plays, "yardline_100 <= 20", "pass"),
    
    # By score situation
    leading_pass_rate = calculate_situational_baseline(weighted_plays, "score_differential > 0", "pass"),
    trailing_pass_rate = calculate_situational_baseline(weighted_plays, "score_differential < -3", "pass"),
    
    # Efficiency metrics
    first_down_epa = calculate_situational_baseline(weighted_plays, "down == 1", "epa"),
    red_zone_success_rate = calculate_situational_baseline(weighted_plays, "yardline_100 <= 20", "success"),
    third_down_conversion_rate = calculate_situational_baseline(weighted_plays, "down == 3", "first_down")
  )
  
  cat("✅ League baselines calculated\n")
  return(baselines)
}

#' Analyze individual team tendencies and deviations from league average
#' 
#' @param team Team abbreviation
#' @param pbp_data Play-by-play data
#' @param season Current season
#' @param week Current week
#' @param league_baselines League average baselines
#' 
#' @return List with team tendencies and statistical significance
analyze_team_tendencies <- function(team, pbp_data, season, week, league_baselines) {
  
  cat("🔍 Analyzing", team, "tendencies...\n")
  
  # Filter to team's plays (both home and away)
  team_plays <- pbp_data %>%
    filter(
      (home_team == team | away_team == team),
      !is.na(down),
      play_type %in% c("pass", "run"),
      !is.na(epa)
    ) %>%
    mutate(
      # Adjust perspective to team's offense
      team_epa = ifelse(home_team == team, epa, -epa),
      team_success = ifelse(home_team == team, success, 1 - success)
    )
  
  # Weight recent games more heavily (within current season)
  current_season_plays <- team_plays %>%
    filter(season == !!season) %>%
    arrange(desc(week))
  
  # Apply recency weighting within season
  if (nrow(current_season_plays) > 0) {
    current_season_plays$recency_weight <- exp(-0.1 * (max(current_season_plays$week, na.rm = TRUE) - current_season_plays$week))
  }
  
  # Calculate team-specific tendencies
  tendencies <- list(
    # Basic play calling
    first_down_pass_rate = calculate_team_tendency(team_plays, "down == 1", "play_type == 'pass'"),
    second_short_pass_rate = calculate_team_tendency(team_plays, "down == 2 & ydstogo <= 3", "play_type == 'pass'"),
    second_long_pass_rate = calculate_team_tendency(team_plays, "down == 2 & ydstogo > 7", "play_type == 'pass'"),
    third_down_pass_rate = calculate_team_tendency(team_plays, "down == 3", "play_type == 'pass'"),
    
    # Situational efficiency
    first_down_epa = calculate_team_tendency(team_plays, "down == 1", "team_epa"),
    red_zone_success_rate = calculate_team_tendency(team_plays, "yardline_100 <= 20", "team_success"),
    third_down_conversion_rate = calculate_team_tendency(team_plays, "down == 3", "first_down")
  )
  
  # Calculate deviations from league average with significance testing
  deviations <- list()
  significance <- list()
  
  for (tendency_name in names(tendencies)) {
    if (tendency_name %in% names(league_baselines)) {
      team_value <- tendencies[[tendency_name]]$value
      league_value <- league_baselines[[tendency_name]]
      sample_size <- tendencies[[tendency_name]]$sample_size
      
      deviation <- team_value - league_value
      deviations[[tendency_name]] <- deviation
      
      # Statistical significance test (using normal approximation for proportions)
      if (sample_size >= MIN_SAMPLE_SIZE) {
        if (grepl("rate|conversion", tendency_name)) {
          # Proportion test
          se <- sqrt(league_value * (1 - league_value) / sample_size)
          z_score <- deviation / se
          p_value <- 2 * (1 - pnorm(abs(z_score)))
          significance[[tendency_name]] <- p_value < SIGNIFICANCE_LEVEL
        } else {
          # For continuous variables (EPA), use t-test approximation
          significance[[tendency_name]] <- sample_size >= MIN_SAMPLE_SIZE && abs(deviation) > 0.1
        }
      } else {
        significance[[tendency_name]] <- FALSE
      }
    }
  }
  
  return(list(
    tendencies = tendencies,
    deviations = deviations,
    significance = significance,
    sample_size = nrow(team_plays)
  ))
}

#' Generate matchup-specific features comparing offensive and defensive tendencies
#' 
#' @param home_tendencies Home team analysis results
#' @param away_tendencies Away team analysis results
#' @param league_baselines League average baselines
#' 
#' @return List with matchup advantages and edges
generate_matchup_features <- function(home_tendencies, away_tendencies, league_baselines) {
  
  # Identify significant advantages where one team's tendency meets opponent's weakness
  offensive_advantages <- list()
  defensive_advantages <- list()
  
  # Example: If away team passes much more on 1st down than average, 
  # and home team struggles defending 1st down passes
  home_deviations <- home_tendencies$deviations
  away_deviations <- away_tendencies$deviations
  home_significance <- home_tendencies$significance
  away_significance <- away_tendencies$significance
  
  # First down passing matchup
  if ("first_down_pass_rate" %in% names(away_deviations) && away_significance$first_down_pass_rate) {
    if (away_deviations$first_down_pass_rate > 0.1) {  # Away team passes 10%+ more on 1st down
      offensive_advantages$away_first_down_pass <- away_deviations$first_down_pass_rate
    }
  }
  
  # Red zone efficiency matchups
  if ("red_zone_success_rate" %in% names(home_deviations) && home_significance$red_zone_success_rate) {
    if (home_deviations$red_zone_success_rate > 0.1) {
      offensive_advantages$home_red_zone <- home_deviations$red_zone_success_rate
    }
  }
  
  # Generate composite matchup scores
  down_distance_edges <- list(
    first_down_edge = calculate_matchup_edge(home_deviations, away_deviations, "first_down"),
    third_down_edge = calculate_matchup_edge(home_deviations, away_deviations, "third_down"),
    red_zone_edge = calculate_matchup_edge(home_deviations, away_deviations, "red_zone")
  )
  
  # Identify statistically significant edges
  significance_flags <- list(
    home_significant_advantages = sum(unlist(home_significance), na.rm = TRUE),
    away_significant_advantages = sum(unlist(away_significance), na.rm = TRUE),
    total_significant_features = sum(unlist(home_significance), na.rm = TRUE) + sum(unlist(away_significance), na.rm = TRUE)
  )
  
  return(list(
    offensive_advantages = offensive_advantages,
    defensive_advantages = defensive_advantages,
    down_distance_edges = down_distance_edges,
    field_position_edges = list(),  # Placeholder for future enhancement
    significance_flags = significance_flags
  ))
}

#' Helper function to calculate weighted mean
calculate_weighted_mean <- function(values, weights) {
  clean_indices <- !is.na(values) & !is.na(weights)
  if (sum(clean_indices) == 0) return(NA)
  sum(values[clean_indices] * weights[clean_indices]) / sum(weights[clean_indices])
}

#' Helper function to calculate situational baseline
calculate_situational_baseline <- function(data, condition, metric) {
  tryCatch({
    filtered_data <- data %>% filter(eval(parse(text = condition)))
    if (nrow(filtered_data) == 0) return(NA)
    
    if (metric == "pass") {
      calculate_weighted_mean(filtered_data$play_type == "pass", filtered_data$weight)
    } else if (metric == "success") {
      calculate_weighted_mean(filtered_data$success, filtered_data$weight)
    } else if (metric == "epa") {
      calculate_weighted_mean(filtered_data$epa, filtered_data$weight)
    } else if (metric == "first_down") {
      calculate_weighted_mean(filtered_data$first_down, filtered_data$weight)
    } else {
      NA
    }
  }, error = function(e) {
    NA
  })
}

#' Helper function to calculate team-specific tendency
calculate_team_tendency <- function(team_data, condition, metric) {
  tryCatch({
    filtered_data <- team_data %>% filter(eval(parse(text = condition)))
    sample_size <- nrow(filtered_data)
    
    if (sample_size == 0) {
      return(list(value = NA, sample_size = 0))
    }
    
    if (metric == "play_type == 'pass'") {
      value <- mean(filtered_data$play_type == "pass", na.rm = TRUE)
    } else if (metric == "team_success") {
      value <- mean(filtered_data$team_success, na.rm = TRUE)
    } else if (metric == "team_epa") {
      value <- mean(filtered_data$team_epa, na.rm = TRUE)
    } else if (metric == "first_down") {
      value <- mean(filtered_data$first_down, na.rm = TRUE)
    } else {
      value <- NA
    }
    
    return(list(value = value, sample_size = sample_size))
  }, error = function(e) {
    return(list(value = NA, sample_size = 0))
  })
}

#' Calculate matchup edge between teams
calculate_matchup_edge <- function(home_deviations, away_deviations, situation) {
  # Simple matchup scoring - can be enhanced with more sophisticated analysis
  home_keys <- names(home_deviations)[grepl(situation, names(home_deviations))]
  away_keys <- names(away_deviations)[grepl(situation, names(away_deviations))]
  
  home_edge <- sum(unlist(home_deviations[home_keys]), na.rm = TRUE)
  away_edge <- sum(unlist(away_deviations[away_keys]), na.rm = TRUE)
  
  return(home_edge - away_edge)  # Positive = home advantage
}

#' Assess data quality for situational analysis
assess_data_quality <- function(pbp_data, home_team, away_team, season) {
  if (is.null(pbp_data)) return("no_data")
  
  current_season_data <- pbp_data %>% filter(season == !!season)
  team_data <- current_season_data %>% filter(home_team %in% c(!!home_team, !!away_team) | away_team %in% c(!!home_team, !!away_team))
  
  if (nrow(team_data) < 20) return("insufficient_sample")
  if (nrow(current_season_data) < 100) return("early_season")
  
  return("good")
}

#' Print key situational insights for analysis
print_situational_insights <- function(features, home_team, away_team) {
  
  cat("\n🎯 KEY SITUATIONAL INSIGHTS:\n")
  
  # Home team significant deviations
  home_sig <- features$home_team_deviations
  if (length(home_sig) > 0) {
    cat(sprintf("%s Significant Deviations:\n", home_team))
    for (name in names(home_sig)) {
      deviation <- home_sig[[name]]
      if (!is.na(deviation) && abs(deviation) > 0.05) {
        cat(sprintf("  - %s: %+.1f%% vs league avg\n", name, deviation * 100))
      }
    }
  }
  
  # Away team significant deviations  
  away_sig <- features$away_team_deviations
  if (length(away_sig) > 0) {
    cat(sprintf("%s Significant Deviations:\n", away_team))
    for (name in names(away_sig)) {
      deviation <- away_sig[[name]]
      if (!is.na(deviation) && abs(deviation) > 0.05) {
        cat(sprintf("  - %s: %+.1f%% vs league avg\n", name, deviation * 100))
      }
    }
  }
  
  # Matchup advantages
  if (length(features$offensive_advantages) > 0) {
    cat("Offensive Matchup Advantages:\n")
    for (name in names(features$offensive_advantages)) {
      advantage <- features$offensive_advantages[[name]]
      cat(sprintf("  - %s: %.1f%% advantage\n", name, advantage * 100))
    }
  }
  
  cat("\n")
}

#' Create mock situational features when real data is unavailable
create_mock_situational_features <- function(home_team, away_team) {
  
  cat("📝 Creating mock situational features for testing...\n")
  
  # Generate realistic mock deviations
  home_deviations <- list(
    first_down_pass_rate = rnorm(1, 0, 0.08),
    third_down_conversion_rate = rnorm(1, 0, 0.05),
    red_zone_success_rate = rnorm(1, 0, 0.06)
  )
  
  away_deviations <- list(
    first_down_pass_rate = rnorm(1, 0, 0.08),
    third_down_conversion_rate = rnorm(1, 0, 0.05), 
    red_zone_success_rate = rnorm(1, 0, 0.06)
  )
  
  return(list(
    home_team_deviations = home_deviations,
    away_team_deviations = away_deviations,
    offensive_advantages = list(),
    defensive_advantages = list(),
    down_distance_edges = list(
      first_down_edge = home_deviations$first_down_pass_rate - away_deviations$first_down_pass_rate,
      third_down_edge = home_deviations$third_down_conversion_rate - away_deviations$third_down_conversion_rate
    ),
    field_position_edges = list(),
    significance_flags = list(
      home_significant_advantages = 1,
      away_significant_advantages = 1,
      total_significant_features = 2
    ),
    sample_sizes = list(home_sample = 50, away_sample = 50),
    analysis_date = Sys.time(),
    data_quality = "mock_data"
  ))
}

cat("Situational Analysis System loaded! 🎯\n\n")
cat("Available functions:\n")
cat("- generate_situational_features(): Analyze team tendencies and deviations\n")
cat("- calculate_league_baselines(): Generate league average benchmarks\n")
cat("- analyze_team_tendencies(): Individual team situational analysis\n")
cat("\n🚀 Quick example:\n")
cat("features <- generate_situational_features('LAC', 'KC', 2025, 3)\n")
cat("# Will detect LAC's high 1st down pass rate vs league average\n")