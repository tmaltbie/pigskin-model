# Week 3 2025 EV-Based Betting Chart
# ML & ATS suggestions based on Expected Value

# Function to create betting recommendations chart
create_week3_betting_chart <- function() {
  
  cat("📊 WEEK 3 2025 BETTING RECOMMENDATIONS - EV ANALYSIS\n")
  cat("===================================================\n\n")
  
  # Our predictions vs market (from the output above)
  betting_data <- data.frame(
    game = c("MIA @ BUF", "ATL @ CAR", "GB @ CLE", "HOU @ JAX", "CIN @ MIN", 
             "PIT @ NE", "LAR @ PHI", "NYJ @ TB", "IND @ TEN", "LV @ WAS",
             "DEN @ LAC", "NO @ SEA", "DAL @ CHI", "ARI @ SF", "KC @ NYG", "DET @ BAL"),
    
    our_spread = c(-4.9, +1.4, -1.0, -3.5, -0.3, -1.8, -2.1, -2.4, -0.0, -3.1,
                   -2.4, -2.1, -0.7, -1.8, +0.7, -0.0),
    
    vegas_spread = c(-6.5, -3.0, -2.5, -3.0, -3.5, -4.0, -3.5, -2.5, -2.0, -1.5,
                     -3.0, -4.5, -3.5, -7.0, -3.0, -3.5),
    
    edge = c(-1.6, -4.4, -1.5, +0.5, -3.1, -2.2, -1.4, -0.1, -2.0, +1.6,
             -0.6, -2.4, -2.8, -5.2, -3.7, -3.5),
    
    # Estimated current moneyline odds
    away_ml = c(+265, -115, +115, +125, +140, +165, +140, +115, +100, +65,
                +125, +185, +140, +280, +125, +140),
    home_ml = c(-320, -105, -135, -145, -160, -185, -160, -135, -120, -85,
                -145, -220, -160, -340, -145, -160),
    
    # Our ATS pick
    ats_pick = c("MIA", "ATL", "PUSH", "PUSH", "CIN", "PIT", "PUSH", "PUSH", "IND", "WAS",
                 "PUSH", "NO", "DAL", "ARI", "KC", "DET"),
    
    # Our ML pick  
    ml_pick = c("BUF", "ATL", "CLE", "JAX", "MIN", "NE", "PHI", "TB", "IND", "WAS",
                "LAC", "SEA", "CHI", "SF", "KC", "BAL"),
    
    # Betting recommendation from our system
    bet_rec = c("PASS", "ATL (1u)", "PASS", "PASS", "CIN (1u)", "PIT (1u)", "PASS", "PASS", 
                "IND (1u)", "PASS", "PASS", "NO (1u)", "DAL (1u)", "ARI (1u)", "KC (1u)", "DET (1u)"),
    
    stringsAsFactors = FALSE
  )
  
  cat("🎯 MONEYLINE (ML) RECOMMENDATIONS - Based on Win Probability Edge\n")
  cat("===============================================================\n")
  cat("GAME              OUR ML PICK    MARKET ODDS    EV%     RECOMMENDATION\n")
  cat("-----------------------------------------------------------------------\n")
  
  # Calculate EV for each ML pick
  for (i in 1:nrow(betting_data)) {
    game <- betting_data$game[i]
    ml_pick <- betting_data$ml_pick[i]
    
    # Determine if pick is home or away
    teams <- strsplit(game, " @ ")[[1]]
    away_team <- teams[1]
    home_team <- teams[2]
    
    if (ml_pick == home_team) {
      odds <- betting_data$home_ml[i]
      pick_type <- "HOME"
    } else {
      odds <- betting_data$away_ml[i]  
      pick_type <- "AWAY"
    }
    
    # Calculate implied probability from odds
    if (odds > 0) {
      implied_prob <- 100 / (odds + 100)
    } else {
      implied_prob <- abs(odds) / (abs(odds) + 100)
    }
    
    # Our win probability (derived from spread)
    home_win_prob <- pnorm(-(-betting_data$our_spread[i]) / 13.86)
    our_prob <- if (pick_type == "HOME") home_win_prob else (1 - home_win_prob)
    
    # Calculate EV
    if (odds > 0) {
      ev <- (our_prob * odds/100) - (1 - our_prob)
    } else {
      ev <- (our_prob * 100/abs(odds)) - (1 - our_prob)
    }
    
    ev_pct <- round(ev * 100, 1)
    
    # Recommendation based on EV
    if (ev_pct >= 5.0) {
      recommendation <- sprintf("✅ BET %s (%.1f%% EV)", ml_pick, ev_pct)
    } else if (ev_pct >= 2.0) {
      recommendation <- sprintf("⚠️  LEAN %s (%.1f%% EV)", ml_pick, ev_pct)
    } else {
      recommendation <- sprintf("❌ PASS (%.1f%% EV)", ev_pct)
    }
    
    odds_str <- if (odds > 0) sprintf("+%d", odds) else sprintf("%d", odds)
    
    cat(sprintf("%-16s %-12s %-12s %-6.1f%%  %s\n", 
                game, ml_pick, odds_str, ev_pct, recommendation))
  }
  
  cat("\n🎯 AGAINST THE SPREAD (ATS) RECOMMENDATIONS - Based on Spread Edge\n")
  cat("=================================================================\n")
  cat("GAME              ATS PICK       VEGAS LINE     EDGE    RECOMMENDATION\n")  
  cat("----------------------------------------------------------------------\n")
  
  for (i in 1:nrow(betting_data)) {
    game <- betting_data$game[i]
    ats_pick <- betting_data$ats_pick[i]
    vegas_line <- betting_data$vegas_spread[i]
    edge <- betting_data$edge[i]
    bet_rec <- betting_data$bet_rec[i]
    
    # Format vegas line
    vegas_str <- if (vegas_line > 0) sprintf("+%.1f", vegas_line) else sprintf("%.1f", vegas_line)
    
    # Format edge
    edge_str <- if (edge > 0) sprintf("+%.1f", edge) else sprintf("%.1f", edge)
    
    # Recommendation based on edge magnitude
    if (abs(edge) >= 4.0) {
      recommendation <- sprintf("✅ STRONG BET - %s", bet_rec)
    } else if (abs(edge) >= 2.0) {
      recommendation <- sprintf("✅ BET - %s", bet_rec)  
    } else {
      recommendation <- "❌ PASS - Edge too small"
    }
    
    cat(sprintf("%-16s %-12s %-12s %-6s  %s\n",
                game, ats_pick, vegas_str, edge_str, recommendation))
  }
  
  # Summary statistics
  strong_bets <- sum(abs(betting_data$edge) >= 4.0)
  good_bets <- sum(abs(betting_data$edge) >= 2.0 & abs(betting_data$edge) < 4.0)
  total_units <- sum(grepl("\\(1u\\)", betting_data$bet_rec))
  
  cat("\n📈 BETTING SUMMARY\n")
  cat("==================\n")
  cat(sprintf("🔥 Strong ATS Bets (4+ point edge): %d games\n", strong_bets))
  cat(sprintf("✅ Good ATS Bets (2-4 point edge): %d games\n", good_bets))
  cat(sprintf("💰 Total Recommended Units: %d units\n", total_units))
  cat(sprintf("📊 Betting Rate: %.1f%% of games\n", (total_units/16) * 100))
  
  cat("\n⚡ TOP 3 BEST BETS BY EDGE:\n")
  
  # Sort by absolute edge value
  top_bets <- betting_data[order(abs(betting_data$edge), decreasing = TRUE)[1:3], ]
  
  for (i in 1:3) {
    edge_val <- abs(top_bets$edge[i])
    recommendation <- top_bets$bet_rec[i]
    
    cat(sprintf("%d. %s - %.1f point edge - %s\n", 
                i, top_bets$game[i], edge_val, recommendation))
  }
}

# Generate the betting chart
cat("Generating Week 3 betting recommendations...\n")
create_week3_betting_chart()