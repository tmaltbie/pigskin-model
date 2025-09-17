# Data Requirements Analysis for NFL Spread Prediction

# ============= AVAILABLE IN NFLREADR =============
# ✅ Play-by-play data (offensive/defensive stats)
# ✅ Schedule data (game results, scores)
# ✅ Team rosters and player stats
# ✅ Historical data back to ~1999

# ============= MISSING DATA WE NEED =============

# 1. BETTING DATA (CRITICAL for backtesting)
#    - Opening spreads
#    - Closing spreads  
#    - Over/under totals
#    - Line movement throughout week
#    
#    SOURCES:
#    - ESPN API (some betting data)
#    - The Odds API (paid service)
#    - Sports betting sites (web scraping)
#    - Historical betting databases

# 2. INJURY REPORTS
#    - Player injury status (Out, Doubtful, Questionable)
#    - Impact on key players
#    
#    SOURCES:
#    - NFL injury reports (official)
#    - ESPN injury data
#    - Team websites

# 3. WEATHER DATA
#    - Temperature, wind speed, precipitation
#    - Dome vs outdoor games
#    
#    SOURCES:
#    - Weather APIs (OpenWeather, etc.)
#    - Historical weather databases

# 4. ADVANCED TEAM METRICS
#    - EPA (Expected Points Added) - might be in nflreadr
#    - DVOA (Defense-adjusted Value Over Average)
#    - Team efficiency ratings
#    
#    SOURCES:
#    - Football Outsiders
#    - Pro Football Focus (PFF)
#    - Custom calculations from play-by-play

# 5. PUBLIC BETTING PERCENTAGES
#    - What % of public is betting each side
#    - Sharp vs square money indicators
#    
#    SOURCES:
#    - Action Network
#    - Betting sites with public betting %

# 6. COACHING/SITUATIONAL DATA
#    - Head-to-head coaching records
#    - Rest days between games
#    - Divisional game factors
#    - Playoff implications

# ============= PRIORITY ORDER =============
# HIGH PRIORITY (Essential for backtesting):
# 1. Historical betting spreads
# 2. Actual game results with scores
# 3. Current season 2025 data

# MEDIUM PRIORITY (Improve accuracy):
# 4. Injury reports for key players
# 5. Weather conditions
# 6. Advanced team metrics (EPA, etc.)

# LOW PRIORITY (Nice to have):
# 7. Public betting percentages
# 8. Coaching situational factors

cat("Data requirements analysis complete!\n")
cat("Next: Test 2025 data availability and identify data sources\n")