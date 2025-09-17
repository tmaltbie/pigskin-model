# 🏈 Pigskin Model

## Goal & Purpose

**Primary Goal**: Build a disciplined, data-driven NFL spread prediction system that consistently identifies value betting opportunities while maintaining strict bankroll management.

**Core Philosophy**:
- **Expected Value Focus**: Only bet when our model identifies genuine statistical edges (2+ points)
- **Conservative Sizing**: Maximum 1.5u bets with disciplined 1u standard sizing
- **Current Season Emphasis**: Heavy weighting on 2025 EPA data (80%) over historical performance
- **Multi-Factor Analysis**: EPA metrics, injuries, rest, weather, and situational factors

**Success Metrics**:
- Target 55%+ Against The Spread (ATS) accuracy
- Positive expected value on all recommended bets
- Maximum 10-15% of bankroll in action per week
- Long-term profitable returns through disciplined process

## Model Performance

**Week 1 2025**: 81.2% ATS Success (13/16 games)  
**Week 2 2025**: 68.8% ATS Success (11/16 games)  
**Prediction Accuracy**: 4.4 point average error (EPA model) vs 9.89 points (basic model)

---

## References & Data Sources

### 📊 Primary Data Sources

**nflverse Ecosystem**
- [nflreadr](https://nflreadr.nflverse.com/) - R package for NFL data access
- [nflfastR](https://www.nflfastr.com/) - EPA and advanced analytics
- [nflverse GitHub](https://github.com/nflverse) - Open source NFL data tools

**Live Betting Odds**
- [The Odds API](https://the-odds-api.com/) - Real-time sportsbook odds
- API Key: `{your-api-key}` - 500 requests/month free tier

### 🏥 Injury & Team Data

**ESPN APIs** ([Full ESPN API Reference](https://gist.github.com/nntrn/ee26cb2a0716de0947a0a4e9a157bc1c))
- Injury Reports: `sports.core.api.espn.com/v2/sports/football/leagues/nfl/teams/{TEAM_ID}/injuries`
- Team Rosters: `site.api.espn.com/apis/site/v2/sports/football/nfl/teams/{TEAM_ID}/roster`
- Scoreboard: `site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard`
- Game Odds: `sports.core.api.espn.com/v2/sports/football/leagues/nfl/events/{EVENT_ID}/competitions/{EVENT_ID}/odds`

### 📈 Advanced Metrics

**Expected Points Added (EPA)**
- [Open Source Football](https://opensourcefootball.com) - EPA concepts and applications
- [nflfastR EPA Guide](https://www.nflfastr.com/articles/nflfastR.html) - Technical implementation
- [EPA Primer](https://www.opensourcefootball.com/posts/2020-09-28-nflfastr-beginners-guide/) - Beginner's guide

**Win Probability & Modeling**
- [Football Study Hall](https://www.footballstudyhall.com/) - Advanced analytics concepts
- [Pro Football Reference](https://www.pro-football-reference.com/) - Historical data validation
- [Football Outsiders](https://www.footballoutsiders.com/) - DVOA and advanced metrics

### 🔧 Technical Resources

**R Packages Used**
- `nflreadr` - Primary data access
- `httr` - API requests
- `jsonlite` - JSON parsing
- `dplyr` - Data manipulation

**API Documentation**
- [ESPN API Endpoints](https://gist.github.com/nntrn/ee26cb2a0716de0947a0a4e9a157bc1c) - Comprehensive ESPN API reference
- [The Odds API Docs](https://the-odds-api.com/liveapi/guides/v4/) - Betting odds integration
- [GitHub Actions for Automation](https://docs.github.com/en/actions) - Future automated updates

### 📋 Data Considerations

**Historical Context**
- NFL schedule data: Official 2025 season CSV
- Weather data: [OpenWeatherMap API](https://openweathermap.org/api) (future enhancement)
- Stadium information: [NFL Venues API](https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/venues)

**Validation Sources**
- [Covers.com](https://www.covers.com/) - ATS records validation
- [TeamRankings](https://www.teamrankings.com/nfl/) - Statistical benchmarking
- [Sharp Football Stats](https://www.sharpfootballstats.com/) - Advanced metrics comparison

### 🎯 Future Enhancements

**Planned Integrations**
- QBR data from ESPN
- Weather impact modeling
- Stadium-specific factors
- Historical ATS validation system
- Real-time line movement tracking

**Research Areas**
- [Football Study Hall Methodology](https://www.footballstudyhall.com/2008/1/23/3014847/college-football-s-most-important) - Situational analysis
- [Pro Football Analytics](https://profootballanalytics.com/) - Model validation techniques
- [Harvard Sports Analysis](https://harvardsportsanalysis.wordpress.com/) - Academic research

---

*This model prioritizes process over results, emphasizing long-term profitability through disciplined analysis and conservative bankroll management.*