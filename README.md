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

## 🤖 Self-Learning System

### Architecture Overview
The Pigskin Model now includes a **self-improving prediction system** that learns from its own performance, continuously refining predictions through automated analysis of mistakes and successes.

**Core Components:**
- **📊 Production Database**: SQLite database tracking all predictions vs actual results
- **🧠 Learning Loop**: Automated analysis of prediction errors and systematic biases  
- **📈 Performance Analytics**: Real-time monitoring of ATS accuracy, ROI, and calibration
- **⚖️ Dynamic Ensemble**: Model weights adjust based on recent performance
- **🎯 Meta-Cognition**: Self-evaluation, competitive alternatives, iterative refinement

### Self-Learning Features
**Self-Evaluation:**
- Model confidence calibration (predicted vs actual accuracy)
- Performance degradation detection and alerts
- Feature importance drift monitoring
- Systematic bias identification and correction

**Competitive Alternatives:**
- Multiple models competing (EPA, XGBoost, Random Forest, Neural Networks)
- Dynamic ensemble weighting based on recent performance  
- A/B testing with statistical significance testing
- Alternative feature sets and preprocessing pipelines

**Iterative Refinement:**
- Weekly model retraining with new game data
- Monthly deep learning cycles with architecture updates
- Error pattern recognition and corrective feature engineering
- Automated hyperparameter optimization

### Machine Learning Architecture
**Ensemble Prediction Weights:**
- **30%** - Proven EPA-based system
- **35%** - ML ensemble (XGBoost, Random Forest, Elastic Net, SVM)
- **25%** - Advanced factors (injuries, weather, coaching, rest)
- **10%** - Market efficiency adjustment

### Advanced nflfastR Features
✅ **CPOE** (Completion Percentage Over Expected)  
✅ **XYAC_EPA** (Expected Yards After Catch EPA)  
✅ **Win Probability** metrics and WPA  
✅ **Situational EPA** (down, distance, field position)  
✅ **Drive success rates** and red zone efficiency  
✅ **Rolling averages** and trend analysis  

### Expected Improvements
- **Reduce prediction error** from 4.4 points to ~3.5-4.0 points
- **Improve directional accuracy** vs Vegas by 5-8%
- **Increase betting ROI** through better game selection
- **Maintain conservative risk** profile while adding ML sophistication

---

## 🚀 Getting Started

### Prerequisites

- **R** (version 4.0+)
- **RStudio** (recommended)
- **The Odds API Key** (free tier: 500 requests/month)

### Installation

1. **Clone the repository**
   ```bash
   git clone git@github.com:tmaltbie/pigskin-model.git
   cd pigskin-model
   ```

2. **Install R dependencies**
   ```r
   # The renv package manager will automatically install required packages
   renv::restore()
   ```

3. **Set up API key**
   ```r
   # Add your The Odds API key to odds_api.R
   # Replace: API_KEY <- "your-api-key-here"
   ```

### Quick Start

**Generate Week 3 2025 Predictions:**
```r
# Load the complete prediction system
source('complete_week3_enhanced.R')

# Generate all predictions
predictions <- generate_complete_week3_predictions()

# View win probabilities
source('week3_probabilities.R')
probabilities <- generate_week3_probabilities()

# Get betting recommendations with EV analysis
source('week3_betting_chart.R')
create_week3_betting_chart()
```

**Test the EPA System:**
```r
# Load and test EPA prediction system
source('epa_prediction_system.R')
test_epa_system()
```

**Enhanced ML System:**
```r
# Load the enhanced ML system with nflfastR advanced metrics
source('enhanced_ml_system.R')

# Quick start with ML enhancements
quick_start_example()

# Train complete ML system (30-60 minutes)
train_complete_system()

# Make enhanced predictions with ensemble
make_enhanced_prediction(
  home_team = "KC", 
  away_team = "BUF", 
  vegas_spread = -3.5
)
```

**Self-Learning System:**
```r
# Initialize the prediction tracking database
source('learning_system/prediction_database.R')
db <- initialize_prediction_db()

# Store a prediction for learning
prediction_id <- insert_prediction(
  game_id = "2025_03_KC_BUF",
  home_team = "KC",
  away_team = "BUF", 
  predictions = list(
    predicted_margin = -3.2,
    predicted_total = 52.1,
    home_win_prob = 0.65,
    confidence = 0.78
  ),
  model_version = "v2.0_learning"
)

# Update with actual results after the game
update_prediction_results(
  game_id = "2025_03_KC_BUF",
  actual_home_score = 28,
  actual_away_score = 21
)

# Analyze performance
performance <- calculate_model_performance(
  start_date = "2025-09-01", 
  end_date = "2025-09-30"
)

# Get database statistics
stats <- get_database_stats()
```

### Key Dependencies

**Core Packages:**
- `nflreadr` - NFL data access
- `dplyr` - Data manipulation  
- `httr` - API requests
- `jsonlite` - JSON parsing
- `renv` - Package management

**ML Enhancement Packages:**
- `xgboost` - Gradient boosting
- `randomForest` - Random forest models
- `glmnet` - Elastic net regression
- `e1071` - Support vector machines
- `caret` - Model training framework

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