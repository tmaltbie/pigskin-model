# 🏈 Claude Development Notes - Pigskin Model

## Project Overview

This document contains detailed development information, troubleshooting, and internal notes for the Pigskin NFL prediction model development with Claude Code.

## Development Timeline

### Phase 1: Basic System (Completed)
- ✅ EPA-based prediction system using nflfastR
- ✅ The Odds API integration (API key: `629c3e74c23fee9ad4809b40ea3df597`)
- ✅ ESPN injury API with position-weighted impact scoring
- ✅ Conservative betting system (max 1.5u)
- ✅ Week 1-2 2025 backtesting: 81.2% and 68.8% ATS success

### Phase 2: Enhanced ML System (In Progress)
- ✅ Advanced nflfastR metrics integration (CPOE, XYAC_EPA, WPA)
- ✅ ML ensemble: XGBoost + Random Forest + Elastic Net + SVM
- ✅ Historical play-by-play data (1999-2024) access via nflreadr
- ⏳ Time-series cross-validation framework
- ⏳ Production API with monitoring

### Phase 3: Self-Learning System (Current)
- 🔄 **Production Database System** - SQLite schema for prediction tracking
- 🔄 **Automated Learning Loop** - Predictions vs results analysis
- 🔄 **Performance Analytics** - Comprehensive metrics and calibration
- ⏳ **Error Pattern Recognition** - Systematic mistake detection
- ⏳ **Model Retraining Pipeline** - Continuous improvement cycles
- ⏳ **Real-time Monitoring Dashboard** - Shiny app for performance tracking

## Key Technical Decisions

### Data Architecture
**Primary Data Sources:**
- `nflreadr` package for historical play-by-play data (1999-2024)
- The Odds API for live betting lines
- ESPN APIs for injuries and backup odds
- Official 2025 NFL schedule CSV

**Data Weighting Strategy:**
- 70% EPA weight with 80% emphasis on 2025 data
- 15% injury impact (position-weighted: QB=10.0, CB=3.0, etc.)
- 10% situational factors (rest, weather, primetime)
- 5% coaching/divisional adjustments

### Self-Learning Architecture (Meta-Cognition)
**Self-Evaluation Components:**
- Model confidence calibration (predicted vs actual accuracy)
- Performance degradation detection and alerts
- Feature importance drift monitoring
- Systematic bias identification and correction

**Competitive Alternative Generation:**
- Multiple model architectures competing (EPA, XGBoost, Random Forest, Neural Net)
- Dynamic ensemble weighting based on recent performance
- A/B testing of model improvements with statistical significance
- Alternative feature sets and preprocessing pipelines

**Iterative Refinement:**
- Weekly model retraining with new game data
- Monthly deep learning cycles with architecture updates
- Error pattern recognition and corrective feature engineering
- Automated hyperparameter optimization with Bayesian methods

**Dynamic Thinking Depth:**
- Context-aware model routing (use best model for specific game types)
- Confidence-based prediction depth (deeper analysis for uncertain games)
- Meta-learning to learn when to trust which models
- Adaptive complexity scaling based on available data quality

### ML Enhancement Architecture
**Ensemble Weights:**
- 30% - Proven EPA-based system (maintains current performance)
- 35% - ML ensemble (XGBoost, Random Forest, Elastic Net, SVM)
- 25% - Advanced factors (injuries, weather, coaching, rest)
- 10% - Market efficiency adjustment

## Critical Files

### Core Prediction System
- `epa_prediction_system.R` - Advanced EPA calculations with nflfastR
- `complete_week3_enhanced.R` - Full Week 3 2025 predictions
- `odds_api.R` - The Odds API integration with fallback
- `espn_injuries_api.R` - Real-time injury data with impact scoring

### ML Enhancement System (New)
- `enhanced_ml_system.R` - Main ML system interface
- `ml_feature_engineering.R` - Advanced feature engineering with nflfastR
- `ml_models.R` - ML model training (XGBoost, RF, Elastic Net, SVM)
- `ml_validation.R` - Time-series cross-validation and backtesting
- `ensemble_system.R` - Ensemble methods combining EPA + ML
- `production_pipeline.R` - Production API with monitoring

### Self-Learning System (Current)
- `learning_system/prediction_database.R` - **✅ COMPLETED** - Production SQLite database
- `learning_system/outcome_tracker.R` - Automated prediction vs result tracking
- `learning_system/performance_analyzer.R` - Advanced analytics and calibration
- `learning_system/model_ensemble.R` - Dynamic ensemble with learning weights
- `learning_system/monitoring_dashboard.R` - Shiny performance monitoring

### Analysis Tools
- `week3_probabilities.R` - Win probability calculations
- `week3_betting_chart.R` - EV-based betting recommendations
- `weighted_epa_system.R` - 2024/2025 data blending

## Data Access Configuration

### nflfastR Historical Data
The nflverse team provides pre-processed data at: https://github.com/nflverse/nflverse-data/releases/tag/pbp

**Access via nflreadr:**
```r
# Historical play-by-play data (1999-2024)
pbp_data <- nflreadr::load_pbp(seasons = 1999:2024)

# Specific season data
pbp_2024 <- nflreadr::load_pbp(2024)
pbp_2025 <- nflreadr::load_pbp(2025)

# Roster and schedule data
rosters <- nflreadr::load_rosters(seasons = 2020:2025)
schedules <- nflreadr::load_schedules(seasons = 2020:2025)
```

**Key nflfastR Metrics for ML Enhancement:**
- `epa` - Expected Points Added
- `cpoe` - Completion Percentage Over Expected  
- `xyac_epa` - Expected Yards After Catch EPA
- `wp` - Win Probability
- `wpa` - Win Probability Added
- `success` - Successful play indicator
- `qb_epa` - Quarterback EPA contribution
- `air_epa` - Air yards EPA
- `yac_epa` - Yards after catch EPA

## API Keys and Configuration

### The Odds API
- **API Key**: `629c3e74c23fee9ad4809b40ea3df597`
- **Free Tier**: 500 requests/month
- **Endpoints**: NFL spreads, moneylines, totals
- **Rate Limiting**: Built into `odds_api.R`

### ESPN APIs (Public)
- **No API key required**
- **Endpoints**: Injuries, rosters, odds, scoreboard
- **Rate Limiting**: Conservative 1 request/second
- **Fallback**: Automatic failover in unified system

## Performance Benchmarks

### Current System Performance
- **Prediction Accuracy**: 4.4 point average error (vs 9.89 basic model)
- **ATS Success**: 81.2% Week 1, 68.8% Week 2 (2025)
- **Betting Discipline**: Max 1.5u, average 9 units per week
- **Conservative Approach**: 56.2% of games recommended

### ML Enhancement Targets
- **Prediction Accuracy**: Target 3.5-4.0 point average error  
- **Directional Accuracy**: +5-8% improvement vs Vegas
- **Feature Count**: 50+ advanced metrics from nflfastR
- **Backtesting**: 25 years of historical validation (1999-2024)

## Troubleshooting Common Issues

### Data Access Problems
**Issue**: nflreadr HTTP errors
```
URL 'https://github.com/nflverse/nfldata/raw/master/data/games.rds': status was 'HTTP response code said error'
```
**Solution**: Implemented robust fallback system using local Week 1-2 performance ratings

**Issue**: ESPN API 403 errors
**Solution**: Added retry logic and rate limiting (1 req/second)

### Schedule Accuracy Issues
**Critical Fix**: Week 3 schedule correction
- **Wrong**: ATL @ KC, KC @ NYJ  
- **Correct**: KC @ NYG (Kansas City Chiefs @ New York Giants)
- **Source**: Official 2025 NFL schedule CSV

### Model Performance
**Issue**: Initial model 9.89 point average error
**Solution**: EPA integration reduced to 4.4 points
**Next**: ML ensemble targeting 3.5-4.0 points

## Development Commands

### Git Repository Setup
```bash
# SSH key for pigskin-model
ssh-keygen -t ed25519 -C "trevormaltbie@gmail.com" -f ~/.ssh/pigskin_model_ed25519

# Repository setup
git remote add origin github.com-pigskin:tmaltbie/pigskin-model.git
git push -u origin main
```

### R Environment Management
```r
# Package management with renv
renv::init()
renv::snapshot()
renv::restore()

# Load complete system
source('enhanced_ml_system.R')
```

### Testing and Validation
```r
# Test current EPA system
source('epa_prediction_system.R')
test_epa_system()

# Test ML enhancements
source('enhanced_ml_system.R')
quick_start_example()

# Comprehensive backtesting
source('ml_validation.R')
run_comprehensive_validation()
```

## Future Roadmap

### Immediate (Next 2 weeks)
- ✅ Complete nflreadr historical data integration
- ✅ Implement ML ensemble system
- ✅ Time-series cross-validation framework
- ⏳ Production API deployment

### Short-term (1-2 months)
- QBR integration from ESPN
- Weather impact modeling with real stadium data
- Real-time line movement tracking
- Mobile-friendly betting interface

### Long-term (Season-long)
- Automated model retraining pipeline
- Advanced situational modeling
- Player-level impact analysis
- Portfolio optimization across multiple weeks

## Notes for Claude Code Sessions

### Context Preservation
- Always check todo list status before starting
- Maintain conservative betting philosophy (max 1.5u)
- Preserve EPA system as core component (30% weight in ensemble)
- Keep focus on long-term profitability over short-term gains

### Code Standards
- Use descriptive function names with clear documentation
- Implement robust error handling for API failures
- Add progress indicators for long-running processes
- Maintain backward compatibility with existing EPA system

### Testing Philosophy
- Always backtest on historical data before production
- Use walk-forward validation (respects temporal order)
- Validate against Week 1-2 2025 known results
- Conservative thresholds for betting recommendations

---

*This document serves as the central knowledge base for all Pigskin Model development with Claude Code. Update regularly as the system evolves.*