# CLAUDE.md

## Project Overview

This is an R-based baseball analytics project that predicts home run breakout candidates using Statcast data and Gradient Boosting (GBM). It has three scripts that form a pipeline: model training, early-season scanning, and spring training prediction.

## Key Commands

```bash
# Train the model (must run first, generates output/hr_breakout_model.rds)
Rscript hr_bbe_analysis.R

# Run early-season scanner (requires trained model)
Rscript early_season_scanner.R

# Run spring training prediction (requires trained model)
Rscript spring_training_hr_prediction.R
```

There are no tests, linters, or build steps. The scripts are standalone R programs that download data from MLB APIs and produce CSV/markdown output.

## Architecture

- `hr_bbe_analysis.R` — Trains a GBM on 2024-2025 Statcast data. Outputs `output/hr_breakout_model.rds` which the other two scripts depend on. Uses `sabRmetrics::download_baseballsavant()` for data, `gbm` for modeling, and the MLB StatsAPI for player names.
- `early_season_scanner.R` — Loads the trained model, downloads current regular-season data, computes deltas vs the baseline year, and scores players. Standalone (duplicates helper functions from the main script).
- `spring_training_hr_prediction.R` — Same approach as the scanner but pulls spring training data (game types "S" and "E") and filters on plate appearances (10+ PA) instead of batted ball events.

All three scripts share the same pattern: download Statcast data, aggregate to player-season level, compute deltas vs a baseline, apply the GBM model, compute a weighted breakout score, and output ranked results.

## Data Flow

1. Raw Statcast pitch data is downloaded via `sabRmetrics` and cached as RDS files in `cache/`
2. Data is aggregated to per-player metrics (exit velocity, launch angle, bat speed, etc.)
3. Year-over-year deltas are computed for 11 metrics
4. A GBM predicts HR/BBE change from these deltas + baseline levels
5. A composite breakout score ranks players (31% model prediction + process improvements)
6. Results are saved to `output/` as CSV and markdown

## Conventions

- Helper functions (`safe_max`, `safe_scale`, `coalesce_batter_id`) are duplicated across scripts for standalone use rather than shared via a package
- Statcast column names vary across data sources; `coalesce_batter_id()` handles this
- Player names are resolved from MLB StatsAPI and cached to `cache/mlbam_batter_cache.csv`
- Output directories (`output/`, `cache/`) are created at runtime, not checked into git
- The GBM model excludes barrel_rate and optimal_hr_rate as predictors to avoid circularity (too correlated with outcome)

## GitHub Actions

`.github/workflows/spring-training-prediction.yml` runs the spring training predictor weekly during Feb-Mar and supports manual dispatch with configurable PA threshold and end date. Results go to artifacts and job summary.
