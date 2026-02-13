# Home Run Breakout Prediction

A machine learning pipeline for predicting which MLB hitters are likely to gain (or lose) home runs year-over-year, using Statcast pitch-level data and a Gradient Boosting model.

## Overview

This project analyzes swing and contact quality metrics — exit velocity, launch angle, bat speed, barrel rate, and more — to identify players whose underlying process changes suggest they're due for a HR breakout or regression. It runs across three stages:

1. **Model Training** (`hr_bbe_analysis.R`) — Trains a GBM on 2024-2025 regular season data to learn what metric changes predict HR/BBE improvements.
2. **Early Season Scanner** (`early_season_scanner.R`) — Applies the trained model to in-season data (April onward) to flag emerging breakouts in real time.
3. **Spring Training Predictor** (`spring_training_hr_prediction.R`) — Uses spring training Statcast data to get the earliest possible read on HR gainers before the regular season starts.

## How It Works

The model is trained on year-over-year deltas in 11 Statcast metrics for players with 150+ batted ball events in consecutive seasons. It predicts changes in HR/BBE (home runs per batted ball event). Players are then ranked by a composite breakout score that weights:

- **31%** — GBM model prediction
- **20%** — Room to grow (low prior-year HR/BBE)
- **15%** — Launch angle improvement toward the 20-35 degree sweet spot
- **12%** — Hard hit rate improvement
- **12%** — 90th percentile exit velocity improvement
- **10%** — Bat tracking improvements (bat speed, swing length)

## Scripts

### `hr_bbe_analysis.R` — Model Training

Trains the core GBM model using two full regular seasons of Statcast data.

```bash
Rscript hr_bbe_analysis.R
```

**Outputs:**
- `output/hr_breakout_model.rds` — Serialized model (required by the other scripts)
- `output/hr_bbe_candidates.csv` — Ranked player list
- `output/hr_bbe_variable_importance.csv` — Feature importance scores
- `output/visualizations/` — PNG charts

### `early_season_scanner.R` — In-Season Scanner

Applies the trained model to current-season data to catch breakouts as they emerge.

```bash
Rscript early_season_scanner.R
```

**Requires:** `output/hr_breakout_model.rds` from the training step.

**Outputs:**
- `output/early_scan_<start>_<end>.csv` — Ranked results with sample size flags

### `spring_training_hr_prediction.R` — Spring Training Predictor

Downloads spring training Statcast data, filters to players with 10+ plate appearances, and predicts HR gainers over last year.

```bash
Rscript spring_training_hr_prediction.R
```

**Requires:** `output/hr_breakout_model.rds` from the training step.

**Outputs:**
- `output/spring_training_hr_prediction_<date>.csv` — Full results
- `output/spring_training_hr_report_<date>.md` — Markdown report with top 20 gainers and bottom 10 decliners

## GitHub Actions

### Spring Training HR Prediction

The workflow at `.github/workflows/spring-training-prediction.yml` runs the spring training predictor automatically:

- **Scheduled:** Every Monday at noon UTC during February and March
- **Manual trigger:** Via workflow_dispatch with optional `min_pa` and `st_end_date` inputs
- **Results:** Uploaded as artifacts and posted to the job summary

To trigger manually from the CLI:

```bash
gh workflow run "Spring Training HR Prediction" --field min_pa=10
```

## Requirements

**R 4.x** with these packages:

```r
install.packages(c(
  "dplyr", "tidyr", "purrr", "stringr", "lubridate",
  "readr", "tibble", "gbm", "httr", "jsonlite",
  "sabRmetrics"
))

# Optional (for visualizations):
install.packages("ggplot2")
```

## Data Sources

- **MLB Statcast** via `sabRmetrics::download_baseballsavant()` — Pitch-level data including launch speed, launch angle, bat tracking metrics, and batted ball outcomes
- **MLB StatsAPI** (`statsapi.mlb.com`) — Player name resolution by MLBAM ID

## Project Structure

```
homeruns/
├── hr_bbe_analysis.R                # Model training (run first)
├── early_season_scanner.R           # In-season breakout detection
├── spring_training_hr_prediction.R  # Pre-season spring training predictor
├── .github/workflows/
│   └── spring-training-prediction.yml
├── output/                          # Generated at runtime
│   ├── hr_breakout_model.rds
│   ├── hr_bbe_candidates.csv
│   ├── hr_bbe_variable_importance.csv
│   ├── spring_training_hr_prediction_*.csv
│   ├── spring_training_hr_report_*.md
│   ├── early_scan_*.csv
│   └── visualizations/
└── cache/                           # Statcast data cache (generated at runtime)
```

## Caveats

- **Spring training data is noisy.** Players face different competition, experiment with mechanics, and have small samples. Confidence flags in the output help assess reliability.
- **Bat tracking metrics** (bat speed, swing length, squared-up rate) only became available in 2024, so historical YoY data is limited.
- **The model predicts process-based HR/BBE changes**, not raw HR totals. Playing time, lineup position, and park factors are not included.
