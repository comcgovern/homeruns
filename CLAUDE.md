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

- `hr_bbe_analysis.R` — Trains a GBM on 2024-2025 Statcast data. Outputs `output/hr_breakout_model.rds` which the other two scripts depend on. Uses `sabRmetrics::download_baseballsavant()` for data, `gbm` for modeling, and the MLB StatsAPI for player names and birth dates.
- `early_season_scanner.R` — Loads the trained model, downloads current regular-season data, computes deltas vs the baseline year, and scores players. Standalone (duplicates helper functions from the main script).
- `spring_training_hr_prediction.R` — Same approach as the scanner but pulls spring training data (game types "S" and "E") and filters on plate appearances (10+ PA) instead of batted ball events.

All three scripts share the same pattern: download Statcast data, aggregate to player-season level, compute deltas vs a baseline, apply the GBM model, compute a weighted breakout score, and output ranked results.

## The Mathematical Model

### What it predicts

The model predicts **change in HR/BBE** (home runs per batted ball event) from one season to the next. A positive prediction means the player's process metrics suggest they should hit more home runs per batted ball in the upcoming season compared to the previous one.

### Training data

The GBM is trained on a single year-over-year pair (2024 → 2025). Both seasons must have bat-tracking data (available since 2024), which limits historical training depth. Qualifying batters need 150+ BBE in each season, yielding roughly 200 training observations.

### Feature groups

The model uses three categories of predictors:

**Year-over-year deltas** (16 features) — Changes in process metrics between seasons:

| Feature | What it measures |
|---------|-----------------|
| `delta_avg_ev` | Change in average exit velocity |
| `delta_max_ev` | Change in max exit velocity |
| `delta_ev_90th` | Change in 90th-percentile exit velocity |
| `delta_avg_la` | Change in average launch angle |
| `delta_la_sd` | Change in launch angle consistency |
| `delta_hard_hit_rate` | Change in rate of 95+ mph batted balls |
| `delta_sweet_spot_rate` | Change in rate of 8-32 degree launch angles |
| `delta_flyball_rate` | Change in rate of 25+ degree launch angles |
| `delta_pull_rate` | Change in pull-side batted ball rate |
| `delta_pull_fly_rate` | Change in pull-side flyball rate |
| `delta_avg_bat_speed` | Change in average bat speed |
| `delta_avg_swing_length` | Change in average swing length |
| `delta_squared_up_rate` | Change in squared-up contact rate |
| `delta_whiff_rate` | Change in swinging strike rate |
| `delta_chase_rate` | Change in swing rate on pitches outside the zone |
| `delta_zone_contact_rate` | Change in contact rate on pitches in the zone |

**Baseline levels** (8 features) — Prior-season levels that establish context:

| Feature | Why it matters |
|---------|---------------|
| `avg_ev_y1` | Players with high EV already have less room to improve via EV gains |
| `avg_la_y1` | Context for whether LA changes are moving toward or away from optimal |
| `hard_hit_rate_y1` | Baseline contact quality |
| `pull_rate_y1` | Baseline pull tendency |
| `hr_per_bbe_y1` | Prior HR rate (high = less room to grow, low = more ceiling) |
| `flyball_rate_y1` | Baseline flyball tendency |
| `whiff_rate_y1` | Baseline swing-and-miss rate |
| `chase_rate_y1` | Baseline discipline |

**Demographics** (1 feature):

| Feature | Why it matters |
|---------|---------------|
| `age` | Younger players have more room for genuine mechanical improvement |

### Excluded features

`barrel_rate` and `optimal_hr_rate` are computed during aggregation but **excluded as predictors**. Both are too correlated with the outcome — they essentially measure "did you hit more HR-type batted balls," which is nearly the same thing as HR/BBE itself. Including them would be circular.

### GBM hyperparameters

The model uses conservative settings given the small training set (~200 rows):

| Parameter | Value | Rationale |
|-----------|-------|-----------|
| `n.trees` | 1000 | Large search space, but CV-based early stopping selects the actual count |
| `interaction.depth` | 2 | Limits to pairwise interactions; depth-3 overfits with N < 300 |
| `shrinkage` | 0.005 | Slow learning rate for better generalization |
| `n.minobsinnode` | max(5, N/30) | Prevents tiny leaf nodes |
| `bag.fraction` | 0.6 | Stochastic gradient boosting subsample |
| `cv.folds` | min(5, N/10) | Cross-validation for tree selection, adapts to sample size |

### Model diagnostics

After training, the model reports:
- **CV R-squared**: Cross-validated explained variance (the honest estimate of predictive power)
- **Train R-squared**: In-sample fit (expect this to be higher than CV; a large gap signals overfitting)
- **CV RMSE / Train RMSE**: Root mean squared error in HR/BBE units
- **Prediction correlation**: Pearson correlation between predicted and actual HR/BBE changes

A warning is emitted if CV R-squared < 0.05, indicating the model may have limited predictive power.

## The Breakout Score

The breakout score is a composite ranking that combines the GBM prediction with supplementary signals the model cannot capture. It is designed to avoid double-counting — the GBM already uses all the delta features as inputs, so the supplementary components only include factors *outside* the model.

### Components and weights

| Weight | Component | What it captures |
|--------|-----------|-----------------|
| **45%** | `model_score` | GBM's predicted HR/BBE change (z-scored using fixed training-data parameters) |
| **15%** | `hr_bbe_room` | Ceiling: players with low current HR/BBE have more room to grow |
| **12%** | `la_improvement` | Directional LA change toward the optimal 20-35 degree HR band |
| **12%** | `discipline_improvement` | Decreased whiff rate + decreased chase rate (better approach) |
| **8%** | `flyball_improvement` | Increased flyball rate (more balls in the air = more HR opportunities) |
| **8%** | `age_factor` | Age prior: <=25 gets +0.5, 26-28 gets +0.2, 29-32 gets 0, 33+ gets -0.3 |

### Why these weights

The GBM prediction gets dominant weight (45%) because it is the only empirically-fitted component. The supplementary factors capture things the GBM structurally cannot:

- **Room to grow** is a *ceiling* concept, not a delta — a player at 0.02 HR/BBE with positive process changes has more upside than one at 0.08 with the same changes.
- **LA directionality** is nonlinear in a way the linear delta misses — a +3 degree LA change from 17 to 20 (entering the HR band) is much more valuable than 28 to 31 (already optimal).
- **Plate discipline** signals approach changes that may not yet show up in batted ball metrics.
- **Age** acts as a Bayesian prior on whether changes are sustainable.

### Scaling consistency

All z-scored components use **fixed scaling parameters** (mean and SD) computed from the training data and stored in the model package (`scaling_params`). This prevents score drift — without it, `safe_scale()` would recompute z-scores on whatever cohort happens to be in a scan, making scores incomparable across runs.

When the scanner scripts load the saved model, they use these stored parameters for scoring. If a parameter is missing (e.g., a new metric not in the training data), the fallback is cohort-level z-scoring via `safe_scale()`.

## Data Flow

1. Raw Statcast pitch-level data is downloaded via `sabRmetrics` and cached as RDS files in `cache/`
2. Data is aggregated to per-player metrics (exit velocity, launch angle, bat speed, plate discipline, etc.)
3. Year-over-year deltas are computed for 16 metrics
4. Player age is fetched from MLB StatsAPI (birth dates cached in `cache/mlbam_batter_cache.csv`)
5. A GBM predicts HR/BBE change from these deltas + baseline levels + age
6. A composite breakout score ranks players (45% model prediction + supplementary signals)
7. Results are saved to `output/` as CSV and markdown

## Conventions

- Helper functions (`safe_max`, `safe_scale`, `coalesce_batter_id`) are duplicated across scripts for standalone use rather than shared via a package
- Statcast column names vary across data sources; `coalesce_batter_id()` handles this
- Player names and birth dates are resolved from MLB StatsAPI and cached to `cache/mlbam_batter_cache.csv`
- Output directories (`output/`, `cache/`) are created at runtime, not checked into git
- The GBM model excludes barrel_rate and optimal_hr_rate as predictors to avoid circularity (too correlated with outcome)
- Plate discipline metrics (whiff_rate, chase_rate, zone_contact_rate) are computed from ALL pitches, not just batted ball events

## Known Limitations

- **Small training set**: Only one YoY pair (2024-2025) is available because bat-tracking data started in 2024. With ~200 training rows, the GBM has limited statistical power. As more seasons accumulate, retraining on multiple YoY pairs (2024→2025, 2025→2026, etc.) will substantially improve reliability.
- **Spring training noise**: The spring training predictor compares ST metrics to full-season baselines. ST involves different competition, pitcher intent, and player intent — no adjustment is made for this context difference.
- **No xwOBA/expected stats**: Statcast provides xBA, xSLG, xwOBA which could capture contact quality more comprehensively. These are not currently used.
- **Breakout score weights are hand-tuned**: The 45/15/12/12/8/8 split is based on domain reasoning, not empirically optimized. With more training data, these could be fit via a second-stage model.

## GitHub Actions

`.github/workflows/spring-training-prediction.yml` runs the spring training predictor weekly during Feb-Mar and supports manual dispatch with configurable PA threshold and end date. Results go to artifacts and job summary.
