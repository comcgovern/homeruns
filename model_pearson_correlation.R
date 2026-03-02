# ============================================================================
# model_pearson_correlation.R — Cross-Validation & Stabilization Analysis
# ============================================================================
# Tests GBM prediction accuracy via Monte Carlo cross-validation. Splits the
# 2024→2025 player data into random train/test sets, trains the GBM on the
# train set, and measures prediction accuracy on the test set.
#
# Two analyses:
#   1. BBE SUBSAMPLING — For each test player, subsample their Y2 batted ball
#      events to simulate having limited data. Shows how Pearson r improves
#      as more BBE accumulate (stabilization curve).
#   2. THRESHOLD FILTERING — Filter test players by minimum Y2 BBE or PA.
#      Shows whether high-BBE/PA players are more predictable.
#
# Outputs:
#   - Console tables with Pearson r at each BBE/PA level
#   - output/cv_results_by_bbe_subsample.csv
#   - output/cv_results_by_bbe_threshold.csv
#   - output/cv_results_by_pa_threshold.csv
#   - output/cv_summary.csv
#   - output/visualizations/pearson_r_vs_bbe_subsample.png
#   - output/visualizations/pearson_r_vs_pa_threshold.png
#   - output/visualizations/cv_metrics_dashboard.png
#   - output/visualizations/cv_actual_vs_predicted.png
#
# REQUIRES: Run hr_bbe_analysis.R first to populate cache/ with Statcast data.
# USAGE:    Rscript model_pearson_correlation.R [N_ITERATIONS]
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(tibble)
  library(gbm)
  library(ggplot2)
})

# ============================================================================
# HELPER FUNCTIONS (duplicated from hr_bbe_analysis.R for standalone use)
# ============================================================================

safe_max <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = na.rm)
}

safe_min <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = na.rm)
}

safe_scale <- function(x) {
  if (all(is.na(x))) return(rep(0, length(x)))
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 2 || sd(x_clean) == 0) return(rep(0, length(x)))
  as.numeric(scale(x))
}

coalesce_batter_id <- function(df) {
  id_cols <- c("batter", "batter_id", "batter_mlbam", "batterId",
               "batter.1", "player_id_batter", "mlbam_batter_id")
  for (nm in id_cols) {
    if (!nm %in% names(df)) df[[nm]] <- NA
  }
  id_vec <- Reduce(function(x, y) dplyr::coalesce(x, y), df[id_cols])
  id_num <- suppressWarnings(as.numeric(id_vec))
  ifelse(is.finite(id_num), id_num, NA_real_)
}

is_batted_ball <- function(df) {
  if (!"type" %in% names(df)) df$type <- NA_character_
  if (!"description" %in% names(df)) df$description <- NA_character_
  df %>%
    mutate(
      is_bbe = case_when(
        type == "X" ~ TRUE,
        grepl("hit_into_play", description, ignore.case = TRUE) ~ TRUE,
        TRUE ~ FALSE
      )
    )
}

# ============================================================================
# CONFIGURATION
# ============================================================================

YEAR_1 <- 2024
YEAR_2 <- 2025

YEAR_1_START <- "2024-03-28"
YEAR_1_END   <- "2024-09-29"
YEAR_2_START <- "2025-03-27"
YEAR_2_END   <- "2025-09-28"

CACHE_DIR <- "cache"
OUT_DIR   <- "output"

# Cross-validation settings
N_ITERATIONS   <- 100    # Monte Carlo iterations (override via command line arg)
TRAIN_FRACTION <- 0.7    # Train/test split ratio
SEED           <- 2024   # Base seed for reproducibility

# BBE thresholds
MIN_BBE_TRAIN  <- 150    # Minimum BBE to include a player in GBM training
MIN_BBE_Y1     <- 150    # Baseline year minimum BBE (must be reliable)
MIN_BBE_Y2     <- 1      # Lowest Y2 BBE to include in evaluation dataset

# Subsampling levels (BBE)
BBE_SUBSAMPLE_LEVELS <- c(1, seq(5, 350, by = 5))

# Threshold levels (for filtering analysis)
BBE_THRESHOLD_LEVELS <- seq(0, 400, by = 10)
PA_THRESHOLD_LEVELS  <- seq(0, 700, by = 25)

# Override N_ITERATIONS from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  N_ITERATIONS <- as.integer(args[1])
  if (is.na(N_ITERATIONS) || N_ITERATIONS < 1) N_ITERATIONS <- 100
}

# ============================================================================
# BATTER AGGREGATION (adapted from hr_bbe_analysis.R with flexible min_bbe)
# ============================================================================

aggregate_batter_season <- function(df, year, min_bbe = 150, verbose = TRUE) {

  if (verbose) message("Aggregating batter stats for ", year, " (min BBE = ", min_bbe, ")...")

  df$batter_id <- coalesce_batter_id(df)

  # Coalesce game identifier
  game_id_cols <- c("game_pk", "game_id", "gamePk", "gameId", "game")
  for (nm in game_id_cols) {
    if (nm %in% names(df) && !all(is.na(df[[nm]]))) {
      df$game_pk <- df[[nm]]
      break
    }
  }
  if (!"game_pk" %in% names(df)) df$game_pk <- 1

  # Coalesce at-bat identifier
  ab_cols <- c("at_bat_number", "atBatNumber", "ab_number", "at_bat")
  for (nm in ab_cols) {
    if (nm %in% names(df) && !all(is.na(df[[nm]]))) {
      df$at_bat_number <- df[[nm]]
      break
    }
  }
  if (!"at_bat_number" %in% names(df)) df$at_bat_number <- seq_len(nrow(df))

  # Identify batted ball events
  df <- df %>% is_batted_ball()

  # Ensure required columns exist
  if (!"events" %in% names(df)) df$events <- NA_character_
  if (!"launch_speed" %in% names(df)) df$launch_speed <- NA_real_
  if (!"launch_angle" %in% names(df)) df$launch_angle <- NA_real_
  if (!"barrel" %in% names(df)) df$barrel <- NA_integer_
  if (!"hc_x" %in% names(df)) df$hc_x <- NA_real_
  if (!"stand" %in% names(df)) df$stand <- NA_character_
  if (!"bat_speed" %in% names(df)) df$bat_speed <- NA_real_
  if (!"swing_length" %in% names(df)) df$swing_length <- NA_real_
  if (!"squared_up" %in% names(df)) df$squared_up <- NA_real_
  if (!"zone" %in% names(df)) df$zone <- NA_integer_

  # PA counts
  pa_counts <- df %>%
    filter(!is.na(batter_id)) %>%
    group_by(batter_id) %>%
    summarise(pa = n_distinct(game_pk, at_bat_number), .groups = "drop")

  # Plate discipline (from ALL pitches)
  pitch_discipline <- df %>%
    filter(!is.na(batter_id)) %>%
    group_by(batter_id) %>%
    summarise(
      whiff_rate = {
        swings <- sum(description %in% c("swinging_strike", "swinging_strike_blocked",
                                          "foul", "foul_tip", "foul_bunt",
                                          "hit_into_play", "hit_into_play_no_out",
                                          "hit_into_play_score"), na.rm = TRUE)
        whiffs <- sum(description %in% c("swinging_strike", "swinging_strike_blocked"), na.rm = TRUE)
        if (swings > 0) whiffs / swings else NA_real_
      },
      chase_rate = {
        outside <- zone %in% c(11, 12, 13, 14) | (is.na(zone) & !zone %in% 1:9)
        outside_pitches <- sum(outside, na.rm = TRUE)
        outside_swings <- sum(outside & description %in% c(
          "swinging_strike", "swinging_strike_blocked",
          "foul", "foul_tip", "hit_into_play",
          "hit_into_play_no_out", "hit_into_play_score"
        ), na.rm = TRUE)
        if (outside_pitches > 0) outside_swings / outside_pitches else NA_real_
      },
      zone_contact_rate = {
        in_zone <- zone %in% 1:9
        zone_swings <- sum(in_zone & description %in% c(
          "swinging_strike", "swinging_strike_blocked",
          "foul", "foul_tip", "foul_bunt",
          "hit_into_play", "hit_into_play_no_out",
          "hit_into_play_score"
        ), na.rm = TRUE)
        zone_contact <- sum(in_zone & description %in% c(
          "foul", "foul_tip", "foul_bunt",
          "hit_into_play", "hit_into_play_no_out",
          "hit_into_play_score"
        ), na.rm = TRUE)
        if (zone_swings > 0) zone_contact / zone_swings else NA_real_
      },
      .groups = "drop"
    )

  # BBE-only metrics
  bbe_data <- df %>% filter(is_bbe, !is.na(batter_id))
  if (nrow(bbe_data) == 0) {
    warning("No batted ball events found for ", year)
    return(tibble())
  }

  batter_stats <- bbe_data %>%
    group_by(batter_id) %>%
    summarise(
      bbe = n(),
      hr = sum(events == "home_run", na.rm = TRUE),
      avg_ev = mean(launch_speed, na.rm = TRUE),
      max_ev = safe_max(launch_speed),
      ev_90th = if (sum(!is.na(launch_speed)) > 0)
                  quantile(launch_speed, 0.9, na.rm = TRUE) else NA_real_,
      avg_la = mean(launch_angle, na.rm = TRUE),
      la_sd = sd(launch_angle, na.rm = TRUE),
      hard_hit_rate = mean(launch_speed >= 95, na.rm = TRUE),
      barrel_rate = mean(barrel == 1, na.rm = TRUE),
      sweet_spot_rate = mean(launch_angle >= 8 & launch_angle <= 32, na.rm = TRUE),
      optimal_hr_rate = mean(
        launch_angle >= 25 & launch_angle <= 35 & launch_speed >= 95, na.rm = TRUE
      ),
      flyball_rate = mean(launch_angle > 25, na.rm = TRUE),
      pull_rate = mean(
        (stand == "R" & hc_x < 125.42) | (stand == "L" & hc_x > 125.42), na.rm = TRUE
      ),
      pull_fly_rate = mean(
        launch_angle > 25 &
        ((stand == "R" & hc_x < 125.42) | (stand == "L" & hc_x > 125.42)),
        na.rm = TRUE
      ),
      avg_bat_speed = mean(bat_speed, na.rm = TRUE),
      avg_swing_length = mean(swing_length, na.rm = TRUE),
      squared_up_rate = mean(squared_up, na.rm = TRUE),
      stand = first(stand),
      .groups = "drop"
    ) %>%
    mutate(hr_per_bbe = hr / bbe, year = year) %>%
    left_join(pa_counts, by = "batter_id") %>%
    left_join(pitch_discipline, by = "batter_id") %>%
    rename(batter = batter_id) %>%
    filter(bbe >= min_bbe)

  if (verbose) message("  ", nrow(batter_stats), " batters with ", min_bbe, "+ BBE")
  batter_stats
}

# ============================================================================
# COMPUTE YEAR-OVER-YEAR DELTAS
# ============================================================================

compute_deltas <- function(year1_data, year2_data, verbose = TRUE) {

  if (verbose) message("Computing year-over-year deltas...")

  delta_metrics <- c(
    "avg_ev", "max_ev", "ev_90th",
    "avg_la", "la_sd",
    "hard_hit_rate", "barrel_rate", "sweet_spot_rate", "optimal_hr_rate",
    "flyball_rate", "pull_fly_rate",
    "pull_rate",
    "avg_bat_speed", "avg_swing_length", "squared_up_rate",
    "whiff_rate", "chase_rate", "zone_contact_rate"
  )

  available_metrics <- delta_metrics[
    delta_metrics %in% names(year1_data) & delta_metrics %in% names(year2_data)
  ]

  combined <- year1_data %>%
    select(batter, stand, all_of(available_metrics), hr_per_bbe, bbe, pa) %>%
    rename_with(~ paste0(.x, "_y1"), -c(batter, stand)) %>%
    inner_join(
      year2_data %>%
        select(batter, all_of(available_metrics), hr_per_bbe, bbe, pa) %>%
        rename_with(~ paste0(.x, "_y2"), -batter),
      by = "batter"
    )

  if (verbose) message("  ", nrow(combined), " batters with data in both years")

  for (metric in available_metrics) {
    y1_col <- paste0(metric, "_y1")
    y2_col <- paste0(metric, "_y2")
    delta_col <- paste0("delta_", metric)
    combined[[delta_col]] <- combined[[y2_col]] - combined[[y1_col]]
  }

  combined <- combined %>%
    mutate(delta_hr_per_bbe = hr_per_bbe_y2 - hr_per_bbe_y1)

  combined
}

# ============================================================================
# FIT GBM (simplified for CV — no diagnostics printing)
# ============================================================================

fit_gbm_cv <- function(delta_data) {

  predictor_cols <- c(
    "delta_avg_ev", "delta_max_ev", "delta_ev_90th",
    "delta_avg_la", "delta_la_sd",
    "delta_hard_hit_rate", "delta_sweet_spot_rate",
    "delta_flyball_rate",
    "delta_pull_rate", "delta_pull_fly_rate",
    "delta_avg_bat_speed", "delta_avg_swing_length", "delta_squared_up_rate",
    "delta_whiff_rate", "delta_chase_rate", "delta_zone_contact_rate"
  )

  level_cols <- c(
    "avg_ev_y1", "avg_la_y1",
    "hard_hit_rate_y1", "pull_rate_y1", "hr_per_bbe_y1",
    "flyball_rate_y1", "whiff_rate_y1", "chase_rate_y1"
  )

  age_col <- if ("age" %in% names(delta_data)) "age" else NULL
  all_predictors <- c(predictor_cols, level_cols, age_col)
  available_predictors <- all_predictors[all_predictors %in% names(delta_data)]

  usable_predictors <- c()
  for (pred in available_predictors) {
    if (mean(!is.na(delta_data[[pred]])) >= 0.5) {
      usable_predictors <- c(usable_predictors, pred)
    }
  }

  if (length(usable_predictors) == 0) return(NULL)

  model_data <- delta_data %>%
    select(delta_hr_per_bbe, all_of(usable_predictors)) %>%
    drop_na()

  if (nrow(model_data) < 30) return(NULL)

  n_folds <- min(5, floor(nrow(model_data) / 10))
  n_folds <- max(n_folds, 2)

  gbm_fit <- gbm(
    formula = delta_hr_per_bbe ~ .,
    data = model_data,
    distribution = "gaussian",
    n.trees = 1000,
    interaction.depth = 2,
    shrinkage = 0.005,
    n.minobsinnode = max(5, floor(nrow(model_data) / 30)),
    bag.fraction = 0.6,
    cv.folds = n_folds,
    verbose = FALSE
  )

  best_trees <- gbm.perf(gbm_fit, method = "cv", plot.it = FALSE)

  list(
    model = gbm_fit,
    best_trees = best_trees,
    predictors = usable_predictors
  )
}

# ============================================================================
# COMPUTE BBE METRICS FROM A SUBSAMPLE
# ============================================================================

compute_bbe_metrics <- function(bbe_rows, stand_val) {
  # Compute per-player BBE metrics from a (sub)sample of BBE rows
  # bbe_rows: data frame of pitch-level BBE data for one player
  # stand_val: "R" or "L"

  n <- nrow(bbe_rows)
  if (n == 0) return(NULL)

  tibble(
    bbe = n,
    hr = sum(bbe_rows$events == "home_run", na.rm = TRUE),
    hr_per_bbe = sum(bbe_rows$events == "home_run", na.rm = TRUE) / n,
    avg_ev = mean(bbe_rows$launch_speed, na.rm = TRUE),
    max_ev = safe_max(bbe_rows$launch_speed),
    ev_90th = if (sum(!is.na(bbe_rows$launch_speed)) > 0)
                quantile(bbe_rows$launch_speed, 0.9, na.rm = TRUE) else NA_real_,
    avg_la = mean(bbe_rows$launch_angle, na.rm = TRUE),
    la_sd = if (sum(!is.na(bbe_rows$launch_angle)) >= 2)
              sd(bbe_rows$launch_angle, na.rm = TRUE) else NA_real_,
    hard_hit_rate = mean(bbe_rows$launch_speed >= 95, na.rm = TRUE),
    sweet_spot_rate = mean(bbe_rows$launch_angle >= 8 & bbe_rows$launch_angle <= 32, na.rm = TRUE),
    flyball_rate = mean(bbe_rows$launch_angle > 25, na.rm = TRUE),
    pull_rate = mean(
      (stand_val == "R" & bbe_rows$hc_x < 125.42) |
      (stand_val == "L" & bbe_rows$hc_x > 125.42),
      na.rm = TRUE
    ),
    pull_fly_rate = mean(
      bbe_rows$launch_angle > 25 &
      ((stand_val == "R" & bbe_rows$hc_x < 125.42) |
       (stand_val == "L" & bbe_rows$hc_x > 125.42)),
      na.rm = TRUE
    ),
    avg_bat_speed = mean(bbe_rows$bat_speed, na.rm = TRUE),
    avg_swing_length = mean(bbe_rows$swing_length, na.rm = TRUE),
    squared_up_rate = mean(bbe_rows$squared_up, na.rm = TRUE)
  )
}

# BBE metric names that get subsampled (maps to delta_* columns)
BBE_METRIC_NAMES <- c(
  "avg_ev", "max_ev", "ev_90th",
  "avg_la", "la_sd",
  "hard_hit_rate", "sweet_spot_rate",
  "flyball_rate", "pull_rate", "pull_fly_rate",
  "avg_bat_speed", "avg_swing_length", "squared_up_rate"
)

# ============================================================================
# PREDICTION METRICS
# ============================================================================

compute_prediction_metrics <- function(actual, predicted) {
  # Returns a named list of prediction accuracy metrics
  valid <- !is.na(actual) & !is.na(predicted)
  a <- actual[valid]
  p <- predicted[valid]
  n <- length(a)

  if (n < 5) {
    return(list(
      pearson_r = NA_real_, spearman_r = NA_real_,
      rmse = NA_real_, mae = NA_real_,
      directional_acc = NA_real_, r_squared = NA_real_,
      top5_precision = NA_real_, top10_precision = NA_real_,
      n = n
    ))
  }

  pearson_r  <- cor(a, p, method = "pearson")
  spearman_r <- cor(a, p, method = "spearman")
  rmse       <- sqrt(mean((a - p)^2))
  mae        <- mean(abs(a - p))

  # Directional accuracy: does the prediction get the sign right?
  dir_acc <- mean(sign(a) == sign(p))

  # R-squared
  ss_res <- sum((a - p)^2)
  ss_tot <- sum((a - mean(a))^2)
  r_sq <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

  # Top-N precision: of the top N predicted, how many actually improved?
  rank_order <- order(p, decreasing = TRUE)
  top5_actual  <- if (n >= 5)  mean(a[rank_order[1:5]] > 0) else NA_real_
  top10_actual <- if (n >= 10) mean(a[rank_order[1:10]] > 0) else NA_real_

  list(
    pearson_r = pearson_r,
    spearman_r = spearman_r,
    rmse = rmse,
    mae = mae,
    directional_acc = dir_acc,
    r_squared = r_sq,
    top5_precision = top5_actual,
    top10_precision = top10_actual,
    n = n
  )
}

# ============================================================================
# MAIN CROSS-VALIDATION ANALYSIS
# ============================================================================

run_cv_analysis <- function(verbose = TRUE) {

  # Ensure output directories exist
  for (d in c(CACHE_DIR, OUT_DIR, file.path(OUT_DIR, "visualizations"))) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  cat("\n")
  cat("============================================================\n")
  cat("  Model Cross-Validation & Stabilization Analysis\n")
  cat("============================================================\n")
  cat("  Years: ", YEAR_1, " -> ", YEAR_2, "\n")
  cat("  MC iterations:  ", N_ITERATIONS, "\n")
  cat("  Train/test split:", TRAIN_FRACTION * 100, "/", (1 - TRAIN_FRACTION) * 100, "\n")
  cat("  Training min BBE:", MIN_BBE_TRAIN, "\n")
  cat("  Subsample range: ", min(BBE_SUBSAMPLE_LEVELS), "-", max(BBE_SUBSAMPLE_LEVELS), " BBE\n")
  cat("============================================================\n\n")

  # ------------------------------------------------------------------
  # 1. Load cached Statcast data
  # ------------------------------------------------------------------
  y1_cache <- file.path(CACHE_DIR, sprintf("savant_raw_%d_R.Rds", YEAR_1))
  y2_cache <- file.path(CACHE_DIR, sprintf("savant_raw_%d_R.Rds", YEAR_2))

  if (!file.exists(y1_cache) || !file.exists(y2_cache)) {
    stop("Cached Statcast data not found. Run hr_bbe_analysis.R first.\n",
         "  Expected: ", y1_cache, "\n",
         "  Expected: ", y2_cache)
  }

  if (verbose) message("Loading cached Statcast data...")
  raw_y1 <- readRDS(y1_cache)
  raw_y2 <- readRDS(y2_cache)
  if (verbose) message("  Y1: ", nrow(raw_y1), " pitches | Y2: ", nrow(raw_y2), " pitches")

  # ------------------------------------------------------------------
  # 2. Aggregate to batter-season level
  # ------------------------------------------------------------------
  batter_y1 <- aggregate_batter_season(raw_y1, YEAR_1, min_bbe = MIN_BBE_Y1, verbose = verbose)
  batter_y2 <- aggregate_batter_season(raw_y2, YEAR_2, min_bbe = MIN_BBE_Y2, verbose = verbose)

  # ------------------------------------------------------------------
  # 3. Compute deltas
  # ------------------------------------------------------------------
  delta_data <- compute_deltas(batter_y1, batter_y2, verbose = verbose)

  # ------------------------------------------------------------------
  # 4. Add player age from cached batter info (no API calls)
  # ------------------------------------------------------------------
  batter_cache_file <- file.path(CACHE_DIR, "mlbam_batter_cache.csv")
  if (file.exists(batter_cache_file)) {
    batter_cache <- suppressWarnings(read_csv(batter_cache_file, show_col_types = FALSE))
    if ("birth_date" %in% names(batter_cache)) {
      season_midpoint <- as.Date(paste0(YEAR_2, "-07-01"))
      age_data <- batter_cache %>%
        filter(!is.na(birth_date)) %>%
        transmute(
          batter = as.integer(batter),
          age = as.numeric(difftime(season_midpoint, as.Date(birth_date), units = "days")) / 365.25
        )
      delta_data <- delta_data %>% left_join(age_data, by = "batter")
      if (verbose) message("  Added age for ", sum(!is.na(delta_data$age)), "/",
                            nrow(delta_data), " players")
    }
  }

  n_total <- nrow(delta_data)
  n_qualified <- sum(delta_data$bbe_y2 >= MIN_BBE_TRAIN)
  cat("\n  Total players in dataset:      ", n_total, "\n")
  cat("  Players with Y2 BBE >= ", MIN_BBE_TRAIN, ": ", n_qualified, "\n")
  cat("  Players with Y2 BBE < ", MIN_BBE_TRAIN, ":  ", n_total - n_qualified, "\n\n")

  # ------------------------------------------------------------------
  # 5. Pre-process Y2 pitch-level BBE data per player (for subsampling)
  # ------------------------------------------------------------------
  if (verbose) message("Pre-processing Y2 BBE data per player for subsampling...")

  # Tag BBE and extract needed columns
  raw_y2$batter_id <- coalesce_batter_id(raw_y2)
  raw_y2 <- raw_y2 %>% is_batted_ball()

  # Ensure columns exist
  if (!"events" %in% names(raw_y2)) raw_y2$events <- NA_character_
  if (!"launch_speed" %in% names(raw_y2)) raw_y2$launch_speed <- NA_real_
  if (!"launch_angle" %in% names(raw_y2)) raw_y2$launch_angle <- NA_real_
  if (!"hc_x" %in% names(raw_y2)) raw_y2$hc_x <- NA_real_
  if (!"bat_speed" %in% names(raw_y2)) raw_y2$bat_speed <- NA_real_
  if (!"swing_length" %in% names(raw_y2)) raw_y2$swing_length <- NA_real_
  if (!"squared_up" %in% names(raw_y2)) raw_y2$squared_up <- NA_real_

  player_ids <- unique(delta_data$batter)
  y2_bbe_per_player <- raw_y2 %>%
    filter(is_bbe, batter_id %in% player_ids) %>%
    select(batter_id, events, launch_speed, launch_angle, hc_x,
           bat_speed, swing_length, squared_up) %>%
    split(.$batter_id)

  if (verbose) message("  Pre-processed BBE data for ", length(y2_bbe_per_player), " players")

  # Free memory
  rm(raw_y1, raw_y2)
  gc(verbose = FALSE)

  # ------------------------------------------------------------------
  # 6. Monte Carlo cross-validation loop
  # ------------------------------------------------------------------
  if (verbose) message("\nRunning Monte Carlo cross-validation (", N_ITERATIONS, " iterations)...")

  subsample_results <- list()
  threshold_bbe_results <- list()
  threshold_pa_results <- list()
  full_pred_results <- list()

  for (iter in 1:N_ITERATIONS) {
    set.seed(SEED + iter)

    if (verbose && iter %% 10 == 0) {
      message(sprintf("  Iteration %d/%d", iter, N_ITERATIONS))
    }

    # --- Split players randomly ---
    n <- nrow(delta_data)
    train_idx <- sample(n, floor(n * TRAIN_FRACTION))
    train_all <- delta_data[train_idx, ]
    test_all  <- delta_data[-train_idx, ]

    # --- Train GBM on qualified training players (BBE >= MIN_BBE_TRAIN in both years) ---
    train_qualified <- train_all %>% filter(bbe_y2 >= MIN_BBE_TRAIN)
    gbm_result <- fit_gbm_cv(train_qualified)
    if (is.null(gbm_result)) next

    predictors <- gbm_result$predictors

    # --- Full-data predictions on test set ---
    test_pred_data <- test_all %>% select(any_of(predictors))
    # Fill missing predictor columns with NA (gbm handles missing values)
    for (p in predictors) {
      if (!p %in% names(test_pred_data)) test_pred_data[[p]] <- NA_real_
    }
    full_preds <- predict(gbm_result$model, newdata = test_pred_data,
                          n.trees = gbm_result$best_trees)
    test_all$predicted <- full_preds

    # Store full predictions for scatter plot
    full_pred_results[[iter]] <- test_all %>%
      select(batter, bbe_y2, pa_y2, delta_hr_per_bbe, predicted)

    # --- Threshold analysis (BBE) ---
    for (threshold in BBE_THRESHOLD_LEVELS) {
      subset <- test_all %>% filter(bbe_y2 >= threshold)
      if (nrow(subset) >= 10) {
        metrics <- compute_prediction_metrics(subset$delta_hr_per_bbe, subset$predicted)
        threshold_bbe_results[[length(threshold_bbe_results) + 1]] <- tibble(
          iteration = iter, threshold = threshold, !!!metrics
        )
      }
    }

    # --- Threshold analysis (PA) ---
    for (threshold in PA_THRESHOLD_LEVELS) {
      subset <- test_all %>% filter(pa_y2 >= threshold)
      if (nrow(subset) >= 10) {
        metrics <- compute_prediction_metrics(subset$delta_hr_per_bbe, subset$predicted)
        threshold_pa_results[[length(threshold_pa_results) + 1]] <- tibble(
          iteration = iter, threshold = threshold, !!!metrics
        )
      }
    }

    # --- BBE subsampling analysis ---
    # For each BBE level, subsample each test player's Y2 data and re-predict
    for (bbe_level in BBE_SUBSAMPLE_LEVELS) {

      actuals <- c()
      preds   <- c()

      for (i in 1:nrow(test_all)) {
        player_id <- as.character(test_all$batter[i])
        player_bbe <- y2_bbe_per_player[[player_id]]

        # Skip if player doesn't have enough BBE to subsample
        if (is.null(player_bbe) || nrow(player_bbe) < bbe_level) next

        # Subsample BBE
        sampled_rows <- player_bbe[sample(nrow(player_bbe), bbe_level), ]
        sub_metrics <- compute_bbe_metrics(sampled_rows, test_all$stand[i])
        if (is.null(sub_metrics)) next

        # Build predictor row: start with full-data values, replace BBE-derived deltas
        pred_row <- test_all[i, ] %>% select(any_of(predictors))
        for (p in predictors) {
          if (!p %in% names(pred_row)) pred_row[[p]] <- NA_real_
        }

        for (metric in BBE_METRIC_NAMES) {
          delta_col <- paste0("delta_", metric)
          y1_col    <- paste0(metric, "_y1")
          if (delta_col %in% predictors && y1_col %in% names(test_all)) {
            y1_val  <- test_all[[y1_col]][i]
            sub_val <- sub_metrics[[metric]]
            if (!is.null(sub_val) && !is.na(sub_val) && !is.na(y1_val)) {
              pred_row[[delta_col]] <- sub_val - y1_val
            }
          }
        }

        pred_val <- predict(gbm_result$model, newdata = pred_row,
                            n.trees = gbm_result$best_trees)
        actuals <- c(actuals, test_all$delta_hr_per_bbe[i])
        preds   <- c(preds, pred_val)
      }

      if (length(actuals) >= 10) {
        metrics <- compute_prediction_metrics(actuals, preds)
        subsample_results[[length(subsample_results) + 1]] <- tibble(
          iteration = iter, bbe_level = bbe_level, !!!metrics
        )
      }
    }
  }

  # ------------------------------------------------------------------
  # 7. Aggregate results
  # ------------------------------------------------------------------
  if (verbose) message("\nAggregating results...")

  subsample_df <- bind_rows(subsample_results)
  threshold_bbe_df <- bind_rows(threshold_bbe_results)
  threshold_pa_df <- bind_rows(threshold_pa_results)
  predictions_df <- bind_rows(full_pred_results)

  metric_cols <- c("pearson_r", "spearman_r", "rmse", "mae",
                    "directional_acc", "r_squared", "top5_precision", "top10_precision", "n")

  # Subsample summary
  subsample_summary <- subsample_df %>%
    group_by(bbe_level) %>%
    summarise(
      across(all_of(metric_cols),
             list(mean = ~mean(.x, na.rm = TRUE),
                  sd = ~sd(.x, na.rm = TRUE),
                  median = ~median(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      n_iterations = n(),
      .groups = "drop"
    ) %>%
    arrange(bbe_level)

  # Threshold BBE summary
  threshold_bbe_summary <- threshold_bbe_df %>%
    group_by(threshold) %>%
    summarise(
      across(all_of(metric_cols),
             list(mean = ~mean(.x, na.rm = TRUE),
                  sd = ~sd(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      n_iterations = n(),
      .groups = "drop"
    ) %>%
    arrange(threshold)

  # Threshold PA summary
  threshold_pa_summary <- threshold_pa_df %>%
    group_by(threshold) %>%
    summarise(
      across(all_of(metric_cols),
             list(mean = ~mean(.x, na.rm = TRUE),
                  sd = ~sd(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      n_iterations = n(),
      .groups = "drop"
    ) %>%
    arrange(threshold)

  # Overall summary at the standard training threshold
  overall <- threshold_bbe_df %>%
    filter(threshold == MIN_BBE_TRAIN)

  # ------------------------------------------------------------------
  # 8. Console output
  # ------------------------------------------------------------------
  cat("\n")
  cat("============================================================\n")
  cat("  BBE SUBSAMPLING: Pearson r vs Number of BBE\n")
  cat("============================================================\n")
  cat("  (How many BBE does the model need for reliable predictions?)\n")
  cat("  Features computed from subsampled BBE; actual outcome from full data.\n\n")
  cat(sprintf("  %-10s  %-16s  %-16s  %s\n", "BBE", "Pearson r", "Spearman r", "N players"))
  cat("  ", strrep("-", 60), "\n")

  key_levels <- subsample_summary %>%
    filter(bbe_level %in% c(1, 5, 10, 20, 30, 50, 75, 100, 125, 150, 200, 250, 300, 350))
  for (i in 1:nrow(key_levels)) {
    row <- key_levels[i, ]
    cat(sprintf("  %-10d  %5.3f +/- %5.3f  %5.3f +/- %5.3f  %.0f\n",
                row$bbe_level,
                row$pearson_r_mean, row$pearson_r_sd,
                row$spearman_r_mean, row$spearman_r_sd,
                row$n_mean))
  }

  cat("\n")
  cat("============================================================\n")
  cat("  BBE THRESHOLD: Pearson r by Minimum Y2 BBE\n")
  cat("============================================================\n")
  cat("  (Filtering test players by actual BBE count.)\n\n")
  cat(sprintf("  %-10s  %-16s  %-16s  %s\n", "BBE >=", "Pearson r", "Spearman r", "N players"))
  cat("  ", strrep("-", 60), "\n")

  key_thresholds <- threshold_bbe_summary %>%
    filter(threshold %in% seq(0, 400, by = 25))
  for (i in 1:nrow(key_thresholds)) {
    row <- key_thresholds[i, ]
    cat(sprintf("  %-10d  %5.3f +/- %5.3f  %5.3f +/- %5.3f  %.0f\n",
                row$threshold,
                row$pearson_r_mean, row$pearson_r_sd,
                row$spearman_r_mean, row$spearman_r_sd,
                row$n_mean))
  }

  cat("\n")
  cat("============================================================\n")
  cat("  PA THRESHOLD: Pearson r by Minimum Y2 PA\n")
  cat("============================================================\n\n")
  cat(sprintf("  %-10s  %-16s  %-16s  %s\n", "PA >=", "Pearson r", "Spearman r", "N players"))
  cat("  ", strrep("-", 60), "\n")

  for (i in 1:nrow(threshold_pa_summary)) {
    row <- threshold_pa_summary[i, ]
    cat(sprintf("  %-10d  %5.3f +/- %5.3f  %5.3f +/- %5.3f  %.0f\n",
                row$threshold,
                row$pearson_r_mean, row$pearson_r_sd,
                row$spearman_r_mean, row$spearman_r_sd,
                row$n_mean))
  }

  # Overall model performance at standard threshold
  if (nrow(overall) > 0) {
    cat("\n")
    cat("============================================================\n")
    cat("  OVERALL MODEL PERFORMANCE (Y2 BBE >= ", MIN_BBE_TRAIN, ")\n")
    cat("============================================================\n")
    cat(sprintf("  Pearson r:          %5.3f +/- %5.3f\n",
                mean(overall$pearson_r, na.rm = TRUE), sd(overall$pearson_r, na.rm = TRUE)))
    cat(sprintf("  Spearman r:         %5.3f +/- %5.3f\n",
                mean(overall$spearman_r, na.rm = TRUE), sd(overall$spearman_r, na.rm = TRUE)))
    cat(sprintf("  RMSE:               %6.4f +/- %6.4f\n",
                mean(overall$rmse, na.rm = TRUE), sd(overall$rmse, na.rm = TRUE)))
    cat(sprintf("  MAE:                %6.4f +/- %6.4f\n",
                mean(overall$mae, na.rm = TRUE), sd(overall$mae, na.rm = TRUE)))
    cat(sprintf("  Directional Acc:    %5.1f%% +/- %4.1f%%\n",
                mean(overall$directional_acc, na.rm = TRUE) * 100,
                sd(overall$directional_acc, na.rm = TRUE) * 100))
    cat(sprintf("  R-squared:          %5.3f +/- %5.3f\n",
                mean(overall$r_squared, na.rm = TRUE), sd(overall$r_squared, na.rm = TRUE)))
    cat(sprintf("  Top-5 Precision:    %5.1f%% +/- %4.1f%%\n",
                mean(overall$top5_precision, na.rm = TRUE) * 100,
                sd(overall$top5_precision, na.rm = TRUE) * 100))
    cat(sprintf("  Top-10 Precision:   %5.1f%% +/- %4.1f%%\n",
                mean(overall$top10_precision, na.rm = TRUE) * 100,
                sd(overall$top10_precision, na.rm = TRUE) * 100))
    cat(sprintf("  Avg test set size:  %.0f players\n",
                mean(overall$n, na.rm = TRUE)))
  }

  # ------------------------------------------------------------------
  # 9. Save CSVs
  # ------------------------------------------------------------------
  write_csv(subsample_summary, file.path(OUT_DIR, "cv_results_by_bbe_subsample.csv"))
  write_csv(threshold_bbe_summary, file.path(OUT_DIR, "cv_results_by_bbe_threshold.csv"))
  write_csv(threshold_pa_summary, file.path(OUT_DIR, "cv_results_by_pa_threshold.csv"))

  # Overall summary
  if (nrow(overall) > 0) {
    overall_summary <- tibble(
      metric = c("pearson_r", "spearman_r", "rmse", "mae",
                  "directional_acc", "r_squared", "top5_precision", "top10_precision"),
      mean = c(mean(overall$pearson_r, na.rm = TRUE),
               mean(overall$spearman_r, na.rm = TRUE),
               mean(overall$rmse, na.rm = TRUE),
               mean(overall$mae, na.rm = TRUE),
               mean(overall$directional_acc, na.rm = TRUE),
               mean(overall$r_squared, na.rm = TRUE),
               mean(overall$top5_precision, na.rm = TRUE),
               mean(overall$top10_precision, na.rm = TRUE)),
      sd = c(sd(overall$pearson_r, na.rm = TRUE),
             sd(overall$spearman_r, na.rm = TRUE),
             sd(overall$rmse, na.rm = TRUE),
             sd(overall$mae, na.rm = TRUE),
             sd(overall$directional_acc, na.rm = TRUE),
             sd(overall$r_squared, na.rm = TRUE),
             sd(overall$top5_precision, na.rm = TRUE),
             sd(overall$top10_precision, na.rm = TRUE))
    )
    write_csv(overall_summary, file.path(OUT_DIR, "cv_summary.csv"))
  }

  # ------------------------------------------------------------------
  # 10. Visualizations
  # ------------------------------------------------------------------
  create_cv_visualizations(subsample_summary, threshold_bbe_summary,
                            threshold_pa_summary, predictions_df)

  cat("\n")
  cat("============================================================\n")
  cat("  Output Files\n")
  cat("============================================================\n")
  cat("  csv: output/cv_results_by_bbe_subsample.csv\n")
  cat("  csv: output/cv_results_by_bbe_threshold.csv\n")
  cat("  csv: output/cv_results_by_pa_threshold.csv\n")
  cat("  csv: output/cv_summary.csv\n")
  cat("  png: output/visualizations/pearson_r_vs_bbe_subsample.png\n")
  cat("  png: output/visualizations/pearson_r_vs_pa_threshold.png\n")
  cat("  png: output/visualizations/cv_metrics_dashboard.png\n")
  cat("  png: output/visualizations/cv_actual_vs_predicted.png\n")
  cat("============================================================\n\n")

  invisible(list(
    subsample_summary = subsample_summary,
    threshold_bbe_summary = threshold_bbe_summary,
    threshold_pa_summary = threshold_pa_summary,
    predictions = predictions_df
  ))
}

# ============================================================================
# VISUALIZATIONS
# ============================================================================

create_cv_visualizations <- function(subsample_summary, threshold_bbe_summary,
                                      threshold_pa_summary, predictions_df,
                                      output_dir = "output/visualizations") {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # ------------------------------------------------------------------
  # Plot 1: Pearson r vs BBE (Subsampling Stabilization Curve)
  # ------------------------------------------------------------------
  p1 <- ggplot(subsample_summary, aes(x = bbe_level, y = pearson_r_mean)) +
    geom_ribbon(aes(ymin = pmax(pearson_r_mean - pearson_r_sd, -1),
                    ymax = pmin(pearson_r_mean + pearson_r_sd, 1)),
                fill = "steelblue", alpha = 0.2) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(color = "steelblue", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    # Add sample size annotation at key points
    geom_text(
      data = subsample_summary %>%
        filter(bbe_level %in% c(1, 5, 10, 20, 50, 100, 150, 200, 250, 300, 350)),
      aes(label = sprintf("n=%.0f", n_mean)),
      vjust = -1.5, size = 2.8, color = "grey40"
    ) +
    scale_x_continuous(breaks = seq(0, 350, by = 50)) +
    labs(
      title = "Prediction Accuracy vs Number of Batted Ball Events",
      subtitle = paste0("BBE Subsampling | Monte Carlo CV (",
                        N_ITERATIONS, " iterations, ",
                        TRAIN_FRACTION * 100, "/", (1 - TRAIN_FRACTION) * 100, " split)"),
      x = "Number of BBE (subsampled from Y2 data)",
      y = "Pearson r (predicted vs actual delta HR/BBE)",
      caption = "Ribbon = +/- 1 SD across iterations. Labels = mean test set size at that BBE level."
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(output_dir, "pearson_r_vs_bbe_subsample.png"), p1,
         width = 11, height = 7, dpi = 150)

  # ------------------------------------------------------------------
  # Plot 2: Pearson r vs PA (Threshold)
  # ------------------------------------------------------------------
  if (nrow(threshold_pa_summary) > 0) {
    p2 <- ggplot(threshold_pa_summary, aes(x = threshold, y = pearson_r_mean)) +
      geom_ribbon(aes(ymin = pmax(pearson_r_mean - pearson_r_sd, -1),
                      ymax = pmin(pearson_r_mean + pearson_r_sd, 1)),
                  fill = "darkgreen", alpha = 0.2) +
      geom_line(color = "darkgreen", linewidth = 1.2) +
      geom_point(color = "darkgreen", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_text(
        data = threshold_pa_summary %>%
          filter(threshold %% 100 == 0 | threshold == min(threshold)),
        aes(label = sprintf("n=%.0f", n_mean)),
        vjust = -1.5, size = 2.8, color = "grey40"
      ) +
      labs(
        title = "Prediction Accuracy vs Minimum Plate Appearances",
        subtitle = paste0("Threshold Filtering | Monte Carlo CV (",
                          N_ITERATIONS, " iterations)"),
        x = "Minimum PA in Prediction Year (Y2)",
        y = "Pearson r (predicted vs actual delta HR/BBE)",
        caption = "Ribbon = +/- 1 SD across iterations. Labels = mean test set size."
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
    ggsave(file.path(output_dir, "pearson_r_vs_pa_threshold.png"), p2,
           width = 11, height = 7, dpi = 150)
  }

  # ------------------------------------------------------------------
  # Plot 3: Multi-metric dashboard (BBE Subsampling)
  # ------------------------------------------------------------------
  dashboard_data <- subsample_summary %>%
    select(bbe_level,
           `Pearson r` = pearson_r_mean,
           `Spearman r` = spearman_r_mean,
           RMSE = rmse_mean,
           `Directional Acc` = directional_acc_mean) %>%
    pivot_longer(-bbe_level, names_to = "metric", values_to = "value")

  dashboard_sd <- subsample_summary %>%
    select(bbe_level,
           `Pearson r` = pearson_r_sd,
           `Spearman r` = spearman_r_sd,
           RMSE = rmse_sd,
           `Directional Acc` = directional_acc_sd) %>%
    pivot_longer(-bbe_level, names_to = "metric", values_to = "sd")

  dashboard <- dashboard_data %>%
    left_join(dashboard_sd, by = c("bbe_level", "metric")) %>%
    mutate(metric = factor(metric,
                            levels = c("Pearson r", "Spearman r", "RMSE", "Directional Acc")))

  p3 <- ggplot(dashboard, aes(x = bbe_level, y = value)) +
    geom_ribbon(aes(ymin = value - sd, ymax = value + sd),
                fill = "steelblue", alpha = 0.15) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 1.5) +
    facet_wrap(~ metric, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(0, 350, by = 50)) +
    labs(
      title = "Model Performance Metrics vs BBE (Subsampling)",
      subtitle = paste0("Monte Carlo CV (", N_ITERATIONS, " iterations)"),
      x = "Number of BBE (subsampled)",
      y = "Metric value"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"))
  ggsave(file.path(output_dir, "cv_metrics_dashboard.png"), p3,
         width = 12, height = 8, dpi = 150)

  # ------------------------------------------------------------------
  # Plot 4: Actual vs Predicted scatter (pooled from all iterations)
  # ------------------------------------------------------------------
  if (nrow(predictions_df) > 0) {
    # Sample to avoid overplotting (keep up to 5000 points)
    scatter_data <- if (nrow(predictions_df) > 5000) {
      predictions_df[sample(nrow(predictions_df), 5000), ]
    } else {
      predictions_df
    }

    pooled_cor <- cor(predictions_df$delta_hr_per_bbe, predictions_df$predicted,
                      use = "complete.obs")

    p4 <- ggplot(scatter_data, aes(x = predicted, y = delta_hr_per_bbe)) +
      geom_point(aes(color = bbe_y2), alpha = 0.25, size = 1.5) +
      scale_color_viridis_c(name = "Y2 BBE", option = "C") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red",
                  linewidth = 0.8) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.7,
                  alpha = 0.3) +
      annotate("text", x = Inf, y = Inf,
               label = sprintf("Pooled Pearson r = %.3f\n(N = %s predictions)",
                               pooled_cor,
                               format(nrow(predictions_df), big.mark = ",")),
               hjust = 1.1, vjust = 1.5, size = 3.5, color = "grey30") +
      labs(
        title = "Actual vs Predicted Change in HR/BBE",
        subtitle = paste0("Pooled across ", N_ITERATIONS, " MC iterations (full-data predictions)"),
        x = "Predicted delta HR/BBE",
        y = "Actual delta HR/BBE",
        caption = "Red dashed = perfect prediction. Black line = linear fit."
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
    ggsave(file.path(output_dir, "cv_actual_vs_predicted.png"), p4,
           width = 10, height = 8, dpi = 150)
  }

  message("Created visualizations in ", output_dir)
}

# ============================================================================
# RUN
# ============================================================================

if (interactive() || length(commandArgs(trailingOnly = TRUE)) <= 1) {
  results <- run_cv_analysis(verbose = TRUE)
}
