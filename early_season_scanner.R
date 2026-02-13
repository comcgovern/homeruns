# ============================================================================
# early_season_scanner.R — Early Season Breakout Detection
# ----------------------------------------------------------------------------
# Uses the trained HR/BBE model to identify breakout candidates early in
# the new season based on process changes vs prior full season.
#
# Run this periodically during April-May to catch emerging breakouts.
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
})

# ============================================================================
# CONFIGURATION - ADJUST THESE FOR YOUR SCAN
# ============================================================================

# Path to saved model from hr_bbe_analysis.R
MODEL_PATH <- "output/hr_breakout_model.rds"

# Current season to scan
SCAN_YEAR <- 2026

# Date range for current season data (adjust as season progresses)
# Start with opening day, end with "today"
SCAN_START <- "2026-03-26"  # Approximate opening day
SCAN_END   <- Sys.Date()     # Today

# Minimum BBE to include a player (lower = more players but noisier)
# Recommendation: 50 in April, 75 in May, 100 by June
MIN_BBE_SCAN <- 50

# Output
OUT_DIR <- "output"
CACHE_DIR <- "cache"

# ============================================================================
# HELPER FUNCTIONS (copied from main script for standalone use)
# ============================================================================

safe_max <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = na.rm)
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

# ============================================================================
# DATA LOADING
# ============================================================================

load_statcast_range <- function(start_date, end_date, game_type = "R", 
                                 level = "MLB", verbose = TRUE) {
  
  if (!requireNamespace("sabRmetrics", quietly = TRUE)) {
    stop("Please install 'sabRmetrics': install.packages('sabRmetrics')")
  }
  
  if (verbose) message("Downloading Savant (", level, "): ", start_date, " -> ", end_date)
  
  df <- try(sabRmetrics::download_baseballsavant(
    start_date = start_date,
    end_date   = end_date,
    game_type  = game_type,
    cl         = NULL,
    verbose    = verbose
  ), silent = TRUE)
  
  if (inherits(df, "try-error") || is.null(df) || nrow(df) == 0) {
    warning("No Savant rows returned for this window.")
    return(tibble())
  }
  
  tibble::as_tibble(df)
}

# ============================================================================
# AGGREGATION (simplified for scanner)
# ============================================================================

aggregate_batter_season <- function(df, year, min_bbe = 50, verbose = TRUE) {
  
  if (verbose) message("Aggregating batter stats for ", year, "...")
  
  # Coalesce batter ID
 df$batter_id <- coalesce_batter_id(df)
  
  # Coalesce game/AB identifiers
  game_id_cols <- c("game_pk", "game_id", "gamePk", "gameId", "game")
  for (nm in game_id_cols) {
    if (nm %in% names(df) && !all(is.na(df[[nm]]))) {
      df$game_pk <- df[[nm]]
      break
    }
  }
  if (!"game_pk" %in% names(df)) df$game_pk <- 1
  
  ab_cols <- c("at_bat_number", "atBatNumber", "ab_number", "at_bat")
  for (nm in ab_cols) {
    if (nm %in% names(df) && !all(is.na(df[[nm]]))) {
      df$at_bat_number <- df[[nm]]
      break
    }
  }
  if (!"at_bat_number" %in% names(df)) df$at_bat_number <- seq_len(nrow(df))
  
  # Ensure columns exist
  if (!"type" %in% names(df)) df$type <- NA_character_
  if (!"description" %in% names(df)) df$description <- NA_character_
  if (!"events" %in% names(df)) df$events <- NA_character_
  if (!"launch_speed" %in% names(df)) df$launch_speed <- NA_real_
  if (!"launch_angle" %in% names(df)) df$launch_angle <- NA_real_
  if (!"barrel" %in% names(df)) df$barrel <- NA_integer_
  if (!"hc_x" %in% names(df)) df$hc_x <- NA_real_
  if (!"stand" %in% names(df)) df$stand <- NA_character_
  if (!"bat_speed" %in% names(df)) df$bat_speed <- NA_real_
  if (!"swing_length" %in% names(df)) df$swing_length <- NA_real_
  if (!"squared_up" %in% names(df)) df$squared_up <- NA_real_
  
  # Identify BBEs
  df <- df %>%
    mutate(
      is_bbe = case_when(
        type == "X" ~ TRUE,
        grepl("hit_into_play", description, ignore.case = TRUE) ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  # Batted ball events only
  bbe_data <- df %>%
    filter(is_bbe, !is.na(batter_id))
  
  if (nrow(bbe_data) == 0) {
    warning("No batted ball events found for ", year)
    return(tibble())
  }
  
  # Aggregate
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
      sweet_spot_rate = mean(launch_angle >= 8 & launch_angle <= 32, na.rm = TRUE),
      pull_rate = mean(
        (stand == "R" & hc_x < 125.42) | (stand == "L" & hc_x > 125.42),
        na.rm = TRUE
      ),
      avg_bat_speed = mean(bat_speed, na.rm = TRUE),
      avg_swing_length = mean(swing_length, na.rm = TRUE),
      squared_up_rate = mean(squared_up, na.rm = TRUE),
      stand = first(stand),
      .groups = "drop"
    ) %>%
    mutate(
      hr_per_bbe = hr / bbe,
      year = year
    ) %>%
    rename(batter = batter_id) %>%
    filter(bbe >= min_bbe)
  
  if (verbose) message("  ", nrow(batter_stats), " batters with ", min_bbe, "+ BBE")
  
  batter_stats
}

# ============================================================================
# NAME RESOLUTION
# ============================================================================

resolve_batter_names <- function(df, cache_file = "cache/mlbam_batter_cache.csv", verbose = TRUE) {
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'.")
  if (!requireNamespace("httr", quietly = TRUE)) stop("Please install 'httr'.")
  
  ids <- df %>%
    filter(!is.na(batter)) %>%
    distinct(batter) %>%
    pull(batter) %>%
    as.integer()
  
  if (length(ids) == 0) {
    return(tibble(batter = integer(0), batter_name = character(0)))
  }
  
  cache <- if (file.exists(cache_file)) {
    suppressWarnings(readr::read_csv(cache_file, show_col_types = FALSE)) %>%
      mutate(batter = as.integer(batter)) %>%
      distinct()
  } else {
    tibble(batter = integer(0), batter_name = character(0))
  }
  
  need_ids <- setdiff(ids, cache$batter)
  
  if (length(need_ids) > 0) {
    if (verbose) message("Resolving ", length(need_ids), " batter names...")
    
    batch_size <- 100
    new_rows <- tibble(batter = integer(0), batter_name = character(0))
    
    for (i in seq(1, length(need_ids), by = batch_size)) {
      slice <- need_ids[i:min(i + batch_size - 1, length(need_ids))]
      q <- paste(slice, collapse = ",")
      url <- paste0("https://statsapi.mlb.com/api/v1/people?personIds=", q)
      
      resp <- try(httr::GET(url, httr::timeout(30)), silent = TRUE)
      
      if (!inherits(resp, "try-error") && !httr::http_error(resp)) {
        txt <- httr::content(resp, as = "text", encoding = "UTF-8")
        dat <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)
        
        if (!is.null(dat$people) && nrow(dat$people) > 0) {
          people <- tibble::as_tibble(dat$people) %>%
            transmute(
              batter = as.integer(id),
              batter_name = fullName
            )
          new_rows <- bind_rows(new_rows, people)
        }
      }
      Sys.sleep(0.5)
    }
    
    cache <- bind_rows(cache, new_rows) %>% distinct()
    if (!dir.exists(dirname(cache_file))) dir.create(dirname(cache_file), recursive = TRUE)
    readr::write_csv(cache, cache_file)
  }
  
  cache %>%
    filter(batter %in% ids) %>%
    mutate(batter_name = if_else(is.na(batter_name), paste0("Batter_", batter), batter_name))
}

# ============================================================================
# MAIN SCANNER FUNCTION
# ============================================================================

scan_early_season <- function(model_path = MODEL_PATH,
                               scan_start = SCAN_START,
                               scan_end = SCAN_END,
                               min_bbe = MIN_BBE_SCAN,
                               verbose = TRUE) {
  
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
  if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
  
  # ========== LOAD SAVED MODEL ==========
  if (!file.exists(model_path)) {
    stop("Model not found at ", model_path, "\nRun hr_bbe_analysis.R first to train the model.")
  }
  
  if (verbose) message("📦 Loading saved model from ", model_path)
  model_pkg <- readRDS(model_path)
  
  baseline_year <- model_pkg$baseline_year
  baseline_data <- model_pkg$baseline_data
  
  if (verbose) {
    message("  Baseline year: ", baseline_year)
    message("  Baseline players: ", nrow(baseline_data))
    message("  Model predictors: ", paste(model_pkg$predictors, collapse = ", "))
  }
  
  # ========== LOAD CURRENT SEASON DATA ==========
  if (verbose) {
    message("\n⬇️ Downloading ", SCAN_YEAR, " data: ", scan_start, " to ", scan_end)
  }
  
  cache_file <- sprintf("%s/savant_scan_%s_%s.Rds", CACHE_DIR, scan_start, scan_end)
  
  if (file.exists(cache_file) && as.Date(scan_end) < Sys.Date()) {
    # Only use cache if scan_end is in the past (otherwise we want fresh data)
    message("Using cached data: ", cache_file)
    raw_scan <- readRDS(cache_file)
  } else {
    raw_scan <- load_statcast_range(scan_start, as.character(scan_end), verbose = verbose)
    if (nrow(raw_scan) > 0 && as.Date(scan_end) < Sys.Date()) {
      saveRDS(raw_scan, cache_file)
    }
  }
  
  if (nrow(raw_scan) == 0) {
    stop("No data found for scan period. Season may not have started yet.")
  }
  
  # Aggregate current season
  current_data <- aggregate_batter_season(raw_scan, SCAN_YEAR, min_bbe = min_bbe, verbose = verbose)
  
  if (nrow(current_data) == 0) {
    stop("No players have ", min_bbe, "+ BBE yet. Try lowering MIN_BBE_SCAN or waiting longer.")
  }
  
  # ========== COMPUTE DELTAS ==========
  if (verbose) message("\n📊 Computing deltas vs ", baseline_year, " baseline...")
  
  # Metrics for deltas
  delta_metrics <- c(
    "avg_ev", "max_ev", "ev_90th",
    "avg_la", "la_sd",
    "hard_hit_rate", "sweet_spot_rate",
    "pull_rate",
    "avg_bat_speed", "avg_swing_length", "squared_up_rate"
  )
  
  available_metrics <- delta_metrics[
    delta_metrics %in% names(baseline_data) & delta_metrics %in% names(current_data)
  ]
  
  # Join baseline (Y1) to current (Y2)
  combined <- baseline_data %>%
    select(batter, stand, all_of(available_metrics), hr_per_bbe, bbe) %>%
    rename_with(~ paste0(.x, "_y1"), -c(batter, stand)) %>%
    inner_join(
      current_data %>%
        select(batter, all_of(available_metrics), hr_per_bbe, bbe) %>%
        rename_with(~ paste0(.x, "_y2"), -batter),
      by = "batter"
    )
  
  if (verbose) message("  ", nrow(combined), " players with data in both seasons")
  
  # Also track players NOT in baseline (potential callups/rookies)
  new_players <- current_data %>%
    filter(!batter %in% baseline_data$batter)
  
  if (verbose && nrow(new_players) > 0) {
    message("  ", nrow(new_players), " new players not in baseline (rookies/callups)")
  }
  
  # Compute deltas
  for (metric in available_metrics) {
    y1_col <- paste0(metric, "_y1")
    y2_col <- paste0(metric, "_y2")
    delta_col <- paste0("delta_", metric)
    combined[[delta_col]] <- combined[[y2_col]] - combined[[y1_col]]
  }
  
  combined <- combined %>%
    mutate(delta_hr_per_bbe = hr_per_bbe_y2 - hr_per_bbe_y1)
  
  # ========== APPLY MODEL ==========
  if (verbose) message("\n🎯 Scoring players with breakout model...")
  
  # Prepare prediction data
  predict_data <- combined %>% select(any_of(model_pkg$predictors))
  
  # Fill missing predictors with 0 (neutral)
  for (pred in model_pkg$predictors) {
    if (!pred %in% names(predict_data)) {
      predict_data[[pred]] <- 0
    }
  }
  
  # Get predictions
  predictions <- predict(
    model_pkg$model,
    newdata = predict_data,
    n.trees = model_pkg$best_trees
  )
  
  combined$predicted_delta_hr_bbe <- predictions
  
  # ========== SCORE & RANK ==========
  if (verbose) message("📈 Computing breakout scores...")
  
  # Resolve names
  name_map <- resolve_batter_names(combined, verbose = verbose)
  
  results <- combined %>%
    left_join(name_map, by = "batter") %>%
    mutate(
      # Breakout score components
      hr_bbe_room = -safe_scale(hr_per_bbe_y2),
      la_improvement = case_when(
        is.na(avg_la_y1) | is.na(delta_avg_la) ~ 0,
        avg_la_y1 < 20 & delta_avg_la > 0 ~ delta_avg_la / 5,
        avg_la_y1 > 35 & delta_avg_la < 0 ~ -delta_avg_la / 5,
        avg_la_y1 >= 20 & avg_la_y1 <= 35 ~ 0.5,
        TRUE ~ 0
      ),
      hh_improvement = safe_scale(delta_hard_hit_rate),
      ev90_improvement = safe_scale(delta_ev_90th),
      swing_improvement = if_else(
        !is.na(delta_avg_swing_length) & !is.na(delta_avg_bat_speed),
        (safe_scale(delta_avg_swing_length) + safe_scale(delta_avg_bat_speed)) / 2,
        0
      ),
      breakout_score = (
        0.20 * hr_bbe_room +
        0.15 * la_improvement +
        0.12 * coalesce(hh_improvement, 0) +
        0.12 * coalesce(ev90_improvement, 0) +
        0.10 * swing_improvement +
        0.31 * safe_scale(predicted_delta_hr_bbe)
      ),
      # Flag for early-season sample size
      sample_flag = case_when(
        bbe_y2 >= 100 ~ "solid",
        bbe_y2 >= 75 ~ "moderate",
        TRUE ~ "small"
      )
    ) %>%
    select(
      batter, batter_name, stand,
      hr_per_bbe_y1, hr_per_bbe_y2, delta_hr_per_bbe,
      predicted_delta_hr_bbe, breakout_score, sample_flag,
      any_of(c(
        "avg_la_y1", "delta_avg_la",
        "hard_hit_rate_y1", "delta_hard_hit_rate",
        "ev_90th_y1", "delta_ev_90th",
        "avg_bat_speed_y1", "delta_avg_bat_speed",
        "avg_swing_length_y1", "delta_avg_swing_length"
      )),
      bbe_y1, bbe_y2
    ) %>%
    arrange(desc(breakout_score))
  
  # ========== OUTPUT ==========
  cat("\n")
  cat("============================================================\n")
  cat("  EARLY SEASON BREAKOUT SCAN - ", SCAN_YEAR, "\n")
  cat("============================================================\n")
  cat("Scan period: ", as.character(scan_start), " to ", as.character(scan_end), "\n")
  cat("Min BBE: ", min_bbe, "\n")
  cat("Players scanned: ", nrow(results), "\n")
  cat("============================================================\n\n")
  
  cat("🔥 TOP 15 BREAKOUT CANDIDATES\n")
  cat("Players showing positive swing changes early in ", SCAN_YEAR, ":\n\n")
  
  top_15 <- results %>%
    head(15) %>%
    select(batter_name, breakout_score, sample_flag, 
           hr_per_bbe_y2, delta_avg_la, delta_hard_hit_rate, 
           delta_ev_90th, bbe_y2)
  
  print(top_15, n = 15)
  
  cat("\n\n📉 REGRESSION WATCH (Bottom 10)\n")
  cat("Players whose process metrics are declining:\n\n")
  
  bottom_10 <- results %>%
    tail(10) %>%
    arrange(breakout_score) %>%
    select(batter_name, breakout_score, sample_flag,
           hr_per_bbe_y2, delta_avg_la, delta_hard_hit_rate,
           delta_ev_90th, bbe_y2)
  
  print(bottom_10, n = 10)
  
  # Save results
  output_file <- sprintf("%s/early_scan_%s_%s.csv", OUT_DIR, scan_start, scan_end)
  readr::write_csv(results, output_file)
  
  cat("\n\n✅ Full results saved to: ", output_file, "\n")
  cat("============================================================\n")
  
  # Return results invisibly
  invisible(list(
    results = results,
    new_players = new_players,
    scan_date = scan_end,
    baseline_year = baseline_year
  ))
}

# ============================================================================
# RUN
# ============================================================================

if (interactive() || length(commandArgs(trailingOnly = TRUE)) == 0) {
  cat("\n")
  cat("============================================================\n")
  cat("  Early Season Breakout Scanner\n")
  cat("============================================================\n")
  cat("Configuration:\n")
  cat("  Model path:  ", MODEL_PATH, "\n")
  cat("  Scan year:   ", SCAN_YEAR, "\n")
  cat("  Scan start:  ", SCAN_START, "\n")
  cat("  Scan end:    ", as.character(SCAN_END), "\n")
  cat("  Min BBE:     ", MIN_BBE_SCAN, "\n")
  cat("============================================================\n\n")
  
  scan_results <- scan_early_season(verbose = TRUE)
}
