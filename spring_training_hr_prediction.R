# ============================================================================
# spring_training_hr_prediction.R — Spring Training HR Gainer Prediction
# ----------------------------------------------------------------------------
# Downloads spring training Statcast data, compares player swing/contact
# metrics to their prior regular season baseline, and uses the trained GBM
# model to predict which players are likely to gain home runs this year.
#
# Requires: output/hr_breakout_model.rds from hr_bbe_analysis.R
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
# CONFIGURATION
# ============================================================================

# Path to saved model from hr_bbe_analysis.R
MODEL_PATH <- "output/hr_breakout_model.rds"

# Spring training date range (adjust per year)
ST_YEAR  <- 2026
ST_START <- "2026-02-20"
ST_END   <- as.character(Sys.Date())

# Minimum plate appearances to include a player
MIN_PA <- 10

# Spring training game types in Statcast
# "S" = spring training, "E" = exhibition
ST_GAME_TYPES <- c("S", "E")

# Output
OUT_DIR   <- "output"
CACHE_DIR <- "cache"

# ============================================================================
# HELPER FUNCTIONS
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

  if (verbose) message("Downloading Savant (", game_type, "): ", start_date, " -> ", end_date)

  df <- try(sabRmetrics::download_baseballsavant(
    start_date = start_date,
    end_date   = end_date,
    game_type  = game_type,
    cl         = NULL,
    verbose    = verbose
  ), silent = TRUE)

  if (inherits(df, "try-error") || is.null(df) || nrow(df) == 0) {
    warning("No Savant rows returned for game_type=", game_type, " (", start_date, " to ", end_date, ")")
    return(tibble())
  }

  tibble::as_tibble(df)
}

load_spring_training_data <- function(start_date, end_date,
                                      game_types = ST_GAME_TYPES,
                                      verbose = TRUE) {

  cache_file <- sprintf("%s/savant_spring_%s_%s.Rds", CACHE_DIR, start_date, end_date)

  if (file.exists(cache_file) && as.Date(end_date) < Sys.Date()) {
    message("Using cached spring training data: ", cache_file)
    return(readRDS(cache_file))
  }

  # Download each game type and combine
  all_data <- map_dfr(game_types, function(gt) {
    load_statcast_range(start_date, end_date, game_type = gt, verbose = verbose)
  })

  if (nrow(all_data) > 0 && as.Date(end_date) < Sys.Date()) {
    saveRDS(all_data, cache_file)
    message("Cached to ", cache_file)
  }

  all_data
}

# ============================================================================
# SPRING TRAINING AGGREGATION
# ============================================================================

aggregate_spring_training <- function(df, year, min_pa = MIN_PA, verbose = TRUE) {

  if (verbose) message("Aggregating spring training batter stats for ", year, "...")

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

  # Count plate appearances per batter (before filtering to BBE)
  pa_counts <- df %>%
    filter(!is.na(batter_id)) %>%
    group_by(batter_id) %>%
    summarise(
      pa = n_distinct(game_pk, at_bat_number),
      .groups = "drop"
    )

  # Identify batted ball events
  df <- df %>%
    mutate(
      is_bbe = case_when(
        type == "X" ~ TRUE,
        grepl("hit_into_play", description, ignore.case = TRUE) ~ TRUE,
        TRUE ~ FALSE
      )
    )

  bbe_data <- df %>%
    filter(is_bbe, !is.na(batter_id))

  if (nrow(bbe_data) == 0) {
    warning("No batted ball events found for spring training ", year)
    return(tibble())
  }

  if (verbose) message("  Batted ball events: ", nrow(bbe_data))

  # Aggregate batted ball metrics
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
    # Join PA counts and filter on PAs (not BBE)
    left_join(pa_counts, by = c("batter" = "batter_id")) %>%
    filter(pa >= min_pa)

  if (verbose) {
    message("  ", nrow(batter_stats), " batters with ", min_pa, "+ PA in spring training")
    message("  Median BBE per qualifying batter: ", median(batter_stats$bbe))
  }

  batter_stats
}

# ============================================================================
# NAME RESOLUTION
# ============================================================================

resolve_batter_names <- function(df, cache_file = "cache/mlbam_batter_cache.csv",
                                 verbose = TRUE) {

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
# MAIN PREDICTION FUNCTION
# ============================================================================

predict_spring_hr_gainers <- function(model_path = MODEL_PATH,
                                      st_start = ST_START,
                                      st_end = ST_END,
                                      min_pa = MIN_PA,
                                      verbose = TRUE) {

  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
  if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

  # ========== LOAD SAVED MODEL ==========
  if (!file.exists(model_path)) {
    stop("Model not found at ", model_path,
         "\nRun hr_bbe_analysis.R first to train the model.")
  }

  if (verbose) message("Loading saved model from ", model_path)
  model_pkg <- readRDS(model_path)

  baseline_year <- model_pkg$baseline_year
  baseline_data <- model_pkg$baseline_data

  if (verbose) {
    message("  Baseline year: ", baseline_year)
    message("  Baseline players: ", nrow(baseline_data))
    message("  Model predictors: ", paste(model_pkg$predictors, collapse = ", "))
  }

  # ========== LOAD SPRING TRAINING DATA ==========
  if (verbose) {
    message("\nDownloading ", ST_YEAR, " spring training data: ", st_start, " to ", st_end)
  }

  raw_st <- load_spring_training_data(st_start, as.character(st_end), verbose = verbose)

  if (nrow(raw_st) == 0) {
    stop("No spring training data found. Spring training may not have started yet.")
  }

  if (verbose) message("  Total spring training pitches: ", nrow(raw_st))

  # Aggregate spring training stats
  st_data <- aggregate_spring_training(raw_st, ST_YEAR, min_pa = min_pa, verbose = verbose)

  if (nrow(st_data) == 0) {
    stop("No players have ", min_pa, "+ PA in spring training yet.")
  }

  # ========== COMPUTE DELTAS VS LAST SEASON ==========
  if (verbose) message("\nComputing spring training deltas vs ", baseline_year, " regular season...")

  delta_metrics <- c(
    "avg_ev", "max_ev", "ev_90th",
    "avg_la", "la_sd",
    "hard_hit_rate", "sweet_spot_rate",
    "pull_rate",
    "avg_bat_speed", "avg_swing_length", "squared_up_rate"
  )

  available_metrics <- delta_metrics[
    delta_metrics %in% names(baseline_data) & delta_metrics %in% names(st_data)
  ]

  # Join baseline (prior regular season) to spring training
  combined <- baseline_data %>%
    select(batter, stand, all_of(available_metrics), hr_per_bbe, bbe) %>%
    rename_with(~ paste0(.x, "_y1"), -c(batter, stand)) %>%
    inner_join(
      st_data %>%
        select(batter, all_of(available_metrics), hr_per_bbe, bbe, pa) %>%
        rename_with(~ paste0(.x, "_y2"), -batter),
      by = "batter"
    )

  if (verbose) message("  ", nrow(combined), " players with both spring training and ", baseline_year, " data")

  if (nrow(combined) == 0) {
    stop("No overlapping players between spring training and baseline season.")
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
  if (verbose) message("\nScoring players with breakout model...")

  predict_data <- combined %>% select(any_of(model_pkg$predictors))

  for (pred in model_pkg$predictors) {
    if (!pred %in% names(predict_data)) {
      predict_data[[pred]] <- 0
    }
  }

  predictions <- predict(
    model_pkg$model,
    newdata = predict_data,
    n.trees = model_pkg$best_trees
  )

  combined$predicted_delta_hr_bbe <- predictions

  # ========== SCORE & RANK ==========
  if (verbose) message("Computing breakout scores...")

  name_map <- resolve_batter_names(combined, verbose = verbose)

  results <- combined %>%
    left_join(name_map, by = "batter") %>%
    mutate(
      # Breakout score components (same weighting as main analysis)
      hr_bbe_room = -safe_scale(hr_per_bbe_y1),
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
      # Confidence based on spring training PAs
      confidence = case_when(
        pa_y2 >= 40 ~ "high",
        pa_y2 >= 25 ~ "moderate",
        TRUE ~ "low"
      ),
      # Estimated HR gain direction
      hr_gain_flag = case_when(
        predicted_delta_hr_bbe > 0.02 ~ "strong gain",
        predicted_delta_hr_bbe > 0 ~ "mild gain",
        predicted_delta_hr_bbe > -0.02 ~ "roughly flat",
        TRUE ~ "decline"
      )
    ) %>%
    select(
      batter, batter_name, stand,
      hr_per_bbe_y1, hr_per_bbe_y2, delta_hr_per_bbe,
      predicted_delta_hr_bbe, breakout_score, hr_gain_flag, confidence,
      any_of(c(
        "avg_la_y1", "delta_avg_la",
        "hard_hit_rate_y1", "delta_hard_hit_rate",
        "ev_90th_y1", "delta_ev_90th",
        "avg_bat_speed_y1", "delta_avg_bat_speed",
        "avg_swing_length_y1", "delta_avg_swing_length"
      )),
      bbe_y1, bbe_y2, pa_y2
    ) %>%
    arrange(desc(breakout_score))

  # ========== CONSOLE OUTPUT ==========
  cat("\n")
  cat("============================================================\n")
  cat("  SPRING TRAINING HR GAINER PREDICTION - ", ST_YEAR, "\n")
  cat("============================================================\n")
  cat("Spring training data: ", st_start, " to ", as.character(st_end), "\n")
  cat("Baseline season:      ", baseline_year, " regular season\n")
  cat("Min PA:               ", min_pa, "\n")
  cat("Players analyzed:     ", nrow(results), "\n")
  cat("============================================================\n\n")

  # Top predicted HR gainers
  gainers <- results %>% filter(predicted_delta_hr_bbe > 0)
  decliners <- results %>% filter(predicted_delta_hr_bbe <= 0)

  cat("TOP 20 PREDICTED HR GAINERS OVER LAST YEAR\n")
  cat("Based on spring training swing/contact changes vs ", baseline_year, ":\n\n")

  top_display <- results %>%
    head(20) %>%
    select(batter_name, breakout_score, hr_gain_flag, confidence,
           predicted_delta_hr_bbe, hr_per_bbe_y1,
           delta_avg_la, delta_hard_hit_rate, delta_ev_90th, pa_y2)

  print(top_display, n = 20)

  cat("\n\nBOTTOM 10: PREDICTED HR DECLINERS\n")
  cat("Players showing negative swing changes in spring training:\n\n")

  bottom_display <- results %>%
    tail(10) %>%
    arrange(breakout_score) %>%
    select(batter_name, breakout_score, hr_gain_flag, confidence,
           predicted_delta_hr_bbe, hr_per_bbe_y1,
           delta_avg_la, delta_hard_hit_rate, delta_ev_90th, pa_y2)

  print(bottom_display, n = 10)

  # ========== SAVE CSV ==========
  output_csv <- sprintf("%s/spring_training_hr_prediction_%s.csv", OUT_DIR, st_end)
  readr::write_csv(results, output_csv)
  if (verbose) message("\nFull results saved to: ", output_csv)

  # ========== GENERATE MARKDOWN REPORT ==========
  report_file <- sprintf("%s/spring_training_hr_report_%s.md", OUT_DIR, st_end)

  report_lines <- c(
    sprintf("# Spring Training HR Gainer Predictions - %s", ST_YEAR),
    "",
    sprintf("**Generated:** %s", Sys.time()),
    sprintf("**Spring training data:** %s to %s", st_start, st_end),
    sprintf("**Baseline:** %d regular season", baseline_year),
    sprintf("**Min PA filter:** %d", min_pa),
    sprintf("**Players analyzed:** %d", nrow(results)),
    "",
    "## Methodology",
    "",
    "This analysis compares each player's spring training Statcast metrics",
    sprintf("(exit velocity, launch angle, bat speed, etc.) to their %d regular", baseline_year),
    "season baseline. A GBM model trained on historical year-over-year changes",
    "predicts which players are likely to see HR/BBE improvements.",
    "",
    "**Caveat:** Spring training data is inherently noisy. Players face different",
    "competition levels, may be working on new approaches, and sample sizes are",
    "small. Treat these as early signals, not certainties.",
    "",
    "## Top 20 Predicted HR Gainers",
    "",
    "| Rank | Player | Predicted HR/BBE Change | Breakout Score | Confidence | Last Year HR/BBE | ST PA |",
    "|------|--------|------------------------|----------------|------------|-----------------|-------|"
  )

  top20 <- results %>% head(20)
  for (i in seq_len(nrow(top20))) {
    row <- top20[i, ]
    report_lines <- c(report_lines, sprintf(
      "| %d | %s | %+.4f | %.3f | %s | %.3f | %d |",
      i,
      ifelse(is.na(row$batter_name), paste0("ID:", row$batter), row$batter_name),
      row$predicted_delta_hr_bbe,
      row$breakout_score,
      row$confidence,
      row$hr_per_bbe_y1,
      row$pa_y2
    ))
  }

  report_lines <- c(report_lines,
    "",
    "## Bottom 10 Predicted HR Decliners",
    "",
    "| Rank | Player | Predicted HR/BBE Change | Breakout Score | Confidence | Last Year HR/BBE | ST PA |",
    "|------|--------|------------------------|----------------|------------|-----------------|-------|"
  )

  bottom10 <- results %>% tail(10) %>% arrange(breakout_score)
  for (i in seq_len(nrow(bottom10))) {
    row <- bottom10[i, ]
    report_lines <- c(report_lines, sprintf(
      "| %d | %s | %+.4f | %.3f | %s | %.3f | %d |",
      i,
      ifelse(is.na(row$batter_name), paste0("ID:", row$batter), row$batter_name),
      row$predicted_delta_hr_bbe,
      row$breakout_score,
      row$confidence,
      row$hr_per_bbe_y1,
      row$pa_y2
    ))
  }

  report_lines <- c(report_lines,
    "",
    "## Key Metrics Explained",
    "",
    "- **Predicted HR/BBE Change**: Model's predicted change in home runs per batted ball event vs last season",
    "- **Breakout Score**: Composite score weighting model prediction (31%), HR/BBE room (20%), launch angle improvement (15%), hard hit rate (12%), EV 90th (12%), bat tracking (10%)",
    "- **Confidence**: Based on spring training PA count (high: 40+, moderate: 25-39, low: <25)",
    ""
  )

  writeLines(report_lines, report_file)
  if (verbose) message("Markdown report saved to: ", report_file)

  cat("\n\nFull results: ", output_csv, "\n")
  cat("Report:       ", report_file, "\n")
  cat("============================================================\n")

  invisible(list(
    results = results,
    report_file = report_file,
    csv_file = output_csv,
    scan_date = st_end,
    baseline_year = baseline_year
  ))
}

# ============================================================================
# RUN
# ============================================================================

if (interactive() || length(commandArgs(trailingOnly = TRUE)) == 0) {
  cat("\n")
  cat("============================================================\n")
  cat("  Spring Training HR Gainer Prediction\n")
  cat("============================================================\n")
  cat("Configuration:\n")
  cat("  Model path:  ", MODEL_PATH, "\n")
  cat("  ST year:     ", ST_YEAR, "\n")
  cat("  ST start:    ", ST_START, "\n")
  cat("  ST end:      ", as.character(ST_END), "\n")
  cat("  Min PA:      ", MIN_PA, "\n")
  cat("============================================================\n\n")

  st_results <- predict_spring_hr_gainers(verbose = TRUE)
}
