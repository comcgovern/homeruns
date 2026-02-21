# ============================================================================
# hr_bbe_analysis.R — Exploring Predictors of HR/BBE Improvement
# ----------------------------------------------------------------------------
# Uses Statcast data (including bat tracking metrics) to explore what changes
# in a player's profile are associated with HR/BBE improvements year-over-year.
#
# NOTE: Bat tracking metrics (bat speed, swing length, squared-up rate) only
# became available in 2024, so we have limited YoY data. This is exploratory.
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

# Safe max/min that returns NA instead of Inf/-Inf for empty vectors
safe_max <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = na.rm)
}

safe_min <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = na.rm)
}

# Safe scale that handles edge cases
safe_scale <- function(x) {
  if (all(is.na(x))) return(rep(0, length(x)))
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 2 || sd(x_clean) == 0) return(rep(0, length(x)))
  as.numeric(scale(x))
}

# ============================================================================
# CONFIGURATION
# ============================================================================

# Years to analyze
YEAR_1 <- 2024
YEAR_2 <- 2025

# Date ranges (regular season)
YEAR_1_START <- "2024-03-28"
YEAR_1_END   <- "2024-09-29"
YEAR_2_START <- "2025-03-27"
YEAR_2_END   <- "2025-09-28"  # Adjust if season incomplete

# Minimum batted ball events per year to include player
MIN_BBE_PER_YEAR <- 150

# Output paths
OUT_DIR <- "output"
CACHE_DIR <- "cache"

# ============================================================================
# DIRECTORY SETUP
# ============================================================================

ensure_directories <- function() {
  dirs <- c(CACHE_DIR, OUT_DIR, paste0(OUT_DIR, "/visualizations"))
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE)
      message("✓ Created directory: ", d)
    }
  }
}

# ============================================================================
# STATCAST DATA LOADER (adapted from pitch_ppi.R)
# ============================================================================

load_statcast_range <- function(start_date, end_date, game_type = "R", level = "MLB", verbose = TRUE) {

  # For MLB, use sabRmetrics (it works perfectly)
  if (level == "MLB") {
    if (!requireNamespace("sabRmetrics", quietly = TRUE)) {
      stop("Please install 'sabRmetrics' (install.packages('sabRmetrics')).")
    }
    if (verbose) message("Downloading Savant (MLB): ", start_date, " -> ", end_date, " | game_type=", game_type)
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
    return(tibble::as_tibble(df))
  }

  # For AAA, use minors endpoint with sabRmetrics-style chunking
  if (verbose) message("Downloading Savant (AAA): ", start_date, " -> ", end_date, " | game_type=", game_type)

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Please install 'httr': install.packages('httr')")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Please install 'readr': install.packages('readr')")
  }

  # Split into 5-day chunks (same as sabRmetrics strategy)
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  days <- as.numeric(end - start)

  # Build payload of URLs for each chunk
  payload <- tibble::tibble(
    start_chunk = seq(start, by = 5, length.out = ceiling((days + 1) / 5))
  ) %>%
    dplyr::mutate(
      end_chunk = pmin(.data$start_chunk + 4, end),
      chunk_id = dplyr::row_number()
    )

  # Build URLs for each chunk
  base_url <- "https://baseballsavant.mlb.com/statcast-search-minors/csv"

  payload <- payload %>%
    dplyr::mutate(
      game_type_filter = paste0("hfGT=", game_type, "%7C"),
      date_filter = sprintf("game_date_gt=%s&game_date_lt=%s", .data$start_chunk, .data$end_chunk),
      level_filter = "hfLevel=AAA%7C",
      season_filter = paste0("hfSea=", format(start, "%Y"), "%7C"),
      url = paste0(
        base_url,
        "?all=true",
        "&type=details",
        "&minors=true",
        "&player_type=batter",
        "&", .data$game_type_filter,
        "&", .data$date_filter,
        "&", .data$level_filter,
        "&", .data$season_filter,
        "&group_by=name",
        "&min_pitches=0",
        "&min_results=0"
      )
    )

  n_chunks <- nrow(payload)
  if (verbose) message("Downloading ", n_chunks, " chunk(s) (5-day periods)...")

  # Step 1: Submit initial requests (like sabRmetrics does)
  if (verbose) message("Submitting initial API requests...")
  initial_requests <- lapply(payload$url, function(url) {
    try(httr::GET(url, httr::timeout(1)), silent = TRUE)
  })

  # Step 2: Download with proper timeout, retrying as needed
  if (verbose) message("Downloading data chunks...")

  data_list <- vector("list", n_chunks)
  names(data_list) <- paste0("chunk_", payload$chunk_id)
  is_error <- rep(TRUE, n_chunks)

  max_retries <- 3
  retry_count <- 0

  while (any(is_error) && retry_count < max_retries) {
    retry_count <- retry_count + 1

    if (retry_count > 1 && verbose) {
      message("Retry attempt ", retry_count, " for ", sum(is_error), " chunk(s)...")
    }

    for (i in which(is_error)) {
      if (verbose) message(sprintf("  Chunk %d/%d: %s to %s",
                                    i, n_chunks,
                                    payload$start_chunk[i],
                                    payload$end_chunk[i]))

      response <- try(httr::GET(payload$url[i], httr::timeout(120)), silent = TRUE)

      if (inherits(response, "try-error")) {
        if (verbose) message("    ✗ Connection error")
        next
      }

      if (httr::http_error(response)) {
        if (verbose) message("    ✗ HTTP ", httr::status_code(response))
        next
      }

      content <- httr::content(response, as = "text", encoding = "UTF-8")

      if (nchar(content) < 100) {
        if (verbose) message("    • No data (likely no games)")
        data_list[[i]] <- NULL
        is_error[i] <- FALSE
        next
      }

      if (grepl("<html", content, ignore.case = TRUE)) {
        if (verbose) message("    ✗ Got HTML error page")
        next
      }

      chunk_data <- try(readr::read_csv(content, show_col_types = FALSE), silent = TRUE)

      if (inherits(chunk_data, "try-error")) {
        if (verbose) message("    ✗ CSV parse error")
        next
      }

      if (nrow(chunk_data) == 0) {
        if (verbose) message("    • No data")
        data_list[[i]] <- NULL
        is_error[i] <- FALSE
        next
      }

      data_list[[i]] <- chunk_data
      is_error[i] <- FALSE
      if (verbose) message("    ✓ ", nrow(chunk_data), " rows")

      if (nrow(chunk_data) == 25000) {
        warning(sprintf("Chunk %d returned exactly 25,000 rows - data may be truncated", i))
      }

      Sys.sleep(1)
    }
  }

  if (any(is_error)) {
    warning(sprintf("%d chunk(s) failed after %d retries", sum(is_error), max_retries))
  }

  successful_data <- data_list[!sapply(data_list, is.null)]

  if (length(successful_data) == 0) {
    warning("No AAA data returned for this window.")
    return(tibble())
  }

  combined <- dplyr::bind_rows(successful_data)

  chunk_sizes <- sapply(successful_data, nrow)
  if (any(chunk_sizes == 25000)) {
    n_at_limit <- sum(chunk_sizes == 25000)
    warning(sprintf("%d chunk(s) returned exactly 25,000 rows. Data are likely missing.", n_at_limit))
  }

  if (verbose) message("✓ Total AAA rows: ", nrow(combined))

  tibble::as_tibble(combined)
}

load_season_data <- function(year, start_date, end_date, game_type = "R", verbose = TRUE) {
  
  cache_file <- sprintf("%s/savant_raw_%d_%s.Rds", CACHE_DIR, year, game_type)
  
  if (file.exists(cache_file)) {
    message("✅ Using cached data for ", year, ": ", cache_file)
    return(readRDS(cache_file))
  }
  
  message("⬇️ Downloading ", year, " data...")
  raw <- load_statcast_range(start_date, end_date, game_type = game_type, 
                              level = "MLB", verbose = verbose)
  
  if (nrow(raw) > 0) {
    saveRDS(raw, cache_file)
    message("💾 Cached to ", cache_file)
  }
  
  raw
}

# ============================================================================
# BATTER ID COALESCING (handle different column names)
# ============================================================================

coalesce_batter_id <- function(df) {
  # Possible column names for batter ID in Statcast data
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
# BATTED BALL EVENT IDENTIFICATION
# ============================================================================

is_batted_ball <- function(df) {
  # A batted ball event is when the ball is put in play
  # Identified by: type == "X" or description contains "hit_into_play"
  
  # Ensure columns exist
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
# BATTER AGGREGATION - SEASON LEVEL
# ============================================================================

aggregate_batter_season <- function(df, year, verbose = TRUE) {
  
  if (verbose) message("Aggregating batter stats for ", year, "...")
  
  # Debug: show available columns
  if (verbose) {
    batter_cols <- grep("batter|player", names(df), value = TRUE, ignore.case = TRUE)
    message("  Available batter-related columns: ", paste(batter_cols, collapse = ", "))
    game_cols <- grep("game|pk|ab|at_bat", names(df), value = TRUE, ignore.case = TRUE)
    message("  Available game-related columns: ", paste(game_cols, collapse = ", "))
  }
  
  # Coalesce batter ID from various possible column names
  df$batter_id <- coalesce_batter_id(df)
  
  # Coalesce game identifier
  game_id_cols <- c("game_pk", "game_id", "gamePk", "gameId", "game")
  for (nm in game_id_cols) {
    if (nm %in% names(df) && !all(is.na(df[[nm]]))) {
      df$game_pk <- df[[nm]]
      break
    }
  }
  if (!"game_pk" %in% names(df)) df$game_pk <- 1  # Fallback
  
  # Coalesce at-bat identifier
  ab_cols <- c("at_bat_number", "atBatNumber", "ab_number", "at_bat")
  for (nm in ab_cols) {
    if (nm %in% names(df) && !all(is.na(df[[nm]]))) {
      df$at_bat_number <- df[[nm]]
      break
    }
  }
  if (!"at_bat_number" %in% names(df)) df$at_bat_number <- seq_len(nrow(df))  # Fallback
  
  if (verbose) {
    message("  Unique batters found: ", length(unique(df$batter_id[!is.na(df$batter_id)])))
  }
  
  # Filter to batted ball events only for BBE-specific metrics
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
  
  # All plate appearances for context
  pa_counts <- df %>%
    filter(!is.na(batter_id)) %>%
    group_by(batter_id) %>%
    summarise(
      pa = n_distinct(game_pk, at_bat_number),
      .groups = "drop"
    )
  
  # Batted ball events only
  bbe_data <- df %>%
    filter(is_bbe, !is.na(batter_id))
  
  if (nrow(bbe_data) == 0) {
    warning("No batted ball events found for ", year)
    return(tibble())
  }
  
  if (verbose) message("  Batted ball events: ", nrow(bbe_data))
  
  # Core batted ball metrics
  batter_stats <- bbe_data %>%
    group_by(batter_id) %>%
    summarise(
      # Outcomes
      bbe = n(),
      hr = sum(events == "home_run", na.rm = TRUE),
      
      # Exit velocity
      avg_ev = mean(launch_speed, na.rm = TRUE),
      max_ev = safe_max(launch_speed),
      ev_90th = if (sum(!is.na(launch_speed)) > 0) 
                  quantile(launch_speed, 0.9, na.rm = TRUE) else NA_real_,
      
      # Launch angle
      avg_la = mean(launch_angle, na.rm = TRUE),
      la_sd = sd(launch_angle, na.rm = TRUE),
      
      # Quality of contact rates
      hard_hit_rate = mean(launch_speed >= 95, na.rm = TRUE),
      barrel_rate = mean(barrel == 1, na.rm = TRUE),
      sweet_spot_rate = mean(launch_angle >= 8 & launch_angle <= 32, na.rm = TRUE),
      
      # Optimal HR zone (25-35 degrees, 95+ mph)
      optimal_hr_rate = mean(
        launch_angle >= 25 & launch_angle <= 35 & launch_speed >= 95, 
        na.rm = TRUE
      ),
      
      # Spray angle (pull tendency)
      avg_spray_angle = mean(hc_x - 125.42, na.rm = TRUE),
      pull_rate = mean(
        (stand == "R" & hc_x < 125.42) | (stand == "L" & hc_x > 125.42),
        na.rm = TRUE
      ),
      
      # Bat tracking metrics (2024+ only)
      avg_bat_speed = mean(bat_speed, na.rm = TRUE),
      avg_swing_length = mean(swing_length, na.rm = TRUE),
      squared_up_rate = mean(squared_up, na.rm = TRUE),
      
      # Handedness (for context)
      stand = first(stand),
      
      .groups = "drop"
    ) %>%
    # Calculate HR/BBE
    mutate(
      hr_per_bbe = hr / bbe,
      year = year
    ) %>%
    # Join PA counts
    left_join(pa_counts, by = "batter_id") %>%
    # Rename for consistency downstream
    rename(batter = batter_id) %>%
    # Filter to minimum BBE
    filter(bbe >= MIN_BBE_PER_YEAR)
  
  if (verbose) message("  ", nrow(batter_stats), " batters with ", MIN_BBE_PER_YEAR, "+ BBE")
  
  batter_stats
}

# ============================================================================
# RESOLVE BATTER NAMES
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
  
  # Load cache
  cache <- if (file.exists(cache_file)) {
    suppressWarnings(readr::read_csv(cache_file, show_col_types = FALSE)) %>%
      mutate(batter = as.integer(batter)) %>%
      distinct()
  } else {
    tibble(batter = integer(0), batter_name = character(0))
  }
  
  need_ids <- setdiff(ids, cache$batter)
  
  if (length(need_ids) > 0) {
    if (verbose) message("Resolving ", length(need_ids), " batter names via StatsAPI...")
    
    # Batch fetch
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
      
      Sys.sleep(0.5)  # Rate limiting
    }
    
    # Update cache
    cache <- bind_rows(cache, new_rows) %>% distinct()
    readr::write_csv(cache, cache_file)
  }
  
  cache %>%
    filter(batter %in% ids) %>%
    mutate(batter_name = if_else(
      is.na(batter_name), 
      paste0("Batter_", batter), 
      batter_name
    ))
}

# ============================================================================
# COMPUTE YEAR-OVER-YEAR DELTAS
# ============================================================================

compute_deltas <- function(year1_data, year2_data, verbose = TRUE) {
  
  if (verbose) message("Computing year-over-year deltas...")
  
  # Metrics to compute deltas for
  delta_metrics <- c(
    "avg_ev", "max_ev", "ev_90th",
    "avg_la", "la_sd",
    "hard_hit_rate", "barrel_rate", "sweet_spot_rate", "optimal_hr_rate",
    "pull_rate",
    "avg_bat_speed", "avg_swing_length", "squared_up_rate"
  )
  
  # Only include metrics that exist in both datasets
  available_metrics <- delta_metrics[
    delta_metrics %in% names(year1_data) & delta_metrics %in% names(year2_data)
  ]
  
  if (verbose) message("  Metrics available in both years: ", paste(available_metrics, collapse = ", "))
  
  # Join years
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
  
  # Compute deltas
  for (metric in available_metrics) {
    y1_col <- paste0(metric, "_y1")
    y2_col <- paste0(metric, "_y2")
    delta_col <- paste0("delta_", metric)
    
    combined[[delta_col]] <- combined[[y2_col]] - combined[[y1_col]]
  }
  
  # Target variable: change in HR/BBE
  combined <- combined %>%
    mutate(
      delta_hr_per_bbe = hr_per_bbe_y2 - hr_per_bbe_y1,
      # Also compute % change for context
      pct_change_hr_bbe = (hr_per_bbe_y2 - hr_per_bbe_y1) / pmax(hr_per_bbe_y1, 0.01)
    )
  
  # Diagnostic: show data availability for each delta column
  if (verbose) {
    message("\n  Data availability by metric:")
    delta_cols <- grep("^delta_", names(combined), value = TRUE)
    for (col in delta_cols) {
      pct_valid <- round(mean(!is.na(combined[[col]])) * 100, 1)
      message("    ", col, ": ", pct_valid, "% non-NA")
    }
  }
  
  combined
}

# ============================================================================
# FIT GBM MODEL
# ============================================================================

fit_gbm_model <- function(delta_data, verbose = TRUE) {
  
  if (verbose) message("\n🔧 Fitting GBM model for HR/BBE change...")
  
  # Predictor columns (the deltas)
  # NOTE: Excluding barrel_rate and optimal_hr_rate - these are too close to the outcome
  # (they essentially measure "did you hit more HR-type batted balls")
  predictor_cols <- c(
    "delta_avg_ev", "delta_max_ev", "delta_ev_90th",
    "delta_avg_la", "delta_la_sd",
    "delta_hard_hit_rate",  # Keep this - it's EV-based only, not outcome-based
    "delta_sweet_spot_rate",  # LA-based only
    "delta_pull_rate",
    "delta_avg_bat_speed", "delta_avg_swing_length", "delta_squared_up_rate"
  )
  
  # Also include Year 1 levels as predictors (room to grow)
  # Excluding barrel_rate_y1 for same reason
  level_cols <- c(
    "avg_ev_y1", "avg_la_y1", 
    "hard_hit_rate_y1",
    "pull_rate_y1", "hr_per_bbe_y1"
  )
  
  all_predictors <- c(predictor_cols, level_cols)
  
  # Filter to columns that actually exist in the data
  available_predictors <- all_predictors[all_predictors %in% names(delta_data)]
  
  # Check which predictors have sufficient non-NA data (at least 50% non-NA)
  usable_predictors <- c()
  for (pred in available_predictors) {
    pct_valid <- mean(!is.na(delta_data[[pred]]))
    if (pct_valid >= 0.5) {
      usable_predictors <- c(usable_predictors, pred)
    } else if (verbose) {
      message("  Dropping ", pred, " (", round(pct_valid * 100, 1), "% valid)")
    }
  }
  
  if (length(usable_predictors) == 0) {
    stop("No predictors have sufficient non-NA data")
  }
  
  if (verbose) {
    message("  Usable predictors (>=50% non-NA): ", length(usable_predictors))
  }
  
  # Select columns and remove rows with NA in usable predictors
  model_data <- delta_data %>%
    select(delta_hr_per_bbe, all_of(usable_predictors)) %>%
    drop_na()
  
  if (verbose) message("  Using ", nrow(model_data), " complete cases")
  if (verbose) message("  Predictors: ", paste(usable_predictors, collapse = ", "))
  
  if (nrow(model_data) < 30) {
    stop("Insufficient data for modeling (", nrow(model_data), " rows). Need at least 30.")
  }
  
  # Fit GBM with adjusted parameters for smaller datasets
  set.seed(42)
  
  # Adjust cv.folds based on sample size
  n_folds <- min(5, floor(nrow(model_data) / 10))
  n_folds <- max(n_folds, 2)  # At least 2-fold CV
  
  gbm_fit <- gbm(
    formula = delta_hr_per_bbe ~ .,
    data = model_data,
    distribution = "gaussian",
    n.trees = 500,
    interaction.depth = 3,
    shrinkage = 0.01,
    n.minobsinnode = max(3, floor(nrow(model_data) / 50)),  # Adaptive
    bag.fraction = 0.5,
    cv.folds = n_folds,
    verbose = FALSE
  )
  
  # Optimal number of trees
  best_trees <- gbm.perf(gbm_fit, method = "cv", plot.it = FALSE)
  
  if (verbose) {
    message("  ✅ GBM fitted")
    message("  Optimal trees (CV): ", best_trees)
    message("  CV folds used: ", n_folds)
  }
  
  list(
    model = gbm_fit,
    best_trees = best_trees,
    predictors = usable_predictors,
    model_data = model_data
  )
}

# ============================================================================
# VARIABLE IMPORTANCE
# ============================================================================

get_variable_importance <- function(gbm_result, verbose = TRUE) {
  
  if (verbose) message("\n📊 Variable Importance:")
  
  importance <- summary(gbm_result$model, n.trees = gbm_result$best_trees, 
                        plotit = FALSE)
  
  importance <- importance %>%
    as_tibble() %>%
    rename(variable = var, rel_influence = rel.inf) %>%
    arrange(desc(rel_influence))
  
  if (verbose) {
    print(importance, n = 15)
  }
  
  importance
}

# ============================================================================
# IDENTIFY BREAKOUT CANDIDATES
# ============================================================================

identify_breakout_candidates <- function(delta_data, gbm_result, year2_data, 
                                          name_map, verbose = TRUE) {
  
  if (verbose) message("\n🎯 Identifying potential breakout candidates...")
  
  # Get predictions for each player
  predict_data <- delta_data %>% select(any_of(gbm_result$predictors))
  
  # Handle case where some predictors might be missing
  missing_preds <- setdiff(gbm_result$predictors, names(predict_data))
  if (length(missing_preds) > 0) {
    for (mp in missing_preds) {
      predict_data[[mp]] <- 0  # Neutral value for missing predictors
    }
  }
  
  predictions <- predict(
    gbm_result$model, 
    newdata = predict_data,
    n.trees = gbm_result$best_trees
  )
  
  delta_data$predicted_delta_hr_bbe <- predictions
  
  # Calculate a "breakout score" based on key indicators
  # Higher = more likely to improve HR/BBE
  candidates <- delta_data %>%
    left_join(name_map, by = "batter") %>%
    mutate(
      # Residual: actual - predicted (negative = underperformed model expectation)
      residual = delta_hr_per_bbe - predicted_delta_hr_bbe,
      
      # "Unrealized potential": positive model prediction but small/negative actual change
      unrealized = predicted_delta_hr_bbe - delta_hr_per_bbe,
      
      # === BREAKOUT SCORE COMPONENTS ===
      # 1. Low HR/BBE in Y2 = room to grow (standardized)
      hr_bbe_room = -safe_scale(hr_per_bbe_y2),
      
      # 2. Positive LA change toward optimal range
      # If LA was below 20, positive delta is good
      # If LA was above 35, negative delta is good
      la_improvement = case_when(
        is.na(avg_la_y1) | is.na(delta_avg_la) ~ 0,
        avg_la_y1 < 20 & delta_avg_la > 0 ~ delta_avg_la / 5,  # Reward raising low LA
        avg_la_y1 > 35 & delta_avg_la < 0 ~ -delta_avg_la / 5, # Reward lowering high LA
        avg_la_y1 >= 20 & avg_la_y1 <= 35 ~ 0.5,  # Already optimal
        TRUE ~ 0
      ),
      
      # 3. Hard hit rate improvement
      hh_improvement = safe_scale(delta_hard_hit_rate),
      
      # 4. Top-end EV improvement
      ev90_improvement = safe_scale(delta_ev_90th),
      
      # 5. Bat tracking improvements (if available)
      swing_improvement = if_else(
        !is.na(delta_avg_swing_length) & !is.na(delta_avg_bat_speed),
        (safe_scale(delta_avg_swing_length) + safe_scale(delta_avg_bat_speed)) / 2,
        0
      ),
      
      # Combined breakout score (weighted by variable importance)
      breakout_score = (
        0.20 * hr_bbe_room +
        0.15 * la_improvement +
        0.12 * coalesce(hh_improvement, 0) +
        0.12 * coalesce(ev90_improvement, 0) +
        0.10 * swing_improvement +
        0.31 * safe_scale(predicted_delta_hr_bbe)  # Model's prediction
      )
    ) %>%
    select(
      batter, batter_name, stand,
      hr_per_bbe_y1, hr_per_bbe_y2, delta_hr_per_bbe,
      predicted_delta_hr_bbe, breakout_score,
      # Key metrics for interpretation (use any_of for flexibility)
      any_of(c(
        "avg_la_y1", "delta_avg_la",
        "hard_hit_rate_y1", "delta_hard_hit_rate",
        "ev_90th_y1", "delta_ev_90th",
        "avg_bat_speed_y1", "delta_avg_bat_speed",
        "avg_swing_length_y1", "delta_avg_swing_length",
        "avg_ev_y1", "delta_avg_ev",
        "pull_rate_y1", "delta_pull_rate"
      )),
      bbe_y1, bbe_y2
    ) %>%
    arrange(desc(breakout_score))
  
  if (verbose) {
    message("\n========================================")
    message("  TOP 20 BREAKOUT CANDIDATES FOR 2026")
    message("========================================")
    message("Players with swing/contact improvements who may see HR gains:\n")
    
    # Select columns that exist
    display_cols <- c("batter_name", "hr_per_bbe_y2", "breakout_score",
                      "delta_avg_la", "delta_hard_hit_rate", "delta_ev_90th")
    if ("delta_avg_bat_speed" %in% names(candidates)) {
      display_cols <- c(display_cols, "delta_avg_bat_speed")
    }
    
    top_candidates <- candidates %>%
      head(20) %>%
      select(any_of(display_cols))
    
    print(top_candidates, n = 20)
    
    message("\n========================================")
    message("  BOTTOM 10: Potential HR Regression DOWN")
    message("========================================")
    message("Players whose process metrics declined:\n")
    
    bottom_candidates <- candidates %>%
      tail(10) %>%
      arrange(breakout_score) %>%
      select(any_of(c("batter_name", "hr_per_bbe_y2", "breakout_score",
                      "delta_avg_la", "delta_hard_hit_rate", "delta_ev_90th")))
    
    print(bottom_candidates, n = 10)
  }
  
  candidates
}

# ============================================================================
# VISUALIZATIONS
# ============================================================================

create_hr_visualizations <- function(delta_data, gbm_result, importance, 
                                      candidates, output_dir = "output/visualizations") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 not available; skipping visualizations")
    return(invisible(NULL))
  }
  
  library(ggplot2)
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # 1. Variable Importance
  p1 <- ggplot(importance %>% head(12), 
               aes(x = reorder(variable, rel_influence), y = rel_influence)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Variable Importance: Predicting HR/BBE Change",
      x = NULL, y = "Relative Influence"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "hr_bbe_variable_importance.png"), p1, width = 10, height = 6)
  
  # 2. Actual vs Predicted Change
  p2 <- ggplot(candidates, aes(x = predicted_delta_hr_bbe, y = delta_hr_per_bbe)) +
    geom_point(alpha = 0.6, color = "darkgreen") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
    labs(
      title = "Actual vs Predicted HR/BBE Change",
      subtitle = "Points below line = underperformed swing changes",
      x = "Predicted Change in HR/BBE",
      y = "Actual Change in HR/BBE"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "hr_bbe_actual_vs_predicted.png"), p2, width = 10, height = 8)
  
  # 3. Launch Angle Change vs HR/BBE Change
  p3 <- ggplot(delta_data, aes(x = delta_avg_la, y = delta_hr_per_bbe)) +
    geom_point(alpha = 0.5, color = "coral") +
    geom_smooth(method = "loess", se = TRUE, color = "darkred") +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(
      title = "Launch Angle Change vs HR/BBE Change",
      x = "Change in Average Launch Angle (degrees)",
      y = "Change in HR/BBE"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "hr_la_change_relationship.png"), p3, width = 10, height = 8)
  
  # 4. Barrel Rate Change vs HR/BBE Change
  p4 <- ggplot(delta_data, aes(x = delta_barrel_rate, y = delta_hr_per_bbe)) +
    geom_point(alpha = 0.5, color = "purple") +
    geom_smooth(method = "loess", se = TRUE, color = "darkviolet") +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(
      title = "Barrel Rate Change vs HR/BBE Change",
      x = "Change in Barrel Rate",
      y = "Change in HR/BBE"
    ) +
    theme_minimal()
  ggsave(file.path(output_dir, "hr_barrel_change_relationship.png"), p4, width = 10, height = 8)
  
  message("✅ Created visualizations in ", output_dir)
  invisible(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

run_hr_analysis <- function(verbose = TRUE) {
  
  ensure_directories()
  
  cat("\n")
  cat("============================================================\n")
  cat("  HR/BBE Change Analysis - Exploratory\n")
  cat("============================================================\n")
  cat("Year 1:", YEAR_1, "(", YEAR_1_START, "to", YEAR_1_END, ")\n")
  cat("Year 2:", YEAR_2, "(", YEAR_2_START, "to", YEAR_2_END, ")\n")
  cat("Min BBE per year:", MIN_BBE_PER_YEAR, "\n")
  cat("============================================================\n\n")
  
  # Load data
  raw_y1 <- load_season_data(YEAR_1, YEAR_1_START, YEAR_1_END, verbose = verbose)
  raw_y2 <- load_season_data(YEAR_2, YEAR_2_START, YEAR_2_END, verbose = verbose)
  
  # Aggregate to batter-season level
  batter_y1 <- aggregate_batter_season(raw_y1, YEAR_1, verbose = verbose)
  batter_y2 <- aggregate_batter_season(raw_y2, YEAR_2, verbose = verbose)
  
  # Compute deltas
  delta_data <- compute_deltas(batter_y1, batter_y2, verbose = verbose)
  
  # Resolve names
  name_map <- resolve_batter_names(delta_data, verbose = verbose)
  
  # Fit GBM
  gbm_result <- fit_gbm_model(delta_data, verbose = verbose)
  
  # Variable importance
  importance <- get_variable_importance(gbm_result, verbose = verbose)
  
  # Identify candidates
  candidates <- identify_breakout_candidates(
    delta_data, gbm_result, batter_y2, name_map, verbose = verbose
  )
  
  # Visualizations
  create_hr_visualizations(delta_data, gbm_result, importance, candidates)
  
  # Save outputs
  readr::write_csv(importance, file.path(OUT_DIR, "hr_bbe_variable_importance.csv"))
  readr::write_csv(candidates, file.path(OUT_DIR, "hr_bbe_candidates.csv"))
  
  # Save the model and baseline data for early-season scoring
  model_package <- list(
    model = gbm_result$model,
    best_trees = gbm_result$best_trees,
    predictors = gbm_result$predictors,
    baseline_year = YEAR_2,
    baseline_data = batter_y2,  # This becomes the "Year 1" for next season
    training_importance = importance
  )
  saveRDS(model_package, file.path(OUT_DIR, "hr_breakout_model.rds"))
  message("✅ Saved model package for early-season scoring: ", file.path(OUT_DIR, "hr_breakout_model.rds"))
  
  cat("\n")
  cat("============================================================\n")
  cat("  Analysis Complete!\n")
  cat("============================================================\n")
  cat("✅ Variable importance: ", file.path(OUT_DIR, "hr_bbe_variable_importance.csv"), "\n")
  cat("✅ Player candidates:   ", file.path(OUT_DIR, "hr_bbe_candidates.csv"), "\n")
  cat("✅ Visualizations:      ", file.path(OUT_DIR, "visualizations/"), "\n")
  cat("============================================================\n\n")
  
  # Return results
  list(
    year1_data = batter_y1,
    year2_data = batter_y2,
    delta_data = delta_data,
    gbm_result = gbm_result,
    importance = importance,
    candidates = candidates,
    name_map = name_map
  )
}

# ============================================================================
# RUN
# ============================================================================

if (interactive() || length(commandArgs(trailingOnly = TRUE)) == 0) {
  results <- run_hr_analysis(verbose = TRUE)
}
