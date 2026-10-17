# load_model_data.R
#
# Single entry point for analysis scripts.
# Source build_features.R first, then call load_model_data().
# Caches result to model_data.rds; invalidate via invalidate("starter_pa_full").
#
# train_seasons: when supplied, z-score scaling parameters are estimated from
# those seasons only and applied to all rows. Cache is bypassed so the main
# model_data.rds is never overwritten by a backtest-scaled copy.

load_model_data <- function(tto_max = 4, train_seasons = NULL) {
  data_dir <- if (exists("DATA_DIR")) DATA_DIR else
    "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"
  path <- file.path(data_dir, "model_data.rds")

  if (is.null(train_seasons) && file.exists(path)) {
    message("  [cache] model_data.rds")
    return(readRDS(path))
  }

  starter_pa <- readRDS(file.path(data_dir, "starter_pa_full.rds"))

  wf_path <- file.path(data_dir, "workload_features.rds")
  if (file.exists(wf_path)) {
    workload <- readRDS(wf_path)
    starter_pa <- starter_pa |>
      left_join(workload, by = c("season", "game_id", "inning_topbot", "pitcher_id"))
  }

  result <- starter_pa |>
    filter(n_thruorder <= tto_max)

  # z-score scaling: use training rows only when train_seasons is supplied,
  # otherwise compute from the full dataset (current behaviour).
  if (!is.null(train_seasons)) {
    train_mask <- result$season %in% train_seasons
    scale_to_train <- function(x) {
      m <- mean(x[train_mask], na.rm = TRUE)
      s <-   sd(x[train_mask], na.rm = TRUE)
      (x - m) / s
    }
  } else {
    scale_to_train <- function(x) scale(x)[, 1]
  }

  result <- result |>
    mutate(
      # ── Standardized predictors ───────────────────────────────────────────
      velo_dev_z        = scale_to_train(velo_dev_pa),
      velo_dev_ingame_z = scale_to_train(velo_dev_ingame_pa),
      batter_xwoba_z    = scale_to_train(batter_xwoba_est),
      pitcher_xwoba_z   = scale_to_train(pitcher_xwoba_est),
      ingame_dev_z      = scale_to_train(ingame_deviation),
      ingame_woba_dev_z = scale_to_train(ingame_woba_deviation),
      prev_pitches_z    = scale_to_train(prev_pitches),
      days_rest_z       = scale_to_train(days_rest),

      # ── Game state ────────────────────────────────────────────────────────
      run_diff  = bat_score - fld_score,
      runner_1b = as.integer(!is.na(pre_runner_1b)),
      runner_2b = as.integer(!is.na(pre_runner_2b)),
      runner_3b = as.integer(!is.na(pre_runner_3b)),

      # ── Factors ───────────────────────────────────────────────────────────
      tto_fac     = factor(n_thruorder),
      pitcher_fac = factor(pitcher_id)
    ) |>
    group_by(pitcher_id) |>
    mutate(
      pc_z = (cum_pitch_count - mean(cum_pitch_count, na.rm = TRUE)) /
        sd(cum_pitch_count, na.rm = TRUE)
    ) |>
    ungroup()

  if (is.null(train_seasons)) {
    saveRDS(result, path)
    message("  [build] model_data.rds")
  }

  result
}
