# load_model_data.R
#
# Single entry point for analysis scripts.
# Source build_features.R first, then call load_model_data().
# Caches result to model_data.rds; invalidate via invalidate("starter_pa_full").

load_model_data <- function(tto_max = 4) {
  data_dir <- if (exists("DATA_DIR")) DATA_DIR else
    "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"
  path <- file.path(data_dir, "model_data.rds")
  if (file.exists(path)) {
    message("  [cache] model_data.rds")
    return(readRDS(path))
  }

  pipeline   <- build_all()
  starter_pa <- pipeline$starter_pa_full

  result <- starter_pa |>
    filter(n_thruorder <= tto_max) |>
    mutate(
      # ── Standardized predictors ───────────────────────────────────────────
      velo_dev_z        = scale(velo_dev_pa)[, 1],
      velo_dev_ingame_z = scale(velo_dev_ingame_pa)[, 1],
      batter_xwoba_z  = scale(batter_xwoba_est)[, 1],
      pitcher_xwoba_z = scale(pitcher_xwoba_est)[, 1],
      ingame_dev_z    = scale(ingame_deviation)[, 1],

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

  saveRDS(result, path)
  message("  [build] model_data.rds")
  result
}