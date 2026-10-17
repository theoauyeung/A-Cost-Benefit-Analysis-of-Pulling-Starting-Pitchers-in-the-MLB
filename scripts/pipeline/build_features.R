# build_features.R
#
# Runs once (or per-stage when upstream inputs change).
# Each stage saves a .rds checkpoint and short-circuits on reload.
# To invalidate a stage: delete its checkpoint file and all downstream ones.
#
# Stage dependency chain:
#   clean_data.rds
#       └─ starter_pa_raw.rds
#             └─ starter_pa_est.rds
#                   └─ starter_pa_full.rds   ← most iteration happens here
#
# reliever_rolling.rds is built alongside starter_pa_est.rds (same pass).

library(tidyverse)
library(data.table)
library(baseballr)
library(slider)
library(fixest)
library(mgcv)

# ── Config ─────────────────────────────────────────────────────────────────

DATA_DIR       <- "~/Google Drive/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"
SAVANT_PATH    <- file.path(DATA_DIR, "savant_data.csv")
STUFF_PATH     <- "data/stuff_scores.csv"

# ── Checkpoint helper ───────────────────────────────────────────────────────
# Pass a file path and a *quoted* expression.
# Returns cached object if the file exists; otherwise evaluates, saves, returns.

checkpoint <- function(path, expr) {
  if (file.exists(path)) {
    message("  [cache] ", basename(path))
    return(readRDS(path))
  }
  message("  [build] ", basename(path))
  result <- force(expr)
  saveRDS(result, path)
  result
}

# ── Shared helpers ──────────────────────────────────────────────────────────

clean_xy <- function(df) {
  df |>
    select(-ends_with(".y")) |>
    rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))
}

resolve_xwoba <- function(expected_woba, woba_value, woba_denom) {
  case_when(
    !is.na(expected_woba) ~ expected_woba,
    woba_denom == 1       ~ woba_value,
    TRUE                  ~ NA_real_
  )
}

eval_prior <- function(prior_n, pa_data, league_avg) {
  pa_data |>
    arrange(season, pitcher_id, game_id, at_bat_number) |>
    group_by(season, pitcher_id) |>
    mutate(
      cum_xwoba = lag(cumsum(replace_na(xwoba_outcome, 0)), default = 0),
      cum_pa    = lag(cumsum(!is.na(xwoba_outcome)), default = 0),
      est       = (prior_n * league_avg + cum_xwoba) / (prior_n + cum_pa)
    ) |>
    ungroup() |>
    filter(!is.na(xwoba_outcome)) |>
    summarise(mse = mean((xwoba_outcome - est)^2)) |>
    mutate(prior_n = prior_n)
}

eval_decay <- function(decay, pa_data, prior_n, league_avg, id_col = "pitcher_id") {
  rolled <- build_rolling(pa_data, prior_n, league_avg,
                          est_col = "est", id_col = id_col, season_decay = decay)
  rolled |>
    filter(!is.na(xwoba_outcome)) |>
    group_by(season, .data[[id_col]]) |>
    slice_head(n = 50) |>
    ungroup() |>
    summarise(mse = mean((xwoba_outcome - est)^2, na.rm = TRUE)) |>
    mutate(decay = decay)
}

build_rolling <- function(pa_data, prior_n, league_avg, est_col,
                          id_col = "pitcher_id", season_decay) {
  pa_data <- pa_data |> rename(player_id = all_of(id_col))
  
  posteriors <- pa_data |>
    filter(!is.na(xwoba_outcome)) |>
    group_by(season, player_id) |>
    summarise(
      season_pa    = n(),
      season_xwoba = mean(xwoba_outcome, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      posterior_mean = (prior_n * league_avg + season_pa * season_xwoba) /
        (prior_n + season_pa),
      posterior_n    = prior_n + season_pa
    )
  
  prior_lookup <- posteriors |>
    mutate(
      next_season   = as.integer(season + 1L),
      carry_prior_n = posterior_n * season_decay
    ) |>
    bind_rows(
      posteriors |>
        filter(season == 2019L) |>
        mutate(
          next_season   = 2021L,
          carry_prior_n = posterior_n * season_decay
        )
    ) |>
    select(player_id, next_season,
           carry_prior_mean = posterior_mean,
           carry_prior_n)
  
  pa_data |>
    left_join(prior_lookup, by = c("player_id", "season" = "next_season")) |>
    mutate(
      prior_mean = if_else(is.na(carry_prior_mean), league_avg, carry_prior_mean),
      prior_wt   = if_else(is.na(carry_prior_n),    prior_n,    carry_prior_n)
    ) |>
    arrange(season, player_id, game_id, at_bat_number) |>
    group_by(season, player_id) |>
    mutate(
      cum_xwoba = lag(cumsum(replace_na(xwoba_outcome, 0)), default = 0),
      cum_pa    = lag(cumsum(!is.na(xwoba_outcome)), default = 0),
      !!est_col := (prior_wt * prior_mean + cum_xwoba) / (prior_wt + cum_pa)
    ) |>
    ungroup() |>
    rename(!!id_col := player_id) |>
    select(-carry_prior_mean, -carry_prior_n, -prior_mean, -prior_wt,
           -cum_xwoba, -cum_pa)
}

# ══════════════════════════════════════════════════════════════════════════════
# Stage 1: clean_data
#   Raw Savant + pitch physics features + starter/reliever flags + name lookups
#   Invalidate when: savant_data.csv or stuff_scores.csv changes
# ══════════════════════════════════════════════════════════════════════════════

build_clean_data <- function() {
  message("\n── Stage 1: clean_data ──")
  
  savant_data <- fread(SAVANT_PATH)
  clean_data  <- savant_data |>
    mutate(season = as.integer(year)) |>
    filter(season != 2020)
  rm(savant_data)
  
  pitcher_ids <- chadwick_player_lu()
  
  starters <- clean_data |>
    filter(inning == 1) |>
    group_by(season, game_id, inning_topbot) |>
    slice_min(at_bat_number, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(season, game_id, inning_topbot, starter_id = pitcher_id) |>
    distinct()
  
  clean_data <- clean_data |>
    left_join(starters, by = c("season", "game_id", "inning_topbot")) |>
    mutate(is_starter = pitcher_id == starter_id) |>
    clean_xy()
  
  clean_data <- clean_data |>
    left_join(
      pitcher_ids |> select(key_mlbam, name_first, name_last),
      by = c("pitcher_id" = "key_mlbam")
    ) |>
    mutate(pitcher_name = paste(name_first, name_last)) |>
    select(-c(name_first, name_last)) |>
    clean_xy()
  
  clean_data <- clean_data |>
    left_join(
      pitcher_ids |> select(key_mlbam, name_first, name_last),
      by = c("batter_id" = "key_mlbam")
    ) |>
    mutate(batter_name = paste(name_first, name_last)) |>
    select(-c(name_first, name_last)) |>
    clean_xy()
  
  # Pitch physics
  stuff_scores <- fread(STUFF_PATH)
  g <- 32.5
  
  clean_data <- clean_data |>
    left_join(stuff_scores, by = c("game_id", "inning_topbot", "at_bat_number", "pitch_number")) |>
    clean_xy() |>
    mutate(
      pfx_x_inches       = pfx_x * 12,
      pfx_z_inches       = pfx_z * 12,
      ivb                = pfx_z_inches,
      hb                 = case_when(
        pitch_hand == "R" ~ -pfx_x_inches,
        pitch_hand == "L" ~  pfx_x_inches,
        TRUE              ~ NA_real_
      ),
      arm_angle          = if_else(is.na(arm_angle), mean(arm_angle, na.rm = TRUE), arm_angle),
      ax_spin            = ax,
      az_spin            = az - g,
      bauer_units        = release_spin_rate / release_speed,
      spin_axis_rad      = spin_axis * pi / 180,
      pred_ivb_magnus    = bauer_units * cos(spin_axis_rad),
      pred_hb_magnus     = bauer_units * sin(spin_axis_rad),
      ssw_vertical       = ivb - pred_ivb_magnus,
      ssw_horizontal     = hb  - pred_hb_magnus,
      movement_angle     = atan2(ivb, hb) * 180 / pi,
      movement_angle_adj = case_when(
        pitch_hand == "R" ~  movement_angle,
        pitch_hand == "L" ~ -movement_angle,
        TRUE              ~ NA_real_
      ),
      diff_from_arm      = abs(((movement_angle - arm_angle + 180) %% 360) - 180),
      accel_angle        = atan2(az_spin, ax_spin) * 180 / pi,
      accel_arm_dev      = abs(((accel_angle - arm_angle + 180) %% 360) - 180),
      distance           = 60.5 - extension,
      t_flight           = distance / abs(vy0),
      vz_final           = vz0 + az * t_flight,
      vx_final           = vx0 + ax * t_flight,
      vaa                = atan(vz_final / abs(vy0)) * (180 / pi),
      haa                = atan(vx_final / abs(vy0)) * (180 / pi)
    )
  
  cat("  stuff non-NA rate:", round(mean(!is.na(clean_data$stuff)), 3), "\n")
  clean_data
}

# ══════════════════════════════════════════════════════════════════════════════
# Stage 2: starter_pa_raw
#   PA-level table for starters — no estimates yet, just event aggregation.
#   Invalidate when: clean_data.rds changes or PA aggregation logic changes
# ══════════════════════════════════════════════════════════════════════════════

build_starter_pa_raw <- function(clean_data) {
  message("\n── Stage 2: starter_pa_raw ──")
  
  starter_pa <- clean_data |>
    filter(is_starter == TRUE) |>
    group_by(season, game_id, inning_topbot, at_bat_number) |>
    summarise(
      pitcher_id    = first(pitcher_id),
      pitcher_name  = first(pitcher_name),
      batter_id     = first(batter_id),
      batter_name   = first(batter_name),
      inning        = first(inning),
      outs          = first(outs),
      balls         = last(balls),
      strikes       = last(strikes),
      n_thruorder   = first(n_thruorder_pitcher),
      events        = last(events),
      xwoba_outcome = resolve_xwoba(last(expected_woba), last(woba_value), last(woba_denom)),
      woba_value    = last(woba_value),
      woba_denom    = last(woba_denom),
      delta_run_exp = sum(delta_run_exp, na.rm = TRUE),
      pre_runner_1b = first(pre_runner_1b_id),
      pre_runner_2b = first(pre_runner_2b_id),
      pre_runner_3b = first(pre_runner_3b_id),
      bat_score     = first(bat_score),
      fld_score     = first(fld_score),
      home_win_exp  = first(home_win_exp),
      .groups = "drop"
    )
  
  cum_pitch_lookup <- clean_data |>
    filter(is_starter == TRUE) |>
    group_by(season, game_id, inning_topbot, pitcher_id, at_bat_number) |>
    summarise(pitches_in_pa = n(), .groups = "drop") |>
    arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number) |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    mutate(cum_pitch_count = lag(cumsum(pitches_in_pa), default = 0) + 1) |>
    ungroup() |>
    select(season, game_id, inning_topbot, at_bat_number, cum_pitch_count)
  
  starter_pa <- starter_pa |>
    left_join(cum_pitch_lookup, by = c("season", "game_id", "inning_topbot", "at_bat_number")) |>
    clean_xy()
  
  cat("  xwoba coverage:    ", round(mean(!is.na(starter_pa$xwoba_outcome)), 3), "\n")
  cat("  batter name cover: ", round(mean(!is.na(starter_pa$batter_name) & starter_pa$batter_name != "NA NA"), 3), "\n")
  starter_pa
}

# ══════════════════════════════════════════════════════════════════════════════
# Stage 3: starter_pa_est  +  reliever_rolling
#   Bayesian rolling estimates for pitchers, batters, matchups, relievers.
#   Invalidate when: prior search grid changes, season_decay changes,
#                    matchup prior changes, or starter_pa_raw.rds changes
# ══════════════════════════════════════════════════════════════════════════════

PRIOR_GRID       <- c(50, 100, 150, 200, 300, 400, 500)
DECAY_GRID       <- seq(0, 1, by = 0.1)
MATCHUP_PRIOR_N  <- 20L

build_estimates <- function(clean_data, starter_pa) {
  message("\n── Stage 3: estimates (pitcher / batter / matchup / reliever) ──")
  
  # 3a. Pitcher rolling
  league_xwoba_sp <- mean(starter_pa$xwoba_outcome, na.rm = TRUE)
  prior_n_sp <- map_dfr(PRIOR_GRID, eval_prior,
                        pa_data = starter_pa, league_avg = league_xwoba_sp) |>
    slice_min(mse, n = 1) |> pull(prior_n)
  cat("  Starter optimal prior_n:", prior_n_sp, "\n")
  decay_mse_sp <- map_dfr(DECAY_GRID, eval_decay,
                          pa_data = starter_pa, prior_n = prior_n_sp, league_avg = league_xwoba_sp)
  decay_sp <- decay_mse_sp |> slice_min(mse, n = 1, with_ties = FALSE) |> pull(decay)
  cat("  Starter optimal decay:", decay_sp, "\n")

  starter_pa <- build_rolling(starter_pa, prior_n_sp, league_xwoba_sp,
                              est_col = "pitcher_xwoba_est", id_col = "pitcher_id",
                              season_decay = decay_sp) |>
    clean_xy()
  
  # 3b. Batter rolling
  batter_pa <- clean_data |>
    filter(woba_denom == 1) |>
    group_by(season, game_id, inning_topbot, at_bat_number) |>
    summarise(
      batter_id     = first(batter_id),
      xwoba_outcome = resolve_xwoba(last(expected_woba), last(woba_value), last(woba_denom)),
      .groups = "drop"
    )
  
  league_xwoba_bat <- mean(batter_pa$xwoba_outcome, na.rm = TRUE)
  prior_n_bat <- map_dfr(PRIOR_GRID, eval_prior,
                         pa_data = batter_pa |> rename(pitcher_id = batter_id),
                         league_avg = league_xwoba_bat) |>
    slice_min(mse, n = 1) |> pull(prior_n)
  cat("  Batter optimal prior_n:", prior_n_bat, "\n")
  decay_mse_bat <- map_dfr(DECAY_GRID, eval_decay,
                           pa_data = batter_pa |> rename(pitcher_id = batter_id),
                           prior_n = prior_n_bat, league_avg = league_xwoba_bat)
  decay_bat <- decay_mse_bat |> slice_min(mse, n = 1, with_ties = FALSE) |> pull(decay)
  cat("  Batter optimal decay:", decay_bat, "\n")

  batter_rolling <- build_rolling(batter_pa, prior_n_bat, league_xwoba_bat,
                                  est_col = "batter_xwoba_est", id_col = "batter_id",
                                  season_decay = decay_bat) |>
    select(season, game_id, inning_topbot, at_bat_number, batter_id, batter_xwoba_est) |>
    mutate(batter_xwoba_est = if_else(is.na(batter_xwoba_est), league_xwoba_bat, batter_xwoba_est))
  
  starter_pa <- starter_pa |>
    left_join(batter_rolling, by = c("season", "game_id", "inning_topbot",
                                     "at_bat_number", "batter_id")) |>
    mutate(batter_xwoba_est = if_else(is.na(batter_xwoba_est), league_xwoba_bat, batter_xwoba_est)) |>
    clean_xy()
  
  # 3c. Matchup estimates
  h2h_stats <- starter_pa |>
    filter(!is.na(xwoba_outcome)) |>
    arrange(season, game_id, at_bat_number) |>
    group_by(batter_id, pitcher_id) |>
    mutate(
      h2h_cum_xwoba = lag(cumsum(replace_na(xwoba_outcome, 0)), default = 0),
      h2h_cum_pa    = lag(row_number() - 1L, default = 0L)
    ) |>
    ungroup() |>
    select(season, game_id, inning_topbot, at_bat_number,
           batter_id, pitcher_id, h2h_cum_xwoba, h2h_cum_pa)
  
  starter_pa <- starter_pa |>
    left_join(h2h_stats, by = c("season", "game_id", "inning_topbot",
                                "at_bat_number", "batter_id", "pitcher_id")) |>
    mutate(
      h2h_cum_pa        = replace_na(h2h_cum_pa, 0L),
      h2h_cum_xwoba     = replace_na(h2h_cum_xwoba, 0),
      matchup_xwoba_est = (MATCHUP_PRIOR_N * batter_xwoba_est + h2h_cum_xwoba) /
        (MATCHUP_PRIOR_N + h2h_cum_pa)
    ) |>
    clean_xy()
  
  # 3d. Reliever rolling
  reliever_pa <- clean_data |>
    filter(is_starter == FALSE, woba_denom == 1) |>
    group_by(season, game_id, inning_topbot, at_bat_number) |>
    summarise(
      pitcher_id    = first(pitcher_id),
      pitcher_name  = first(pitcher_name),
      xwoba_outcome = resolve_xwoba(last(expected_woba), last(woba_value), last(woba_denom)),
      .groups = "drop"
    )
  
  league_xwoba_rel <- mean(reliever_pa$xwoba_outcome, na.rm = TRUE)
  prior_n_rel <- map_dfr(PRIOR_GRID, eval_prior,
                         pa_data = reliever_pa, league_avg = league_xwoba_rel) |>
    slice_min(mse, n = 1) |> pull(prior_n)
  cat("  Reliever optimal prior_n:", prior_n_rel, "\n")
  decay_mse_rel <- map_dfr(DECAY_GRID, eval_decay,
                           pa_data = reliever_pa, prior_n = prior_n_rel, league_avg = league_xwoba_rel)
  decay_rel <- decay_mse_rel |> slice_min(mse, n = 1, with_ties = FALSE) |> pull(decay)
  cat("  Reliever optimal decay:", decay_rel, "\n")

  reliever_rolling <- build_rolling(reliever_pa, prior_n_rel, league_xwoba_rel,
                                    est_col = "reliever_xwoba_est", id_col = "pitcher_id",
                                    season_decay = decay_rel) |>
    clean_xy()
  
  # Save reliever_rolling as its own checkpoint alongside this stage
  saveRDS(reliever_rolling, file.path(DATA_DIR, "reliever_rolling.rds"))
  cat("  Saved reliever_rolling.rds\n")

  saveRDS(prior_n_sp, file.path(DATA_DIR, "prior_n_sp.rds"))
  cat("  Saved prior_n_sp.rds\n")

  decay_mse_grids <- list(
    starter  = decay_mse_sp  |> mutate(entity = "Starter"),
    batter   = decay_mse_bat |> mutate(entity = "Batter"),
    reliever = decay_mse_rel |> mutate(entity = "Reliever")
  )
  saveRDS(decay_mse_grids, file.path(DATA_DIR, "decay_mse_grids.rds"))
  cat("  Saved decay_mse_grids.rds\n")

  list(starter_pa = starter_pa, reliever_rolling = reliever_rolling, prior_n_sp = prior_n_sp)
}

# ══════════════════════════════════════════════════════════════════════════════
# Stage 4: starter_pa_full
#   Pure-starter filter, pull flags, VELOCITY depreciation, in-game performance,
#   team reliever quality. Everything needed before the model mutate block.
#
#   Velocity depreciation:
#     baseline = per-pitcher-season mean release_speed by pitch_category
#                (does NOT assume inning 1 is peak)
#     velo_dev = release_speed - baseline   (negative = throwing below season norm)
#     velo_dev_roll = rolling mean of velo_dev over last VELO_ROLL_WINDOW+1 pitches,
#                     within game / half-inning / pitcher / pitch_category
#     velo_dev_pa = mean velo_dev_roll over the PA's pitches
#
#     velo_dev_ingame_pa = within-game deviation from early-outing category baseline.
#                          Fastball baseline: first INGAME_BASELINE_PITCHES pitches.
#                          Breaking/Offspeed: first INGAME_BASELINE_NONFAST pitches.
#                          Falls back to season average when outing count < threshold.
#                          Covers all pitch types — no fastball-only restriction.
#
#   Invalidate when: velo window/baseline changes, pull flag logic changes,
#                    pure-starter thresholds change, or upstream .rds changes
# ══════════════════════════════════════════════════════════════════════════════

INGAME_CURVE_K          <- 20L  # GAM basis dimension for the in-game warm-up curve
VELO_ROLL_WINDOW        <- 14   # pitches lookback for rolling velocity depreciation
VELO_BASELINE_MIN_N     <- 25   # min season pitches in a category to trust the baseline
INGAME_BASELINE_PITCHES <- 30L  # first N Fastball pitches defining within-game peak
INGAME_BASELINE_NONFAST <- 15L  # first N Breaking/Offspeed pitches for within-game baseline
PURE_STARTER_MIN_GS     <- 20
PURE_STARTER_MIN_INN    <- 4

build_starter_pa_full <- function(clean_data, starter_pa, reliever_rolling, prior_n_sp) {
  message("\n── Stage 4: starter_pa_full ──")
  
  # Pure-starter filter
  pitcher_role <- clean_data |>
    group_by(season, pitcher_id) |>
    summarise(
      n_starts = n_distinct(game_id[is_starter]),
      n_relief = n_distinct(game_id[!is_starter]),
      .groups  = "drop"
    )
  
  starter_outing_len <- starter_pa |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    summarise(max_inning = max(inning), .groups = "drop") |>
    group_by(season, pitcher_id) |>
    summarise(med_max_inning = median(max_inning), .groups = "drop")
  
  pure_starters <- pitcher_role |>
    left_join(starter_outing_len, by = c("season", "pitcher_id")) |>
    filter(n_starts >= PURE_STARTER_MIN_GS,
           n_relief == 0,
           med_max_inning >= PURE_STARTER_MIN_INN) |>
    select(season, pitcher_id)
  
  starter_pa <- starter_pa |>
    semi_join(pure_starters, by = c("season", "pitcher_id"))
  cat("  Pure-starter filter:", nrow(pure_starters), "pitcher-seasons,",
      nrow(starter_pa), "PA rows\n")
  
  # Pull flags
  starter_pa <- starter_pa |>
    group_by(season, game_id, inning_topbot) |>
    arrange(at_bat_number) |>
    mutate(
      last_pa       = at_bat_number == max(at_bat_number),
      complete_game = max(inning) >= 9 & last_pa,
      pulled        = last_pa & !complete_game
    ) |>
    ungroup()

  # PA sequence number within each outing (1 = first PA faced in this start)
  starter_pa <- starter_pa |>
    arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number) |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    mutate(pa_seq_ingame = as.integer(row_number())) |>
    ungroup()

  # ── In-game warm-up curve: E[xwOBA | PA position k] ──────────────────────
  curve_path <- file.path(DATA_DIR, "ingame_curve.rds")

  if (file.exists(curve_path)) {
    message("  [cache] ingame_curve.rds")
    ingame_curve <- readRDS(curve_path)
  } else {
    message("  [build] ingame_curve.rds (GAM warm-up curve)")
    gam_data <- starter_pa |> filter(!is.na(xwoba_outcome))
    gam_fit  <- mgcv::gam(
      xwoba_outcome ~ s(pa_seq_ingame, bs = "tp", k = INGAME_CURVE_K),
      data   = gam_data,
      family = gaussian()
    )
    max_pa       <- max(starter_pa$pa_seq_ingame, na.rm = TRUE)
    mu_curve_vec <- as.numeric(predict(
      gam_fit,
      newdata = data.frame(pa_seq_ingame = seq_len(max_pa))
    ))
    names(mu_curve_vec) <- as.character(seq_len(max_pa))
    mu_mean      <- mean(mu_curve_vec)
    ingame_curve <- list(gam_fit      = gam_fit,
                         mu_curve_vec = mu_curve_vec,
                         mu_mean      = mu_mean,
                         max_pa       = max_pa)
    saveRDS(ingame_curve, curve_path)
    cat("  Saved ingame_curve.rds (max_pa =", max_pa,
        ", mu_mean =", round(mu_mean, 4), ")\n")
  }

  # ── Season posterior weight for each outing ───────────────────────────────
  # prior_n_sp + number of PA outcomes this pitcher had before this outing
  cum_pa_before_game <- starter_pa |>
    filter(!is.na(xwoba_outcome)) |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    summarise(pa_in_outing = n(), .groups = "drop") |>
    arrange(season, pitcher_id, game_id) |>
    group_by(season, pitcher_id) |>
    mutate(cum_pa_before_game = lag(cumsum(pa_in_outing), default = 0L)) |>
    ungroup() |>
    select(season, game_id, inning_topbot, pitcher_id, cum_pa_before_game)

  starter_pa <- starter_pa |>
    left_join(cum_pa_before_game,
              by = c("season", "game_id", "inning_topbot", "pitcher_id")) |>
    mutate(
      cum_pa_before_game = replace_na(cum_pa_before_game, 0L),
      season_post_weight = prior_n_sp + cum_pa_before_game
    )

  # ── Bayesian in-game outing deviation ─────────────────────────────────────
  # curve_adj_resid(j) = xwoba(j) - pitcher_xwoba_est(j) - (μ(j) - μ̄)
  # ingame_deviation(k) = Σ curve_adj_resid(1..k-1) / (season_post_weight + n_obs(1..k-1))
  # At k = 1: ingame_deviation = 0 (no in-game evidence yet)
  #
  # ingame_woba_deviation mirrors the above using actual woba_value (woba_denom==1)
  # instead of xwoba_outcome, reusing the same TTO GAM curve as the baseline.
  # This captures what managers observe (actual hit outcomes) rather than contact
  # quality, and is used as a robustness-check predictor in the manager model.

  mu_curve_df <- tibble(
    pa_seq_ingame = seq_len(ingame_curve$max_pa),
    mu_k          = ingame_curve$mu_curve_vec
  )
  mu_mean_val <- ingame_curve$mu_mean

  starter_pa <- starter_pa |>
    left_join(mu_curve_df, by = "pa_seq_ingame") |>
    mutate(
      mu_k                 = if_else(is.na(mu_k), mu_mean_val, mu_k),
      curve_adj_resid      = if_else(
        !is.na(xwoba_outcome),
        xwoba_outcome - pitcher_xwoba_est - (mu_k - mu_mean_val),
        NA_real_
      ),
      woba_curve_adj_resid = if_else(
        !is.na(woba_value) & woba_denom == 1L,
        woba_value - pitcher_xwoba_est - (mu_k - mu_mean_val),
        NA_real_
      )
    ) |>
    arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number) |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    mutate(
      cum_resid             = lag(cumsum(replace_na(curve_adj_resid, 0)),           default = 0),
      cum_obs               = lag(cumsum(!is.na(xwoba_outcome)),                    default = 0L),
      ingame_deviation      = cum_resid / (season_post_weight + cum_obs),
      cum_woba_resid        = lag(cumsum(replace_na(woba_curve_adj_resid, 0)),      default = 0),
      cum_woba_obs          = lag(cumsum(!is.na(woba_value) & woba_denom == 1L),    default = 0L),
      ingame_woba_deviation = cum_woba_resid / (season_post_weight + cum_woba_obs)
    ) |>
    ungroup() |>
    select(-cum_resid, -cum_obs, -mu_k, -curve_adj_resid,
           -woba_curve_adj_resid, -cum_woba_resid, -cum_woba_obs,
           -cum_pa_before_game)

  cat("  ingame_deviation coverage:",
      round(mean(!is.na(starter_pa$ingame_deviation)), 3), "\n")
  cat("  ingame_deviation mean:",
      round(mean(starter_pa$ingame_deviation, na.rm = TRUE), 4),
      " sd:", round(sd(starter_pa$ingame_deviation, na.rm = TRUE), 4), "\n")
  cat("  ingame_deviation at PA 1 (must be 0):",
      round(mean(starter_pa$ingame_deviation[starter_pa$pa_seq_ingame == 1L],
                 na.rm = TRUE), 6), "\n")
  cat("  ingame_woba_deviation coverage:",
      round(mean(!is.na(starter_pa$ingame_woba_deviation)), 3), "\n")
  cat("  ingame_woba_deviation mean:",
      round(mean(starter_pa$ingame_woba_deviation, na.rm = TRUE), 4),
      " sd:", round(sd(starter_pa$ingame_woba_deviation, na.rm = TRUE), 4), "\n")

  # ── Upcoming-batter quality: mean xwOBA est of next 3 hitters faced ────────
  # Lookahead over the starter's own batter sequence (same half of the game),
  # so it never crosses to the opponent's half. Spans inning boundaries within
  # the outing. For a strict per-inning reset, add `inning` to the group_by.
  
  starter_pa <- starter_pa |>
    arrange(season, game_id, inning_topbot, at_bat_number) |>
    group_by(season, game_id, inning_topbot) |>
    mutate(
      next3_batter_xwoba = rowMeans(
        cbind(lead(batter_xwoba_est, 1),
              lead(batter_xwoba_est, 2),
              lead(batter_xwoba_est, 3)),
        na.rm = TRUE
      )
    ) |>
    ungroup() |>
    mutate(next3_batter_xwoba = if_else(is.nan(next3_batter_xwoba),
                                        NA_real_, next3_batter_xwoba))
  
  cat("  next3_batter_xwoba coverage:", round(mean(!is.na(starter_pa$next3_batter_xwoba)), 3), "\n")
  
  # Reliever estimates onto clean_data (needed for team reliever quality)
  clean_data <- clean_data |>
    left_join(
      reliever_rolling |> select(season, game_id, inning_topbot, at_bat_number,
                                 pitcher_id, reliever_xwoba_est),
      by = c("season", "game_id", "inning_topbot", "at_bat_number", "pitcher_id")
    ) |>
    clean_xy()
  
  # ── Velocity depreciation signal ────────────────────────────────────────
  
  # Per-pitcher-season baseline velocity by pitch category (over their starts).
  # This is the "whole baseline" — no inning-1 peak assumption.
  velo_baseline <- clean_data |>
    filter(is_starter == TRUE, !is.na(release_speed)) |>
    group_by(season, pitcher_id, pitch_category) |>
    summarise(
      velo_baseline = mean(release_speed, na.rm = TRUE),
      n_baseline    = n(),
      .groups = "drop"
    ) |>
    filter(n_baseline >= VELO_BASELINE_MIN_N)
  
  rolling_velo_data <- clean_data |>
    filter(is_starter == TRUE, !is.na(release_speed)) |>
    left_join(velo_baseline,
              by = c("season", "pitcher_id", "pitch_category")) |>
    arrange(season, game_id, inning_topbot, pitcher_id, pitch_number) |>
    group_by(season, game_id, inning_topbot, pitcher_id, pitch_category) |>
    mutate(
      velo_dev      = release_speed - velo_baseline,
      velo_dev_roll = slide_dbl(velo_dev, mean, .before = VELO_ROLL_WINDOW, .complete = FALSE)
    ) |>
    ungroup()
  
  velo_pa <- rolling_velo_data |>
    group_by(season, game_id, inning_topbot, at_bat_number) |>
    summarise(
      velo_dev_pa  = mean(velo_dev_roll, na.rm = TRUE),
      velo_pa      = mean(release_speed, na.rm = TRUE),
      n_pitches_pa = n(),
      .groups = "drop"
    )
  
  starter_pa <- starter_pa |>
    left_join(velo_pa, by = c("season", "game_id", "inning_topbot", "at_bat_number")) |>
    clean_xy()

  # Within-game velocity baseline, per pitch category.
  # Pitch-mix confounding is handled by normalising within category rather than
  # restricting to fastballs, so all PAs contribute a signal regardless of which
  # pitch types the pitcher threw.
  #
  # Separate thresholds by category: Breaking and Offspeed reach their pitch count
  # earlier in relative terms (15 pitches at ~27% frequency ≈ PA 7-9), so a lower
  # threshold keeps the baseline window early without requiring deep outings.
  # Season average for that category is the fallback when the outing count is below
  # threshold — this ensures full PA coverage at the cost of a slightly noisier
  # signal for rare pitch-type / short-start combinations.

  starter_pitches <- clean_data |>
    filter(is_starter == TRUE, !is.na(release_speed), !is.na(pitch_category))

  early_outing_cat_velo <- bind_rows(
    starter_pitches |>
      filter(pitch_category == "Fastball") |>
      arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number, pitch_number) |>
      group_by(season, game_id, inning_topbot, pitcher_id, pitch_category) |>
      mutate(cat_pitch_seq = row_number()) |>
      filter(cat_pitch_seq <= INGAME_BASELINE_PITCHES) |>
      summarise(ingame_cat_baseline = mean(release_speed, na.rm = TRUE), .groups = "drop"),
    starter_pitches |>
      filter(pitch_category %in% c("Breaking", "Offspeed")) |>
      arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number, pitch_number) |>
      group_by(season, game_id, inning_topbot, pitcher_id, pitch_category) |>
      mutate(cat_pitch_seq = row_number()) |>
      filter(cat_pitch_seq <= INGAME_BASELINE_NONFAST) |>
      summarise(ingame_cat_baseline = mean(release_speed, na.rm = TRUE), .groups = "drop")
  )

  ingame_velo_pa <- starter_pitches |>
    left_join(early_outing_cat_velo,
              by = c("season", "game_id", "inning_topbot", "pitcher_id", "pitch_category")) |>
    left_join(velo_baseline,
              by = c("season", "pitcher_id", "pitch_category")) |>
    mutate(pitch_dev_ingame = release_speed - coalesce(ingame_cat_baseline, velo_baseline)) |>
    group_by(season, game_id, inning_topbot, at_bat_number) |>
    summarise(velo_dev_ingame_pa = mean(pitch_dev_ingame, na.rm = TRUE), .groups = "drop")

  starter_pa <- starter_pa |>
    left_join(ingame_velo_pa, by = c("season", "game_id", "inning_topbot", "at_bat_number")) |>
    clean_xy()

  cat("  velo_dev_pa coverage:", round(mean(!is.na(starter_pa$velo_dev_pa)), 3), "\n")
  cat("  mean velo_dev_pa:", round(mean(starter_pa$velo_dev_pa, na.rm = TRUE), 3),
      " sd:", round(sd(starter_pa$velo_dev_pa, na.rm = TRUE), 3), "\n")
  cat("  velo_dev_ingame_pa coverage (all categories):",
      round(mean(!is.na(starter_pa$velo_dev_ingame_pa)), 3), "\n")
  cat("  mean velo_dev_ingame_pa:", round(mean(starter_pa$velo_dev_ingame_pa, na.rm = TRUE), 3),
      " sd:", round(sd(starter_pa$velo_dev_ingame_pa, na.rm = TRUE), 3), "\n")
  
  # In-game performance signal
  starter_pa <- starter_pa |>
    arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number) |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    mutate(
      cum_xwoba_game    = lag(cumsum(replace_na(xwoba_outcome, 0)), default = 0),
      cum_pa_game       = lag(cumsum(!is.na(xwoba_outcome)), default = 0),
      avg_xwoba_game    = if_else(cum_pa_game > 0, cum_xwoba_game / cum_pa_game, NA_real_),
      recent_xwoba_game = lag(cumsum(replace_na(xwoba_outcome, 0)), default = 0) -
        lag(cumsum(replace_na(xwoba_outcome, 0)), n = 4, default = 0)
    ) |>
    ungroup()
  
  # Team reliever quality
  team_lookup <- clean_data |>
    filter(is_starter == FALSE) |>
    distinct(season, game_id, pitcher_id, inning_topbot, home_team, away_team) |>
    mutate(team = if_else(inning_topbot == "Bot", away_team, home_team))
  
  team_reliever_avg <- clean_data |>
    filter(is_starter == FALSE, !is.na(reliever_xwoba_est)) |>
    left_join(team_lookup, by = c("season", "game_id", "pitcher_id", "inning_topbot")) |>
    clean_xy() |>
    group_by(season, team) |>
    summarise(
      avg_reliever_xwoba = mean(reliever_xwoba_est, na.rm = TRUE),
      .groups = "drop"
    )
  
  starter_team <- clean_data |>
    filter(is_starter == TRUE) |>
    distinct(season, game_id, inning_topbot, pitcher_id, home_team, away_team) |>
    mutate(team = if_else(inning_topbot == "Bot", home_team, away_team)) |>
    select(season, game_id, inning_topbot, pitcher_id, team)
  
  starter_pa <- starter_pa |>
    left_join(starter_team, by = c("season", "game_id", "inning_topbot", "pitcher_id")) |>
    left_join(team_reliever_avg, by = c("season", "team")) |>
    clean_xy()
  
  cat("  Team reliever avg coverage:", round(mean(!is.na(starter_pa$avg_reliever_xwoba)), 3), "\n")
  starter_pa
}


# ══════════════════════════════════════════════════════════════════════════════
# Stage 4b: reliever_pa_curves
#   For each observed pull, pairs the pull PA with the first entering reliever's
#   rolling xwOBA estimate. A GAM with factor-smooth shrinks each team-season
#   curve toward the league-wide smooth via mgcv's bs="fs" penalty.
#   Output: pred_grid (team, season, pull_pa 1-40, rel_quality)
#           + the fitted GAM, saved together in reliever_pa_curves.rds
#   Invalidate when: starter_pa_full.rds or reliever_rolling.rds changes
# ══════════════════════════════════════════════════════════════════════════════

build_reliever_pa_curves <- function(starter_pa_full, reliever_rolling) {
  message("\n── Stage 4b: reliever_pa_curves (GAM factor-smooth) ──")

  pull_events <- starter_pa_full |>
    filter(pulled == TRUE) |>
    select(season, game_id, inning_topbot, team, pull_pa = pa_seq_ingame)

  first_rel <- reliever_rolling |>
    group_by(season, game_id, inning_topbot) |>
    slice_min(at_bat_number, n = 1L, with_ties = FALSE) |>
    ungroup() |>
    select(season, game_id, inning_topbot, reliever_xwoba_est)

  gam_data <- pull_events |>
    inner_join(first_rel, by = c("season", "game_id", "inning_topbot")) |>
    filter(!is.na(reliever_xwoba_est), !is.na(pull_pa), !is.na(team)) |>
    mutate(team_season = interaction(team, season, drop = TRUE))

  cat("  Training rows:", nrow(gam_data),
      "  Team-seasons:", nlevels(gam_data$team_season), "\n")

  rel_curve_gam <- mgcv::bam(
    reliever_xwoba_est ~ s(pull_pa, team_season, bs = "fs", k = 8),
    data   = gam_data,
    method = "fREML"
  )

  pred_grid <- crossing(
    distinct(gam_data, team, season),
    pull_pa = seq_len(40L)
  ) |>
    mutate(team_season = interaction(team, season, drop = TRUE))
  pred_grid$rel_quality <- as.numeric(predict(rel_curve_gam, newdata = pred_grid))
  pred_grid <- pred_grid |> select(team, season, pull_pa, rel_quality)

  saveRDS(list(curves = pred_grid, gam = rel_curve_gam),
          file.path(DATA_DIR, "reliever_pa_curves.rds"))
  cat("  Saved reliever_pa_curves.rds\n")
  pred_grid
}

# ══════════════════════════════════════════════════════════════════════════════
# Stage 5: full_data
#   Every pitch row in clean_data + engineered PA-level features joined on.
#   Starter features broadcast to all pitches in each starter PA; reliever PAs
#   carry reliever_xwoba_est. Non-starter PAs have NA starter features.
#   Invalidate when: any upstream .rds changes, or the feature set below changes
# ══════════════════════════════════════════════════════════════════════════════

build_full_data <- function(clean_data, starter_pa, reliever_rolling) {
  message("\n── Stage 5: full_data (all pitch rows + engineered features) ──")
  
  # Reliever estimate onto every reliever PA
  clean_data <- clean_data |>
    left_join(
      reliever_rolling |> select(season, game_id, inning_topbot, at_bat_number,
                                 pitcher_id, reliever_xwoba_est),
      by = c("season", "game_id", "inning_topbot", "at_bat_number", "pitcher_id")
    ) |>
    clean_xy()
  
  # Engineered PA-level features from the starter modeling table
  starter_feats <- starter_pa |>
    select(season, game_id, inning_topbot, at_bat_number,
           pitcher_xwoba_est, batter_xwoba_est, matchup_xwoba_est, h2h_cum_pa,
           next3_batter_xwoba,
           velo_dev_pa, velo_pa, n_pitches_pa,
           cum_pitch_count, n_thruorder,
           cum_xwoba_game, cum_pa_game, avg_xwoba_game, recent_xwoba_game,
           ingame_deviation, ingame_woba_deviation,
           avg_reliever_xwoba, pulled)
  
  full_data <- clean_data |>
    left_join(starter_feats,
              by = c("season", "game_id", "inning_topbot", "at_bat_number")) |>
    clean_xy()
  
  cat("  full_data rows:", nrow(full_data), " cols:", ncol(full_data), "\n")
  cat("  starter-feature coverage (pitcher_xwoba_est):",
      round(mean(!is.na(full_data$pitcher_xwoba_est)), 3), "\n")
  full_data
}
# ══════════════════════════════════════════════════════════════════════════════
# Workload features (lightweight — reads raw CSV columns only)
#   prev_pitches: pitch count in the pitcher's immediately prior start.
#   days_rest: calendar days to that start, capped at 8.
#   Runs independently of build_all(); saved to workload_features.rds.
#   Load and join in load_model_data.R before z-scoring.
# ══════════════════════════════════════════════════════════════════════════════

build_workload_features <- function() {
  wf_path <- file.path(DATA_DIR, "workload_features.rds")
  if (file.exists(wf_path)) {
    message("  [cache] workload_features.rds")
    return(invisible(readRDS(wf_path)))
  }
  message("  [build] workload_features.rds")

  raw <- data.table::fread(
    SAVANT_PATH,
    select = c("game_id", "game_date", "inning", "inning_topbot",
               "pitcher_id", "at_bat_number", "year")
  )

  starters_raw <- raw[inning == 1] |>
    group_by(year, game_id, inning_topbot) |>
    slice_min(at_bat_number, n = 1L, with_ties = FALSE) |>
    ungroup() |>
    select(year, game_id, inning_topbot, starter_id = pitcher_id) |>
    distinct()

  start_counts <- raw |>
    filter(year != 2020) |>
    inner_join(starters_raw, by = c("year", "game_id", "inning_topbot")) |>
    filter(pitcher_id == starter_id) |>
    group_by(year, game_id, inning_topbot, pitcher_id) |>
    summarise(
      n_pitches = n(),
      game_date = as.Date(first(game_date)),
      .groups   = "drop"
    )

  workload <- start_counts |>
    arrange(pitcher_id, game_date) |>
    group_by(pitcher_id) |>
    mutate(
      prev_pitches = lag(n_pitches),
      prev_date    = lag(game_date),
      days_rest    = as.integer(game_date - prev_date)
    ) |>
    ungroup() |>
    mutate(days_rest = pmin(days_rest, 8L)) |>
    select(season = year, game_id, inning_topbot, pitcher_id, prev_pitches, days_rest)

  med_prev <- as.integer(median(workload$prev_pitches, na.rm = TRUE))
  med_rest <- as.integer(median(workload$days_rest,    na.rm = TRUE))
  workload <- workload |>
    mutate(
      prev_pitches = coalesce(as.integer(prev_pitches), med_prev),
      days_rest    = coalesce(as.integer(days_rest),    med_rest)
    )

  saveRDS(workload, wf_path)
  cat("  prev_pitches coverage:", round(mean(!is.na(workload$prev_pitches)), 3),
      "  mean:", round(mean(workload$prev_pitches, na.rm = TRUE), 1),
      "  sd:",   round(sd(workload$prev_pitches,   na.rm = TRUE), 1), "\n")
  cat("  days_rest coverage:", round(mean(!is.na(workload$days_rest)), 3),
      "  mean:", round(mean(workload$days_rest, na.rm = TRUE), 1),
      "  sd:",   round(sd(workload$days_rest,   na.rm = TRUE), 1), "\n")
  invisible(workload)
}

# ══════════════════════════════════════════════════════════════════════════════
# Orchestrator
#   Calls each stage through checkpoint(). Delete a .rds file to re-run from
#   that stage forward.
# ══════════════════════════════════════════════════════════════════════════════

build_all <- function() {
  message("=== build_features.R ===")
  
  clean_data <- checkpoint(
    file.path(DATA_DIR, "clean_data.rds"),
    build_clean_data()
  )
  
  starter_pa_raw <- checkpoint(
    file.path(DATA_DIR, "starter_pa_raw.rds"),
    build_starter_pa_raw(clean_data)
  )
  
  est_path <- file.path(DATA_DIR, "starter_pa_est.rds")
  rel_path <- file.path(DATA_DIR, "reliever_rolling.rds")
  
  if (file.exists(est_path) && file.exists(rel_path) &&
      file.exists(file.path(DATA_DIR, "prior_n_sp.rds"))) {
    message("\n  [cache] starter_pa_est.rds + reliever_rolling.rds")
    starter_pa_est   <- readRDS(est_path)
    reliever_rolling <- readRDS(rel_path)
    prior_n_sp       <- readRDS(file.path(DATA_DIR, "prior_n_sp.rds"))
  } else {
    est_out          <- build_estimates(clean_data, starter_pa_raw)
    starter_pa_est   <- est_out$starter_pa
    reliever_rolling <- est_out$reliever_rolling
    prior_n_sp       <- est_out$prior_n_sp
    saveRDS(starter_pa_est, est_path)
    message("  Saved starter_pa_est.rds")
  }
  
  starter_pa_full <- checkpoint(
    file.path(DATA_DIR, "starter_pa_full.rds"),
    build_starter_pa_full(clean_data, starter_pa_est, reliever_rolling, prior_n_sp)
  )
  
  curves_path <- file.path(DATA_DIR, "reliever_pa_curves.rds")
  if (file.exists(curves_path)) {
    message("\n  [cache] reliever_pa_curves.rds")
    reliever_pa_curves <- readRDS(curves_path)$curves
  } else {
    reliever_pa_curves <- build_reliever_pa_curves(starter_pa_full, reliever_rolling)
  }

  full_data <- checkpoint(
    file.path(DATA_DIR, "full_data.rds"),
    build_full_data(clean_data, starter_pa_full, reliever_rolling)
  )

  message("\n=== All stages complete ===\n")
  list(
    clean_data         = clean_data,
    starter_pa_full    = starter_pa_full,    # one row per starter PA (model table)
    full_data          = full_data,          # all pitch rows + engineered features
    reliever_rolling   = reliever_rolling,
    reliever_pa_curves = reliever_pa_curves  # team-season PA-indexed reliever quality
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# Invalidation helper
#   invalidate("starter_pa_full") deletes that checkpoint and all downstream.
#   invalidate("all") starts from scratch.
# ══════════════════════════════════════════════════════════════════════════════

STAGE_ORDER <- c(
  "clean_data",
  "starter_pa_raw",
  "starter_pa_est",     # also clears reliever_rolling.rds
  "starter_pa_full",
  "full_data"
)

invalidate <- function(from_stage = "starter_pa_full") {
  idx <- match(from_stage, c(STAGE_ORDER, "all"))
  if (from_stage == "all") idx <- 1L
  
  stages_to_clear <- STAGE_ORDER[idx:length(STAGE_ORDER)]
  
  for (s in stages_to_clear) {
    p <- file.path(DATA_DIR, paste0(s, ".rds"))
    if (file.exists(p)) { file.remove(p); message("Removed: ", basename(p)) }
    if (s == "starter_pa_est") {
      r <- file.path(DATA_DIR, "reliever_rolling.rds")
      if (file.exists(r)) { file.remove(r); message("Removed: reliever_rolling.rds") }
      p2 <- file.path(DATA_DIR, "prior_n_sp.rds")
      if (file.exists(p2)) { file.remove(p2); message("Removed: prior_n_sp.rds") }
    }
    if (s == "starter_pa_full") {
      m <- file.path(DATA_DIR, "model_data.rds")
      if (file.exists(m)) { file.remove(m); message("Removed: model_data.rds") }
      ic <- file.path(DATA_DIR, "ingame_curve.rds")
      if (file.exists(ic)) { file.remove(ic); message("Removed: ingame_curve.rds") }
      rc <- file.path(DATA_DIR, "reliever_pa_curves.rds")
      if (file.exists(rc)) { file.remove(rc); message("Removed: reliever_pa_curves.rds") }
    }
  }
  invisible(NULL)
}