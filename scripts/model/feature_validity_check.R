library(tidyverse)
library(fixest)
library(slider)

source("scripts/pipeline/build_features.R")
source("scripts/pipeline/load_model_data.R")

DATA_DIR <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"

model_data <- load_model_data()
sim_out    <- readRDS(file.path(DATA_DIR, "simulate_pull_out.rds"))

fatigue_data <- model_data |>
  filter(!is.na(xwoba_outcome), !is.na(velo_dev_pa), !is.na(velo_dev_ingame_pa), !is.na(ingame_dev_z))

fatigue_model <- feols(
  xwoba_outcome ~ velo_dev_z + velo_dev_ingame_z + tto_fac + batter_xwoba_z + ingame_dev_z | pitcher_id,
  data = fatigue_data, cluster = ~pitcher_id
)

batter_coef <- coef(fatigue_model)["batter_xwoba_z"]
coefs       <- coef(fatigue_model)

# ── Diagnostic 1: Practical xwOBA swing per predictor ─────────────────────────
# Coefficient × (P10-to-P90 within-outing range) = the realistic xwOBA shift
# each feature can produce inside a single game.  If all swings < 0.003,
# the features are barely moving the prediction; threshold crossovers are
# driven by pitcher FE vs. reliever baseline alone.
cat("\n=== D1: Practical coefficient impact (xwOBA swing per predictor) ===\n")

within_ranges <- fatigue_data |>
  group_by(season, game_id, inning_topbot, pitcher_id) |>
  summarise(
    r_velo_dev    = quantile(velo_dev_z,        0.9, na.rm=TRUE) - quantile(velo_dev_z,        0.1, na.rm=TRUE),
    r_velo_ingame = quantile(velo_dev_ingame_z, 0.9, na.rm=TRUE) - quantile(velo_dev_ingame_z, 0.1, na.rm=TRUE),
    r_ingame_dev  = quantile(ingame_dev_z,      0.9, na.rm=TRUE) - quantile(ingame_dev_z,      0.1, na.rm=TRUE),
    .groups = "drop"
  ) |>
  summarise(across(starts_with("r_"), median, na.rm = TRUE))

tibble(
  predictor           = c("velo_dev_z", "velo_dev_ingame_z", "ingame_dev_z",
                          "tto_fac2", "tto_fac3", "tto_fac4"),
  coefficient         = c(coefs["velo_dev_z"], coefs["velo_dev_ingame_z"], coefs["ingame_dev_z"],
                          coefs["tto_fac2"], coefs["tto_fac3"], coefs["tto_fac4"]),
  within_outing_range = c(within_ranges$r_velo_dev, within_ranges$r_velo_ingame,
                          within_ranges$r_ingame_dev, 1, 1, 1),
  xwoba_swing         = abs(coefficient) * within_outing_range
) |>
  mutate(across(where(is.numeric), \(x) round(x, 5))) |>
  print()

cat("\nContext: starter quality vs. reliever baseline gap is typically 0.010–0.020 xwOBA.\n")
cat("Swings > 0.005 meaningfully shift the prediction relative to the threshold.\n")
cat("TTO terms are step-changes at each time-through-order transition.\n")

# ── Diagnostic 2: Within-outing prediction spread ─────────────────────────────
# How much does starter_pred_neutral actually move within a single outing?
# Median range near zero → covariates not driving threshold crossovers.
cat("\n=== D2: Within-outing prediction spread ===\n")

pred_data <- fatigue_data |>
  mutate(
    starter_pred         = predict(fatigue_model, newdata = fatigue_data),
    starter_pred_neutral = starter_pred - batter_coef * batter_xwoba_z
  )

within_spread <- pred_data |>
  group_by(season, game_id, inning_topbot, pitcher_id) |>
  summarise(
    n_pa       = n(),
    pred_range = max(starter_pred_neutral) - min(starter_pred_neutral),
    pred_sd    = sd(starter_pred_neutral),
    drift      = last(starter_pred_neutral) - first(starter_pred_neutral),
    .groups    = "drop"
  )

cat("Within-outing starter_pred_neutral (xwOBA units):\n")
cat("  Median range (max-min):     ", round(median(within_spread$pred_range), 5), "\n")
cat("  P75 range:                  ", round(quantile(within_spread$pred_range, 0.75), 5), "\n")
cat("  Median SD:                  ", round(median(within_spread$pred_sd), 5), "\n")
cat("  Median drift (last-first):  ", round(median(within_spread$drift), 5), "\n")
cat("  P75 drift:                  ", round(quantile(within_spread$drift, 0.75), 5), "\n")
cat("\n  Verdict:\n")
cat("  - Median range < 0.003 → effectively intercept-only; features not active.\n")
cat("  - Median range > 0.010 → features are producing realistic within-game variation.\n")

# ── Diagnostic 3: Pull surplus ~ fatigue features ─────────────────────────────
# Does ingame_dev_z (worse performance) and velo_dev_ingame_z (velocity drop)
# predict that pulling is more beneficial at that PA?  Near-zero correlations
# mean the features are not influencing the simulation outcome.
cat("\n=== D3: Correlation between fatigue features and pull_surplus at each PA ===\n")

pa_features <- fatigue_data |>
  arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number) |>
  group_by(season, game_id, inning_topbot, pitcher_id) |>
  mutate(pa_seq = row_number()) |>
  ungroup() |>
  select(season, game_id, inning_topbot, pitcher_id, pa_seq,
         velo_dev_z, velo_dev_ingame_z, ingame_dev_z, n_thruorder)

sim_with_feat <- sim_out$sim |>
  left_join(pa_features,
            by = c("season", "game_id", "inning_topbot", "pitcher_id",
                   "pull_pa" = "pa_seq"))

sim_with_feat <- sim_out$sim |>
  left_join(pa_features,
            by = c("season", "game_id", "inning_topbot", "pitcher_id",
                   "pull_pa" = "pa_seq"))

sim_with_feat |>
  filter(!is.na(pull_surplus)) |>
  summarise(
    cor_velo_dev      = cor(velo_dev_z,        pull_surplus, use = "complete"),
    cor_velo_ingame   = cor(velo_dev_ingame_z, pull_surplus, use = "complete"),
    cor_ingame_dev    = cor(ingame_dev_z,       pull_surplus, use = "complete"),
    cor_tto           = cor(n_thruorder,        pull_surplus, use = "complete"),
    cor_n_window      = cor(n_window,            pull_surplus, use = "complete")
  ) |>
  mutate(across(everything(), \(x) round(x, 4))) |>
  print()

cat("\n  Expected signs if features are working:\n")
cat("  cor_velo_ingame > 0 : velocity drop at this PA → pulling becomes more beneficial\n")
cat("  cor_ingame_dev  > 0 : worse in-game performance → pulling more beneficial\n")
cat("  cor_tto         > 0 : later TTO → pulling more beneficial\n")
cat("  cor_velo_dev    > 0 : below-season-baseline velocity → pulling more beneficial\n")
cat("  Correlations < 0.02 indicate these features have negligible influence.\n")

# ── Diagnostic 4: FE-only ablation ────────────────────────────────────────────
# Reconstruct starter_pred_neutral zeroing all covariate coefficients.
# Compare late-pull rate against the full model.  If the difference is < 2pp,
# the pull decision is determined by pitcher identity alone.
cat("\n=== D4: FE-only ablation — late-pull rate without fatigue covariates ===\n")

fe_vals <- fixef(fatigue_model)$pitcher_id
pitcher_fe_tbl <- tibble(
  pitcher_id = as.integer(names(fe_vals)),
  pitcher_fe = fe_vals
)
grand_mean_xwoba <- mean(fatigue_data$xwoba_outcome, na.rm = TRUE)

pa_floor <- fatigue_data |>
  group_by(season, game_id, inning_topbot, pitcher_id) |>
  summarise(n_pa = n(), .groups = "drop") |>
  pull(n_pa) |>
  quantile(0.10, names = FALSE) |>
  ceiling() |>
  max(5L)

fe_data <- fatigue_data |>
  left_join(pitcher_fe_tbl, by = "pitcher_id") |>
  mutate(fe_only_neutral = grand_mean_xwoba + pitcher_fe) |>
  arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number) |>
  group_by(season, game_id, inning_topbot, pitcher_id) |>
  mutate(
    pa_seq    = row_number(),
    threshold = pmax(first(fe_only_neutral), first(avg_reliever_xwoba)),
    # FE-only: prediction is flat within outing, so above_opt = first_pred > threshold
    # AND pa_seq >= pa_floor.  Without within-outing drift, the crossover is binary:
    # either the pitcher's FE > reliever baseline from PA 1 (starter_worse_than_repl = TRUE)
    # or it never crosses.  This is the worst-case null.
    above_opt = fe_only_neutral > threshold & pa_seq >= pa_floor
  ) |>
  ungroup()

fe_meta <- fe_data |>
  group_by(season, game_id, inning_topbot, pitcher_id) |>
  summarise(
    starter_worse_than_repl = first(fe_only_neutral) > first(avg_reliever_xwoba),
    optimal_pa_fe     = if (any(above_opt)) min(pa_seq[above_opt]) else NA_integer_,
    actual_pull_pa    = max(pa_seq),
    .groups           = "drop"
  ) |>
  mutate(
    pa_delta_fe  = actual_pull_pa - coalesce(optimal_pa_fe, actual_pull_pa),
    late_pull_fe = !is.na(optimal_pa_fe) & pa_delta_fe > 0
  )

outing_meta     <- sim_out$outing_meta
eligible_full   <- outing_meta |> filter(!starter_worse_than_repl, actual_pull_pa >= 2)
eligible_fe     <- fe_meta     |> filter(!starter_worse_than_repl, actual_pull_pa >= 2)

pct_late_full <- mean(eligible_full$weighted_late_pull, na.rm = TRUE)
pct_late_fe   <- mean(eligible_fe$late_pull_fe, na.rm = TRUE)

cat("Full fatigue model late-pull rate:    ", round(pct_late_full, 3), "\n")
cat("FE-only ablation late-pull rate:      ", round(pct_late_fe, 3), "\n")
cat("Difference (full - FE-only):          ", round(pct_late_full - pct_late_fe, 3), "\n\n")
cat("  Note: In the FE-only model, predictions are flat within an outing.\n")
cat("  A crossover only happens if the pitcher's average quality eventually\n")
cat("  exceeds the reliever baseline — which can't happen here since FE is constant.\n")
cat("  So FE-only late-pull rate should be near 0 or driven entirely by the\n")
cat("  pa_floor constraint.  Any positive late-pull rate in the full model\n")
cat("  is generated by within-outing covariate drift.\n")

# ── Diagnostic 5: TTO mean-prediction step-check ──────────────────────────────
# The TTO dummies are the clearest fatigue test.  Mean starter_pred_neutral
# should increase monotonically from TTO1 → TTO2 → TTO3.
cat("\n=== D5: Mean prediction by TTO (confirms TTO terms shift prediction) ===\n")

pred_data |>
  group_by(n_thruorder) |>
  summarise(
    n         = n(),
    mean_pred = round(mean(starter_pred_neutral, na.rm = TRUE), 5),
    sd_pred   = round(sd(starter_pred_neutral, na.rm = TRUE), 5),
    .groups   = "drop"
  ) |>
  print()

cat("\nMonotonic increase from TTO1 → TTO3 = TTO terms are active in predictions.\n")
cat("Flat or non-monotonic = TTO coefficients are too small to matter.\n")
