setwd("/Users/theoauyeung/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490")
source("simulate_pull.R")

out  <- simulate_pull(N_BASE = 50000, N_SIM = 150)
saveRDS(out, file.path(DATA_DIR, "simulate_pull_out.rds"))
sim  <- out$sim
meta <- out$outing_meta
fm   <- out$fatigue_model
sm   <- out$bullpen_strain_model

divider <- function(label) cat("\n", strrep("─", 60), "\n", label, "\n", strrep("─", 60), "\n", sep = "")

# ── 1. Model parameters ───────────────────────────────────────────────────────
# Expected signs:
#   velo_dev_z        < 0  (slower than season avg = worse outcomes)
#   velo_dev_ingame_z < 0  (within-game signal: situational/pressure variation)
#   tto_fac2/3/4      > 0  (2nd/3rd/4th time through order = worse)
#   batter_xwoba_z    > 0  (tougher batter = higher xwOBA allowed)
#   recent_xwoba_z    > 0  (pitching worse recently = continue worse)
#   strain_rate       > 0  (more bullpen PAs = reliever quality degrades)

divider("1. FATIGUE MODEL COEFFICIENTS")
print(round(coef(fm), 5))

divider("2. BULLPEN STRAIN MODEL COEFFICIENTS")
print(round(coef(sm), 6))

divider("3. KEY SCALARS")
cat("sigma_outing (day-to-day SD, xwOBA):", round(out$sigma_outing, 4),
    " — expect 0.02 – 0.06\n")
cat("strain_rate  (xwOBA per extra pen PA):", round(out$strain_rate, 5),
    " — expect small positive\n")

# ── 2. Outing-level coverage ──────────────────────────────────────────────────

divider("4. OUTINGS IN SIM (should exclude starts_above_repl)")
# pct_late_pull uses weighted_late_pull: simulation + empirical pull-distribution
# informed optimal. pct_late_raw is the raw threshold-crossover diagnostic.
meta |>
  summarise(
    total_outings      = n(),
    n_starts_above     = sum(starts_above_repl),
    n_qualifying       = sum(!starts_above_repl & actual_pull_pa >= 2),
    pct_late_pull      = mean(weighted_late_pull, na.rm = TRUE),
    pct_late_raw       = mean(!is.na(optimal_pull_pa) &
                               actual_pull_pa > coalesce(optimal_pull_pa, actual_pull_pa),
                             na.rm = TRUE),
    pct_no_crossover   = mean(is.na(weighted_optimal_pull_pa))
  ) |>
  print()

divider("5. ACTUAL PULL PA DISTRIBUTION (starters, qualifying outings)")
meta |>
  filter(!starts_above_repl, actual_pull_pa >= 2) |>
  pull(actual_pull_pa) |>
  quantile(c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) |>
  round(1) |>
  print()

divider("6. OPTIMAL vs. ACTUAL PULL PA (outings with a weighted crossover)")
# weighted_optimal_pull_pa: PA with max pull_surplus * empirical-pull-weight.
# Anchored to the realistic pull window; compare against raw threshold crossover
# (optimal_pull_pa) to see how much the pull-weight prior shifts the estimate.
meta |>
  filter(!is.na(weighted_optimal_pull_pa)) |>
  summarise(
    n                        = n(),
    mean_weighted_optimal_pa = mean(weighted_optimal_pull_pa),
    mean_raw_optimal_pa      = mean(optimal_pull_pa, na.rm = TRUE),
    mean_actual_pa           = mean(actual_pull_pa),
    mean_pa_delta            = mean(weighted_pa_delta),
    pct_late                 = mean(weighted_late_pull)
  ) |>
  print()

# ── 3. Pull surplus distribution ──────────────────────────────────────────────

divider("7. PULL SURPLUS OVERALL DISTRIBUTION")
cat("Rows in sim:", nrow(sim), "\n")
sim |>
  summarise(
    mean_surplus      = mean(pull_surplus),
    median_surplus    = median(pull_surplus),
    sd_surplus        = sd(pull_surplus),
    pct_positive      = mean(pull_surplus > 0),
    pct_clearly_pos   = mean(pull_surplus > 0.05),
    mean_prob_better  = mean(prob_pull_better)
  ) |>
  mutate(across(everything(), \(x) round(x, 4))) |>
  print()

divider("8. MEAN PULL SURPLUS BY PA NUMBER")
# Negative early (keep the starter), should turn positive later.
# The crossover is the average implied optimal pull point.
surplus_by_pa <- sim |>
  group_by(pull_pa) |>
  summarise(
    n_obs         = n(),
    mean_surplus  = mean(pull_surplus),
    pct_positive  = mean(pull_surplus > 0),
    mean_prob_bet = mean(prob_pull_better),
    mean_adj_rel  = mean(adj_rel_xwoba),
    .groups = "drop"
  )

print(surplus_by_pa |> filter(pull_pa <= 25) |>
        mutate(across(where(is.double), \(x) round(x, 4))), n = 25)

crossover <- surplus_by_pa |>
  filter(mean_surplus > 0) |>
  slice_min(pull_pa, n = 1)
cat("\nFirst PA with positive mean surplus:", crossover$pull_pa,
    "(mean =", round(crossover$mean_surplus, 4), ")\n")

# ── 4. Strain effect sanity check ────────────────────────────────────────────

divider("9. STRAIN PENALTY BY EXTRA BULLPEN PAs")
# adj_rel_xwoba should rise as extra_bullpen_pa increases.
sim |>
  group_by(extra_bullpen_pa) |>
  summarise(
    n            = n(),
    mean_adj_rel = mean(adj_rel_xwoba),
    .groups = "drop"
  ) |>
  filter(extra_bullpen_pa <= 20) |>
  mutate(across(where(is.double), \(x) round(x, 4))) |>
  print(n = 21)

# ── 5. PA-1 sanity check ─────────────────────────────────────────────────────

divider("10. PULL SURPLUS AT PA 1 (should be strongly negative)")
# Pulling in the first PA always hurts — maximum strain, no fatigue signal yet.
sim |>
  filter(pull_pa == 1) |>
  summarise(
    n             = n(),
    mean_surplus  = mean(pull_surplus),
    pct_positive  = mean(pull_surplus > 0),
    mean_adj_rel  = mean(adj_rel_xwoba)
  ) |>
  mutate(across(where(is.double), \(x) round(x, 4))) |>
  print()

# ── 6. Late-outing sanity check ───────────────────────────────────────────────

divider("11. PULL SURPLUS AT PA 15+ (should trend positive)")
sim |>
  filter(pull_pa >= 15) |>
  group_by(pull_pa) |>
  summarise(
    n            = n(),
    mean_surplus = mean(pull_surplus),
    pct_positive = mean(pull_surplus > 0),
    .groups = "drop"
  ) |>
  mutate(across(where(is.double), \(x) round(x, 4))) |>
  print(n = 15)

# ── 7. Pitcher-level top 10 ───────────────────────────────────────────────────

divider("12. TOP 10 PITCHERS BY TOTAL POSITIVE PULL SURPLUS (>= 10 outings)")
sim |>
  filter(pull_surplus > 0) |>
  group_by(pitcher_id, pitcher_name) |>
  summarise(
    n_outings    = n_distinct(paste(season, game_id, inning_topbot)),
    total_surplus = sum(pull_surplus),
    mean_surplus  = mean(pull_surplus),
    .groups = "drop"
  ) |>
  filter(n_outings >= 10) |>
  slice_max(total_surplus, n = 10) |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  print()

# ── 8. Per-season aggregate ───────────────────────────────────────────────────

divider("13. MEAN PULL SURPLUS BY SEASON")
sim |>
  group_by(season) |>
  summarise(
    n_outings    = n_distinct(paste(game_id, inning_topbot, pitcher_id)),
    mean_surplus = mean(pull_surplus),
    pct_positive = mean(pull_surplus > 0),
    .groups = "drop"
  ) |>
  mutate(across(where(is.double), \(x) round(x, 4))) |>
  print()
