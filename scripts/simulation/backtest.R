setwd("/Users/theoauyeung/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490")

library(tidyverse)
library(ggthemes)

source("scripts/pipeline/build_features.R")
source("scripts/simulation/simulate_pull.R")

TRAIN  <- 2015:2021
TEST   <- 2022:2025
PLOTS_DIR <- file.path(DATA_DIR, "../plots")

cat("Backtest — train:", paste(range(TRAIN), collapse = "–"),
    "| test:", paste(range(TEST), collapse = "–"), "\n")

out_bt <- simulate_pull(
  N_SIM         = 500,
  min_surplus   = 0.005,
  train_seasons = TRAIN,
  test_seasons  = TEST
)

sim      <- out_bt$sim
meta     <- out_bt$outing_meta
eligible <- meta |> filter(!starter_worse_than_repl, actual_pull_pa >= 2)

cat("\nTest-set eligible outings:", nrow(eligible), "\n")

# ── Block 1: Policy surplus at the sequential rule PA ─────────────────────────
# optimal_pull_pa is the first PA where pred_smooth > threshold (causal rule —
# uses only information available at PA k). This is the deployable policy.
# pull_surplus at that PA = expected runs saved vs. keeping starter through T.

policy_evals <- sim |>
  inner_join(
    eligible |>
      filter(!is.na(optimal_pull_pa), optimal_pull_pa < actual_pull_pa) |>
      select(season, game_id, inning_topbot, pitcher_id, optimal_pull_pa),
    by = c("season", "game_id", "inning_topbot", "pitcher_id")
  ) |>
  filter(pull_pa == optimal_pull_pa)

cat("\n=== Block 1: Sequential-rule policy surplus (test set) ===\n")
cat("Outings where rule fires before actual pull:", nrow(policy_evals),
    sprintf("(%.1f%% of eligible)\n", 100 * nrow(policy_evals) / nrow(eligible)))
policy_evals |>
  summarise(
    mean_surplus   = round(mean(pull_surplus),   5),
    median_surplus = round(median(pull_surplus), 5),
    pct_positive   = round(mean(pull_surplus > 0), 3),
    total_surplus  = round(sum(pull_surplus), 2)
  ) |>
  print()

# ── Block 2: Late-pull rate OOS ───────────────────────────────────────────────
cat("\n=== Block 2: Late-pull diagnostics (test set) ===\n")
cat("pct_late (weighted):       ", round(mean(eligible$weighted_late_pull, na.rm = TRUE), 3), "\n")
cat("mean_pa_delta (late only): ", round(
  mean(eligible$weighted_pa_delta[eligible$weighted_late_pull], na.rm = TRUE), 2), "\n")
cat("pct_no_crossover:          ", round(mean(is.na(eligible$weighted_optimal_pull_pa)), 3), "\n")

# ── Block 3: Season-by-season breakdown ───────────────────────────────────────
cat("\n=== Block 3: Season-by-season (test set) ===\n")
season_tbl <- eligible |>
  group_by(season) |>
  summarise(
    n_outings     = n(),
    pct_late      = round(mean(weighted_late_pull, na.rm = TRUE), 3),
    mean_pa_delta = round(mean(weighted_pa_delta[weighted_late_pull], na.rm = TRUE), 2),
    pct_crossover = round(mean(!is.na(weighted_optimal_pull_pa)), 3),
    mean_opt_pa   = round(mean(weighted_optimal_pull_pa, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(season_tbl)

# ── Block 4: Surplus profile shape on test data ───────────────────────────────
cat("\n=== Block 4: Mean surplus by PA (test set, PA 15-30) ===\n")
surplus_profile <- sim |>
  filter(pull_pa >= 15, pull_pa <= 30) |>
  group_by(pull_pa) |>
  summarise(
    mean_surplus = round(mean(pull_surplus), 5),
    pct_positive = round(mean(pull_surplus > 0), 3),
    .groups = "drop"
  )
print(surplus_profile, n = 16)

crossover_pa <- surplus_profile |> filter(mean_surplus > 0) |> slice_min(pull_pa, n = 1)
cat("\nFirst PA with positive mean surplus (test):", crossover_pa$pull_pa, "\n")

# ── Plots ──────────────────────────────────────────────────────────────────────

p_surplus <- policy_evals |>
  ggplot(aes(x = pull_surplus)) +
  geom_histogram(binwidth = 0.02, fill = "#2166ac", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#b2182b", linewidth = 0.8) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "Policy Surplus at Sequential Rule PA (Test Set: 2022-2025)",
    subtitle = "Pull surplus at first PA where pred_smooth > reliever threshold; positive = reliever preferred",
    x        = "Pull surplus (xwOBA linear weights)",
    y        = "Count"
  )
print(p_surplus)
ggsave("backtest_policy_surplus.png", p_surplus, path = PLOTS_DIR, width = 9, height = 5)

p_season <- season_tbl |>
  select(season, pct_late, pct_crossover) |>
  pivot_longer(-season, names_to = "metric", values_to = "value") |>
  ggplot(aes(x = factor(season), y = value, fill = metric)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 2)), position = position_dodge(0.9),
            vjust = -0.4, size = 3.5) +
  scale_fill_brewer(palette = "Set1",
                    labels = c(pct_crossover = "% with crossover",
                               pct_late       = "% late pulls")) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "Late-Pull Rate by Season (Test Set: 2022-2025)",
    subtitle = "If stable year-to-year, the finding is not driven by a single season",
    x = NULL, y = "Rate", fill = NULL
  )
print(p_season)
ggsave("backtest_by_season.png", p_season, path = PLOTS_DIR, width = 9, height = 5)

cat("\nBacktest complete.\n")
