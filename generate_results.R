library(tidyverse)
library(fixest)
library(ggthemes)
library(readr)
library(gt)
library(slider)

source("load_model_data.R")

PLOTS_DIR <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/plots"
DATA_DIR   <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"
model_data <- load_model_data()
saveRDS(model_data, file.path(DATA_DIR, "model_data.rds"))

model_data <- readRDS(file.path(DATA_DIR, "model_data.rds"))
starter_pa <- readRDS(file.path(DATA_DIR, "starter_pa_full.rds"))
full_data  <- readRDS(file.path(DATA_DIR, "full_data.rds"))

reliever_rolling <- readRDS(file.path(DATA_DIR, "reliever_rolling.rds"))



# To rebuild from scratch (e.g. after changing build_features.R):
# source("build_features.R"); invalidate("starter_pa_full"); results <- build_all()
# starter_pa <- results$starter_pa_full; full_data <- results$full_data


#xwOBA heatmap: exit velocity × launch angle----

xwoba_heatmap <- full_data |>
  filter(
    !is.na(launch_speed), !is.na(launch_angle), !is.na(expected_woba),
    launch_angle >= -75, launch_angle <= 75
  ) |>
  ggplot(aes(x = launch_speed, y = launch_angle, z = expected_woba)) +
  stat_summary_2d(fun = mean, bins = 50) +
  scale_fill_gradient2(
    name   = "xwOBA",
    low    = "#2166ac",
    mid    = "white",
    high   = "#b2182b",
    midpoint = 1.0,
    limits = c(0, 2.0),
    breaks = c(0, 0.5, 1.0, 1.5, 2.0)
  ) +
  theme_fivethirtyeight(base_size = 13) + 
  theme(
    axis.title = element_text(),
    axis.text = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 12, face = "bold")) + 
  labs(
    title = "xwOBA by Exit Velocity and Launch Angle",
    x = "Exit Velocity (mph)",
    y = "Launch Angle (°)"
  ) 
 

print(xwoba_heatmap)
ggsave(
  filename = "xwoba_heatmap.png",
  plot = xwoba_heatmap,
  path = "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/plots"
)


# Linear Weights Table 
lw_raw <- read_csv(file.path(DATA_DIR, "linear_weight.csv"), show_col_types = FALSE) 


library(gt)
library(dplyr)

lw_table <- lw_raw %>%
  select(event_name, LW) %>%
  arrange(desc(LW)) %>%
  gt() %>%
  data_color(
    columns = LW,
    palette = c("#d73027", "#ffffbf", "#1a9850")
  ) %>%
  fmt_number(
    columns = LW,
    decimals = 3
  )

lw_table

# Fatigue Model Coefficient Table ----

fatigue_data <- model_data |>
  filter(!is.na(xwoba_outcome), !is.na(velo_dev_pa), !is.na(velo_dev_ingame_pa), !is.na(recent_xwoba_z))

fatigue_fit <- feols(
  xwoba_outcome ~ velo_dev_z + velo_dev_ingame_z + tto_fac +
    batter_xwoba_z + recent_xwoba_z | pitcher_id,
  data = fatigue_data, cluster = ~pitcher_id
)

coef_tbl <- coeftable(fatigue_fit) |>
  as.data.frame() |>
  rownames_to_column("term") |>
  rename(estimate = Estimate, std_err = `Std. Error`,
         t_stat = `t value`, p_value = `Pr(>|t|)`) |>
  mutate(
    group = case_when(
      term %in% c("velo_dev_z", "velo_dev_ingame_z", "tto_fac2", "tto_fac3") ~ "Fatigue / TTO",
      TRUE ~ "Controls"
    ),
    term = recode(term,
      "velo_dev_z"        = "Velocity Deviation, Season Baseline (z-score)",
      "velo_dev_ingame_z" = "Velocity Deviation, Within-Game Baseline (z-score)",
      "tto_fac2"          = "2nd Time Through Order",
      "tto_fac3"          = "3rd+ Time Through Order",
      "batter_xwoba_z"    = "Batter Quality (z-score)",
      "recent_xwoba_z"    = "Recent In-Game Performance (z-score)"
    ),
    stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ "†",
      TRUE            ~ ""
    )
  )

max_abs_est <- max(abs(coef_tbl$estimate))

fatigue_gt <- coef_tbl |>
  select(group, term, estimate, std_err, t_stat, p_value, stars) |>
  gt(groupname_col = "group") |>
  tab_header(
    title    = md("**Fatigue Model: Coefficient Estimates**"),
    subtitle = md("Outcome: xwOBA per PA &nbsp;|&nbsp; Pitcher fixed effects &nbsp;|&nbsp; SEs clustered by pitcher")
  ) |>
  cols_label(
    term     = "Variable",
    estimate = "Coeff.",
    std_err  = "Std. Err.",
    t_stat   = md("*t*"),
    p_value  = md("*p*"),
    stars    = ""
  ) |>
  fmt_number(columns = c(estimate, std_err, t_stat), decimals = 4) |>
  fmt_number(columns = p_value, decimals = 3) |>
  data_color(
    columns = estimate,
    palette = c("#1a9850", "#f7f7f7", "#d73027"),
    domain  = c(-max_abs_est, max_abs_est)
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.05)
  ) |>
  tab_source_note(
    md("Significance: † *p* < 0.1 &nbsp;|&nbsp; \\* *p* < 0.05 &nbsp;|&nbsp; \\*\\* *p* < 0.01 &nbsp;|&nbsp; \\*\\*\\* *p* < 0.001")
  ) |>
  tab_source_note(
    paste0("N = ", formatC(nobs(fatigue_fit), format = "d", big.mark = ","), " PA")
  ) |>
  tab_options(
    row_group.font.weight = "bold",
    heading.align         = "left",
    table.font.size       = 13,
    column_labels.font.weight = "bold"
  )

print(fatigue_gt)

# Bullpen Strain Model Coeffs ----
reliever_strain_data <- reliever_rolling |>
  filter(!is.na(xwoba_outcome), !is.na(reliever_xwoba_est)) |>
  arrange(season, game_id, inning_topbot, at_bat_number) |>
  group_by(season, game_id, inning_topbot) |>
  mutate(cum_bullpen_pa = row_number()) |>
  ungroup()

bullpen_strain_model <- feols(
  xwoba_outcome ~ cum_bullpen_pa + reliever_xwoba_est | season,
  data   = reliever_strain_data,
  cluster = ~pitcher_id
)


strain_coef_tbl <- coeftable(bullpen_strain_model) |>
  as.data.frame() |>
  rownames_to_column("term") |>
  rename(estimate = Estimate, std_err = `Std. Error`,
         t_stat = `t value`, p_value = `Pr(>|t|)`) |>
  mutate(
    term = recode(term,
      "cum_bullpen_pa"     = "Cumulative Bullpen PAs",
      "reliever_xwoba_est" = "Reliever Quality (rolling xwOBA estimate)"
    ),
    stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ "†",
      TRUE            ~ ""
    )
  )

max_abs_strain <- max(abs(strain_coef_tbl$estimate))

strain_gt <- strain_coef_tbl |>
  select(term, estimate, std_err, t_stat, p_value, stars) |>
  gt() |>
  tab_header(
    title    = md("**Bullpen Strain Model: Coefficient Estimates**"),
    subtitle = md("Outcome: xwOBA per PA &nbsp;|&nbsp; Season fixed effects &nbsp;|&nbsp; SEs clustered by pitcher")
  ) |>
  cols_label(
    term     = "Variable",
    estimate = "Coeff.",
    std_err  = "Std. Err.",
    t_stat   = md("*t*"),
    p_value  = md("*p*"),
    stars    = ""
  ) |>
  fmt_number(columns = c(estimate, std_err, t_stat), decimals = 5) |>
  fmt_number(columns = p_value, decimals = 3) |>
  data_color(
    columns = estimate,
    palette = c("#1a9850", "#f7f7f7", "#d73027"),
    domain  = c(-max_abs_strain, max_abs_strain)
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = p_value < 0.05)
  ) |>
  tab_source_note(
    md("Significance: † *p* < 0.1 &nbsp;|&nbsp; \\* *p* < 0.05 &nbsp;|&nbsp; \\*\\* *p* < 0.01 &nbsp;|&nbsp; \\*\\*\\* *p* < 0.001")
  ) |>
  tab_source_note(
    paste0("N = ", formatC(nobs(bullpen_strain_model), format = "d", big.mark = ","), " PA")
  ) |>
  tab_options(
    heading.align             = "left",
    table.font.size           = 13,
    column_labels.font.weight = "bold"
  )

print(strain_gt)

# xwOBA Posterior Distributions ----

# Season-end posterior: last estimate per pitcher-season
season_posteriors <- starter_pa |>
  filter(!is.na(pitcher_xwoba_est)) |>
  group_by(season, pitcher_id, pitcher_name) |>
  slice_tail(n = 1) |>
  ungroup()

# Raw season xwOBA
season_raw <- starter_pa |>
  filter(!is.na(xwoba_outcome)) |>
  group_by(season, pitcher_id, pitcher_name) |>
  summarise(season_xwoba = mean(xwoba_outcome, na.rm = TRUE), pa = n(), .groups = "drop")

season_compare <- season_posteriors |>
  left_join(season_raw, by = c("season", "pitcher_id", "pitcher_name")) |>
  filter(pa >= 50)

# Density: posterior vs. raw — shows shrinkage narrowing the distribution
p_dist <- season_compare |>
  select(pitcher_xwoba_est, season_xwoba) |>
  pivot_longer(everything(), names_to = "type", values_to = "xwoba") |>
  mutate(type = recode(type,
    "pitcher_xwoba_est" = "Posterior Estimate",
    "season_xwoba"      = "Raw Season xwOBA"
  )) |>
  ggplot(aes(x = xwoba, fill = type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Posterior Estimate" = "#377eb8",
                               "Raw Season xwOBA"   = "#e41a1c")) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)) +
  labs(title    = "Posterior Estimate vs. Raw Season xwOBA",
       subtitle = "Shrinkage pulls tails toward the league mean",
       x = "xwOBA", fill = NULL)

print(p_dist)
ggsave("posterior_dist.png", p_dist, path = PLOTS_DIR, width = 8, height = 5)

# Scatter: posterior vs. raw — identity line vs. fitted slope shows regression to mean
p_scatter <- season_compare |>
  ggplot(aes(x = season_xwoba, y = pitcher_xwoba_est)) +
  geom_point(alpha = 0.25, size = 1.2) +
  geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "#377eb8") +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)) +
  labs(title    = "Posterior Estimate vs. Raw Season xwOBA",
       subtitle = "Blue = fitted slope; dashed = identity. Slope < 1 confirms shrinkage.",
       x = "Raw Season xwOBA", y = "Season-End Posterior")

print(p_scatter)
ggsave("posterior_scatter.png", p_scatter, path = PLOTS_DIR, width = 8, height = 5)

# Within-season trajectory: 8 random pitchers from 2022 to show how estimate evolves
set.seed(42)
sample_ids <- starter_pa |>
  filter(season == 2025, !is.na(pitcher_xwoba_est)) |>
  group_by(pitcher_id, pitcher_name) |>
  filter(n() >= 100) |>
  ungroup() |>
  distinct(pitcher_id, pitcher_name) |>
  slice_sample(n = 4)

p_trajectory <- starter_pa |>
  semi_join(sample_ids, by = "pitcher_id") |>
  filter(season == 2025, !is.na(pitcher_xwoba_est)) |>
  group_by(pitcher_id, pitcher_name) |>
  mutate(pa_n = row_number()) |>
  ungroup() |>
  ggplot(aes(x = pa_n, y = pitcher_xwoba_est, color = pitcher_name)) +
  geom_line(alpha = 0.85, linewidth = 0.7) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)) +
  labs(title    = "Pitcher Within-Season Posterior Trajectories (2025)",
       subtitle = "Estimate stabilizes as PA accumulate; prior weight diluted by within-season data",
       x = "PA (within season)", y = "Pitcher xwOBA Estimate", color = NULL)

print(p_trajectory)
ggsave("posterior_trajectory.png", p_trajectory, path = PLOTS_DIR, width = 9, height = 5)

# ══════════════════════════════════════════════════════════════════════════════
# Simulation output analysis ----
# ══════════════════════════════════════════════════════════════════════════════

sim_out    <- readRDS(file.path(DATA_DIR, "simulate_pull_out.rds"))
outing_meta <- sim_out$outing_meta

cat("\n=== outing_meta dimensions ===\n")
cat("Rows:", nrow(outing_meta), "\n")
cat("Columns:", paste(names(outing_meta), collapse = ", "), "\n\n")

cat("=== weighted_optimal_pull_pa summary ===\n")
print(summary(outing_meta$weighted_optimal_pull_pa))

# Eligible outings: same filter used in simulate_pull()
eligible <- outing_meta |>
  filter(!starts_above_repl, actual_pull_pa >= 2)

cat("\nEligible outings:", nrow(eligible), "\n")
cat("With a model-identified optimal pull PA:", sum(!is.na(eligible$weighted_optimal_pull_pa)), "\n")
cat("Late pulls:", sum(eligible$weighted_late_pull, na.rm = TRUE),
    sprintf("(%.1f%%)\n", 100 * mean(eligible$weighted_late_pull, na.rm = TRUE)))

# ── Join PA-level inning info back from starter_pa_full ───────────────────────
# starter_pa_full has one row per PA; we need the inning at the optimal pull PA.

pa_inning <- starter_pa |>
  arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number) |>
  group_by(season, game_id, inning_topbot, pitcher_id) |>
  mutate(pa_seq = row_number()) |>
  ungroup() |>
  select(season, game_id, inning_topbot, pitcher_id, pa_seq, inning,
         batter_xwoba_est, pitcher_xwoba_est, recent_xwoba_game,
         velo_dev_pa, velo_dev_ingame_pa)

# Attach features at the optimal pull PA
opt_with_features <- eligible |>
  filter(!is.na(weighted_optimal_pull_pa)) |>
  left_join(
    pa_inning |> rename(pa_seq = pa_seq),
    by = c("season", "game_id", "inning_topbot", "pitcher_id",
           "weighted_optimal_pull_pa" = "pa_seq")
  )

cat("\n=== Inning at optimal pull PA ===\n")
print(table(opt_with_features$inning, useNA = "ifany"))

# ── Distribution of optimal pull PA (PA number within outing) ─────────────────
cat("\n=== Optimal pull PA distribution ===\n")
print(quantile(eligible$weighted_optimal_pull_pa, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE))

p_opt_pa_hist <- eligible |>
  filter(!is.na(weighted_optimal_pull_pa)) |>
  ggplot(aes(x = weighted_optimal_pull_pa)) +
  geom_histogram(binwidth = 1, fill = "#2166ac", color = "white") +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(
    title    = "Distribution of Model-Optimal Pull PA",
    subtitle = "PA number within outing where weighted pull surplus is maximised",
    x        = "Optimal Pull PA (within outing)",
    y        = "Count"
  ) 
print(p_opt_pa_hist)
ggsave("opt_pull_pa_hist.png", p_opt_pa_hist, path = PLOTS_DIR, width = 8, height = 5)

# ── Inning distribution ────────────────────────────────────────────────────────
p_opt_inning <- opt_with_features |>
  filter(!is.na(inning)) |>
  count(inning) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = factor(inning), y = pct)) +
  geom_col(fill = "#2166ac") +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_fivethirtyeight(base_size = 13)+
  theme(axis.title = element_text(),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(
    title    = "Inning at Optimal Pull PA",
    subtitle = "When during the game does the model say the starter should come out?",
    x        = "Inning",
    y        = "Share of Outings"
  )
print(p_opt_inning)
ggsave("opt_pull_inning.png", p_opt_inning, path = PLOTS_DIR, width = 8, height = 5)



# ── Recent in-game performance (recent_xwoba) at optimal pull PA ──────────────
cat("\n=== Recent in-game xwOBA at optimal pull PA ===\n")
print(summary(opt_with_features$recent_xwoba_game))

p_recent_xwoba <- opt_with_features |>
  filter(!is.na(recent_xwoba_game)) |>
  ggplot(aes(x = recent_xwoba_game)) +
  geom_histogram(bins = 60, fill = "#762a83", color = "white", alpha = 0.85) +
  geom_vline(
    xintercept = median(opt_with_features$recent_xwoba_game, na.rm = TRUE),
    linetype = "dashed", linewidth = 0.8
  ) +
  theme_fivethirtyeight(base_size = 13)+
  theme(axis.title = element_text(),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(
    title    = "Recent In-Game xwOBA at Optimal Pull PA",
    subtitle = "Rolling in-game performance at the PA where the model recommends pulling",
    x        = "Recent In-Game xwOBA",
    y        = "Count"
  ) 
print(p_recent_xwoba)
ggsave("opt_pull_recent_xwoba.png", p_recent_xwoba, path = PLOTS_DIR, width = 8, height = 5)

# ── Summary: late pull outings vs. on-time ────────────────────────────────────
cat("\n=== pa_delta distribution for late pulls ===\n")
late_summary <- eligible |>
  filter(weighted_late_pull) |>
  pull(weighted_pa_delta)
print(quantile(late_summary, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE))
cat("Mean PA delta (late pulls):", round(mean(late_summary, na.rm = TRUE), 2), "\n")

p_pa_delta <- eligible |>
  filter(weighted_late_pull, weighted_pa_delta <= 30) |>
  ggplot(aes(x = weighted_pa_delta)) +
  geom_histogram(binwidth = 1, fill = "#b2182b", color = "white") +
  labs(
    title    = "PA Delta for Late-Pull Outings",
    subtitle = "Actual pull PA minus model-optimal pull PA (positive = manager pulled too late)",
    x        = "PA Delta (actual - optimal)",
    y        = "Count"
  ) +
  theme_minimal(base_size = 13)
print(p_pa_delta)
ggsave("late_pull_pa_delta.png", p_pa_delta, path = PLOTS_DIR, width = 8, height = 5)

