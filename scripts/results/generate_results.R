library(tidyverse)
library(fixest)
library(ggthemes)
library(readr)
library(gt)
library(slider)

source("scripts/pipeline/load_model_data.R")

PLOTS_DIR <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/plots"
DATA_DIR   <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"
model_data       <- load_model_data()
starter_pa       <- readRDS(file.path(DATA_DIR, "starter_pa_full.rds"))
reliever_rolling <- readRDS(file.path(DATA_DIR, "reliever_rolling.rds"))



# To rebuild from scratch (e.g. after changing build_features.R):
# source("build_features.R"); invalidate("starter_pa_full"); results <- build_all()
# starter_pa <- results$starter_pa_full; full_data <- results$full_data


#xwOBA heatmap: exit velocity × launch angle----

heatmap_data <- data.table::fread(
  file = path.expand(file.path(DATA_DIR, "savant_data.csv")),
  select = c("launch_speed", "launch_angle", "expected_woba")
)

xwoba_heatmap <- heatmap_data |>
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
rm(heatmap_data)


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
  filter(!is.na(xwoba_outcome), !is.na(velo_dev_pa), !is.na(velo_dev_ingame_pa),
         !is.na(ingame_dev_z), !is.na(prev_pitches_z), !is.na(days_rest_z))

fatigue_fit <- feols(
  xwoba_outcome ~ velo_dev_z + velo_dev_ingame_z + tto_fac +
    batter_xwoba_z + ingame_dev_z + prev_pitches_z + days_rest_z | pitcher_id,
  data = fatigue_data, cluster = ~pitcher_id
)

coef_tbl <- coeftable(fatigue_fit) |>
  as.data.frame() |>
  rownames_to_column("term") |>
  rename(estimate = Estimate, std_err = `Std. Error`,
         t_stat = `t value`, p_value = `Pr(>|t|)`) |>
  mutate(
    group = case_when(
      term %in% c("velo_dev_z", "velo_dev_ingame_z", "tto_fac2", "tto_fac3", "tto_fac4") ~ "Fatigue / TTO",
      term %in% c("prev_pitches_z", "days_rest_z") ~ "Pre-Outing Workload",
      TRUE ~ "Controls"
    ),
    term = recode(term,
      "velo_dev_z"         = "Velo Dev, Season Baseline (z-score)",
      "velo_dev_ingame_z"  = "Velo Dev, Within-Game (z-score)",
      "tto_fac2"           = "2nd Time Through Order",
      "tto_fac3"           = "3rd+ Time Through Order",
      "tto_fac4"           = "4th+ Time Through Order",
      "batter_xwoba_z"     = "Batter Quality (z-score)",
      "ingame_dev_z"       = "In-Game xwOBA Deviation (z-score)",
      "prev_pitches_z"     = "Prior Start Pitch Count (z-score)",
      "days_rest_z"        = "Days Rest, capped at 8 (z-score)"
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
    title    = md("**Fatigue Model: Parametric Coefficient Estimates**"),
    subtitle = md("Outcome: xwOBA per PA &nbsp;|&nbsp; Pitcher FE &nbsp;|&nbsp; SE clustered by pitcher")
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
    paste0("N = ", formatC(nrow(fatigue_data), format = "d", big.mark = ","), " PA")
  ) |>
  tab_options(
    row_group.font.weight     = "bold",
    heading.align             = "left",
    table.font.size           = 13,
    column_labels.font.weight = "bold"
  )

print(fatigue_gt)

# Coefficient forest plot ----
# Visual alternative to the table: easier to scan magnitudes and compare groups.
coef_plot_data <- coef_tbl |>
  mutate(
    ci_low  = estimate - 1.96 * std_err,
    ci_high = estimate + 1.96 * std_err,
    term    = factor(term, levels = rev(term)),
    group   = factor(group, levels = c("Fatigue / TTO", "Pre-Outing Workload", "Controls"))
  )

p_coef_forest <- coef_plot_data |>
  ggplot(aes(x = estimate, y = term, color = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.25, linewidth = 0.9) +
  geom_point(size = 3.5) +
  scale_color_manual(
    values = c("Fatigue / TTO"       = "#2166ac",
               "Pre-Outing Workload" = "#762a83",
               "Controls"            = "#4d4d4d"),
    guide  = guide_legend(reverse = TRUE)
  ) +
  theme_fivethirtyeight(base_size = 13) +
  theme(
    axis.title        = element_text(),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title    = "Fatigue Model Coefficient Estimates",
    subtitle = "OLS with pitcher FEs | 95% CI | SE clustered by pitcher | N = 590,231 PA",
    x        = "Coefficient (xwOBA per 1 SD)",
    y        = NULL,
    color    = NULL
  )

print(p_coef_forest)
ggsave("coef_forest.png", p_coef_forest, path = PLOTS_DIR, width = 10, height = 6)

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

# Season-decay grid search validation ----
# Plots MSE vs. decay factor for batters, starters, and relievers.
# Each curve is normalized so the minimum = 1, making the shape comparable
# across entity types with different raw MSE levels.
# The optimal decay values (Batter 0.2, Starter 0.3, Reliever 0.5) reflect
# how much cross-season information to carry: low decay → heavier regression
# to the league mean at the start of each new season.

decay_grids <- readRDS(file.path(DATA_DIR, "decay_mse_grids.rds"))

decay_plot_data <- bind_rows(decay_grids) |>
  group_by(entity) |>
  mutate(
    mse_norm  = mse / min(mse),
    is_opt    = mse == min(mse),
    entity    = factor(entity, levels = c("Batter", "Starter", "Reliever"))
  ) |>
  ungroup()

opt_labels <- decay_plot_data |>
  filter(is_opt) |>
  mutate(label = paste0("opt = ", decay))

p_decay_mse <- decay_plot_data |>
  ggplot(aes(x = decay, y = mse_norm, color = entity, group = entity)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  geom_point(data = filter(decay_plot_data, is_opt),
             size = 4.5, shape = 18) +
  geom_text(data = opt_labels,
            aes(label = label),
            vjust = -0.9, hjust = 0.5, size = 3.3, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual(values = c("Batter"   = "#e41a1c",
                                "Starter"  = "#2166ac",
                                "Reliever" = "#4dac26")) +
  theme_fivethirtyeight(base_size = 13) +
  theme(
    axis.title        = element_text(),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title    = "Season-Decay Grid Search: MSE vs. Decay Factor",
    subtitle = "MSE normalized to minimum = 1 per entity type. Diamond = optimal. Lower decay = more regression to the league mean at season start.",
    x        = "Season Decay Factor",
    y        = "Normalized MSE (relative to minimum)",
    color    = NULL
  )

print(p_decay_mse)
ggsave("decay_mse_validation.png", p_decay_mse, path = PLOTS_DIR, width = 9, height = 5)

# Year-over-year decay stability ----
# For each target season, hold it out and sweep decay over only the prior
# season's posterior → evaluate MSE on the target season's first 50 PA.
# A flat optimal-decay line year-to-year shows the estimates are not
# artifacts of any particular era.

eval_decay_yoy <- function(pa_data, prior_n, league_avg,
                           decay_grid = seq(0, 1, by = 0.1),
                           eval_pa_n  = 50L) {
  seasons     <- sort(unique(pa_data$season))
  eval_seasons <- seasons[seasons > min(seasons)]

  map_dfr(eval_seasons, function(tgt) {
    prev <- seasons[seasons < tgt]
    prior_season <- max(prev)

    end_posteriors <- pa_data |>
      filter(season == prior_season, !is.na(xwoba_outcome)) |>
      group_by(pitcher_id) |>
      summarise(
        sp = n(),
        sx = mean(xwoba_outcome, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        post_mean = (prior_n * league_avg + sp * sx) / (prior_n + sp),
        post_n    = prior_n + sp
      )

    eval_rows <- pa_data |>
      filter(season == tgt, !is.na(xwoba_outcome)) |>
      arrange(pitcher_id, game_id, at_bat_number) |>
      group_by(pitcher_id) |>
      slice_head(n = eval_pa_n) |>
      ungroup()

    if (nrow(eval_rows) < 10L) return(NULL)

    map_dfr(decay_grid, function(d) {
      preds <- eval_rows |>
        left_join(end_posteriors, by = "pitcher_id") |>
        mutate(
          pm = if_else(is.na(post_mean), league_avg, post_mean),
          pw = if_else(is.na(post_n),    prior_n,    post_n * d)
        ) |>
        arrange(pitcher_id, game_id, at_bat_number) |>
        group_by(pitcher_id) |>
        mutate(
          cx  = lag(cumsum(replace_na(xwoba_outcome, 0)), default = 0),
          cp  = lag(cumsum(!is.na(xwoba_outcome)),        default = 0L),
          est = (pw * pm + cx) / (pw + cp)
        ) |>
        ungroup()

      tibble(
        season = tgt,
        decay  = d,
        mse    = mean((preds$xwoba_outcome - preds$est)^2, na.rm = TRUE)
      )
    })
  })
}

# Build per-entity PA tables (same proxies as _run_decay_plot.R)
sp_pa  <- starter_pa |> select(pitcher_id, season, game_id, at_bat_number, xwoba_outcome)
bat_pa <- starter_pa |> select(pitcher_id = batter_id, season, game_id, at_bat_number, xwoba_outcome) |>
  filter(!is.na(xwoba_outcome))
rel_pa <- reliever_rolling |> select(pitcher_id, season, game_id, at_bat_number, xwoba_outcome)

league_sp  <- mean(sp_pa$xwoba_outcome,  na.rm = TRUE)
league_bat <- mean(bat_pa$xwoba_outcome, na.rm = TRUE)
league_rel <- mean(rel_pa$xwoba_outcome, na.rm = TRUE)

yoy_sp  <- eval_decay_yoy(sp_pa,  prior_n = 200, league_avg = league_sp)  |> mutate(entity = "Starter")
yoy_bat <- eval_decay_yoy(bat_pa, prior_n = 100, league_avg = league_bat) |> mutate(entity = "Batter")
yoy_rel <- eval_decay_yoy(rel_pa, prior_n = 200, league_avg = league_rel) |> mutate(entity = "Reliever")

yoy_opt <- bind_rows(yoy_sp, yoy_bat, yoy_rel) |>
  group_by(entity, season) |>
  slice_min(mse, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(entity = factor(entity, levels = c("Batter", "Starter", "Reliever")))

overall_opt <- tibble(
  entity    = factor(c("Batter", "Starter", "Reliever"),
                     levels = c("Batter", "Starter", "Reliever")),
  decay_opt = c(0.2, 0.3, 0.5)
)

p_yoy_stability <- yoy_opt |>
  ggplot(aes(x = season, y = decay, color = entity, group = entity)) +
  geom_hline(data = overall_opt,
             aes(yintercept = decay_opt, color = entity),
             linetype = "dashed", linewidth = 0.7, alpha = 0.6) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8) +
  scale_x_continuous(breaks = c(2015:2019, 2021:2025)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  scale_color_manual(values = c("Batter"   = "#e41a1c",
                                "Starter"  = "#2166ac",
                                "Reliever" = "#4dac26")) +
  theme_fivethirtyeight(base_size = 13) +
  theme(
    axis.title        = element_text(),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title    = "Year-over-Year Stability of Optimal Decay Estimates",
    subtitle = "Per-season leave-one-year-out optimal decay (solid) vs. pooled estimate (dashed). Flat lines confirm stability.",
    x        = NULL,
    y        = "Optimal Decay Factor",
    color    = NULL
  )

print(p_yoy_stability)
ggsave("decay_yoy_stability.png", p_yoy_stability, path = PLOTS_DIR, width = 10, height = 5)

# ══════════════════════════════════════════════════════════════════════════════
# In-Game xwOBA Deviation ----
# ingame_deviation is a Bayesian running posterior of how far the pitcher's
# in-game outcomes deviate from their expected level (season rolling estimate +
# GAM TTO curve), normalized by posterior weight. Zero at PA 1; diverges as
# results accumulate. This replaced recent_xwoba_z as the in-game signal.
# ══════════════════════════════════════════════════════════════════════════════

# Evolution across PAs within outing: median + IQR ribbon
ingame_by_pa <- starter_pa |>
  filter(!is.na(ingame_deviation), !is.na(pa_seq_ingame), pa_seq_ingame <= 30) |>
  group_by(pa_seq_ingame) |>
  summarise(
    med  = median(ingame_deviation),
    q25  = quantile(ingame_deviation, 0.25),
    q75  = quantile(ingame_deviation, 0.75),
    n    = n(),
    .groups = "drop"
  )

p_ingame_evo <- ingame_by_pa |>
  ggplot(aes(x = pa_seq_ingame)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#2166ac", alpha = 0.20) +
  geom_line(aes(y = med), color = "#2166ac", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        panel.background  = element_rect(fill = "white", color = NA),
        plot.background   = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "In-Game xwOBA Deviation by PA Within Outing",
    x        = "PA Number Within Outing",
    y        = "In-Game Deviation (xwOBA)"
  )
print(p_ingame_evo)
ggsave("ingame_dev_evolution.png", p_ingame_evo, path = PLOTS_DIR, width = 9, height = 5)


# ══════════════════════════════════════════════════════════════════════════════
# Simulation output analysis ----
# ══════════════════════════════════════════════════════════════════════════════

sim_out    <- readRDS(file.path(DATA_DIR, "simulate_pull_out.rds"))
outing_meta <- sim_out$outing_meta

# Eligible outings: same filter used in simulate_pull()
eligible <- outing_meta |>
  filter(!starter_worse_than_repl, actual_pull_pa >= 2)

cat("\nEligible outings:", nrow(eligible), "\n")
cat("With a model-identified optimal pull PA:", sum(!is.na(eligible$sim_optimal_pull_pa)), "\n")
cat("Late pulls:", sum(eligible$sim_late_pull, na.rm = TRUE),
    sprintf("(%.1f%%)\n", 100 * mean(eligible$sim_late_pull, na.rm = TRUE)))

# ── Join PA-level inning info back from starter_pa_full ───────────────────────
# starter_pa_full has one row per PA; we need the inning at the optimal pull PA.

pa_inning <- starter_pa |>
  arrange(season, game_id, inning_topbot, pitcher_id, at_bat_number) |>
  group_by(season, game_id, inning_topbot, pitcher_id) |>
  mutate(pa_seq = row_number()) |>
  ungroup() |>
  select(season, game_id, inning_topbot, pitcher_id, pa_seq, inning,
         batter_xwoba_est, pitcher_xwoba_est, recent_xwoba_game,
         velo_dev_pa, velo_dev_ingame_pa, ingame_deviation)

# Attach features at the optimal pull PA
opt_with_features <- eligible |>
  filter(!is.na(sim_optimal_pull_pa)) |>
  left_join(
    pa_inning |> rename(pa_seq = pa_seq),
    by = c("season", "game_id", "inning_topbot", "pitcher_id",
           "sim_optimal_pull_pa" = "pa_seq")
  )

cat("\n=== Inning at optimal pull PA ===\n")
print(table(opt_with_features$inning, useNA = "ifany"))

# ── Distribution of optimal pull PA (PA number within outing) ─────────────────
cat("\n=== Optimal pull PA distribution ===\n")
print(quantile(eligible$sim_optimal_pull_pa, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE))

mean_opt  <- mean(eligible$sim_optimal_pull_pa, na.rm = TRUE)
mean_act  <- mean(eligible$actual_pull_pa, na.rm = TRUE)

p_opt_pa_hist <- eligible |>
  filter(!is.na(sim_optimal_pull_pa)) |>
  ggplot(aes(x = sim_optimal_pull_pa)) +
  geom_histogram(binwidth = 1, fill = "#2166ac", color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean_opt, color = "#2166ac", linewidth = 1.1) +
  geom_vline(xintercept = mean_act, color = "#b2182b", linewidth = 1.1, linetype = "dashed") +
  annotate("text", x = mean_opt + 0.4, y = Inf, vjust = 1.5, hjust = 0, size = 3.4,
           color = "#2166ac",
           label = paste0("Optimal\n", round(mean_opt, 1))) +
  annotate("text", x = mean_act + 0.4, y = Inf, vjust = 1.5, hjust = 0, size = 3.4,
           color = "#b2182b",
           label = paste0("Actual\n", round(mean_act, 1))) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(
    title    = "Distribution of Model-Optimal Pull PA",
    subtitle = "Blue solid = mean optimal; red dashed = mean actual. Rightward gap = systematic late pulling.",
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


# ── Summary: late pull outings vs. on-time ────────────────────────────────────
cat("\n=== pa_delta distribution for late pulls ===\n")
late_summary <- eligible |>
  filter(sim_late_pull) |>
  pull(sim_pa_delta)
print(quantile(late_summary, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE))
cat("Mean PA delta (late pulls):", round(mean(late_summary, na.rm = TRUE), 2), "\n")

med_delta  <- median(late_summary)
mean_delta <- round(mean(late_summary, na.rm = TRUE), 2)

p_pa_delta <- eligible |>
  filter(sim_late_pull, sim_pa_delta <= 30) |>
  ggplot(aes(x = sim_pa_delta)) +
  geom_histogram(binwidth = 1, fill = "#b2182b", color = "white", alpha = 0.85) +
  geom_vline(xintercept = med_delta,  color = "#b2182b", linewidth = 1.1) +
  geom_vline(xintercept = mean_delta, color = "#4d4d4d", linewidth = 1.0, linetype = "dashed") +
  annotate("text", x = med_delta + 0.3, y = Inf, vjust = 1.5, hjust = 0, size = 3.3,
           color = "#b2182b", label = paste0("Median = ", med_delta)) +
  annotate("text", x = mean_delta + 0.3, y = Inf, vjust = 3.2, hjust = 0, size = 3.3,
           color = "#4d4d4d", label = paste0("Mean = ", mean_delta)) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title        = element_text(),
        axis.text         = element_text(size = 10),
        panel.background  = element_rect(fill = "white", color = NA),
        plot.background   = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "PA Delta for Late-Pull Outings",
    subtitle = "Actual pull PA minus model-optimal pull PA. Solid = median; dashed = mean.",
    x        = "PA Delta (actual - optimal)",
    y        = "Count"
  )
print(p_pa_delta)
ggsave("late_pull_pa_delta.png", p_pa_delta, path = PLOTS_DIR, width = 8, height = 5)

# ── Scatter: actual vs. sim-optimal pull PA ───────────────────────────────────
# Primary visual for the main finding. Points above the 45-degree line = late pulls.

scatter_data <- eligible |>
  filter(!is.na(sim_optimal_pull_pa)) |>
  mutate(
    pull_status = case_when(
      sim_pa_delta > 0 ~ "Late Pull",
      sim_pa_delta < 0 ~ "Early Pull",
      TRUE             ~ "On Time"
    ),
    pull_status = factor(pull_status, levels = c("Late Pull", "On Time", "Early Pull"))
  )

n_late_s  <- sum(scatter_data$pull_status == "Late Pull")
n_total_s <- nrow(scatter_data)
r_val     <- round(cor(scatter_data$sim_optimal_pull_pa, scatter_data$actual_pull_pa), 3)

p_scatter_pull <- scatter_data |>
  ggplot(aes(x = sim_optimal_pull_pa, y = actual_pull_pa, color = pull_status)) +
  geom_jitter(alpha = 0.18, size = 0.85, width = 0.35, height = 0.35) +
  geom_abline(slope = 1, intercept = 0, color = "gray25", linetype = "dashed", linewidth = 0.9) +
  scale_color_manual(
    values = c("Late Pull"  = "#b2182b",
               "On Time"    = "#4dac26",
               "Early Pull" = "#2166ac"),
    guide  = guide_legend(override.aes = list(alpha = 0.9, size = 3))
  ) +
  theme_fivethirtyeight(base_size = 13) +
  theme(
    axis.title        = element_text(),
    axis.text         = element_text(size = 10),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title    = "Model-Optimal vs. Actual Pull PA",
    subtitle = sprintf("r = %.3f  |  %d of %d outings with crossover  |  %d%% late pulls (above dashed line)",
                       r_val, n_total_s, nrow(eligible),
                       round(100 * n_late_s / n_total_s)),
    x        = "Model-Optimal Pull PA",
    y        = "Actual Pull PA",
    color    = NULL
  )
print(p_scatter_pull)
ggsave("scatter_actual_vs_optimal.png", p_scatter_pull, path = PLOTS_DIR, width = 9, height = 7)

# ── Season-level late-pull rate trend ─────────────────────────────────────────
season_trend <- eligible |>
  group_by(season) |>
  summarise(
    n_outings     = n(),
    n_crossover   = sum(!is.na(sim_optimal_pull_pa)),
    n_late        = sum(sim_late_pull, na.rm = TRUE),
    pct_crossover = n_crossover / n_outings,
    pct_late      = n_late / n_outings,
    mean_pa_delta = mean(sim_pa_delta[sim_late_pull & !is.na(sim_late_pull)], na.rm = TRUE),
    .groups       = "drop"
  )

p_season_trend <- season_trend |>
  select(season,
         "% with Crossover" = pct_crossover,
         "% Late Pulls"     = pct_late) |>
  pivot_longer(-season, names_to = "metric", values_to = "value") |>
  ggplot(aes(x = season, y = value, color = metric, group = metric)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.8) +
  scale_x_continuous(breaks = c(2015:2019, 2021:2025)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  scale_color_manual(values = c("% with Crossover" = "#2166ac",
                                 "% Late Pulls"     = "#b2182b")) +
  theme_fivethirtyeight(base_size = 13) +
  theme(
    axis.title        = element_text(),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title    = "Late-Pull Rate by Season (2015-2025)",
    subtitle = "% of eligible outings with a model-identified crossover and % classified as late pulls",
    x        = NULL,
    y        = NULL,
    color    = NULL
  )
print(p_season_trend)
ggsave("season_late_pull_trend.png", p_season_trend, path = PLOTS_DIR, width = 10, height = 6)

# ── In-game deviation at pull decision points ──────────────────────────────────
# For late-pull outings, compare ingame_deviation at the model-optimal PA vs
# the actual pull PA. If managers wait for the in-game signal to be obvious,
# ingame_deviation at actual pull > ingame_deviation at optimal pull.

# Features at actual pull PA (last PA of outing = actual_pull_pa)
actual_pull_features <- eligible |>
  left_join(
    pa_inning,
    by = c("season", "game_id", "inning_topbot", "pitcher_id",
           "actual_pull_pa" = "pa_seq")
  ) |>
  select(season, game_id, inning_topbot, pitcher_id,
         ingame_dev_actual = ingame_deviation,
         sim_late_pull, sim_optimal_pull_pa)

# Features at optimal pull PA (for late-pull outings only)
opt_pull_features <- eligible |>
  filter(!is.na(sim_optimal_pull_pa)) |>
  left_join(
    pa_inning,
    by = c("season", "game_id", "inning_topbot", "pitcher_id",
           "sim_optimal_pull_pa" = "pa_seq")
  ) |>
  select(season, game_id, inning_topbot, pitcher_id,
         ingame_dev_optimal = ingame_deviation)

late_pull_dev <- actual_pull_features |>
  filter(sim_late_pull) |>
  left_join(opt_pull_features,
            by = c("season", "game_id", "inning_topbot", "pitcher_id")) |>
  filter(!is.na(ingame_dev_actual), !is.na(ingame_dev_optimal)) |>
  pivot_longer(c(ingame_dev_actual, ingame_dev_optimal),
               names_to = "timing", values_to = "ingame_deviation") |>
  mutate(timing = recode(timing,
    "ingame_dev_actual"  = "Actual Pull PA",
    "ingame_dev_optimal" = "Model-Optimal Pull PA"
  ))

cat("\n=== In-game deviation at pull decision points (late-pull outings) ===\n")
late_pull_dev |>
  group_by(timing) |>
  summarise(mean = round(mean(ingame_deviation), 4),
            median = round(median(ingame_deviation), 4),
            sd = round(sd(ingame_deviation), 4),
            .groups = "drop") |>
  print()

p_ingame_pull <- late_pull_dev |>
  ggplot(aes(x = ingame_deviation, fill = timing)) +
  geom_density(alpha = 0.45, adjust = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Actual Pull PA" = "#b2182b",
                                "Model-Optimal Pull PA" = "#2166ac")) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title       = element_text(),
        axis.text        = element_text(size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "In-Game xwOBA Deviation at Pull Decision Points",
    subtitle = "Late-pull outings only — if red shifts right, managers wait for obvious signal before pulling",
    x        = "In-Game Deviation (xwOBA)",
    y        = "Density",
    fill     = NULL
  )
print(p_ingame_pull)
ggsave("ingame_dev_pull_comparison.png", p_ingame_pull, path = PLOTS_DIR, width = 9, height = 5)

# Distribution of ingame_deviation at the model-optimal pull PA across all
# eligible outings (not just late pulls) — shows what level triggers the recommendation
p_ingame_at_opt <- opt_with_features |>
  filter(!is.na(ingame_deviation)) |>
  ggplot(aes(x = ingame_deviation)) +
  geom_histogram(bins = 60, fill = "#762a83", color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = median(opt_with_features$ingame_deviation, na.rm = TRUE),
             color = "#762a83", linewidth = 0.9) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title       = element_text(),
        axis.text        = element_text(size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "In-Game xwOBA Deviation at Model-Optimal Pull PA",
    subtitle = paste0("Median = ",
                      round(median(opt_with_features$ingame_deviation, na.rm = TRUE), 4),
                      " — dashed = 0, solid = median"),
    x        = "In-Game Deviation (xwOBA)",
    y        = "Count"
  )
print(p_ingame_at_opt)
ggsave("ingame_dev_at_opt_pull.png", p_ingame_at_opt, path = PLOTS_DIR, width = 8, height = 5)

# Starter vs. Bullpen Crossover ----
# Starter: FE-adjusted fatigue model predictions, batter-neutral, by PA position.
# Removes each pitcher's individual quality intercept so the line reflects only
# structural fatigue/TTO trajectory for the average pure starter.
# Bullpen: GAM-estimated quality of the first entering reliever by pull PA
# (team-season factor-smooth, aggregated to league median + IQR).
# Crossover marks the PA where the average starter first becomes worse than
# the available bullpen arm.

batter_coef_val <- coef(fatigue_fit)["batter_xwoba_z"]
pitcher_fe_vals <- fixef(fatigue_fit)$pitcher_id
grand_mean_fe   <- mean(pitcher_fe_vals, na.rm = TRUE)

fe_vec <- pitcher_fe_vals[as.character(fatigue_data$pitcher_id)]
fe_vec[is.na(fe_vec)] <- grand_mean_fe

starter_by_pa <- fatigue_data |>
  mutate(
    fitted_neutral = predict(fatigue_fit)
                     - fe_vec + grand_mean_fe
                     - batter_coef_val * batter_xwoba_z
  ) |>
  filter(pa_seq_ingame >= 1, pa_seq_ingame <= 30) |>
  group_by(pa_seq_ingame) |>
  summarise(
    sp_med = median(fitted_neutral, na.rm = TRUE),
    sp_q25 = quantile(fitted_neutral, 0.25, na.rm = TRUE),
    sp_q75 = quantile(fitted_neutral, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

rel_curves <- readRDS(file.path(DATA_DIR, "reliever_pa_curves.rds"))$curves

reliever_by_pa <- rel_curves |>
  filter(pull_pa >= 1, pull_pa <= 30) |>
  group_by(pull_pa) |>
  summarise(
    rel_med = median(rel_quality, na.rm = TRUE),
    rel_q25 = quantile(rel_quality, 0.25, na.rm = TRUE),
    rel_q75 = quantile(rel_quality, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

crossover_df <- starter_by_pa |>
  inner_join(reliever_by_pa, by = c("pa_seq_ingame" = "pull_pa"))

crossover_pa <- crossover_df |>
  filter(pa_seq_ingame >= 5, sp_med >= rel_med) |>
  slice_min(pa_seq_ingame, n = 1, with_ties = FALSE) |>
  pull(pa_seq_ingame)

avg_pull_pa   <- mean(eligible$actual_pull_pa, na.rm = TRUE)
median_pull_pa <- median(eligible$actual_pull_pa, na.rm = TRUE)

p_crossover <- crossover_df |>
  ggplot(aes(x = pa_seq_ingame)) +
  geom_ribbon(aes(ymin = sp_q25, ymax = sp_q75), fill = "#2166ac", alpha = 0.12) +
  geom_ribbon(aes(ymin = rel_q25, ymax = rel_q75), fill = "#b2182b", alpha = 0.12) +
  geom_line(aes(y = sp_med, color = "Starter"), linewidth = 1.0) +
  geom_line(aes(y = rel_med, color = "Bullpen"), linewidth = 1.0) +
  geom_vline(xintercept = avg_pull_pa, linetype = "solid",
             color = "#4d9221", linewidth = 0.9) +
  annotate("text", x = avg_pull_pa + 0.4,
           y = max(crossover_df$sp_q75, na.rm = TRUE) - 0.002,
           hjust = 0, size = 3.3, color = "#4d9221",
           label = paste0("Avg. pull\nPA ", round(avg_pull_pa, 1))) +
  scale_color_manual(values = c("Starter" = "#2166ac", "Bullpen" = "#b2182b")) +
  theme_fivethirtyeight(base_size = 13) +
  theme(
    axis.title        = element_text(),
    axis.text         = element_text(size = 10),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title    = "Starter vs. Bullpen Projected xwOBA by PA Position",
    subtitle = "Median +/- IQR. Green line = observed avg. pull PA. Starter: FE-adjusted fatigue model, batter-neutral. Bullpen: GAM-predicted quality of entering reliever.",
    x        = "PA Number Within Outing",
    y        = "Projected xwOBA",
    color    = NULL
  )

if (length(crossover_pa) == 1 && !is.na(crossover_pa)) {
  crossover_y <- crossover_df$sp_med[crossover_df$pa_seq_ingame == crossover_pa]
  p_crossover <- p_crossover +
    geom_vline(xintercept = crossover_pa, linetype = "dashed",
               color = "gray30", linewidth = 0.8) +
    annotate("text", x = crossover_pa + 0.4, y = crossover_y + 0.001,
             hjust = 0, size = 3.3, color = "gray30",
             label = paste0("Crossover\nPA ", crossover_pa))
}

print(p_crossover)
ggsave("starter_bullpen_crossover.png", p_crossover, path = PLOTS_DIR, width = 9, height = 5)

# ── Pitcher-level late-pull heterogeneity ─────────────────────────────────────
# Wilson 95% CI computed manually (no external package dependency).
z_95 <- 1.96

pitcher_lp <- eligible |>
  group_by(pitcher_id, pitcher_name) |>
  summarise(
    n_crossover = sum(!is.na(sim_optimal_pull_pa)),
    n_late      = sum(sim_late_pull, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  filter(n_crossover >= 30) |>
  mutate(
    p_hat    = n_late / n_crossover,
    denom    = 1 + z_95^2 / n_crossover,
    center   = (p_hat + z_95^2 / (2 * n_crossover)) / denom,
    margin   = (z_95 * sqrt(p_hat * (1 - p_hat) / n_crossover +
                              z_95^2 / (4 * n_crossover^2))) / denom,
    ci_low   = pmax(0, center - margin),
    ci_high  = pmin(1, center + margin),
    pitcher_name = fct_reorder(pitcher_name, p_hat)
  )

sample_mean_lp <- mean(eligible$sim_late_pull, na.rm = TRUE)

p_pitcher_lp <- pitcher_lp |>
  ggplot(aes(x = p_hat, y = pitcher_name)) +
  geom_vline(xintercept = sample_mean_lp,
             linetype = "dashed", color = "gray50", linewidth = 0.7) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                 height = 0.3, linewidth = 0.55, color = "gray65") +
  geom_point(size = 1.8, color = "#b2182b") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  theme_fivethirtyeight(base_size = 11) +
  theme(
    axis.title       = element_text(),
    axis.text.y      = element_text(size = 6.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title    = "Late-Pull Rate by Pitcher",
    subtitle = sprintf("Pitchers with ≥ 30 crossover outings (n = %d) | dashed = sample mean (%.0f%%) | Wilson 95%% CI",
                       nrow(pitcher_lp), 100 * sample_mean_lp),
    x        = "Late-Pull Rate",
    y        = NULL
  )

print(p_pitcher_lp)
ggsave("pitcher_lp_dotplot.png", p_pitcher_lp, path = PLOTS_DIR,
       width = 9, height = max(8, nrow(pitcher_lp) * 0.18))

# ── Team-level late-pull aggregation with run cost ────────────────────────────
# Decision cost = pull_surplus(k_opt) - pull_surplus(k_act) for every crossover
# outing regardless of direction. This captures both frequency and magnitude:
# an on-time pull costs 0; a late pull costs the surplus left on the table.
# (In practice no early pulls exist — managers never outpace the model —
# so this metric is equivalent to the late-pull run cost, but is defined
# symmetrically so "best decision" = lowest runs per crossover outing.)
sim_surplus <- sim_out$sim |>
  select(season, game_id, inning_topbot, pitcher_id, pull_pa, pull_surplus)

outing_key <- c("season", "game_id", "inning_topbot", "pitcher_id")

crossover_outings <- eligible |> filter(!is.na(sim_optimal_pull_pa))

surplus_opt <- crossover_outings |>
  select(all_of(outing_key), sim_optimal_pull_pa, actual_pull_pa, team, season) |>
  inner_join(sim_surplus, by = c(outing_key, "sim_optimal_pull_pa" = "pull_pa")) |>
  rename(surplus_at_opt = pull_surplus)

surplus_act <- crossover_outings |>
  select(all_of(outing_key), actual_pull_pa) |>
  inner_join(sim_surplus, by = c(outing_key, "actual_pull_pa" = "pull_pa")) |>
  rename(surplus_at_act = pull_surplus)

outing_run_cost <- surplus_opt |>
  left_join(surplus_act |> select(all_of(outing_key), actual_pull_pa, surplus_at_act),
            by = c(outing_key, "actual_pull_pa")) |>
  mutate(run_cost = pmax(0, surplus_at_opt - coalesce(surplus_at_act, 0))) |>
  select(all_of(outing_key), team, season, run_cost)

team_season_summary <- eligible |>
  group_by(season, team) |>
  summarise(
    n_crossover   = sum(!is.na(sim_optimal_pull_pa)),
    n_late        = sum(sim_late_pull, na.rm = TRUE),
    pct_late      = n_late / n_crossover,
    mean_pa_delta = mean(sim_pa_delta[sim_late_pull & !is.na(sim_late_pull)],
                         na.rm = TRUE),
    .groups       = "drop"
  ) |>
  left_join(
    outing_run_cost |> group_by(season, team) |>
      summarise(runs_surrendered = sum(run_cost, na.rm = TRUE), .groups = "drop"),
    by = c("season", "team")
  ) |>
  mutate(
    runs_surrendered   = coalesce(runs_surrendered, 0),
    runs_per_crossover = runs_surrendered / n_crossover
  )

team_overall_summary <- eligible |>
  group_by(team) |>
  summarise(
    n_crossover   = sum(!is.na(sim_optimal_pull_pa)),
    n_late        = sum(sim_late_pull, na.rm = TRUE),
    pct_late      = n_late / n_crossover,
    mean_pa_delta = mean(sim_pa_delta[sim_late_pull & !is.na(sim_late_pull)],
                         na.rm = TRUE),
    .groups       = "drop"
  ) |>
  left_join(
    outing_run_cost |> group_by(team) |>
      summarise(runs_surrendered = sum(run_cost, na.rm = TRUE), .groups = "drop"),
    by = "team"
  ) |>
  mutate(
    runs_surrendered   = coalesce(runs_surrendered, 0),
    runs_per_crossover = runs_surrendered / n_crossover
  ) |>
  arrange(runs_per_crossover)

saveRDS(
  list(team_season = team_season_summary, team_overall = team_overall_summary),
  file.path(DATA_DIR, "team_pull_summary.rds")
)

cat("\n=== Team overall late-pull rate (best 5) ===\n")
print(head(team_overall_summary, 5))
cat("\n=== Team overall late-pull rate (worst 5) ===\n")
print(tail(team_overall_summary, 5))

# ── Game-state analysis at pull decision points ────────────────────────────────
# Joins game-state features (score, outs, TTO, pitch count, win expectancy)
# from starter_pa onto the optimal and actual pull PAs.

pa_gs <- starter_pa |>
  group_by(season, game_id, inning_topbot, pitcher_id) |>
  mutate(pa_seq = row_number()) |>
  ungroup() |>
  select(season, game_id, inning_topbot, pitcher_id, pa_seq,
         outs, pre_runner_1b, pre_runner_2b, pre_runner_3b,
         bat_score, fld_score, home_win_exp, n_thruorder, cum_pitch_count)

opt_gs <- eligible |>
  filter(!is.na(sim_optimal_pull_pa)) |>
  left_join(pa_gs,
            by = c("season", "game_id", "inning_topbot", "pitcher_id",
                   "sim_optimal_pull_pa" = "pa_seq")) |>
  mutate(
    score_diff  = fld_score - bat_score,
    score_state = cut(score_diff,
      breaks = c(-Inf, -3, -1, 1, 3, Inf),
      labels = c("Down 4+", "Down 1–3", "Tied", "Up 1–3", "Up 4+"),
      right  = FALSE),
    tto_label = factor(n_thruorder, levels = 1:4,
                       labels = c("1st TTO", "2nd TTO", "3rd TTO", "4th TTO"))
  )

act_gs <- eligible |>
  left_join(pa_gs,
            by = c("season", "game_id", "inning_topbot", "pitcher_id",
                   "actual_pull_pa" = "pa_seq")) |>
  mutate(
    score_diff  = fld_score - bat_score,
    score_state = cut(score_diff,
      breaks = c(-Inf, -3, -1, 1, 3, Inf),
      labels = c("Down 4+", "Down 1–3", "Tied", "Up 1–3", "Up 4+"),
      right  = FALSE)
  )

# ── TTO at model-optimal pull PA ──────────────────────────────────────────────
p_gs_tto <- opt_gs |>
  filter(!is.na(tto_label)) |>
  count(tto_label) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = tto_label, y = pct)) +
  geom_col(fill = "#2166ac") +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "Time Through Order at Model-Optimal Pull PA",
    subtitle = "94% of pull recommendations fire at the 3rd time through the order",
    x        = NULL,
    y        = "Share of Outings"
  )
print(p_gs_tto)
ggsave("gs_tto_opt.png", p_gs_tto, path = PLOTS_DIR, width = 7, height = 5)

# ── Pitch count: model-optimal vs. actual pull PA ─────────────────────────────
pc_compare <- bind_rows(
  opt_gs |>
    filter(!is.na(cum_pitch_count), cum_pitch_count <= 130) |>
    transmute(source = "Model-Optimal Pull PA", cum_pitch_count),
  act_gs |>
    filter(!is.na(cum_pitch_count), cum_pitch_count <= 130) |>
    transmute(source = "Actual Pull PA", cum_pitch_count)
)

med_opt <- median(opt_gs$cum_pitch_count, na.rm = TRUE)
med_act <- median(act_gs$cum_pitch_count, na.rm = TRUE)

p_gs_pc <- pc_compare |>
  ggplot(aes(x = cum_pitch_count, fill = source)) +
  geom_density(alpha = 0.45, adjust = 1.1) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_vline(xintercept = med_opt, color = "#2166ac", linewidth = 1.0) +
  geom_vline(xintercept = med_act, color = "#b2182b", linewidth = 1.0) +
  annotate("text", x = med_opt - 1.5, y = Inf, vjust = 1.5, hjust = 1,
           color = "#2166ac", size = 3.3,
           label = paste0("Optimal\nmedian = ", med_opt)) +
  annotate("text", x = med_act + 1.5, y = Inf, vjust = 1.5, hjust = 0,
           color = "#b2182b", size = 3.3,
           label = paste0("Actual\nmedian = ", med_act)) +
  scale_fill_manual(values = c("Model-Optimal Pull PA" = "#2166ac",
                                "Actual Pull PA"        = "#b2182b")) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        panel.background  = element_rect(fill = "white", color = NA),
        plot.background   = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "Pitch Count at Pull: Model vs. Manager",
    subtitle = "Dashed line = 100-pitch mark. Model triggers ~6 pitches before managers on median.",
    x        = "Cumulative Pitch Count",
    y        = "Density",
    fill     = NULL
  )
print(p_gs_pc)
ggsave("gs_pitch_count.png", p_gs_pc, path = PLOTS_DIR, width = 9, height = 5)

# ── Late-pull rate by score state ─────────────────────────────────────────────
score_lp <- act_gs |>
  filter(!is.na(score_state)) |>
  group_by(score_state) |>
  summarise(
    n        = n(),
    pct_late = mean(sim_late_pull, na.rm = TRUE),
    .groups  = "drop"
  )

p_gs_score <- score_lp |>
  ggplot(aes(x = score_state, y = pct_late)) +
  geom_col(fill = "#b2182b", alpha = 0.85) +
  geom_text(aes(label = scales::percent(pct_late, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  geom_hline(yintercept = mean(eligible$sim_late_pull, na.rm = TRUE),
             linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.6)) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "Late-Pull Rate by Score State",
    subtitle = "Dashed = overall mean. Late pulling is flat across all score contexts.",
    x        = "Score State (pitcher's team)",
    y        = "Late-Pull Rate"
  )
print(p_gs_score)
ggsave("gs_score_state.png", p_gs_score, path = PLOTS_DIR, width = 8, height = 5)

# ── Outs at model-optimal pull PA ─────────────────────────────────────────────
p_gs_outs <- opt_gs |>
  filter(!is.na(outs)) |>
  count(outs) |>
  mutate(
    pct       = n / sum(n),
    outs_label = factor(outs, levels = 0:2,
                        labels = c("0 outs", "1 out", "2 outs"))
  ) |>
  ggplot(aes(x = outs_label, y = pct)) +
  geom_col(fill = "#2166ac") +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.5)) +
  theme_fivethirtyeight(base_size = 13) +
  theme(axis.title = element_text(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)) +
  labs(
    title    = "Outs Recorded at Model-Optimal Pull PA",
    subtitle = "Managers often prefer to finish the inning; the model frequently recommends pulling mid-inning.",
    x        = NULL,
    y        = "Share of Outings"
  )
print(p_gs_outs)
ggsave("gs_outs_opt.png", p_gs_outs, path = PLOTS_DIR, width = 7, height = 5)

# ── Pitcher leaderboards: most late vs. most on-time ─────────────────────────
library(mlbplotR)

# Career-end posterior xwOBA per pitcher (last estimate across all seasons)
pitcher_xwoba_career <- starter_pa |>
  filter(!is.na(pitcher_xwoba_est)) |>
  group_by(pitcher_id) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(pitcher_id, pitcher_xwoba_est)

# Per-pitcher pull summary (min 30 crossover outings for stable estimates)
pitcher_pull_tbl <- eligible |>
  group_by(pitcher_id, pitcher_name) |>
  summarise(
    n_crossover  = sum(!is.na(sim_optimal_pull_pa)),
    n_late       = sum(sim_late_pull, na.rm = TRUE),
    pct_late     = n_late / n_crossover,
    mean_delta   = mean(sim_pa_delta[sim_late_pull & !is.na(sim_late_pull)],
                        na.rm = TRUE),
    mean_opt_pa  = mean(sim_optimal_pull_pa, na.rm = TRUE),
    mean_act_pa  = mean(actual_pull_pa),
    .groups      = "drop"
  ) |>
  filter(n_crossover >= 30) |>
  left_join(pitcher_xwoba_career, by = "pitcher_id")

gt_theme <- function(gt_obj) {
  gt_obj |>
    tab_options(
      table.font.size        = 13,
      heading.align          = "left",
      column_labels.font.weight = "bold",
      row_group.font.weight  = "bold",
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      column_labels.border.bottom.width = px(2),
      data_row.padding       = px(4)
    ) |>
    opt_table_font(font = google_font("IBM Plex Sans"))
}

make_leaderboard <- function(data, title, subtitle) {
  data |>
    mutate(rank = row_number()) |>
    select(rank, pitcher_id, pitcher_name, n_crossover,
           pct_late, mean_delta, mean_act_pa, mean_opt_pa, pitcher_xwoba_est) |>
    gt() |>
    gt_fmt_mlb_headshot(columns = pitcher_id, height = 40) |>
    cols_label(
      rank               = "#",
      pitcher_id         = "",
      pitcher_name       = "Pitcher",
      n_crossover        = "Outings",
      pct_late           = "Late Pull %",
      mean_delta         = "Avg PA Late",
      mean_act_pa        = "Avg Actual PA",
      mean_opt_pa        = "Avg Optimal PA",
      pitcher_xwoba_est  = "xwOBA Est."
    ) |>
    fmt_percent(columns = pct_late, decimals = 1) |>
    fmt_number(columns = c(mean_delta, mean_act_pa, mean_opt_pa,
                            pitcher_xwoba_est), decimals = 2) |>
    data_color(
      columns = pct_late,
      palette = c("#f7fbff", "#2166ac")
    ) |>
    data_color(
      columns = pitcher_xwoba_est,
      palette = c("#1a9850", "#ffffbf", "#d73027")
    ) |>
    tab_header(title = md(paste0("**", title, "**")),
               subtitle = subtitle) |>
    tab_source_note(md("Min. 30 crossover outings | xwOBA Est. = career-end Bayesian posterior")) |>
    gt_theme()
}

# Top 15 most consistently pulled too late
tbl_late <- pitcher_pull_tbl |>
  arrange(desc(pct_late), desc(mean_delta)) |>
  head(15) |>
  make_leaderboard(
    title    = "Most Consistently Pulled Too Late",
    subtitle = "Ranked by late-pull rate among starters with ≥ 30 crossover outings"
  )

# Top 15 pulled earliest relative to model
tbl_early <- pitcher_pull_tbl |>
  arrange(pct_late, mean_opt_pa) |>
  head(15) |>
  make_leaderboard(
    title    = "Pulled Earliest Relative to Model",
    subtitle = "Ranked by lowest late-pull rate — managers on time or ahead of model"
  )

print(tbl_late)
print(tbl_early)

gtsave(tbl_late,  "leaderboard_late.png",  path = PLOTS_DIR, zoom = 2)
gtsave(tbl_early, "leaderboard_early.png", path = PLOTS_DIR, zoom = 2)

# ── Team leaderboards ──────────────────────────────────────────────────────────
team_data <- readRDS(file.path(DATA_DIR, "team_pull_summary.rds"))

# mlbplotR uses specific abbreviations; remap mismatches
abbrev_map <- c(AZ = "ARI", SF = "SFG", WSH = "WSN", KC = "KCR",
                SD = "SDP", TB = "TBR", CWS = "CHW", CLE = "CLE",
                ATH = "ATH")
remap <- function(x) {
  m <- abbrev_map[x]
  ifelse(!is.na(m), m, x)
}

gt_team_theme <- function(gt_obj) {
  gt_obj |>
    tab_options(
      table.font.size           = 13,
      heading.align             = "left",
      column_labels.font.weight = "bold",
      table.border.top.style    = "hidden",
      table.border.bottom.style = "hidden",
      column_labels.border.bottom.width = px(2),
      data_row.padding          = px(4)
    ) |>
    opt_table_font(font = google_font("IBM Plex Sans"))
}

# ── Overall team rankings (all teams, best → worst) ───────────────────────────
team_overall <- team_data$team_overall |>
  arrange(pct_late) |>
  mutate(rank = row_number(), team_logo = remap(team))

tbl_teams_overall <- team_overall |>
  select(rank, team_logo, team, pct_late, mean_pa_delta, runs_surrendered, runs_per_crossover) |>
  gt() |>
  gt_fmt_mlb_logo(columns = team_logo, height = 30) |>
  cols_label(
    rank               = "#",
    team_logo          = "",
    team               = "Team",
    pct_late           = "Late Pull %",
    mean_pa_delta      = "Avg PA Late",
    runs_surrendered   = "Runs Surrendered",
    runs_per_crossover = "Runs / Outing"
  ) |>
  fmt_percent(columns = pct_late, decimals = 1) |>
  fmt_number(columns = c(mean_pa_delta, runs_surrendered, runs_per_crossover), decimals = 2) |>
  data_color(columns = pct_late,           palette = c("#1a9850", "#ffffbf", "#d73027")) |>
  data_color(columns = runs_per_crossover, palette = c("#1a9850", "#ffffbf", "#d73027")) |>
  tab_header(
    title    = md("**Team Pull-Timing Rankings (2015–2025)**"),
    subtitle = "Ranked by runs surrendered per crossover outing — captures both frequency and magnitude of timing error"
  ) |>
  tab_source_note(md("Runs / Outing = total run cost of timing errors ÷ crossover outings | run cost = pull_surplus(k_opt) − pull_surplus(k_act)")) |>
  gt_team_theme()

print(tbl_teams_overall)
gtsave(tbl_teams_overall, "leaderboard_teams_overall.png", path = PLOTS_DIR, zoom = 2)

# ── Best and worst individual team-seasons (min 15 crossover outings) ─────────
team_seasons_filt <- team_data$team_season |>
  filter(n_crossover >= 15) |>
  mutate(team_logo = remap(team))

make_team_season_tbl <- function(data, title, subtitle) {
  data |>
    mutate(rank = row_number()) |>
    select(rank, team_logo, team, season, pct_late, mean_pa_delta,
           runs_surrendered, runs_per_crossover) |>
    gt() |>
    gt_fmt_mlb_logo(columns = team_logo, height = 30) |>
    cols_label(
      rank               = "#",
      team_logo          = "",
      team               = "Team",
      season             = "Season",
      pct_late           = "Late Pull %",
      mean_pa_delta      = "Avg PA Late",
      runs_surrendered   = "Runs Surrendered",
      runs_per_crossover = "Runs / Outing"
    ) |>
    fmt_percent(columns = pct_late, decimals = 1) |>
    fmt_number(columns = c(mean_pa_delta, runs_surrendered, runs_per_crossover), decimals = 2) |>
    data_color(columns = pct_late,           palette = c("#1a9850", "#ffffbf", "#d73027")) |>
    data_color(columns = runs_per_crossover, palette = c("#1a9850", "#ffffbf", "#d73027")) |>
    tab_header(title = md(paste0("**", title, "**")), subtitle = subtitle) |>
    tab_source_note(md("Min. 15 crossover outings | Runs / Outing = total run cost ÷ crossover outings")) |>
    gt_team_theme()
}

tbl_team_seasons_best <- team_seasons_filt |>
  arrange(runs_per_crossover, pct_late) |>
  head(15) |>
  make_team_season_tbl(
    title    = "Best Team-Seasons: Lowest Run Cost Per Outing",
    subtitle = "Ranked by runs surrendered per crossover outing (2015–2025)"
  )

tbl_team_seasons_worst <- team_seasons_filt |>
  arrange(desc(runs_per_crossover), desc(pct_late)) |>
  head(15) |>
  make_team_season_tbl(
    title    = "Worst Team-Seasons: Highest Run Cost Per Outing",
    subtitle = "Ranked by runs surrendered per crossover outing (2015–2025)"
  )

print(tbl_team_seasons_best)
print(tbl_team_seasons_worst)
gtsave(tbl_team_seasons_best,  "leaderboard_team_seasons_best.png",  path = PLOTS_DIR, zoom = 2)
gtsave(tbl_team_seasons_worst, "leaderboard_team_seasons_worst.png", path = PLOTS_DIR, zoom = 2)

