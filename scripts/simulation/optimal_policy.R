library(tidyverse)
library(ggthemes)
library(scales)

PLOTS_DIR <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/plots"
DATA_DIR  <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"

sim_out <- readRDS(file.path(DATA_DIR, "simulate_pull_out.rds"))
meta    <- sim_out$outing_meta

# ── Working sample: eligible outings with a model crossover ───────────────────
df <- meta |>
  filter(!starter_worse_than_repl, actual_pull_pa >= 2, !is.na(weighted_optimal_pull_pa)) |>
  mutate(
    # pred_pa1 is predicted xwOBA at PA 1 (lower = better starter).
    # Quartile 1 = best starters, Q4 = weakest.
    sp_q = ntile(pred_pa1, 4),
    sp_label = factor(sp_q,
      levels = 1:4,
      labels = c("Elite SP\n(lowest xwOBA)", "Good SP", "Below-Avg SP", "Weak SP\n(highest xwOBA)")
    ),
    # avg_reliever_xwoba lower = better bullpen.
    rel_q = ntile(avg_reliever_xwoba, 4),
    rel_label = factor(rel_q,
      levels = 1:4,
      labels = c("Elite Pen\n(lowest xwOBA)", "Good Pen", "Below-Avg Pen", "Weak Pen\n(highest xwOBA)")
    ),
    # How much earlier/later the manager pulled vs. the model
    pa_delta = weighted_pa_delta   # positive = manager pulled too late
  )

cat("Sample:", nrow(df), "outings with model crossover\n")
cat("Late pull rate:", round(mean(df$weighted_late_pull), 3), "\n\n")

THEME <- theme_fivethirtyeight(base_size = 13) +
  theme(
    axis.title        = element_text(),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    strip.text        = element_text(size = 12, face = "bold")
  )

# ── Helper: group summary ──────────────────────────────────────────────────────
pull_summary <- function(data, group_var) {
  data |>
    group_by({{ group_var }}) |>
    summarise(
      n              = n(),
      mean_actual    = mean(actual_pull_pa),
      mean_optimal   = mean(weighted_optimal_pull_pa),
      mean_pa_delta  = mean(pa_delta),
      pct_late       = mean(weighted_late_pull),
      median_actual  = median(actual_pull_pa),
      median_optimal = median(weighted_optimal_pull_pa),
      .groups        = "drop"
    ) |>
    mutate(across(where(is.double), \(x) round(x, 2)))
}

# ══════════════════════════════════════════════════════════════════════════════
# Section 1: Pitcher quality
# ══════════════════════════════════════════════════════════════════════════════

cat("=== By starter quality (pred_pa1 quartile) ===\n")
sp_summary <- pull_summary(df, sp_label)
print(sp_summary)

# pred_pa1 cutpoints for the legend subtitle
sp_cuts <- quantile(df$pred_pa1, c(0, 0.25, 0.5, 0.75, 1)) |> round(3)
cat("\nQuartile breaks (pred_pa1):", paste(sp_cuts, collapse=" | "), "\n\n")

# Line: mean optimal vs. mean actual by SP quality
sp_line_data <- sp_summary |>
  pivot_longer(c(mean_actual, mean_optimal), names_to = "source", values_to = "mean_pa") |>
  mutate(source = recode(source,
    "mean_actual"  = "Manager (actual)",
    "mean_optimal" = "Model (optimal)"
  ))

p_sp_line <- ggplot(sp_line_data, aes(x = sp_label, y = mean_pa, color = source, group = source)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5) +
  geom_text(aes(label = round(mean_pa, 1)), vjust = -0.9, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("Manager (actual)" = "#b2182b", "Model (optimal)" = "#2166ac")) +
  THEME +
  labs(
    title    = "Mean Pull PA: Model vs. Manager by Starter Quality",
    subtitle = paste0("Q1 = lowest pred xwOBA (best starters), Q4 = highest\n",
                      "Quartile breaks: ", paste(sp_cuts[-c(1,5)], collapse=" / ")),
    x = "Starter Quality", y = "Mean Pull PA (within outing)", color = NULL
  )
print(p_sp_line)
ggsave("policy_sp_line.png", p_sp_line, path = PLOTS_DIR, width = 9, height = 5.5)

# Distribution: optimal vs. actual, faceted by SP quality
sp_dist_data <- df |>
  select(sp_label, actual = actual_pull_pa, optimal = weighted_optimal_pull_pa) |>
  pivot_longer(c(actual, optimal), names_to = "source", values_to = "pull_pa") |>
  mutate(source = recode(source,
    "actual"  = "Manager (actual)",
    "optimal" = "Model (optimal)"
  ))

p_sp_dist <- ggplot(sp_dist_data, aes(x = pull_pa, fill = source)) +
  geom_density(alpha = 0.45, adjust = 1.2) +
  facet_wrap(~sp_label, ncol = 2) +
  scale_fill_manual(values = c("Manager (actual)" = "#b2182b", "Model (optimal)" = "#2166ac")) +
  THEME +
  labs(
    title    = "Pull PA Distribution: Model vs. Manager by Starter Quality",
    subtitle = "Red = actual pull; blue = model-optimal. Rightward red shift = managers pulling too late.",
    x = "Pull PA (within outing)", y = "Density", fill = NULL
  )
print(p_sp_dist)
ggsave("policy_sp_dist.png", p_sp_dist, path = PLOTS_DIR, width = 10, height = 7)

# ══════════════════════════════════════════════════════════════════════════════
# Section 2: Reliever quality
# ══════════════════════════════════════════════════════════════════════════════

cat("=== By reliever quality (avg_reliever_xwoba quartile) ===\n")
rel_summary <- pull_summary(df, rel_label)
print(rel_summary)

rel_cuts <- quantile(df$avg_reliever_xwoba, c(0, 0.25, 0.5, 0.75, 1)) |> round(4)
cat("\nQuartile breaks (avg_reliever_xwoba):", paste(rel_cuts, collapse=" | "), "\n\n")

rel_line_data <- rel_summary |>
  pivot_longer(c(mean_actual, mean_optimal), names_to = "source", values_to = "mean_pa") |>
  mutate(source = recode(source,
    "mean_actual"  = "Manager (actual)",
    "mean_optimal" = "Model (optimal)"
  ))

p_rel_line <- ggplot(rel_line_data, aes(x = rel_label, y = mean_pa, color = source, group = source)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5) +
  geom_text(aes(label = round(mean_pa, 1)), vjust = -0.9, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("Manager (actual)" = "#b2182b", "Model (optimal)" = "#2166ac")) +
  THEME +
  labs(
    title    = "Mean Pull PA: Model vs. Manager by Reliever Quality",
    subtitle = paste0("Q1 = best bullpens, Q4 = weakest\n",
                      "Quartile breaks: ", paste(rel_cuts[-c(1,5)], collapse=" / ")),
    x = "Reliever Quality", y = "Mean Pull PA (within outing)", color = NULL
  )
print(p_rel_line)
ggsave("policy_rel_line.png", p_rel_line, path = PLOTS_DIR, width = 9, height = 5.5)

rel_dist_data <- df |>
  select(rel_label, actual = actual_pull_pa, optimal = weighted_optimal_pull_pa) |>
  pivot_longer(c(actual, optimal), names_to = "source", values_to = "pull_pa") |>
  mutate(source = recode(source,
    "actual"  = "Manager (actual)",
    "optimal" = "Model (optimal)"
  ))

p_rel_dist <- ggplot(rel_dist_data, aes(x = pull_pa, fill = source)) +
  geom_density(alpha = 0.45, adjust = 1.2) +
  facet_wrap(~rel_label, ncol = 2) +
  scale_fill_manual(values = c("Manager (actual)" = "#b2182b", "Model (optimal)" = "#2166ac")) +
  THEME +
  labs(
    title    = "Pull PA Distribution: Model vs. Manager by Reliever Quality",
    subtitle = "Red = actual pull; blue = model-optimal.",
    x = "Pull PA (within outing)", y = "Density", fill = NULL
  )
print(p_rel_dist)
ggsave("policy_rel_dist.png", p_rel_dist, path = PLOTS_DIR, width = 10, height = 7)

# ══════════════════════════════════════════════════════════════════════════════
# Section 3: 2D interaction — SP quality × reliever quality
# ══════════════════════════════════════════════════════════════════════════════

cat("=== 2×2 interaction: SP quality (above/below median) × reliever quality ===\n")

df_2x2 <- df |>
  mutate(
    sp_tier  = if_else(pred_pa1 <= median(pred_pa1), "Better SP", "Weaker SP"),
    rel_tier = if_else(avg_reliever_xwoba <= median(avg_reliever_xwoba), "Better Pen", "Weaker Pen"),
    sp_tier  = factor(sp_tier,  levels = c("Better SP",  "Weaker SP")),
    rel_tier = factor(rel_tier, levels = c("Better Pen", "Weaker Pen"))
  )

twobytwo <- df_2x2 |>
  group_by(sp_tier, rel_tier) |>
  summarise(
    n            = n(),
    mean_actual  = round(mean(actual_pull_pa), 1),
    mean_optimal = round(mean(weighted_optimal_pull_pa), 1),
    mean_delta   = round(mean(pa_delta), 2),
    pct_late     = round(mean(weighted_late_pull), 3),
    .groups      = "drop"
  )
cat("\n")
print(twobytwo)
cat("\n")

# 4×4 heatmap of mean PA delta (actual - optimal)
grid_4x4 <- df |>
  group_by(sp_q, rel_q) |>
  summarise(
    n          = n(),
    mean_delta = mean(pa_delta),
    pct_late   = mean(weighted_late_pull),
    .groups    = "drop"
  ) |>
  mutate(
    sp_label_s  = factor(sp_q,  labels = c("Elite SP","Good SP","Below-Avg SP","Weak SP")),
    rel_label_s = factor(rel_q, labels = c("Elite Pen","Good Pen","Below-Avg Pen","Weak Pen"))
  )

p_heatmap <- ggplot(grid_4x4, aes(x = rel_label_s, y = fct_rev(sp_label_s), fill = mean_delta)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(mean_delta, 1), "\n(", percent(pct_late, 0.1), ")")),
            size = 3.8, lineheight = 1.2) +
  scale_fill_gradient2(
    low      = "#2166ac",
    mid      = "white",
    high     = "#b2182b",
    midpoint = 0,
    name     = "Mean PA delta\n(actual − optimal)"
  ) +
  THEME +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    title    = "Manager vs. Model Pull Timing: SP × Bullpen Quality",
    subtitle = "Cell: mean (actual − optimal) PA delta | (% late pulls)\nRed = managers pulling too late; blue = pulling earlier than model",
    x = "Reliever Quality", y = "Starter Quality"
  )
print(p_heatmap)
ggsave("policy_heatmap.png", p_heatmap, path = PLOTS_DIR, width = 9, height = 6)

# ══════════════════════════════════════════════════════════════════════════════
# Section 4: Scatter — optimal PA vs. actual PA, colored by SP quality
# ══════════════════════════════════════════════════════════════════════════════

# Jitter slightly so overplotted points are visible; identity line = model ≡ manager
set.seed(42)
p_scatter <- df |>
  sample_n(min(6000, nrow(df))) |>
  ggplot(aes(x = weighted_optimal_pull_pa, y = actual_pull_pa, color = pred_pa1)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.25, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.8, linetype = "dashed") +
  scale_color_gradient2(
    low      = "#1a9850",
    mid      = "#ffffbf",
    high     = "#d73027",
    midpoint = median(df$pred_pa1),
    name     = "Starter\npred xwOBA\n(lower = better)"
  ) +
  coord_fixed(xlim = c(15, 36), ylim = c(5, 36)) +
  THEME +
  labs(
    title    = "Model-Optimal vs. Actual Pull PA",
    subtitle = "Points above dashed line = manager pulled later than model recommends.\nColor = starter quality (green = elite, red = weak).",
    x = "Model-Optimal Pull PA", y = "Actual Pull PA (manager)"
  )
print(p_scatter)
ggsave("policy_scatter.png", p_scatter, path = PLOTS_DIR, width = 8, height = 7)

# ══════════════════════════════════════════════════════════════════════════════
# Section 5: Pitcher leaderboard — most systematically late pulls
# ══════════════════════════════════════════════════════════════════════════════

cat("=== Pitcher leaderboard: most consistently pulled too late (min 15 outings) ===\n")

pitcher_board <- df |>
  group_by(pitcher_id, pitcher_name) |>
  summarise(
    n_outings    = n(),
    pct_late     = round(mean(weighted_late_pull), 3),
    mean_delta   = round(mean(pa_delta), 2),
    mean_optimal = round(mean(weighted_optimal_pull_pa), 1),
    mean_actual  = round(mean(actual_pull_pa), 1),
    mean_sp_xwoba = round(mean(pred_pa1), 4),
    .groups      = "drop"
  ) |>
  filter(n_outings >= 15) |>
  arrange(desc(pct_late), desc(mean_delta))

cat("\nTop 15 most-often pulled too late:\n")
print(head(pitcher_board, 15))

cat("\nTop 15 pulled EARLIEST vs. model (managers pulled ahead of model):\n")
print(pitcher_board |> arrange(mean_delta) |> head(15))

# ══════════════════════════════════════════════════════════════════════════════
# Section 6: Season trend — is manager alignment improving over time?
# ══════════════════════════════════════════════════════════════════════════════

cat("\n=== Season trend ===\n")

season_trend <- df |>
  group_by(season) |>
  summarise(
    n            = n(),
    mean_actual  = round(mean(actual_pull_pa), 2),
    mean_optimal = round(mean(weighted_optimal_pull_pa), 2),
    mean_delta   = round(mean(pa_delta), 2),
    pct_late     = round(mean(weighted_late_pull), 3),
    .groups      = "drop"
  )
print(season_trend)

season_line_data <- season_trend |>
  pivot_longer(c(mean_actual, mean_optimal), names_to = "source", values_to = "mean_pa") |>
  mutate(source = recode(source,
    "mean_actual"  = "Manager (actual)",
    "mean_optimal" = "Model (optimal)"
  ))

p_season <- ggplot(season_line_data, aes(x = season, y = mean_pa, color = source)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Manager (actual)" = "#b2182b", "Model (optimal)" = "#2166ac")) +
  scale_x_continuous(breaks = unique(season_trend$season)) +
  THEME +
  labs(
    title    = "Mean Pull PA by Season: Model vs. Manager",
    subtitle = "Is the gap between manager and model closing over time?",
    x = "Season", y = "Mean Pull PA (within outing)", color = NULL
  )
print(p_season)
ggsave("policy_season_trend.png", p_season, path = PLOTS_DIR, width = 10, height = 5.5)
