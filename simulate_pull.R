library(tidyverse)
library(fixest)
library(slider)
library(data.table)
library(baseballr)

source("build_features.R")
source("load_model_data.R")

DATA_DIR <- "~/Library/CloudStorage/GoogleDrive-theoauyeung@gmail.com/My Drive/Rice-University/2026-Summer/STAT 490/STAT 490/data"

simulate_pull <- function(N_BASE = 100000, N_SIM = 500) {

  # 1. Load count transitions and linear weights ----

  count_trans <- read_csv(file.path(DATA_DIR, "count_transitions.csv"), show_col_types = FALSE) |>
    filter(!str_starts(PitchResult, "truncated")) |>
    mutate(outcome = case_match(
      PitchResult,
      c("field_out", "force_out", "fielders_choice_out",
        "strikeout_double_play")               ~ "out",
      "strikeout"                              ~ "strikeout",
      "walk"                                   ~ "walk",
      c("sac_fly", "sac_bunt")                 ~ "sacrifice",
      "fielders_choice"                        ~ "fielders_choice",
      "field_error"                            ~ "field_error",
      c("hit_by_pitch", "catcher_interf")      ~ "hit_by_pitch",
      .default = PitchResult
    ))

  count_trans_p <- count_trans |>
    group_by(balls, strikes) |>
    mutate(prob = n / sum(n)) |>
    ungroup()

  ct_split <- split(count_trans_p, list(count_trans_p$balls, count_trans_p$strikes))
  count_prob_list <- setNames(
    ct_split,
    sapply(names(ct_split), \(k) { p <- strsplit(k, "\\.")[[1]]; paste0(p[1], ".", p[2]) })
  )

  lw_raw <- read_csv(file.path(DATA_DIR, "linear_weight.csv"), show_col_types = FALSE) |>
    mutate(outcome = case_match(
      event_name,
      "Homerun"        ~ "home_run",
      "Single"         ~ "single",
      "Double"         ~ "double",
      "Triple"         ~ "triple",
      "Walk"           ~ "walk",
      "Strikeout"      ~ "strikeout",
      "Out"            ~ "out",
      "HitByPitch"     ~ "hit_by_pitch",
      "Error"          ~ "field_error",
      "Sacrifice"      ~ "sacrifice",
      "FieldersChoice" ~ "fielders_choice",
      "Interference"   ~ "hit_by_pitch"
    )) |>
    summarise(LW = mean(LW), .by = outcome)

  LW <- setNames(lw_raw$LW, lw_raw$outcome)

  # 2. Pitch-by-pitch PA simulator ----

  simulate_one_pa <- function() {
    b <- 0L; s <- 0L
    repeat {
      cp <- count_prob_list[[paste0(b, ".", s)]]
      if (is.null(cp)) return("out")
      r <- sample(cp$outcome, 1L, prob = cp$prob)
      if (r == "ball") {
        if (b == 3L) return("walk")
        b <- b + 1L
      } else if (r %in% c("called_strike", "swinging_strike")) {
        if (s == 2L) return("strikeout")
        s <- s + 1L
      } else if (r == "foul") {
        if (s < 2L) s <- s + 1L
      } else {
        return(r)
      }
    }
  }

  # 3. Baseline PA outcome distribution ----

  set.seed(2025)
  cat("Simulating", N_BASE, "baseline PAs...\n")

  baseline_outcomes <- replicate(N_BASE, simulate_one_pa())
  baseline_df <- as.data.frame(table(baseline_outcomes) / N_BASE) |>
    rename(outcome = baseline_outcomes, base_prob = Freq) |>
    inner_join(tibble(outcome = names(LW), lw = LW), by = "outcome")

  cat("Baseline mean LW (should be near 0):", round(sum(baseline_df$base_prob * baseline_df$lw), 4), "\n")
  cat("Baseline outcomes:\n")
  print(baseline_df |> arrange(desc(base_prob)) |> mutate(across(where(is.numeric), \(x) round(x, 4))))

  BASE_P        <- baseline_df$base_prob
  BASE_LW       <- baseline_df$lw
  BASE_OUTCOMES <- baseline_df$outcome
  BASE_K        <- as.numeric(BASE_OUTCOMES == "strikeout")

  #  4. Exponential tilting ----
  # Two-dimensional tilting: match both mean LW (xwOBA proxy) and strikeout rate.
  # For a given alpha1 (LW tilt), alpha2 (K-rate tilt) is solved analytically:
  #   P(K | tilted) = exp(alpha2) * B / (A + exp(alpha2) * B) = target_k_rate
  #   => exp(alpha2) = target_k_rate * A / ((1 - target_k_rate) * B)
  # where A = sum of base weights on non-K outcomes, B = sum on K outcomes.
  # This reduces to a single uniroot call on alpha1.

  tilt_probs <- function(target_xwoba, target_k_rate) {
    k_idx  <- BASE_K == 1
    nk_idx <- BASE_K == 0

    achievable_min <- { w <- BASE_P * exp(-15 * BASE_LW); sum(w * BASE_LW) / sum(w) }
    achievable_max <- { w <- BASE_P * exp( 15 * BASE_LW); sum(w * BASE_LW) / sum(w) }
    xwoba_c <- pmax(achievable_min + 1e-6, pmin(achievable_max - 1e-6, target_xwoba))
    k_c     <- pmax(1e-4, pmin(1 - 1e-4, target_k_rate))

    obj <- function(alpha1) {
      w_base  <- BASE_P * exp(alpha1 * BASE_LW)
      scale_k <- k_c * sum(w_base[nk_idx]) / ((1 - k_c) * sum(w_base[k_idx]))
      w       <- w_base * ifelse(k_idx, scale_k, 1)
      sum(w * BASE_LW) / sum(w) - xwoba_c
    }

    alpha1  <- uniroot(obj, c(-15, 15), tol = 1e-6)$root
    w_base  <- BASE_P * exp(alpha1 * BASE_LW)
    scale_k <- k_c * sum(w_base[nk_idx]) / ((1 - k_c) * sum(w_base[k_idx]))
    w       <- w_base * ifelse(k_idx, scale_k, 1)
    w / sum(w)
  }

  # 5. Fit fatigue model ----

  model_data   <- load_model_data()
  fatigue_data <- model_data |>
    filter(!is.na(xwoba_outcome), !is.na(velo_dev_pa), !is.na(velo_dev_ingame_pa), !is.na(ingame_dev_z))

  # velo_dev_ingame_z replaces pc_z: both velocity measures carry independent
  # signal (r = 0.18); pc_z drops out once ingame velo is included. Selected by
  # BIC (504841 vs 506069 for the pc_z*tto_fac spec, Δ = 1228).
  fatigue_model <- feols(
    xwoba_outcome ~ velo_dev_z + velo_dev_ingame_z + tto_fac + batter_xwoba_z + ingame_dev_z | pitcher_id,
    data = fatigue_data, cluster = ~pitcher_id
  )

  # BIC comparison vs. old recent_xwoba_z spec (on matched sample)
  fatigue_model_old <- feols(
    xwoba_outcome ~ velo_dev_z + velo_dev_ingame_z + tto_fac + batter_xwoba_z +
      I(scale(recent_xwoba_game)[, 1]) | pitcher_id,
    data    = fatigue_data |> filter(!is.na(recent_xwoba_game)),
    cluster = ~pitcher_id
  )
  cat("BIC (ingame_dev_z spec):   ", round(BIC(fatigue_model),     1), "\n")
  cat("BIC (recent_xwoba_z spec): ", round(BIC(fatigue_model_old), 1), "\n")
  cat("ΔBIC (new − old):", round(BIC(fatigue_model) - BIC(fatigue_model_old), 1),
      " — negative favors new spec\n")
  rm(fatigue_model_old)

  ## 5a. Outing-level performance SD: captures day-to-day deviation from model prediction. ----
  # Method-of-moments variance decomposition:
  #   Var(outing_mean_resid) = sigma_outing^2 + E[within_var / n_pa]
  # The naive sd(outing_means) overstates sigma_outing because it includes the
  # sampling noise from averaging n_pa PA-level residuals per outing. We subtract
  # the expected within-outing contribution before taking the square root.
  outing_resid_tbl <- fatigue_data |>
    mutate(resid = residuals(fatigue_model)) |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    summarise(
      outing_resid = mean(resid),
      n_pa         = n(),
      within_var   = var(resid),   # NA for single-PA outings
      .groups      = "drop"
    )

  within_correction <- mean(
    outing_resid_tbl$within_var / outing_resid_tbl$n_pa,
    na.rm = TRUE
  )
  sigma_outing <- sqrt(max(0, var(outing_resid_tbl$outing_resid, na.rm = TRUE) - within_correction))

  cat("Outing-level performance SD:", round(sigma_outing, 4), "\n")

  # P10 of starter PA counts — floor for threshold crossover so we never flag
  # "optimal pull at PA 3" when no manager would pull there.
  pa_floor <- fatigue_data |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    summarise(n_pa = n(), .groups = "drop") |>
    pull(n_pa) |>
    quantile(0.10, names = FALSE) |>
    ceiling() |>
    max(5L)
  cat("PA floor (P10 of starter PA counts):", pa_floor, "\n")

  # Per-pitcher-season K rate with a 100-PA shrinkage prior toward the league mean.
  # Used to anchor the strikeout dimension of the 2D tilt so pitcher type
  # (power vs. finesse) shapes the outcome distribution, not just the mean LW.
  league_k_rate <- mean(fatigue_data$events == "strikeout", na.rm = TRUE)
  pitcher_k_rates <- fatigue_data |>
    group_by(season, pitcher_id) |>
    summarise(
      n_k  = sum(events == "strikeout", na.rm = TRUE),
      n_pa = n(),
      .groups = "drop"
    ) |>
    mutate(k_rate_est = round((100 * league_k_rate + n_k) / (100 + n_pa), 3))

  cat("League K rate (starters):", round(league_k_rate, 4), "\n")

  ## 5b. Bullpen strain model ----
  # Estimate how reliever quality (xwOBA) degrades as bullpen PA load grows
  # within a game. Controlling for pitcher quality via reliever_xwoba_est, the
  # coefficient on cum_bullpen_pa captures the strain effect: deeper into the
  # pen means worse pitchers and fatigued arms.

  # reliever_rolling <- readRDS(file.path(DATA_DIR, "reliever_rolling.rds"))
  # 
  # reliever_strain_data <- reliever_rolling |>
  #   filter(!is.na(xwoba_outcome), !is.na(reliever_xwoba_est)) |>
  #   arrange(season, game_id, inning_topbot, at_bat_number) |>
  #   group_by(season, game_id, inning_topbot) |>
  #   mutate(cum_bullpen_pa = row_number()) |>
  #   ungroup()
  # 
  # bullpen_strain_model <- feols(
  #   xwoba_outcome ~ cum_bullpen_pa + reliever_xwoba_est | season,
  #   data   = reliever_strain_data,
  #   cluster = ~pitcher_id
  # )
  # 
  # strain_rate <- coef(bullpen_strain_model)["cum_bullpen_pa"]
  # cat("Bullpen strain rate (xwOBA/extra bullpen PA):", round(strain_rate, 5), "\n")

  ## 5c. Build outing simulation table ----

  # Strip batter-quality contribution before threshold comparison so above_opt
  # reflects pitcher fatigue vs. reliever level, not matchup difficulty.
  batter_coef <- coef(fatigue_model)["batter_xwoba_z"]

  sim_data <- fatigue_data |>
    mutate(
      starter_pred         = predict(fatigue_model, newdata = fatigue_data),
      starter_pred_neutral = starter_pred - batter_coef * batter_xwoba_z
    ) |>
    arrange(season, game_id, inning_topbot, at_bat_number) |>
    group_by(season, game_id, inning_topbot, pitcher_id, pitcher_name, avg_reliever_xwoba) |>
    mutate(
      pa_seq      = row_number(),
      pred_pa1    = first(starter_pred_neutral),
      threshold   = pmax(pred_pa1, avg_reliever_xwoba),
      # 3-PA backward rolling mean smooths single-PA prediction spikes so a
      # transient uptick (e.g. TTO-2 opener) doesn't anchor optimal_pull_pa early.
      pred_smooth = slide_dbl(starter_pred_neutral, mean, .before = 2L, .complete = FALSE),
      above_opt   = pred_smooth > threshold & pa_seq >= pa_floor
    ) |>
    ungroup()

  outing_meta <- sim_data |>
    group_by(season, game_id, inning_topbot, pitcher_id, pitcher_name) |>
    summarise(
      n_pa               = n(),
      avg_reliever_xwoba = first(avg_reliever_xwoba),
      pred_pa1           = first(pred_pa1),
      starts_above_repl  = first(pred_pa1) > first(avg_reliever_xwoba),
      optimal_pull_pa    = if (any(above_opt)) as.integer(min(pa_seq[above_opt])) else NA_integer_,
      actual_pull_pa     = max(pa_seq),
      .groups = "drop"
    ) |>
    mutate(
      pa_delta  = actual_pull_pa - coalesce(optimal_pull_pa, actual_pull_pa),
      late_pull = !is.na(optimal_pull_pa) & pa_delta > 0
    ) |>
    left_join(select(pitcher_k_rates, season, pitcher_id, k_rate_est),
              by = c("season", "pitcher_id")) |>
    mutate(k_rate_est = coalesce(k_rate_est, round(league_k_rate, 3)))

  outing_preds <- sim_data |>
    arrange(season, game_id, inning_topbot, pitcher_id, pa_seq) |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    summarise(preds = list(starter_pred_neutral), .groups = "drop")

  # All outings where the starter begins better than the reliever baseline.
  # No late-pull filter — we evaluate the pull decision at every single PA.
  sim_outings <- outing_meta |>
    filter(!starts_above_repl, actual_pull_pa >= 2) |>
    left_join(outing_preds, by = c("season", "game_id", "inning_topbot", "pitcher_id"))

  cat("Outings to simulate:", nrow(sim_outings), "\n")

  # 5d. Pull-time weights ----
  # Empirical P(pull exactly at PA k) from observed actual_pull_pa.
  # Multiplying pull_surplus by these weights concentrates evaluation on the
  # realistic pull window and down-weights surplus at PA 1–8 where managers
  # almost never pull.

  pull_weights <- outing_meta |>
    filter(!starts_above_repl, actual_pull_pa >= 2) |>
    count(actual_pull_pa, name = "n_pulled") |>
    mutate(pull_weight = n_pulled / sum(n_pulled)) |>
    rename(pull_pa = actual_pull_pa) |>
    select(pull_pa, pull_weight)

  cat("Mean actual pull PA:", round(
    weighted.mean(pull_weights$pull_pa, pull_weights$pull_weight), 1), "\n")

  # 6. Pre-compute tilt probability cache ----
  # Cache keys are "xwoba_krate" strings. The K-rate dimension captures
  # pitcher type: two starters with identical xwOBA but different K rates
  # (power vs. finesse) get different tilted distributions, which affects
  # both prob_pull_better and any future leverage-sensitive evaluation.

  # Reliever K rate: K rate implied by the count-transition baseline.
  rel_k_rate <- round(sum(BASE_P * BASE_K), 3)

  # Unique K rates needed: one per pitcher-season + reliever baseline
  k_rates_used <- sort(unique(round(c(sim_outings$k_rate_est, rel_k_rate), 3)))

  all_xwoba <- round(c(
    unlist(sim_outings$preds),
    sim_outings$avg_reliever_xwoba
  ), 3)

  # Extend xwOBA range to cover outing-level noise shifts (±4σ)
  xwoba_grid <- round(seq(
    max(0.05, min(all_xwoba) - 4 * sigma_outing),
    min(0.85, max(all_xwoba) + 4 * sigma_outing),
    by = 0.001
  ), 3)

  XWOBA_MIN <- min(xwoba_grid)
  XWOBA_MAX <- max(xwoba_grid)

  all_pairs <- crossing(xwoba = xwoba_grid, k_rate = k_rates_used)

  cat("Building tilt cache for", nrow(all_pairs),
      "unique (xwOBA, K-rate) pairs across", length(k_rates_used), "K-rate levels...\n")
  tilt_cache <- map2(all_pairs$xwoba, all_pairs$k_rate, tilt_probs) |>
    setNames(paste0(all_pairs$xwoba, "_", all_pairs$k_rate))

  # n draws from a single fixed (xwOBA, K-rate) level
  sample_lw <- function(xwoba_q, k_rate, n) {
    p <- tilt_cache[[paste0(round(xwoba_q, 3), "_", round(k_rate, 3))]]
    sample(BASE_LW, n, replace = TRUE, prob = p)
  }

  # One draw per rep from a per-rep xwOBA vector; K rate is fixed for the outing
  sample_lw_batch <- function(xwoba_vec, k_rate) {
    k_r <- as.character(round(k_rate, 3))
    out <- numeric(length(xwoba_vec))
    for (q in unique(xwoba_vec)) {
      idx      <- xwoba_vec == q
      out[idx] <- sample(BASE_LW, sum(idx), replace = TRUE,
                         prob = tilt_cache[[paste0(q, "_", k_r)]])
    }
    out
  }

  clamp_xwoba <- function(x) round(pmin(pmax(x, XWOBA_MIN), XWOBA_MAX), 3)

  # 7. Every-PA counterfactual ----
  # For each PA k in each outing, evaluate the pull decision over the window
  # [k, actual_pull_pa] under two policies:
  #
  #   KEEP: starter faces the window using per-PA fatigue model predictions.
  #   PULL: reliever faces the window at strain-adjusted quality. Pulling earlier
  #         increases cum_bullpen_pa, which degrades expected reliever xwOBA by
  #         strain_rate per extra PA.
  #
  # pull_surplus > 0 means the starter allowed more runs than the reliever would
  # have — the manager should have pulled. pull_surplus < 0 means keeping the
  # starter was the right call.
  #
  # Starter side: full Monte Carlo via vectorized backward cumulative sum.
  # Reliever side: CLT approximation — sum of iid PA outcomes is approximately
  #   N(n_window * adj_rel, n_window * sigma_pa_lw^2) — exact in mean, preserves
  #   outing-level shock variance without sampling every PA individually.

  set.seed(2026)
  cat("Simulating every-PA counterfactual for", nrow(sim_outings), "outings...\n")

  sim_results_long <- sim_outings |>
    rowwise() |>
    mutate(
      pa_evals = list({
        T <- actual_pull_pa
        d_preds_outing <- preds[seq_len(T)]

        delta_s <- rnorm(N_SIM, 0, sigma_outing)   # starter outing shock per rep
        delta_r <- rnorm(N_SIM, 0, sigma_outing)   # reliever outing shock per rep

        # Starter: pre-sample LW for every PA × rep → N_SIM × T matrix.
        # K rate is fixed per outing (pitcher type); xwOBA shifts with outing noise.
        lw_mat <- vapply(d_preds_outing, function(q) {
          sample_lw_batch(clamp_xwoba(q + delta_s), k_rate_est)
        }, numeric(N_SIM))
        if (!is.matrix(lw_mat)) lw_mat <- matrix(lw_mat, nrow = N_SIM)

        # Prefix sums: prefix_mat[j, k] = sum of starter LW for PAs 1..k, rep j
        prefix_mat    <- t(apply(lw_mat, 1, cumsum))   # N_SIM × T
        total_per_rep <- prefix_mat[, T]               # total over all T PAs

        # rv_keep_mat[j, k] = sum of starter LW for PAs k..T, rep j
        #                    = total[j] - prefix[j, k-1]  (0 when k = 1)
        rv_keep_mat <- matrix(total_per_rep, nrow = N_SIM, ncol = T) -
                       cbind(0, prefix_mat[, seq_len(T - 1L), drop = FALSE])

        # Reliever: CLT approximation for the window starting at each k
        #   n_window[k] = T - k + 1
        #   adj_rel[k]  = avg_reliever_xwoba + strain_rate * (T - k)
        #     (extra T - k bullpen PAs vs. pulling at the actual pull point)
        n_windows <- T:1L                                                      # length T
        #adj_rels  <- avg_reliever_xwoba + strain_rate * seq(T - 1L, 0L)       # length T

        # PA-level LW SD for the reliever from the tilted distribution at
        # (avg_reliever_xwoba, rel_k_rate). Using the tilted SD rather than the
        # baseline keeps prob_pull_better consistent with pitcher-type-specific
        # outcome variance, which matters for leverage-sensitive decisions.
        p_rel     <- tilt_cache[[paste0(clamp_xwoba(avg_reliever_xwoba), "_", rel_k_rate)]]
        sigma_rel <- sqrt(max(0, sum(p_rel * BASE_LW^2) - avg_reliever_xwoba^2))

        # rv_pull_mat[j, k] = n_window[k] * (adj_rel[k] + delta_r[j]) + PA noise
        rv_pull_mat <-
          outer(delta_r, n_windows) +
          matrix(n_windows * avg_reliever_xwoba, nrow = N_SIM, ncol = T, byrow = TRUE) +
          matrix(
            rnorm(N_SIM * T, 0L, rep(sqrt(n_windows) * sigma_rel, each = N_SIM)),
            nrow = N_SIM, ncol = T
          )

        tibble(
          pull_pa          = seq_len(T),
          n_window         = n_windows,
          extra_bullpen_pa = T - seq_len(T),
          mean_rv_keep     = colMeans(rv_keep_mat),
          mean_rv_pull     = colMeans(rv_pull_mat),
          pull_surplus     = colMeans(rv_keep_mat) - colMeans(rv_pull_mat),
          prob_pull_better = colMeans(rv_pull_mat < rv_keep_mat)
        )
      })
    ) |>
    ungroup() |>
    select(-preds) |>
    unnest(pa_evals) |>
    # Drop outing-level deterministic metadata — these belong in outing_meta, not
    # the PA-level sim table. optimal_pull_pa and late_pull are stale here anyway
    # because the weighted updates to outing_meta happen after this table is built.
    select(-optimal_pull_pa, -pa_delta, -late_pull) |>
    left_join(pull_weights, by = "pull_pa") |>
    mutate(
      pull_weight           = replace_na(pull_weight, 0),
      weighted_pull_surplus = pull_surplus * pull_weight
    )

  # ── 7b. Weighted optimal pull PA per outing ───────────────────────────────────
  # Find the PA with maximum weighted_pull_surplus for each outing; that is the
  # pull point the model recommends after accounting for when managers actually
  # pull. A positive weighted surplus at that PA is required to count it.

  weighted_optimal <- sim_results_long |>
    filter(pull_pa >= pa_floor) |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    slice_max(weighted_pull_surplus, n = 1L, with_ties = FALSE) |>
    filter(weighted_pull_surplus > 0) |>
    select(season, game_id, inning_topbot, pitcher_id,
           weighted_optimal_pull_pa = pull_pa) |>
    ungroup()

  outing_meta <- outing_meta |>
    left_join(weighted_optimal,
              by = c("season", "game_id", "inning_topbot", "pitcher_id")) |>
    mutate(
      weighted_pa_delta  = actual_pull_pa -
        coalesce(weighted_optimal_pull_pa, actual_pull_pa),
      weighted_late_pull = !is.na(weighted_optimal_pull_pa) & weighted_pa_delta > 0,
      # Promote weighted_late_pull as the canonical late_pull: it is anchored to
      # the empirical pull-time distribution and the full Monte Carlo surplus, so
      # it is a far more realistic diagnostic than the raw threshold crossover.
      late_pull          = weighted_late_pull
    )

  cat("\n=== Simulation summary ===\n")
  sim_results_long |>
    summarise(
      total_pa_evals        = n(),
      mean_pull_surplus     = mean(pull_surplus),
      pct_pull_beneficial   = mean(pull_surplus > 0),
      mean_prob_pull_better = mean(prob_pull_better),
      pct_weighted_pos      = mean(weighted_pull_surplus > 0)
    ) |>
    print()

 
}

# ══════════════════════════════════════════════════════════════════════════════
# Weight sensitivity stress test
#
# Re-applies four weight schemes to the already-computed sim_results_long
# without re-running the Monte Carlo. Call after simulate_pull():
#   out  <- simulate_pull()
#   sens <- stress_test_weights(out)
#
# Schemes:
#   raw      — raw surplus crossover (no weights; first PA where pull_surplus > 0)
#   empirical — current empirical P(pull at PA k); concentrated at PA 22–29
#   uniform  — equal weight at every PA; breaks the structural concentration
#   inverted — inverse of empirical; aggressively down-weights PA 22–29
#
# Key diagnostic: if pct_late collapses under uniform vs. empirical, the
# headline 84% figure is largely a weighting artifact, not a data result.
# ══════════════════════════════════════════════════════════════════════════════

stress_test_weights <- function(sim_out) {
  sim_long    <- sim_out$sim
  meta        <- sim_out$outing_meta
  emp_weights <- sim_out$pull_weights

  # Restrict to the same eligible population as the Monte Carlo
  eligible <- meta |> filter(!starts_above_repl, actual_pull_pa >= 2)

  all_pas <- sort(unique(sim_long$pull_pa))
  n_pas   <- length(all_pas)

  uniform_w <- tibble(pull_pa = all_pas, pull_weight = 1 / n_pas)

  inverted_w <- emp_weights |>
    mutate(
      pull_weight = 1 / pmax(pull_weight, 1e-4),
      pull_weight = pull_weight / sum(pull_weight)
    )

  # ── Apply a weight table and return outing-level diagnostics ────────────────
  apply_weights <- function(weights, scheme_label) {
    opt <- sim_long |>
      select(-any_of(c("pull_weight", "weighted_pull_surplus"))) |>
      left_join(weights, by = "pull_pa") |>
      mutate(
        pull_weight = replace_na(pull_weight, 0),
        w_surplus   = pull_surplus * pull_weight
      ) |>
      group_by(season, game_id, inning_topbot, pitcher_id) |>
      slice_max(w_surplus, n = 1L, with_ties = FALSE) |>
      filter(w_surplus > 0) |>
      select(season, game_id, inning_topbot, pitcher_id, opt_pa = pull_pa) |>
      ungroup()

    eligible |>
      left_join(opt, by = c("season", "game_id", "inning_topbot", "pitcher_id")) |>
      mutate(
        scheme      = scheme_label,
        opt_pa_used = opt_pa,
        pa_delta_s  = actual_pull_pa - coalesce(opt_pa, actual_pull_pa),
        late_pull_s = !is.na(opt_pa) & pa_delta_s > 0
      )
  }

  # Raw: first PA where pull_surplus > 0 — no weight adjustment at all
  raw_opt <- sim_long |>
    filter(pull_surplus > 0) |>
    group_by(season, game_id, inning_topbot, pitcher_id) |>
    slice_min(pull_pa, n = 1L, with_ties = FALSE) |>
    select(season, game_id, inning_topbot, pitcher_id, opt_pa = pull_pa) |>
    ungroup()

  res_raw <- eligible |>
    left_join(raw_opt, by = c("season", "game_id", "inning_topbot", "pitcher_id")) |>
    mutate(
      scheme      = "Raw (first surplus crossing)",
      opt_pa_used = opt_pa,
      pa_delta_s  = actual_pull_pa - coalesce(opt_pa, actual_pull_pa),
      late_pull_s = !is.na(opt_pa) & pa_delta_s > 0
    )

  all_res <- bind_rows(
    res_raw,
    apply_weights(emp_weights, "Empirical (current)"),
    apply_weights(uniform_w,   "Uniform"),
    apply_weights(inverted_w,  "Inverted (down-weight PA 22–29)")
  ) |>
    mutate(scheme = factor(scheme, levels = c(
      "Raw (first surplus crossing)",
      "Uniform",
      "Empirical (current)",
      "Inverted (down-weight PA 22–29)"
    )))

  # ── Summary table ─────────────────────────────────────────────────────────
  summary_tbl <- all_res |>
    group_by(scheme) |>
    summarise(
      n_outings     = n(),
      n_crossover   = sum(!is.na(opt_pa_used)),
      pct_crossover = round(mean(!is.na(opt_pa_used)), 3),
      pct_late      = round(mean(late_pull_s), 3),
      mean_opt_pa   = round(mean(opt_pa_used, na.rm = TRUE), 1),
      mean_pa_delta = round(mean(pa_delta_s[late_pull_s], na.rm = TRUE), 2),
      .groups       = "drop"
    )

  cat("\n=== Weight sensitivity: late-pull diagnostics by scheme ===\n")
  print(as.data.frame(summary_tbl))

  # ── Plot 1: density of optimal pull PA under each scheme ──────────────────
  p_density <- all_res |>
    filter(!is.na(opt_pa_used)) |>
    ggplot(aes(x = opt_pa_used, fill = scheme)) +
    geom_density(alpha = 0.35, adjust = 1.2) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title    = "Distribution of Optimal Pull PA by Weighting Scheme",
      subtitle = "If empirical density is shifted right vs. uniform/raw, weighting is driving the late-pull result",
      x        = "Optimal Pull PA",
      y        = "Density",
      fill     = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  print(p_density)

  # ── Plot 2: pct_late and mean_opt_pa side-by-side ─────────────────────────
  p_compare <- summary_tbl |>
    select(scheme, `% Late Pulls` = pct_late, `Mean Optimal PA` = mean_opt_pa) |>
    pivot_longer(-scheme, names_to = "metric", values_to = "value") |>
    ggplot(aes(x = scheme, y = value, fill = scheme)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = round(value, 2)), vjust = -0.4, size = 3.5) +
    facet_wrap(~metric, scales = "free_y") +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title    = "Pull Diagnostic Sensitivity to Weighting Scheme",
      subtitle = "Large pct_late gap between Empirical and Uniform = weighting artifact",
      x        = NULL,
      y        = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
  print(p_compare)

  # ── Plot 3: empirical pull weight distribution (the prior being imposed) ───
  p_weights <- emp_weights |>
    ggplot(aes(x = pull_pa, y = pull_weight)) +
    geom_col(fill = "#2166ac") +
    labs(
      title    = "Empirical Pull Weight Distribution",
      subtitle = "This is the prior imposed on the surplus — concentration at PA 22–29 mechanically suppresses early optimal pulls",
      x        = "Pull PA",
      y        = "Weight (P(pull exactly at PA k))"
    ) +
    theme_minimal(base_size = 13)
  print(p_weights)

  invisible(list(summary = summary_tbl, data = all_res))
}
