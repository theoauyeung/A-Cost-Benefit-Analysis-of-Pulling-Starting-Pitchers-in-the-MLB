# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

STAT 490 research project evaluating whether MLB managers optimally time starting pitcher removal. The pipeline estimates a within-game fatigue model from Statcast pitch data and runs a Monte Carlo simulation comparing the starter's projected xwOBA against the strain-adjusted bullpen replacement level at every plate appearance.

Data covers 2015–2025 MLB seasons (2020 excluded). All data lives in `data/`.

---

## Running the pipeline

**Standard entry point** — source `build_features.R`, then call the orchestrator:

```r
source("build_features.R")
build_all()
```

**Fit the fatigue model and produce plots:**

```r
source("generate_results.R")
```

**Run the pull-decision simulation:**

```r
source("simulate_pull.R")
out <- simulate_pull()
```

**Run weight-sensitivity stress test** (requires prior `simulate_pull()` output):

```r
sens <- stress_test_weights(out)
```

**Invalidate and rebuild from a specific stage:**

```r
source("build_features.R")
invalidate("starter_pa_full")   # deletes starter_pa_full.rds, model_data.rds, full_data.rds
build_all()
```

`invalidate("all")` wipes every checkpoint. `invalidate("starter_pa_est")` also clears `reliever_rolling.rds`.

**Load model-ready data from any script:**

```r
source("build_features.R")
source("load_model_data.R")
model_data <- load_model_data()
```

---

## Pipeline architecture

The feature pipeline (`build_features.R`) runs in five stages. Each stage short-circuits if its `.rds` checkpoint already exists. Delete the checkpoint file (and all downstream ones) to force a rebuild.

| Stage | Checkpoint | Description |
|---|---|---|
| 1 | `clean_data.rds` | Raw Statcast + pitch physics features (IVB, HB, VAA, SSW, Bauer units, arm angle) + starter flag + name lookups via `chadwick_player_lu()` |
| 2 | `starter_pa_raw.rds` | PA-level aggregation for starters; cumulative pitch count within game |
| 3 | `starter_pa_est.rds` + `reliever_rolling.rds` | Bayesian rolling xwOBA estimates for pitchers, batters, matchups, and relievers. Optimal prior_n and cross-season decay are selected by MSE grid search. |
| 4 | `starter_pa_full.rds` | Pure-starter filter (≥20 GS, 0 relief appearances, median max inning ≥4); pull flags; velocity features; in-game performance; team reliever quality |
| 5 | `full_data.rds` | All pitch rows with PA-level features broadcast from `starter_pa_full` |

`load_model_data.R` reads `starter_pa_full.rds`, applies z-score standardization, adds game-state variables, and caches to `model_data.rds`.

---

## Key design decisions

### Bayesian rolling estimates (Stage 3)

Pitcher and batter quality are modeled as a running posterior mean initialized at the league-average xwOBA with prior weight `prior_n` (optimized by grid search over {50, 100, 150, 200, 300, 400, 500} PA). At season transitions the posterior is carried forward with a decay factor (`season_decay`, optimized over {0.0, 0.1, …, 1.0}). The 2019→2021 gap is handled explicitly by duplicating the 2019 posterior into 2021. Matchup estimates blend the batter rolling estimate with head-to-head history using a fixed 20-PA shrinkage prior.

### Velocity features (Stage 4)

Two independent velocity deviation terms are constructed:

- **`velo_dev_pa`** (season baseline): rolling mean of `release_speed − season_mean_by_pitch_category` over the last `VELO_ROLL_WINDOW` (14) pitches, averaged over the PA. Fastball and offspeed categories are kept separate to avoid pitch-mix confounding.
- **`velo_dev_ingame_pa`** (within-game baseline): for each pitch, deviation from that pitch category's early-outing mean velocity in this outing. Fastball baseline uses the first `INGAME_BASELINE_PITCHES` (30) pitches; Breaking and Offspeed use the first `INGAME_BASELINE_NONFAST` (15) pitches. Falls back to the season category average when the outing count is below threshold. Averaged across all pitches in the PA — no fastball restriction, so all PAs contribute a signal. Pitch-mix confounding is handled by normalising within category rather than filtering to fastballs.

Both terms are z-scored in `load_model_data.R` before entering the model.

### Fatigue model

Estimated with `fixest::feols`, pitcher fixed effects, SEs clustered by pitcher:

```r
feols(xwoba_outcome ~ velo_dev_z + velo_dev_ingame_z + tto_fac +
        batter_xwoba_z + recent_xwoba_z | pitcher_id,
      data = fatigue_data, cluster = ~pitcher_id)
```

Pitch count (`pc_z`) is excluded once `velo_dev_ingame_z` is present — pitch count drops out by BIC (ΔBIC ≈ 1228). Time-through-order is a factor (`tto_fac`; reference = 1st TTO).

### Simulation (`simulate_pull.R`)

`simulate_pull()` runs a Monte Carlo over every outing in the eligible sample:

1. **Exponential tilting** constructs a per-PA outcome distribution matching a target (xwOBA, K-rate) pair. The 2D tilt (linear-weights dimension + strikeout dimension) is solved via `uniroot`. A cache of tilted distributions is pre-computed over the xwOBA grid × pitcher K-rate levels before the main loop.
2. **Starter side**: full Monte Carlo — pre-samples an N_SIM × T linear-weight matrix for each outing, then uses prefix sums to evaluate every pull PA without re-sampling.
3. **Reliever side**: CLT approximation — window sum is `N(n · adj_rel, n · σ²_rel)`. `adj_rel` increases by `strain_rate` for each extra bullpen PA vs. the actual pull point.
4. **Weighted optimal pull PA**: pull surplus at each PA is weighted by the empirical P(manager pulls exactly at PA k) to avoid flagging pull points where no manager would actually pull. `stress_test_weights()` checks sensitivity to this choice.

---

## Key constants (all in `build_features.R`)

| Constant | Default | Controls |
|---|---|---|
| `VELO_ROLL_WINDOW` | 14 | Lookback pitches for rolling velo deviation |
| `VELO_BASELINE_MIN_N` | 25 | Min season pitches per category for a valid baseline |
| `INGAME_BASELINE_PITCHES` | 30 | First N Fastball pitches defining within-game peak |
| `INGAME_BASELINE_NONFAST` | 15 | First N Breaking/Offspeed pitches for within-game baseline |
| `PURE_STARTER_MIN_GS` | 20 | Min starts for pure-starter filter |
| `PURE_STARTER_MIN_INN` | 4 | Min median max inning for pure-starter filter |

Changing any of these invalidates `starter_pa_full` downstream.
