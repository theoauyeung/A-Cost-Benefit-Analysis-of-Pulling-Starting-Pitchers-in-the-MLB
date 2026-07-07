# STAT 490 — Starting Pitcher Pull Timing

Research project evaluating whether MLB managers optimally time starting pitcher removal. The pipeline estimates a within-game fatigue model from Statcast pitch data and runs a Monte Carlo simulation comparing the starter's projected xwOBA against the bullpen replacement level at every plate appearance.

Data covers 2015–2025 MLB seasons (2020 excluded).

---

## Script Execution Order

```
data_load.R          ← one-time: download raw Statcast data
stuff_scores.R       ← one-time: compute Stuff+ scores
build_features.R     ← main feature pipeline (run first in every session)
load_model_data.R    ← loads model-ready data (sourced automatically)
generate_results.R   ← fatigue model + diagnostic plots
simulate_pull.R      ← Monte Carlo pull-decision simulation
inspect_sim.R        ← diagnostic inspection of simulation output
```

---

## Scripts

### `data_load.R`
**Run once.** Downloads pitch-by-pitch Statcast data from Baseball Savant for 2015–2019 and 2021–2025 via `sabRmetrics::download_baseballsavant()`, writes one CSV per season to `data/`, then concatenates them into `data/savant_data.csv`.

- Uses parallel processing (`parallel::makeCluster`) — runtime is ~30–60 min depending on connection speed.
- Output: `data/savant_data.csv` (~several GB). All downstream scripts read from this file.
- Re-running this script overwrites the CSVs. Do not run again unless refreshing raw data.

---

### `stuff_scores.R`
**Run once.** Computes Stuff+ scores for every pitch and writes `data/stuff_scores.csv`. This file is joined into Stage 1 of the feature pipeline.

- Must be run before `build_features.R` if `data/stuff_scores.csv` is absent.

---

### `build_features.R`
**Source this at the start of every session.** The main feature engineering pipeline. Does not run automatically on `source()` — call `build_all()` to execute.

```r
source("build_features.R")
build_all()
```

#### Checkpoint system
Each of the five stages saves an `.rds` checkpoint to `data/`. On subsequent runs the stage short-circuits (loads from disk) if its checkpoint exists. Delete a checkpoint to force a rebuild from that stage forward.

| Stage | Checkpoint | Description |
|---|---|---|
| 1 | `clean_data.rds` | Raw Statcast + pitch physics (IVB, HB, VAA, SSW, Bauer units, arm angle) + starter flag + name lookups |
| 2 | `starter_pa_raw.rds` | PA-level aggregation for starters; cumulative pitch count |
| 3 | `starter_pa_est.rds` + `reliever_rolling.rds` + `prior_n_sp.rds` | Bayesian rolling xwOBA estimates for pitchers, batters, matchups, relievers. Optimal prior and cross-season decay selected by MSE grid search. |
| 4 | `starter_pa_full.rds` + `ingame_curve.rds` | Pure-starter filter; pull flags; velocity features; in-game Bayesian outing deviation; team reliever quality |
| 5 | `full_data.rds` | All pitch rows with PA-level features broadcast from `starter_pa_full` |

#### Invalidation
Use `invalidate()` to delete a checkpoint and all downstream ones before rebuilding:

```r
source("build_features.R")
invalidate("starter_pa_full")   # deletes starter_pa_full.rds, ingame_curve.rds, full_data.rds, model_data.rds
build_all()
```

```r
invalidate("starter_pa_est")    # also clears reliever_rolling.rds and prior_n_sp.rds
invalidate("all")               # wipes every checkpoint
```

**When to invalidate which stage:**

| Change made | Invalidate from |
|---|---|
| `savant_data.csv` updated | `"all"` |
| `stuff_scores.csv` updated | `"all"` |
| PA aggregation logic | `"starter_pa_raw"` |
| Bayesian prior grid, decay grid, matchup prior | `"starter_pa_est"` |
| Pure-starter thresholds, velocity constants, pull flag logic, GAM curve, `ingame_deviation` | `"starter_pa_full"` |

#### Key constants (all in `build_features.R`)

| Constant | Default | Controls |
|---|---|---|
| `VELO_ROLL_WINDOW` | 14 | Lookback pitches for rolling velocity deviation |
| `VELO_BASELINE_MIN_N` | 25 | Min season pitches per category for a valid baseline |
| `INGAME_BASELINE_PITCHES` | 30 | First N Fastball pitches defining within-game peak |
| `INGAME_BASELINE_NONFAST` | 15 | First N Breaking/Offspeed pitches for within-game baseline |
| `PURE_STARTER_MIN_GS` | 20 | Min starts for pure-starter filter |
| `PURE_STARTER_MIN_INN` | 4 | Min median max inning for pure-starter filter |
| `INGAME_CURVE_K` | 20 | GAM basis dimension for the in-game warm-up curve |

Changing any of these invalidates `starter_pa_full` and everything downstream.

#### In-game Bayesian outing deviation (`ingame_deviation`)
Stage 4 fits a GAM warm-up curve `μ(k)` — the population-average expected xwOBA at PA position k — and for each PA computes a Bayesian posterior of the outing-level deviation from the pitcher's season prior. The prior weight equals `prior_n_sp + PA outcomes before this outing in the season`, so early-season starters update faster. `ingame_deviation(1) = 0` for every outing by construction. This replaces the former 4-PA trailing sum (`recent_xwoba_game`) as the in-game performance signal in the fatigue model.

---

### `load_model_data.R`
**Do not run directly — sourced automatically by `simulate_pull.R` and `generate_results.R`.** Reads `starter_pa_full.rds`, applies z-score standardization, adds game-state variables, and caches to `model_data.rds`.

```r
source("build_features.R")
source("load_model_data.R")
model_data <- load_model_data()
```

- Caches result to `model_data.rds`. Deleted automatically by `invalidate("starter_pa_full")`.
- Z-scored predictors produced: `velo_dev_z`, `velo_dev_ingame_z`, `batter_xwoba_z`, `pitcher_xwoba_z`, `ingame_dev_z`, `pc_z`.

---

### `generate_results.R`
**Fits the fatigue model and produces diagnostic plots.** Sources `load_model_data.R` internally.

```r
source("generate_results.R")
```

- Requires `starter_pa_full.rds`, `full_data.rds`, `reliever_rolling.rds`, and `model_data.rds` to exist (run `build_all()` first).
- Writes PNG plots to `plots/`.
- The fatigue model spec:

```r
feols(xwoba_outcome ~ velo_dev_z + velo_dev_ingame_z + tto_fac +
        batter_xwoba_z + ingame_dev_z | pitcher_id,
      data = fatigue_data, cluster = ~pitcher_id)
```

- `pc_z` (pitch count) is excluded — drops out by BIC (ΔBIC ≈ 1228) once `velo_dev_ingame_z` is present.
- Time-through-order is a factor (`tto_fac`; reference = 1st TTO).

---

### `simulate_pull.R`
**Main simulation.** Runs the pull-decision Monte Carlo over every eligible outing in the sample.

```r
source("build_features.R")
source("load_model_data.R")
source("simulate_pull.R")
out <- simulate_pull()
```

- `N_BASE = 100000` (baseline PA simulations), `N_SIM = 500` (Monte Carlo reps per outing). Reduce for faster iteration during development.
- Returns a list with `$sim` (PA-level results), `$outing_meta`, `$pull_weights`, `$fatigue_model`, `$sigma_outing`.
- **Exponential tilting:** constructs per-PA outcome distributions matching a target (xwOBA, K-rate) pair. A 2D tilt cache is pre-computed over the xwOBA grid × pitcher K-rate levels before the main loop — this takes several minutes.
- **Starter side:** full Monte Carlo via vectorized prefix sums.
- **Reliever side:** CLT approximation — `N(n · adj_rel, n · σ²_rel)`.
- **Weighted optimal pull PA:** pull surplus at each PA is weighted by empirical P(manager pulls exactly at PA k). `stress_test_weights(out)` checks sensitivity to this choice.
- Prints a BIC comparison against the prior `recent_xwoba_z` spec on each run.

#### Run weight sensitivity stress test
```r
out  <- simulate_pull()
sens <- stress_test_weights(out)
```

---

### `inspect_sim.R`
**Diagnostic inspection of simulation output.** Run after `simulate_pull()`. Prints 13 diagnostic sections: model coefficients, outing coverage, pull surplus distribution by PA, strain effect, pitcher-level and season-level aggregates.

```r
source("inspect_sim.R")   # runs simulate_pull() internally with N_SIM = 150
```

- Saves simulation output to `data/simulate_pull_out.rds`.
- Uses `recent_xwoba_z` references in comments that predate the `ingame_dev_z` rename — these are cosmetic only.

---

## Package Dependencies

```r
install.packages(c(
  "tidyverse", "data.table", "baseballr", "slider", "fixest",
  "mgcv", "ggthemes", "gt", "sabRmetrics"
))
```

`sabRmetrics` is required only for `data_load.R` (initial data download).

---

## Data Directory (`data/`)

Not tracked in git. Required files for running the pipeline from Stage 1:

| File | Source |
|---|---|
| `savant_data.csv` | Generated by `data_load.R` |
| `stuff_scores.csv` | Generated by `stuff_scores.R` |
| `count_transitions.csv` | Manual — pitch count transition probabilities |
| `linear_weight.csv` | Manual — linear weights by event type |

All `.rds` checkpoint files are generated by `build_features.R` and `load_model_data.R`.
