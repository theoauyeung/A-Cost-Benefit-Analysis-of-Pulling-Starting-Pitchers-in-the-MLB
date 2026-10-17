# Results Section Outline

---

## Overall framing

The results section has one primary claim: **managers systematically leave starters in too long.** The fatigue model and Bayesian estimates are covered in prior sections (Methods/Data); this section uses them as inputs and focuses entirely on what the simulation reveals about managerial behavior.

The section moves from aggregate finding → magnitude of the error → when it happens → whether it has changed over time → whether it holds under alternative assumptions → which pitchers and teams drive the pattern.

---

## Section structure

### 4.1 Aggregate Pull Timing: The Main Finding

**Lead with the headline.** Under the primary specification (min_surplus = 0.005, empirical pull-time weights):

- 13,155 eligible outings total (starters who began better than the reliever baseline, ≥ 2 PA)
- 9,371 (71.2%) have a model-identified optimal pull point (crossover detected)
- Of those 9,371, **64% are classified as late pulls** (actual pull PA > model-optimal pull PA)
- Median PA delta for late-pull outings: **2 PA**; mean: **2.82 PA**
- Mean model-optimal pull PA: **22.7**; mean actual pull PA: **23.7**
- r = **0.715** between model-optimal and actual pull PA

**Two ways to state the headline — be explicit about which you use:**

The 64% figure conditions on outings where a crossover was identifiable (the right reference class for asking "when managers faced a clear pull signal, how often did they wait too long?"). The unconditional rate is ~46% of all eligible outings (5,997 / 13,155). Both are valid; the 64% is the cleaner claim and is what the scatter plot subtitle reports. Use 64% as the primary headline and report the unconditional ~46% in a footnote or parenthetical so readers can reconstruct it.

The r = 0.715 is important context: managers and the model agree on *ordering* (who gets pulled early vs. late across the sample), just not on exact timing. Systematic late pulling is not random noise.

**The 1-PA mean gap (22.7 vs. 23.7) sounds small** — the PA delta histogram (median = 2, but heavy right tail with meaningful mass at 5–10 PA) tells a more complete story than the means. Lead with the histogram for conveying magnitude.

**Figures (main text):**
- `scatter_actual_vs_optimal.png` — **primary figure.** Dense red mass above the 45° line makes the directional bias visually immediate. No early-pull cluster, confirming the bias is one-directional.
- `late_pull_pa_delta.png` — distribution of how late, for late-pull outings. Median = 2, right tail communicates that some outings are dramatically late (10+ PA).

**Figure (supporting or panel):**
- `opt_pull_pa_hist.png` — distribution of model-optimal pull PA with mean optimal (22.7) and mean actual (23.7) marked. Best used as a panel alongside the scatter rather than standalone, because the means are close enough that the histogram alone undersells the finding.

---

### 4.2 When Does the Model Say Pull?

**One paragraph.** The inning distribution of model-optimal pull PAs grounds the abstract PA numbers in something intuitive.

Key numbers from `opt_pull_inning.png`:
- 39.6% of optimal pull points fall in the **6th inning**
- 28.7% in the **5th**, 20.0% in the **7th**
- Very little action before the 4th or after the 8th

This is squarely in the conventional decision zone — validation that the simulation isn't recommending unusual pull points, and a useful sanity check to present before the robustness section.

**Figure:** `opt_pull_inning.png`

---

### 4.3 Temporal Stability

**What to write.** The late-pull rate has been relatively flat across 2015–2025, declining modestly from ~50–51% (2015, 2017) to ~43% (2025). The crossover rate has stayed in the 65–77% range throughout.

Two things to address:
1. **No sharp break in the analytics/opener era (2018–2019).** Despite widespread changes in bullpen usage strategy, the model-identified late-pull rate does not show a meaningful step change. Either managers haven't systematically improved on the timing dimension, or the gains in bullpen usage came through other channels (more relievers per game, not better starter exit timing).
2. **The trend is noisy.** Do not over-claim a secular decline. The ~7–8 percentage point drop from 2015–2017 to recent years is real but not monotonic.

**[OPEN: External narrative — see remaining questions]**

**Figure:** `season_late_pull_trend.png`

---

### 4.4 Robustness

**Two distinct robustness checks:**

#### Weight and threshold sensitivity

The empirical pull-time weights and the min_surplus threshold are modeling choices. The headline finding holds under alternatives.

**Weight sensitivity (from `stress_test_weights()`):**

| Scheme | % Crossover | % Late Pulls | Mean Opt PA | Mean Δ PA |
|---|---|---|---|---|
| Raw (first surplus > 0) | 84.5% | 81.8% | 8.2 | 16.1 |
| Uniform | 43.8% | 43.8% | 9.9 | 14.4 |
| **Empirical (primary)** | **71.2%** | **~46% all / 64% crossover** | **22.7** | **2.82** |
| Inverted | 37.2% | 37.2% | 3.7 | 20.1 |

Key comparison: **Uniform vs. Empirical.** The weighting scheme shifts which PA is called optimal, but the *direction* of the late-pull finding holds under both. Raw is inflated — it picks up early random surplus fluctuations as crossovers, which is why down-weighting early PAs matters.

**Threshold sensitivity.** pct_late declines as min_surplus rises, but mean_pa_delta stays stable at ~2.8 PA. Raising the bar for "clearly optimal" reduces how often something is called a late pull; the *size* of late pulls when they exist does not shrink.

**Presentation.** A single summary table in the main text. Density plots and bar charts from `stress_test_weights` go in an appendix.

#### Out-of-sample backtest

`backtest.R` implements a train (2015–2021) / test (2022–2025) split. The fatigue model is fit on pre-2022 data only and applied to held-out seasons. Test-set late-pull rates from `backtest_by_season.png`:

- 2022: 30%, 2023: 34%, 2024: 32%, 2025: 31%

These are lower than the in-sample 64% rate, which is expected — the model is fit to the training data and will identify crossovers more sharply in-sample. The more important result is that the directional finding (systematic late pulling) holds out-of-sample across all four held-out seasons, consistently in the 30–34% range. The stability year-to-year in the test set (`backtest_by_season.png`) confirms the finding is not driven by a single season.

**Figures:** `backtest_by_season.png` (main text or appendix); `backtest_policy_surplus.png` (appendix — diagnostic).

---

### 4.5 Pitcher Heterogeneity

**Key question.** Is systematic late pulling concentrated in certain pitchers, or is it uniform across the population? If "horse" veterans are pulled late far more often than newer arms, the behavioral story becomes more specific.

**What to show.** A dotplot of per-pitcher late-pull rate (Wilson 95% CI), filtered to pitchers with ≥ 30 crossover outings, sorted by rate. The dashed vertical line marks the sample mean.

**What to look for in the narrative:**
- Width of the distribution: if most pitchers cluster near the mean, late pulling is a general tendency. If the distribution is wide, some pitchers are dramatically more likely to be held too long.
- Identify whether the highest late-pull rates are concentrated among known workhorses / ace-type pitchers vs. no obvious pattern.
- The CI width reflects sample size — acknowledge that pitchers at the extremes with fewer crossover outings may be driven by small-sample noise.

**Figure:** `pitcher_lp_dotplot.png` (generated by new block in `generate_results.R`; tall plot, all qualifying pitchers on y-axis)

---

### 4.6 Team Analysis (deferred)

**What this will cover.** Which franchises have been consistently late vs. on time across the 2015–2025 sample? Is there franchise-level persistence (teams that are late in 2015 are still late in 2025), or is late pulling a league-wide pattern with no franchise signal?

**Data setup.** `team_pull_summary.rds` (generated by new block in `generate_results.R`) contains:
- `team_season`: per-team per-season counts, crossover rate, late-pull rate, mean PA delta
- `team_overall`: aggregated across all seasons per team, sorted by pct_late

**Visualization deferred.** No plot in this version — data is ready for a future heatmap (teams × seasons, late-pull rate) or dotplot when the writing reaches this subsection.

---

## Plot inventory

| Plot | Section | Priority | Notes |
|---|---|---|---|
| `scatter_actual_vs_optimal.png` | 4.1 | **Must include** | Primary figure; makes the finding immediate |
| `late_pull_pa_delta.png` | 4.1 | **Must include** | Shows magnitude and right-tail severity |
| `opt_pull_pa_hist.png` | 4.1 | Include | Best as panel with scatter |
| `opt_pull_inning.png` | 4.2 | Include | Sanity check / grounding |
| `season_late_pull_trend.png` | 4.3 | Include | Keep if narrative peg exists |
| `pitcher_lp_dotplot.png` | 4.5 | Include | Tall plot; needs enough qualifying pitchers |
| `backtest_by_season.png` | 4.4 | Include | Out-of-sample validation |
| `backtest_policy_surplus.png` | 4.4 | Appendix | Diagnostic |
| `starter_bullpen_crossover.png` | Prior section | Prior section | Methods/intro to simulation |
| `ingame_dev_pull_comparison.png` | — | Cut | Near-null result; removed from section |
| `ingame_dev_at_opt_pull.png` | — | Cut | Cut with above |

---

## Remaining open questions

**Q4 — Season trend narrative.**
The late-pull rate is flat to slightly declining across 2015–2025. Is there an external story to attach — analytics adoption, the 2018–2019 bullpen revolution, COVID 2020 gap, Statcast-era changes? Without a narrative peg, the trend plot is "interesting but inconclusive" and risks being a weak standalone. Consider whether it belongs in the main text or as a supplementary figure.

**Q5 — Run-value translation.**
Do you want to translate the PA delta (median 2, mean 2.82) into a run cost or win-probability number? Even a rough estimate ("X additional PAs against a fatigued starter costs approximately Y runs per team per season") would give the finding practical stakes. Or stay in xwOBA/PA units throughout and leave cost estimation for future work.

---

## Length guidance

For a thesis (simulation results only, fatigue model and Bayesian estimates already written):

- 4.1 Main finding: 2 pages (scatter + delta histogram + prose)
- 4.2 When to pull: 0.5 page
- 4.3 Temporal stability: 1 page
- 4.4 Robustness: 1.5 pages (table + backtest paragraph)
- 4.5 Pitcher heterogeneity: 1–1.5 pages
- 4.6 Team analysis: deferred (placeholder paragraph pointing to data)

**Total: ~7–8 pages including figures.**
