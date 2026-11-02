# USF Keynote Deck — Handoff

Last updated: 2026-11-02 (end of day)

## The talk

- **Venue:** Psychology department keynote, University of South Florida, **Friday Nov 6, 2026**.
- **Length:** ~50 min talk + 10 min questions. Current notes are ~2,700 words (~21 min at
  130 wpm) before the new sections are written, so there is room to add.
- **Deck:** [usf.qmd](usf.qmd) (revealjs only; no book output).
- **Published:** https://connect.posit.cloud/jjcurtin/content/01a19dfd-0622-2035-a06c-e7f7e4b9b915
  (`./publish.sh usf.qmd`; ID now in `_publish.yml`, so republishing updates in place).
- John's working notes on the plan are in [todo.md](todo.md).

## Working agreements (important)

- **John directs every slide edit.** Discuss first; do not change `usf.qmd` unless asked.
  Figure scripts (`figs/`, `mak/`) can be edited when asked.
- **Untitled / title-only slides with notes are intentional** (blank screen while John talks).
- **Add first, cut later.** Keep the AUD material for now; trim after the full draft exists.
- **Communications sensing:** mention only, as a future direction (paper exists; results underwhelming).
- **Optimize trial:** started, but no data/results to show.
- Formatting conventions are documented in [README_formatting.qmd](README_formatting.qmd)
  (rendered to `README_formatting.pdf` via Typst).

## Agreed talk arc (John has inserted placeholder slides for this)

| # | Section | Slides (titles in usf.qmd) | Status |
|---|---|---|---|
| 1 | Motivation, sensing, AUD sample, EMA, week/day/hour auROC | "A Smart Recovery...", "Personal Sensing", "Lapse Prediction for AUD", "Participant Characteristics", "EMA", "Model Predictions", "Model Performance" | Done (existing) |
| 2 | Lagged AUD models | "Lagged Prediction: Looking Farther into the Future" | Placeholder; **figure ready** (`fig_lag_ci`) |
| 3 | AUD calibration | "Model Calibration: Trusting the Probabilities" | Placeholder; chunk currently contains a **copy of the demographics plot** — replace |
| 4 | GPS as passive sensing (+ John's GPS demo slides) + AUD GPS results | "Passive Sensing Methods: Geolocation" (x2) + 2 background-image slides | Concept/demo exist; results slide is placeholder; transition notes say "FIX TRANSITION" |
| 5 | Fairness: AUD problems → OUD improvement | "Algorithmic Fairness", "Next Steps: Algorithmic Fairness" (x3), "Algorithmic Fairness: NIDA Opioid Use Lapses" (x3) | AUD done; **OUD slides out of date** (old numbers/wording) |
| 6 | Global feature importance AUD + OUD, then OUD "most important by day" | "Feature Importance: Looking Under the Hood" (x3) | Placeholders |
| 7 | Personalized recommendations | "Beyond Prediction: Personalized Recommendations" (x3) | Done (AUD SHAP examples) |
| 8 | Smart DTx with native prediction (Insights / Rudra app) | "Smart Digital Therapeutics" | Placeholder (see [rudra.qmd](rudra.qmd)) |
| 9 | Optimize grant | "Next Steps: Optimize System Feedback to Patients" | Exists; wording outdated ("begins in ~1 month") |

## Data sources and key numbers

Research drive is mounted at `~/mnt/standard` (= `studydata`). `format_path("risk/...")`
resolves correctly on this Linux box; a `"studydata/..."` prefix does **not** (see Gotchas).

### AUD EMA models (risk) — in deck
- Cached data in `data/risk/` (committed despite `/data/` in .gitignore).
- auROC: week 0.90, day 0.90, hour 0.93.
- Fairness (day model): race 0.70 vs 0.90 (Δ −0.19), income below vs above poverty
  (Δ −0.13), sex female vs male (Δ −0.04).
- Global SHAP figures already exist but are unused: `fig_global_all/day/hour` in
  `figs/risk/figs_ema_shaps.R` (top day-level features: past use, future efficacy, craving).

### AUD lagged models — `~/github/study_lag`, drive `risk/models/lag/`
- **Updated today.** `mak/risk/mak_lag_ci.R` now reads `pp_perf_tibble.csv` (same file the
  paper uses). Old code kept commented out; old cache moved to `data/risk/lag_ci_old.csv`.
- auROC: no lag 0.91 [0.90, 0.92], 1 day 0.89, 3 day 0.88, 1 week 0.87, 2 week 0.85 [0.83, 0.87].
  Every lag credibly worse than no lag (Δ −0.02 to −0.06) and each adjacent step credibly worse
  (`contrast_baseline.csv`, `contrast_adjacent.csv`).
- No-lag 0.91 now agrees with the day model (0.90) on earlier slides.
- The lag paper also has a **calibration figure** (Fig 3: raw vs Platt, no lag vs 2-week) built
  from `final_preds_kfold_1_x_5_1day_{0,336}_v3_strat_lh_final.rds` — a newer AUD model than the
  deck's `fig_cal`.

### AUD calibration — two candidates (undecided)
- `fig_cal` in `figs/risk/figs_ema_probability.R`: original EMA paper next-day predictions,
  beta-calibrated (`prob_beta`), green line, 0.1 bins.
- Lag paper Fig 3 (see above): see `~/github/study_lag/notebooks/mak_figures.qmd` (chunk after `fig-pp`, label `fig-cal`).

### AUD GPS model — `~/github/study_gps` (current draft is `index_2026_0917.qmd`, not `index.qmd`), drive `risk/models/gps/`
- N = 146; next-day lapse; geolocation + demographics only; 7.4% lapse base rate.
- auROC 0.72 [0.69, 0.74] vs demographics-only 0.52 (`pp_all_tibble.csv`).
- Top features: time at self-rated medium/high/no-risk locations, places previously drank.
  Income top demographic (~1/3 as important).
- Fairness differs from EMA model: worse for age 55+ (Δ 0.08), better for lower education;
  race/sex/poverty not credible. Recommend **not** showing GPS fairness (muddies the AUD→OUD story).
- Probabilities cluster near base rate (narrow range).
- No combined EMA+GPS model in this paper.

### OUD (NIDA risk2) — `~/github/study_risk2` (`index.qmd`), drive `risk2/models/combined/` (v13 files)
- 330 enrolled, **296 analyzed**, 47 states, Apr 2021–Dec 2024, on MOUD 1–12 months;
  one daily survey + geolocation with monthly location-context questions; up to ~1 year.
- Mean 278 days on study; 77.8% daily survey adherence; 78% stayed ≥6 months.
- 36% lapsed; 1.8% of 82,156 day labels positive.
- auROC **0.94 [0.93, 0.95]**, stable 1–18+ months in recovery (Fig S1). Brier 0.014.
- Ablation: no daily survey 0.63; no geolocation 0.94 (no loss); demographics only 0.55.
- Fairness: 5 subgroups (gender, race/ethnicity, income at **$25k**, sexual orientation,
  rural/urban), all 0.92–0.95; income gap favors lower-income group.
- Feature importance: mean |SHAP| → past opioid use (2x next); proportion-of-days-most-important →
  urge and abstinence confidence first; location features in top 10.
- Surprising patterns: recent *decreases* in stimulant use or urges raise risk.
- Local SHAP example image: `~/github/study_risk2/figures/shapley_local.png`.
- Figure code: `~/github/study_risk2/notebooks/mak_figures.qmd` (fig-1 fairness, fig-2 calibration,
  fig-3a importance, fig-4a partial dependence). Files read: `pp_perf_tibble_v13.csv`,
  `preds_1_x_5_day_v13_kfold_full_final.rds`, `shapsgrp_1_x_5_day_v13_kfold_full_final_raw.rds`, etc.
- **Deck's OUD fairness data are stale:** `data/risk2/auroc_fair_ci.csv` is an earlier model
  (3 subgroups, poverty split, "other"). Needs a new `mak/risk2` + `figs/risk2` script from v13.
- Paper issue to mention to John/Kendra: `notebooks/mak_tables.qmd` builds Table 2 from
  `contrast_ablate_v10.csv` though `v13` exists.

### Framing cautions discussed
- **GPS story:** "lower burden + different, actionable risk factors," not "better prediction."
  Payoff lands in feature-importance section (location features in OUD top 10).
- **Fairness improvement** can't be credited to representation alone (OUD study differs in
  substance, recruitment, MOUD, 1 EMA/day, duration, GPS). Notes should hedge.

## Done today

- Fixed unclosed `<!--Blank` comment in usf.qmd (was hiding `:::` fences on a SHAP slide).
- Replaced all `devtools::source_url()` with base `source()` in figs/ and mak/ (12 files).
- Removed deprecated ggplot2 `%+%` in `cm_mosaic()` (figs_ema_probability.R, figs_dev_probability.R).
- Lag data updated (above); `figs_lag_ci.R`: fixed `linewitdh` typo → `linewidth = 1.5`,
  `case_match()` → `recode_values()`.
- auROC figures (ema_probability, lag, fairema, risk2 fair): "perfect" label now just above its
  line (y = 1.01, y-axis limit 1.05), "random" just below; both start at x = 0.5 (hjust = 0).
  Not applied to `figs/risk2/figs_dev_probability.R` (unused).
- AUD fairness Income labels wrapped ("above\npoverty"); `align = "h"` added to the AUD fairness
  `plot_grid()` in usf.qmd.
- Wrote README_formatting.qmd (Typst PDF). First draft published to Connect Cloud.

## Next steps (in rough order; John will direct)

- [ ] Lag slide(s): add `fig_lag_ci` (source `figs/risk/figs_lag_ci.R`) + rationale/notes.
- [ ] Calibration slide: choose `fig_cal` vs lag-paper calibration; replace the copied
      demographics chunk; possibly brief OUD calibration later.
- [ ] GPS results slide: new figure from `risk/models/gps/pp_all_tibble.csv` (GPS vs demographics),
      maybe top features; rewrite GPS transition notes; mention communications as future.
- [ ] OUD section: update design/sample slide (296, 47 states, MOUD, 1 survey/day, GPS + context),
      performance 0.94, new 5-subgroup fairness figure from v13; rewrite outdated notes
      ("midst of finalizing", "~300").
- [ ] Feature importance: AUD global (`figs_ema_shaps.R`), OUD global (two methods), OUD by-day.
- [ ] Optional: OUD local SHAP example; Smart DTx slide; update Optimize slide wording.
- [ ] Housekeeping (when John says): YAML date still "March 5th, 2026" → "November 6, 2026";
      typos ("Communcations", "reasonble", "charactersitics", "martital", "ethinic", "lessor");
      notes referencing "after this symposium ends... 1 pm".
- [ ] Re-estimate timing once sections are drafted; then decide AUD cuts.

## Gotchas

- **Figure-script dependencies:** each `figs_*.R` is sourced in the first chunk that uses it;
  later chunks reuse the objects (e.g., `fig_race_bar` from `figs_ema_demographics.R`,
  `fig_shap_ex*` from `figs/opt/figs_opt_shaps.R`). Reordering slides can break this.
- **`format_path()` prefixes:** most `mak/` scripts still use `"studydata/risk/..."`, which resolves
  to a nonexistent `~/mnt/standard/studydata/...` on Linux. Works only because cached data exist.
  Use `"risk/..."` / `"risk2/..."` for any new or regenerated mak scripts.
- **Editor-open files:** when a file is open in RStudio, edits land in the editor buffer and must
  be saved before `source()` sees them. The read/edit tools also timed out while a render was
  running.
- `publish.sh` deletes every `*.html` and `*_files/` in the root.
- Render needs network (GitHub `format_path.R`, image URLs, dionysus GPS backgrounds) — accepted.

## CSS / formatting architecture (from October refactor)

- `_quarto_uw.yml` loads `[presentation.css, utilities.css]`. `presentation.css` = theme;
  `utilities.css` = opt-in classes (`.uwred/.blue/.gray/...`, `.gap-*`, `.hl`,
  `.callout-corner`, `.callout-wide`). Details in README_formatting.qmd.
- **book_git was never migrated:** the October handoff claimed `~/github/book_git` also got
  `utilities.css` and stripped theme files, but it did not (no `utilities.css`; configs load a
  single CSS; old utility classes remain). No book_git .qmd uses the removed classes, so the
  migration is safe to redo later. Deferred until after the talk.
- Archived 2024 decks reference `presentation.css` directly and would lose utility classes if
  republished.
