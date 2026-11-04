# USF Keynote Deck — Handoff

Last updated: 2026-11-03 (late evening session; second Linux machine)

## Where we left off (2026-11-03, evening)

- **GPS section done** (5 slides, notes drafted; John reviewed and approved). See GPS section.
- **Feature importance section drafted** (AUD x2, OUD x2). Rendered as revealjs and checked.
- **OPEN ISSUE, decide first next session:** the risk-split check (late in session) shows the
  "share of days as top feature" view is dominated by **low-risk days** where the winning SHAP is
  tiny. On higher-risk days, past use is almost always the top feature (AUD 66–77%, OUD 87–100%)
  and location features essentially never are. So the current **second OUD feature-importance
  slide is misleading** (bullet/notes: "a location feature is the top risk factor on about 30% of
  days … most useful thing we could tell someone"). John will decide how to integrate this into a
  coherent story. Assistant's suggested next analysis: top feature on higher-risk days
  **excluding past use** (the modifiable factor a recommendation engine would act on), AUD + OUD.
- Still open from before: lag slide notes (verbatim neurotech; see Lag section), personalized
  recommendations transition (now should refer back to SHAP), Smart DTx, Optimize wording, timing.
- **Uncommitted work:** new scripts (`figs/risk/figs_gps_ci.R`, `mak/risk/mak_gps_ci.R`,
  `figs/risk2/figs_shaps.R`, `mak/risk2/mak_shaps.R`) are untracked, and new caches in `data/` are
  gitignored — commit them with `git add -f data/...` (see Feature importance section for the list).

## Publishing — FIXED 2026-11-03 (this machine)

- System Quarto upgraded to **1.11.9** (`.deb` in `/opt/quarto`; `/usr/local/bin/quarto` links
  to it), so the shell `quarto` supports `posit-connect-cloud` and `./publish.sh usf.qmd` works as-is.
- Silent exit-0 cause was **no stored Quarto login**: Quarto's Connect Cloud token lives in
  `~/.local/share/quarto/publish/accounts/posit-connect-cloud/accounts.json`. Without it, Quarto
  shows an authorize prompt that a non-interactive shell can't answer, and exits silently.
  An RStudio/rsconnect login (`~/.config/R/rsconnect/...`) does NOT count for `quarto publish`.
- John authorized in the RStudio Terminal; publishing works. The first run hit a missing R package
  (John installing). Not yet tested whether the assistant's bash tool can publish now that the
  token is stored.
- **Other machines:** if publishing exits silently, check Quarto version (needs ≥1.9) and that
  `accounts.json` exists; if not, run `./publish.sh` once in a real terminal to authorize.

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
| 2 | AUD calibration | "Model Calibration: Trusting the Probabilities" | **Drafted 2026-11-03** (`fig_cal`, bullets, notes); John to review bullets/notes |
| 3 | Lagged AUD models (now **after** calibration; John reordered) | "Lagged Prediction: Advanced Warning" (x2 build) | Slides + figure in; **notes still need revision** |
| 4 | GPS as passive sensing (+ John's GPS demo slides) + AUD GPS results | "Passive Sensing Methods: Geolocation" (title-only) → 2 untitled background-image demo slides → "Geolocation: Lapse Prediction for AUD" (x3: title-only implementation, then 2-slide figure build) | **Done 2026-11-03 evening**; John approved |
| 5 | Fairness: AUD problems → OUD improvement | "Algorithmic Fairness", "Next Steps: Algorithmic Fairness" (x3), "Algorithmic Fairness: NIDA Opioid Use Lapses" (x3) | AUD done; OUD design, performance, and fairness slides **updated 2026-11-03** (v13) |
| 6 | Global feature importance AUD + OUD, then OUD "most important by day" | "Feature Importance: Looking Under the Hood" (x4: AUD 2-slide build, OUD global, OUD share of days) | **Drafted 2026-11-03 evening**; OUD share-of-days slide needs rethinking (see top) |
| 7 | Personalized recommendations | "Beyond Prediction: Personalized Recommendations" (x3) | Done (AUD SHAP examples); notes still say "WORK ON TRANSITION" and introduce SHAP as new — SHAP is now introduced in section 6 |
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
- Feature importance: see "Feature importance (2026-11-03 evening)" section below.

### AUD lagged models — `~/github/study_lag`, drive `risk/models/lag/`
- **Updated today.** `mak/risk/mak_lag_ci.R` now reads `pp_perf_tibble.csv` (same file the
  paper uses). Old code kept commented out; old cache moved to `data/risk/lag_ci_old.csv`.
- auROC: no lag 0.91 [0.90, 0.92], 1 day 0.89, 3 day 0.88, 1 week 0.87, 2 week 0.85 [0.83, 0.87].
  Every lag credibly worse than no lag (Δ −0.02 to −0.06) and each adjacent step credibly worse
  (`contrast_baseline.csv`, `contrast_adjacent.csv`).
- No-lag 0.91 now agrees with the day model (0.90) on earlier slides.
- Data verified 2026-11-03: deck cache `data/risk/lag_ci.csv` = drive `pp_perf_tibble.csv` (diffs
  only at 16th decimal); paper `index.qmd` and `mak_figures.qmd` read the same file.
- **All lag models are 24-hour-window (day) models** (John confirmed).
- **Lag slides in usf.qmd (2026-11-03):** two-slide build copied from `archive/2025/0409_neurotech.qmd`
  ("Next Steps: Advanced Warning"), retitled "Lagged Prediction: Advanced Warning". Slide 1: two
  bullets, blank right column. Slide 2: those bullets grayed + "Can lag model up to two weeks into
  the future" / "Performance drops but still remains good" + `fig_lag_ci`. Notes are **verbatim
  neurotech** and need fixing: (a) "lagged the next week model… one week period" → 24-hour window;
  (b) opening references "these past few examples… next day model… personalized recommendations",
  which now come *later* in the talk; (c) "currently exploring other methods" is from Apr 2025 — ask
  John if still true. Neurotech source had a `:::` vs `::::` fence bug (fixed in copy).
- Other archived lag slides: 1002_chm / 1111_etg ("Clinical Uses: Who and When?", week-window
  bullets, notes "FREE TALK"), 1208_acnp (figure-only backup slide).
- The lag paper also has a **calibration figure** (Fig 3: raw vs Platt, no lag vs 2-week) built
  from `final_preds_kfold_1_x_5_1day_{0,336}_v3_strat_lh_final.rds` — a newer AUD model than the
  deck's `fig_cal`.

### AUD calibration — DECIDED: `fig_cal` (day model)
- John chose `fig_cal` (`figs/risk/figs_ema_probability.R`) because it is the **day** model, which
  is the one he wants to implement. (Correction 2026-11-03: lag-paper models are all **24-hour
  window** models, not week-window — `1day` in filenames; paper Methods confirm; John confirmed.
  Archived decks' notes saying "lagged the next week model" reflect earlier models.)
- `fig_cal`: original EMA paper next-day predictions, beta-calibrated (`prob_beta`), 0.1 bins,
  day blue (see Model colors), `linewidth = 1.5`, open-circle points.
- Bin check (day model): well calibrated below .2 (~89% of obs; 82% in 0–.1 bin);
  overpredicts by ~5–15 pts in .3–.8; slight underprediction in top two bins; strictly
  monotonic. Top bin (.9–1, ~17.7k obs) has observed rate exactly 1.00 — know this for Q&A.
- **Do not mention beta calibration** in notes (too detailed for general audience).
- Prior notes for this figure (identical across archive/2024 spring_prep, spring_uwash,
  1002_chm, 1111_etg and archive/2025/0409_neurotech) said "very well calibrated and at least
  ordinal… essentially what we see"; new notes soften this.

### AUD GPS model — `~/github/study_gps` (current draft is `index_2026_0917.qmd`, not `index.qmd`), drive `risk/models/gps/`
- N = 146; next-day lapse; geolocation + demographics only; 7.4% lapse base rate.
- auROC 0.72 [0.69, 0.74] vs demographics-only 0.52 (`pp_all_tibble.csv`).
- Top features: time at self-rated medium/high/no-risk locations, places previously drank.
  Income top demographic (~1/3 as important).
- Fairness differs from EMA model: worse for age 55+ (Δ 0.08), better for lower education;
  race/sex/poverty not credible. Recommend **not** showing GPS fairness (muddies the AUD→OUD story).
- Probabilities cluster near base rate (narrow range).
- No combined EMA+GPS model in this paper, so **never say GPS "doesn't add much to EMA"** — only
  that GPS alone predicts less well than EMA alone.
- Same outcome as the EMA day model (24-hour windows advancing hourly); GPS sample is the EMA
  sample minus 5 with poor GPS data. GPS vs EMA comparison is **descriptive only** (different CV
  schemes, no formal contrast); GPS vs demographics was tested in the paper (probability = 1).
- Method details (for notes/Q&A): Moves then FollowMee app, continuous GPS + WiFi, 3 months,
  participants could pause sharing. Monthly visits asked about frequently visited places (>2x/month):
  type, drank there before, alcohol available, pleasant/unpleasant, risk to recovery (none–high),
  trying to avoid. Plus passive features (time home, evening out, location variance, entropy,
  weather).
- **Deck (2026-11-03 evening):** `mak/risk/mak_gps_ci.R` caches `pp_all_tibble.csv` →
  `data/risk/gps_ci.csv`. `figs/risk/figs_gps_ci.R` → `fig_gps_ci_1` (Demographics, Geolocation)
  and `fig_gps_ci_2` (adds "Daily surveys (4x daily)" = EMA day model 0.90 from
  `data/risk/ema_auroc_ci.csv`, the original paper value matching the Model Performance slide).
  All three x-axis levels on both figs (`drop = FALSE`) so the build only adds a point; all day blue.
- **GPS slide structure:** (1) "Passive Sensing Methods: Geolocation" title-only, general intro,
  new bridge from lag slides, no communications mention; (2–3) John's GPS demo images (demo slide 2
  notes now end at the public-map-data line; communications + "launching now" lines cut);
  (4) "Geolocation: Lapse Prediction for AUD" title-only, coarse implementation details;
  (5–6) same title, lag-style 50/50 build with `fig_gps_ci_1` / `fig_gps_ci_2`. Notes cover: credible
  signal above demographics (at chance), .72 = fair, top features were participant-described
  location features; then surveys predict better, low burden, works on skipped-survey days,
  captures risk factors hard to report or that would lengthen the survey, callback to feature
  importance. Deliberately **no** OUD "lighter survey + GPS = 0.94" setup (OUD ablation shows no
  loss without GPS).

### OUD (NIDA risk2) — `~/github/study_risk2` (`index.qmd`), drive `risk2/models/combined/` (v13 files)
- 330 enrolled, **296 analyzed**, 47 states, Apr 2021–Dec 2024, on MOUD 1–12 months;
  one daily survey + geolocation with monthly location-context questions; up to ~1 year.
- Mean 278 days on study; 77.8% daily survey adherence; 78% stayed ≥6 months.
- 36% lapsed; 1.8% of 82,156 day labels positive.
- auROC **0.94 [0.93, 0.95]**, stable 1–18+ months in recovery (Fig S1). Brier 0.014.
- Ablation: no daily survey 0.63; no geolocation 0.94 (no loss); demographics only 0.55.
- Fairness: 5 subgroups (gender, race/ethnicity, income at **$25k**, sexual orientation,
  rural/urban), all 0.92–0.95; income gap favors lower-income group.
- Feature importance: see "Feature importance (2026-11-03 evening)" section below.
- Surprising patterns: recent *decreases* in stimulant use or urges raise risk.
- Local SHAP example image: `~/github/study_risk2/figures/shapley_local.png`.
- Figure code: `~/github/study_risk2/notebooks/mak_figures.qmd` (fig-1 fairness, fig-2 calibration,
  fig-3a importance, fig-4a partial dependence). Files read: `pp_perf_tibble_v13.csv`,
  `preds_1_x_5_day_v13_kfold_full_final.rds`, `shapsgrp_1_x_5_day_v13_kfold_full_final_raw.rds`, etc.
- **OUD fairness updated to v13 (2026-11-03):** `mak/risk2/mak_fair_ci.R` (old ACNP code commented
  out) now caches `pp_fairness_v13_full.csv` → `data/risk2/auroc_fair_ci.csv` and
  `pp_fairness_contrast_v13_full.csv` → `data/risk2/auroc_fair_contrast.csv`. Old cache kept as
  `auroc_fair_ci_old.csv`. Figure shows 4 panels (race, income, gender, geography; sexual orientation
  dropped per John), gray/purple, Delta = posterior contrast median. Income kept as "above/below
  poverty" labels (actually $25k split; John: not worth unpacking). Gender panel uses "not male"
  (the v13 variable). Slide notes rewritten: gaps small (.01–.03) but credible; lower income does
  slightly better; all groups .92–.95. Chunk fig-width 10 → 12.
- Note: v13 contrasts are all credible (P > .96), so do not say "no differences."
- **OUD overall performance figure (2026-11-03):** `figs/risk2/figs_perf_ci.R` → `fig_auroc_ci_oud`:
  AUD week/day/hour + "Day (OUD)" (v13 full, .943 [.934, .950]) in day blue, set apart by a light
  vertical line. Data cached by new `mak/risk2/mak_perf_ci.R` → `data/risk2/auroc_perf_ci.csv`
  (also has ablation rows). Framing agreed: "as good or better with less burden" (1 survey/day +
  geolocation); John says the 1 survey/day rather than labeling it. Placed on the first
  "Algorithmic Fairness: NIDA Opioid Use Lapses" performance slide (40/60 columns, bullet kept to
  match the gray bullet on the next slide); notes rewritten (no "midst of finalizing").
- Paper issue to mention to John/Kendra: `notebooks/mak_tables.qmd` builds Table 2 from
  `contrast_ablate_v10.csv` though `v13` exists.

### Feature importance (2026-11-03 evening)

**AUD (day model only; John's decision).**
- Source verified: paper pipeline `~/github/proj_risk/ema/mak_scripts/archive/2_metrics_outer.qmd`
  (SHAPforxgboost on 30 outer folds, averaged across 3 repeats per obs, summed into 18 categories).
  Deck cache `data/risk/ema_shaps_day.rds` = drive `outer_shapsgrp_1day_0_v5_nested_main.rds`
  (same byte size; content not compared because the R session can't read the drive).
- `mak/risk/mak_ema_shaps.R` additions (original `ema_shaps_global.csv` block untouched):
  - `ema_shaps_global_day.csv`: valence + arousal → "emotional state" **summed per observation**
    (not summing global means; 0.030 vs 0.039), then mean |SHAP|.
  - `ema_shaps_day_max.csv`: share of observations each category had the largest **signed** SHAP.
  - `ema_shaps_day_max_risk.csv`: same shares for all / risk ≥ base rate (7.7%) / risk ≥ .2, plus
    median winning SHAP per category. Risk = `prob_beta` from `ema_preds_1day.rds` averaged over
    3 repeats.
- `figs/risk/figs_ema_shaps.R`: `fig_global_day` rebuilt (16 categories, day blue, lapse hour dropped
  — exactly 0 for day model; "lapse day" → "day of week"; "future efficacy" → "abstinence
  confidence" to match EMA slide). New `fig_daymax_day` and `fig_daymax_day_risk` (≥ base rate).
  `fig_global_all/hour` unchanged.
- Global: past use 0.84, abstinence confidence 0.55, craving 0.20; demographics < 0.006.
- Share of days (all): past stressful event 26%, abstinence confidence 20%, past use 17%, craving 12%.
  But past stressful event wins with median SHAP 0.09 (54% < 0.1). Risk ≥ base rate: past use 66%,
  abstinence confidence 30%, craving 3%. Risk ≥ .2: past use 77%, abstinence confidence 23%.
- AUD observations are 24-hour windows advancing hourly (274,179), not one per calendar day.

**OUD (v13).**
- Paper method (`study_risk2/notebooks/mak_figures.qmd` fig-3a; Methods in `index.qmd`): grouped
  raw SHAPs from single 5-fold CV; global = mean |SHAP|; day-level = proportion of days each
  category had the highest SHAP. **Paper code uses `slice_max(value)` (signed); fig-3a caption
  says "highest absolute" — tell Kendra.** Paper also leaves gender/race/orientation/employment/
  day-of-week as separate dummies (tell Kendra). Raw past opioid use missing (8 categories are
  change-only, probably near-zero-variance filtering) — confirm with Kendra.
- `mak/risk2/mak_shaps.R` (John runs it; R session can't read drive `.rds`):
  - `shaps_global.csv`, `shaps_day_max.csv`: paper-faithful 88 categories.
  - `shaps_global_combined.csv`, `shaps_day_max_combined.csv`: **talk version** — raw + change
    combined per construct, dummies grouped, all summed per observation before either metric.
  - `shaps_day_max_combined_risk.csv`: shares for all / risk ≥ base rate (1.8%) / risk ≥ .2, plus
    median winning SHAP. Risk = `prob_raw`. **ID gotcha:** preds `id_obs` = `label_num` (has gaps,
    max 89,693), SHAP `id_obs` = rank of `label_num`; script remaps by rank (verified: winning SHAP
    vs logit risk Spearman .82 vs .13 naive). This CSV was computed by the assistant from John's
    in-memory objects with identical code; a rerun would reproduce it.
- `figs/risk2/figs_shaps.R`: `fig_global_oud`, `fig_daymax_oud` (own order),
  `fig_daymax_oud_global_order` (paper-style shared order). Top 15, bars colored by source
  (survey `#0072B2`, geolocation `#E69F00`, other/demographic `#A6A6A6`); labels lowercased,
  no raw/change suffix (John: no raw vs change point), "religious" → "religious locations".
- Global: past opioid use 1.03, urge 0.46, abstinence confidence 0.41, stimulant use 0.13, then
  mostly location features (6 in top 15). Share of days (all): abstinence confidence 15.6%, urge
  15.2%, past opioid use 12.3%; geolocation categories total 29.6%. **But** location winners have
  median SHAP 0.03–0.07 (location variance < 0.1 on 98.5% of its wins). Risk ≥ base rate (9,037
  days): past opioid use 87%, urge 9%, abstinence confidence 4%, location ~0%. Risk ≥ .2 (1,280):
  past opioid use 100%.

**Slides in usf.qmd** (all titled "Feature Importance: Looking Under the Hood"; 40/60 columns,
figs 8×6): AUD slide 1 introduces SHAP (first mention in deck) + top 3 + all items contribute + day
of week; AUD slide 2 (grayed build) demographics contribute little, ties to GPS demographics-only
model and to fairness (ignoring demographics ≠ fair). OUD slide 1 `fig_global_oud` (same core risk
factors as alcohol; location features next tier; bracketed IF ASKED note on OUD ablation). OUD
slide 2 `fig_daymax_oud` — **needs revision** (see top). AUD notes still say "weekends, as you might
guess" (from archive; global SHAP doesn't show direction) — John to keep or drop.

### Framing cautions discussed
- **GPS story:** "lower burden + different, actionable risk factors," not "better prediction."
  Payoff lands in feature-importance section (location features in OUD top 10).
- **Fairness improvement** can't be credited to representation alone (OUD study differs in
  substance, recruitment, MOUD, 1 EMA/day, duration, GPS). Notes should hedge.

## Done 2026-11-03

- **Path fix:** all active `mak/` scripts now use `format_path("risk/...")` / `"risk2/..."`
  (8 calls, 7 files). Research drive confirmed mounted on this machine; all deck folders resolve
  (risk/models/{ema,lag,gps}, risk/data_processed/{ema,shared,fairema}, risk2/data_processed/ema,
  risk2/models/combined). Note: `format_path()` is not vectorized (one path per call).
- **Calibration slide** (usf.qmd, "Model Calibration: Trusting the Probabilities"): replaced the
  copied demographics chunk with `fig_cal` (6×6). Two-column layout: bullets left (45%), figure
  right (55%). Bullets (John to review): "Probabilities are well calibrated where most predictions
  fall" / "Ordinal across the full range" / "Provides fine gradations of relative risk for
  clinical decision-making". Notes drafted with softened calibration claim.
- Notes begin with a bracketed reviewer comment: **[COMMENT - REVIEW WHEN FINE-TUNING NOTES: …]**
  — this slide may be where to discuss plans to give patients feedback as level of risk /
  recovery health rather than a formal lapse vs. no-lapse prediction. Leave for John.
- Removing the demographics chunk did not break anything (`figs_ema_demographics.R` is still
  sourced earlier, ~line 196).
- `figs_ema_probability.R`: thicker `fig_cal` line (addresses improvements.qmd note "calibration
  plot is odd color and line too thin"); color later changed to day blue.
- Publishing fixed (see top of file).
- Lag slides added (see Lag section); OUD design/performance/fairness slides updated (see OUD
  section); model and fairness color/style overhaul (see Model colors).
- Slide text highlights: all 12 `[...]{.blue}` spans in usf.qmd → `{.uwred}`.
- Housekeeping in R session: assistant leaves no objects in the global env (sources figure scripts
  then removes new objects).

## Done 2026-11-02

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

## Model colors (2026-11-03)

- **Final palette (Okabe-Ito, colorblind safe):** week = orange `#D55E00`, **day = blue `#0072B2`**
  (John's favorite; day model is shown most), hour = green `#009E73`. Red avoided because UW red
  is used for titles and word accents (slide highlights switched `.blue` → `.uwred`, 12 spans).
- Defined once as `model_colors` at top of `figs_ema_probability.R` (auROC CI, histograms,
  `fig_cal` + "Next day model" label, ROC); hard-coded with a sync comment in `figs_ema_shaps.R`.
- Left as is: confusion-matrix fill in `cm_mosaic()` (correct/incorrect, lime green/red) and unused
  `figs/risk2/figs_dev_probability.R`.
- Lag figure (`figs_lag_ci.R`) now uses day blue `#0072B2` (`day_color`), since all lagged models are
  24-hour-window (day) models. Stale "DATA NEED TO BE CHECKED" note replaced.
- CI style (auROC `plot_ci()` and lag fig): error bar linewidth 1.25 drawn first, then hollow point
  on top (shape 21, white fill, model-colored outline, size 3.5, stroke 1.5). `fig_cal` points use the
  same open-circle style (line linewidth 1.5). Fairness figs (`figs_fairema_ci.R`, `figs/risk2/figs_fair_ci.R`)
  now use the same CI style too.
- **Fairness group colors:** reference/advantaged group = light gray `#A6A6A6`, comparison group =
  purple `#7B3294` (replaced ggplot default salmon/teal; dark gray `#666666` rejected because it is as
  dark as the purple, so the groups merge on a washed-out projector or in grayscale). If light gray
  looks too faint on the projector, try `#999999`. Same colors on `fig_race_bar`
  (`figs_ema_demographics.R`) to keep the race bar → race auROC link on the first fairness slide.
  OUD v13 fairness figure uses these too. Other demographic bars on the Participant
  Characteristics slide still use default colors (John: deal with later).

## Next steps (in rough order; John will direct)

- [ ] **Decide how to handle the risk-split finding** in the OUD share-of-days slide (see top);
      optional analysis: top feature on higher-risk days excluding past use (AUD + OUD).
- [ ] Commit new scripts and `git add -f` new caches: `data/risk/{gps_ci,ema_shaps_global_day,
      ema_shaps_day_max,ema_shaps_day_max_risk}.csv`, `data/risk2/{shaps_global,shaps_day_max,
      shaps_global_combined,shaps_day_max_combined,shaps_day_max_combined_risk}.csv`.
- [ ] Personalized recommendations notes: fix "WORK ON TRANSITION"; refer back to SHAP.
- [x] GPS section (5 slides) and GPS figure.
- [x] Feature importance slides drafted (AUD x2, OUD x2).
- [x] Lag slides added (2-slide build with `fig_lag_ci`).
- [ ] Lag notes: revise for 24-hour window and new position after calibration (see Lag section).
- [x] Shell Quarto upgraded to 1.11.9 and Connect Cloud authorized (see top of file).
- [ ] Publish after John's review (missing R package was being installed); optionally test whether
      the assistant's bash tool can now publish.
- [ ] Optional: recolor other Participant Characteristics demographic bars (still default colors).
- [ ] Mention to Kendra: `mak_tables.qmd` uses `contrast_ablate_v10.csv` (v13 exists).
- [x] Calibration slide drafted with `fig_cal`. John to review bullets/notes and the
      bracketed comment; possibly brief OUD calibration later.
- [x] OUD fairness figure + notes updated to v13 (4 subgroups).
- [x] OUD section: design/sample slide (296, 47 states, MOUD, 1–12 months on MOUD, 1 survey/day +
      geolocation, up to 12 months) and performance slide + notes updated 2026-11-03.
- [ ] Communications: John will strip mentions from earlier slides (keep only as future direction).
- [ ] Optional: OUD local SHAP example; Smart DTx slide; update Optimize slide wording.
- [ ] Housekeeping (when John says): typos ("Communcations", "reasonble", "charactersitics",
      "martital", "ethinic", "lessor"); notes referencing "after this symposium ends... 1 pm".
      (Title and date done by John: date is `'2026-11-06'`, renders "November 6, 2026".)
- [ ] Re-estimate timing once sections are drafted; then decide AUD cuts.

## Gotchas

- **Figure-script dependencies:** each `figs_*.R` is sourced in the first chunk that uses it;
  later chunks reuse the objects (e.g., `fig_race_bar` from `figs_ema_demographics.R`,
  `fig_shap_ex*` from `figs/opt/figs_opt_shaps.R`). Reordering slides can break this.
- Figure scripts share the render's global env and reuse names (`plot_ci`, `ci`, `delta`,
  `path_data`, `model_colors`). Fine because each script builds its figures at source time, but
  don't call a helper like `plot_ci()` from a later chunk expecting an earlier script's version.
- **`format_path()` prefixes:** fixed 2026-11-03. All active `mak/` scripts now use `"risk/..."` /
  `"risk2/..."` (the old `"studydata/..."` prefix resolved to a nonexistent
  `~/mnt/standard/studydata/...` on Linux). `notebooks_old/` and the commented-out old code in
  `mak_lag_ci.R` still use the old prefix (left as-is). Note: the research drive must be mounted at
  `~/mnt/standard` for any mak script to regenerate cache.
- **Editor-open files:** when a file is open in RStudio, edits land in the editor buffer and must
  be saved before `source()` sees them. The read/edit tools also timed out while a render was
  running.
- `publish.sh` deletes every `*.html` and `*_files/` in the root.
- **Rendering locally:** `quarto render usf.qmd` alone gives plain HTML. Copy the config first, as
  `publish.sh` does: `cp _quarto_uw.yml _quarto.yml && quarto render usf.qmd; rm _quarto.yml`.
- **Headless Chrome screenshots** of revealjs slides (`google-chrome --headless=new --screenshot
  ... usf.html#/slide-id`) crop the left edge at any window size; it's an artifact (reviewed lag
  slide crops the same way), not a layout bug.
- **R session can't read the research drive with `read_rds()`** (agent file sandbox blocks
  `file()` on `~/mnt/standard`; `read_csv()` happened to work). Mak scripts that read drive `.rds`
  files must be run by John in his console. Don't work around it.
- **New caches are gitignored** (`/data/`), so commit with `git add -f`.
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
