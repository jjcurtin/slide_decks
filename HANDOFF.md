# USF Keynote Deck — Handoff

Last updated: 2026-11-04 (~16:15). Talk is **Friday Nov 6**.

## Where we left off (2026-11-04, 16:15) — ON HOLD

- **John is doing his own round of edits and an out-loud run-through**, then will come back. When
  he returns: re-read `usf.qmd` (it will have changed), recount timing, then consider the Under the
  Hood trim (below).
- **Session 2026-11-04 afternoon (after 15:10):**
  - Render fixed: 5 duplicate chunk labels from John's slide splits renamed with suffixes
    (`aud_calibration_plot_2`, `figs_gps_ci_3`, `figs_fair_risk2_ci_2`, `figs_global_oud_2`,
    `figs_daymax_oud_2`). Also `source(figs_ema_demographics.R)` had been deleted with the original
    participant slides → added to the combined "Participant Characteristics" chunk. Full render to
    scratch succeeded.
  - OUD performance figure (`figs/risk2/figs_perf_ci.R`, `fig_auroc_ci_oud`): AUD week/day/hour
    faded (alpha .3, data-driven `alpha` column + `scale_alpha_identity()`), colors mapped by name
    via `scale_color_manual()`, OUD point labeled ".94" in bold UW red (`#c5050c`) computed from the
    data. A UW red box was tried and removed (John preferred the fade).
  - **Geolocation notes rewritten** (all 7 slides, John approved; tagged [DRAFT]): 1,012 → 725
    words. Sharpening: map-data-can't-give-meaning point now pays off on results slide ("exactly
    the personal meaning that map data alone can't give us"); "had drank" → "used to drink there"
    (John dislikes "drunk"); "months or years" line cut (duplicated in OUD performance notes);
    callback now says "under the hood of our opioid model" (AUD model has no location features).
    Assistant's optional idea not applied: one-sentence privacy nod on GPS zoom slide.
- **Timing (16:14 file):** 5,351 spoken notes words ≈ **44.6 min at 120 wpm** (60 notes blocks,
  brackets and HTML comments excluded). Clicks/pauses add ~2–3 min → ~47 min spoken.
  **New target (John): ~45 min total**, since the introduction may eat a few minutes → notes
  ≈ 42.5 min ≈ **5,100 words; cut ~250 more**. Next candidate: Under the Hood (946 words, 7.9 min;
  share-of-days slides are the weakest given the low-risk-day issue). Section timing table
  (pre-geolocation edit) was: Disclosures+opening 3.0, Gustafson 3.2, sensing/AUD/participants/EMA
  3.4, predictions+auROC 2.9, calibration 2.0, lag 1.6, geolocation 8.4 (now ~6.0), AUD fairness
  2.3, OUD 4.0, under the hood 7.9, smart app 3.0, Optimize 1.2, WARF 2.1, closing 1.9.
- **Word-count method:** parse `.notes` blocks line by line (a single regex over the file missed
  one block), drop YAML, HTML comments, and `[...]` text, count `\S+`, divide by 120.
- **Recovery health score: CHECK WITH JOHN.** The assistant added a 2-slide "From Lapse Risk to
  Recovery Health" build after the loop; it is **no longer in the file** (John apparently removed
  it). The Optimize slide now carries the definition ("Recovery health score (inverse of lapse
  probability)"). Two notes still point to the removed slides: calibration slide 2 ("I'll come back
  to how we present that signal to patients near the end") and loop Discover ("which I'll say more
  about in a moment"). Also the well-being/low-risk paragraph lived on the removed slide 2 — confirm
  whether John wants it somewhere (it is no longer anywhere in the deck).
- **Uncommitted work:** last commit is John's `1993608` ("first full draft edits"; usf.qmd at the
  15:07 state). Modified since: `usf.qmd` (labels, demographics source, geolocation notes),
  `HANDOFF.md`, `figs/opt/figs_opt_shaps.R`, `figs/risk/figs_ema_demographics.R`,
  `figs/risk2/figs_perf_ci.R`. Untracked: `figs/rudra/figs_loop.R`, `images/engagement_loop.png`.
  Earlier caches still need `git add -f` (see Next steps).
- **Post-talk analysis (deferred by John):** "share of days as top feature" is dominated by
  low-risk days where the winning SHAP is tiny. On higher-risk days past use is almost always top
  (AUD 66–77%, OUD 87–100%); location features essentially never. Implications for the
  recommendation engine to be worked out after the talk. Suggested analysis: top feature on
  higher-risk days **excluding past use**, AUD + OUD.

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
- **Length:** ~50 min talk + 10 min questions. Use **120 wpm** for timing (John's request).
- **Deck:** [usf.qmd](usf.qmd) (revealjs only; no book output).
- **Published:** https://connect.posit.cloud/jjcurtin/content/01a19dfd-0622-2035-a06c-e7f7e4b9b915
  (`./publish.sh usf.qmd`; ID now in `_publish.yml`, so republishing updates in place).
- John's working notes on the plan are in [todo.md](todo.md).

## Working agreements (important)

- **John directs every slide edit.** Discuss first; do not change `usf.qmd` unless asked.
  Figure scripts (`figs/`, `mak/`) can be edited when asked.
- **Re-read before editing or describing.** John edits `usf.qmd` in RStudio at the same time.
  Use exact-string `edit` calls only; **no whole-file rewrites from R** (`readLines`/`writeLines`)
  — they read disk and can collide with unsaved editor buffers. Never describe slide text from
  memory (2026-11-04: assistant wrongly claimed typos remained that John had already fixed).
- **Slide builds = separate slides, not fragments.** John prefers each new bullet/image to be a new
  slide that replaces the previous one (earlier bullets grayed with `[...]{.gray}`), so each notes
  page is short enough to read in presenter view without scrolling. Split long notes the same way.
- **Untitled / title-only slides with notes are intentional** (blank screen while John talks).
- **Add first, cut later** — now in the cutting phase (see timing above).
- **Notes drafted by the assistant are marked [DRAFT];** open questions as [COMMENT - ...].
- **Communications sensing:** future direction only. John reworked the sensing slides
  ("Brief daily surveys", "(other sensing methods)").
- **Optimize trial (NIAAA, AUD):** collecting data now; **4 months** per person; shows a **recovery
  health score** (not lapse probability). No results to show.
- **Terminology:** lapse probability/risk throughout the algorithm half; recovery health score only
  once the app/patient view starts.
- Formatting conventions are documented in [README_formatting.qmd](README_formatting.qmd)
  (rendered to `README_formatting.pdf` via Typst).

## Current slide order (re-read from usf.qmd 2026-11-04 15:07)

Slide numbers include the title slide as 1. Notes word counts exclude bracketed text.

| Slides | Section | Titles / content | Notes |
|---|---|---|---|
| 2 | Disclosures | "Disclosures" (Claire + Kendra + John built models; WARF copyright/IP; Rudra Health AI license; John co-founder/CSO) | [DRAFT]; doubles as preview |
| 3–4 | Opening story | untitled (decade ago; family) | the notes block right after the YAML is its own blank slide |
| 5–8 | Gustafson + precision MH | "A Smart Recovery Monitoring and Support System" x4 | who/when/how quote |
| 9–11 | Sensing + AUD study | "Personal Sensing" x2, "Lapse Prediction for AUD" | John reworked (brief surveys, geolocation; other methods in parens) |
| 12 | Participants | "Participant Characteristics" (7-panel combined; John deleted the two originals) | [DRAFT] tag still on |
| 13 | EMA | "Brief Daily Surveys" (retitled by John) | |
| 14–19 | Predictions + auROC | "Model Predictions" x2, "Model Performance" x3, "...Excellent auROCs" | slide 15 notes still "after this symposium ends... 2 pm" |
| 20–21 | Calibration | "Model Calibration: Trusting the Probabilities" x2 (John split) | slide 21 plants "graded signal... I'll come back near the end" |
| 22–23 | Lag | "Lagged Prediction: Advanced Warning" x2 | revised; [COMMENT] on "exploring other methods" may be resolved — check |
| 24–30 | Geolocation | intro, 2 demo images, "Geolocation: Lapse Prediction for AUD" x4 (John split results) | notes rewritten/trimmed 2026-11-04 16:00 [DRAFT], John approved |
| 31–35 | Fairness (AUD) | "Algorithmic Fairness" x5 | "lessor" typo remains (slide 35) |
| 36–39 | OUD | "NIDA OUD Recovery Project" (+ ": Model Performance", ": Algorithmic Fairness" x2) — John retitled | performance fig: AUD faded, OUD ".94" in UW red (2026-11-04) |
| 40–48 | Under the hood | "Feature Importance: Looking Under the Hood" x9: SHAP cartoon x3 → AUD global x2 → OUD global x2 → OUD share of days x2 | SHAP introduced on slide 40; "weekends, as you might guess" still in slide 43 notes |
| 49–52 | Smart app | "Next Steps: A Smart Digital Health App" x4: toolbox → smart app → loop → loop + dashed arrow (John added bullet "Helps patients improve their own self-monitoring") | Discover notes dangle "say more in a moment" |
| 53 | Optimize | "Next Steps: Optimize System Feedback to Patients" | defines health score as inverse of lapse probability |
| 54–56 | WARF pilot | "Next Steps: Insights App Pilot Trial" x3 (John split) | WARF explained on Disclosures; callback here |
| 57–61 | Closing | "A Smart Recovery Monitoring and Support System" x5 (quote → Who → When → How → what we don't know/thanks) | [COMMENT] on "Who" reframing may be resolved — check |

## Section history and details (2026-11-03/04; superseded rows kept for context)

| # | Section | Slides (titles in usf.qmd) | Status |
|---|---|---|---|
| 1b | Participant Characteristics, combined (added 2026-11-04 as 3rd slide after the two originals; John may cut the originals) | "Participant Characteristics" | 7 panels: top row AUD symptom count, age, income; bottom row sex, race/ethnicity, education, marital status. Notes [DRAFT] merge both originals. New `fig_race_bar_demo` (default ggplot fill) used on demographics slides; `fig_race_bar` stays gray/purple for fairness slides |
| 0 | Disclosures (added 2026-11-04, directly after title slide) | "Disclosures" | 4 bullets: Claire Punturieri + Kendra Wyant built models; WARF holds copyright/manages IP; Rudra Health AI licensed, building app; John co-founder + CSO. Notes [DRAFT] frame it as a preview. Note: the notes block right after the YAML is its own blank slide (not title-slide notes); the "About a decade ago" blank slide now follows Disclosures. WARF is now defined twice (here and WARF pilot slide notes) — trim one later |
| 1 | Motivation, sensing, AUD sample, EMA, week/day/hour auROC | "A Smart Recovery...", "Personal Sensing", "Lapse Prediction for AUD", "Participant Characteristics", "EMA", "Model Predictions", "Model Performance" | Done (existing) |
| 2 | AUD calibration | "Model Calibration: Trusting the Probabilities" | **Drafted 2026-11-03** (`fig_cal`, bullets, notes); John to review bullets/notes |
| 3 | Lagged AUD models (now **after** calibration; John reordered) | "Lagged Prediction: Advanced Warning" (x2 build) | Slides + figure in; **notes still need revision** |
| 4 | GPS as passive sensing (+ John's GPS demo slides) + AUD GPS results | "Passive Sensing Methods: Geolocation" (title-only) → 2 untitled background-image demo slides → "Geolocation: Lapse Prediction for AUD" (x3: title-only implementation, then 2-slide figure build) | **Done 2026-11-03 evening**; John approved |
| 5 | Fairness: AUD problems → OUD improvement | "Algorithmic Fairness", "Next Steps: Algorithmic Fairness" (x3), "Algorithmic Fairness: NIDA Opioid Use Lapses" (x3) | AUD done; OUD design, performance, and fairness slides **updated 2026-11-03** (v13) |
| 6 | Global feature importance AUD + OUD, then OUD "most important by day" | "Feature Importance: Looking Under the Hood" (x4: AUD 2-slide build, OUD global, OUD share of days) | **Drafted 2026-11-03 evening**; OUD share-of-days slide needs rethinking (see top) |
| 7 | Personalized recommendations | "Beyond Prediction: Personalized Recommendations" (x3) | Done (AUD SHAP examples); notes still say "WORK ON TRANSITION" and introduce SHAP as new — SHAP is now introduced in section 6 |
| 8 | Smart DTx with native prediction (Insights / Rudra app) | "Smart Digital Therapeutics" | Placeholder (see [rudra.qmd](rudra.qmd)) |
| 6a (moved 2026-11-04) | SHAP cartoon (Patients 1–3) now OPENS the feature importance section, retitled "Feature Importance: Looking Under the Hood" | 3 slides before AUD global | SHAP introduced here (local → global → share of days); option (a): one recommendation sentence per patient. Cartoon data are simulated (`mak/opt/mak_opt_shaps.R` → `data/opt/opt_shaps_examples.csv`); `figs/opt/figs_opt_shaps.R` restyled to match `fig_global_day` (day blue, "abstinence confidence", valence+arousal summed to "emotional state", global item order, shared 0–1.7 x-axis, red labels on top factors via `plot_shap_ex()`). Well-being/low-risk paragraph moved to app Learn step. AUD global notes now start "summarize these explanations across everyone". Agreed reorder plan: next add recovery health score slide after loop (plant "graded signal" at calibration, name/define at Discover) and a closing slide after WARF |
| 8 (now) | Smart digital health app / engagement loop (2026-11-04) | "Next Steps: A Smart Digital Health App" (replaces Smart DTx placeholder) | Drafted as a 3-slide replace build (no fragments; John prefers separate slides so notes fit presenter view): toolbox bullets → grayed + smart-app bullets → all grayed + loop figure; notes [DRAFT] call back to Gustafson/CHESS app (relapsers didn't use app/best tools), map Reflect→daily survey (ablation .94→.63), Discover→score + SHAP top factor, Learn→recommendations (well-being on low-risk days), Reward→CM. Bracketed comment: still need to decide where risk → recovery health score transition happens. Figure: `figs/rudra/figs_loop.R` (run manually) → static `images/engagement_loop.png` (DiagrammeR/rsvg, Source Sans Pro, day blue circles, UW red dashed self-monitoring arrow). Source Sans Pro copied from Quarto's revealjs fonts to `~/.local/share/fonts/source-sans-pro` so rsvg uses the deck font; on another machine, do the same before regenerating |
| 8b (added 2026-11-04, after loop) | Recovery health score | "From Lapse Risk to Recovery Health" (2-slide build) | Slide 1: lapse probability = model-evaluation metric; recovery health score = what patients see (inverse of next-day lapse prob, 0–100, higher better, daily + 2-wk trend); notes tie to calibration. Slide 2 adds "Why recovery health?" (information not verdict; strengths/progress; targeted skills on low days, well-being on good days); the well-being paragraph moved here from the loop Learn step. Calibration notes: bracketed comment replaced by a "graded signal, not yes/no alarm; I'll come back to it" plant. Loop Discover notes say "I'll say more about [the score] in a moment"; loop health-score comment removed. Optimize slide updated 2026-11-04: shows a health score (John confirmed), bullets reworded (score, trend, top factors, matched recommendation), status "Data collection underway" merged into first bullet (a standalone line was hidden by the PI banner image); notes rewritten [DRAFT]. |
| 10 | WARF-funded Insights app pilot trial (added 2026-11-04, after Optimize) | "Next Steps: Insights App Pilot Trial" | Drafted from `~/github/proj_rudra1/irb/protocol.qmd` (aims, 210 pts, 8-wk 3-arm RCT, outcomes); notes marked [DRAFT]; WARF now explained on Disclosures, pilot notes use a callback. John later split it into 3 slides |
| 11 | Closing (added 2026-11-04, after WARF) | "A Smart Recovery Monitoring and Support System" (reprises opening title + Gustafson quote/photo) | 4-slide replace build (quote only → +Who → +When (Who grayed) → +How (Who/When grayed)); 72% left column so text clears photo; notes [DRAFT] end with "Thanks for your time" (removed from Optimize notes). Bracketed comment: "Who" reframes Dave's "who is at risk" as "who the models work for" — John to decide |
| 9 | Optimize grant | "Next Steps: Optimize System Feedback to Patients" | Updated 2026-11-04 (underway, 4 months, health score); John further edited bullets/notes |

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

**Slides in usf.qmd** — SUPERSEDED: the section now opens with the SHAP cartoon (see Current slide
order). Original description: (all titled "Feature Importance: Looking Under the Hood"; 40/60 columns,
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

- [ ] **Recovery health score placement** — confirm with John (see top): removed slides, dangling
      references in calibration slide 21 and loop Discover notes, orphaned well-being paragraph.
- [ ] **After John's own edits + out-loud run:** re-read usf.qmd, recount timing.
- [ ] **Trim notes to ≈ 5,100 words** (16:14: 5,351 ≈ 44.6 min; target ~45 min total incl. clicks).
      Geolocation done; next propose a sharpen-and-trim of Under the Hood (~250 words) in chat
      first (John liked the "propose in chat, then apply" workflow).
- [ ] Resolve open [COMMENT]s and remove [DRAFT] tags as John approves sections.
- [ ] Housekeeping: "lessor" (slide 35), "after this symposium ends... 2 pm" (slide 15), "weekends,
      as you might guess" (slide 43; global SHAP shows no direction).
- [ ] **Commit** (checkpoint): `usf.qmd`, `HANDOFF.md`, `figs/opt/figs_opt_shaps.R`,
      `figs/risk/figs_ema_demographics.R`, `figs/risk2/figs_perf_ci.R`, `figs/rudra/figs_loop.R`,
      `images/engagement_loop.png`;
      and `git add -f` caches: `data/risk/{gps_ci,ema_shaps_global_day,ema_shaps_day_max,
      ema_shaps_day_max_risk}.csv`, `data/risk2/{shaps_global,shaps_day_max,shaps_global_combined,
      shaps_day_max_combined,shaps_day_max_combined_risk}.csv` (check `git status --ignored`).
- [ ] Render check and publish (`./publish.sh usf.qmd`) after John's review.
- [ ] Post-talk: risk-split analysis (see top).
- [x] 2026-11-04: SHAP cartoon moved to open Under the Hood (option a: one recommendation sentence
      per patient); cartoon fig restyled; smart app/loop slides + figure; WARF pilot slide;
      closing slides; disclosures; combined participant slide + `fig_race_bar_demo`; Optimize
      wording (health score, 4 months, underway); lag notes revised; calibration "graded signal" plant.
- [x] GPS section (5 slides) and GPS figure.
- [x] Feature importance slides drafted (AUD x2, OUD x2).
- [x] Lag slides added (2-slide build with `fig_lag_ci`).
- [x] Lag notes revised 2026-11-04: stand alone (no recommendations reference), next day model lagged, 24-hour window starting two weeks out, .91 → .85. Bracketed comment asks John whether "currently exploring other methods" (Apr 2025) is still true.
- [x] Optimize: 4 months per person (was six), slide + notes.
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
- [x] Communications stripped from early slides by John (now "(other sensing methods)").
- [x] Smart DTx slide (now smart app/loop) and Optimize wording done. Optional still: OUD local SHAP example.
- [x] Most typos fixed by John; remaining ones listed in housekeeping item above.
- [x] Timing re-estimated (see top).

## Gotchas

- **Figure-script dependencies:** each `figs_*.R` is sourced in the first chunk that uses it;
  later chunks reuse the objects (e.g., `fig_race_bar` from `figs_ema_demographics.R`,
  `fig_shap_ex*` from `figs/opt/figs_opt_shaps.R`). Reordering or deleting slides can break this
  (2026-11-04: deleting the original participant slides removed the demographics `source()`).
- **Splitting slides by copy-paste duplicates chunk labels** → knitr "Duplicate chunk label" error.
  Check with `grep -oE '^#\| *label: *.*' usf.qmd | sort | uniq -d`. Duplicated slides are often
  byte-identical, so `edit` anchors must include the end of the preceding slide's notes.
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
- **Render to scratch** to avoid touching the repo: `cp _quarto_uw.yml _quarto.yml && quarto render
  usf.qmd --output-dir <scratch>/render; rm _quarto.yml` (harmless warnings about output dir).
  Duplicate slide titles get ids `title`, `title-1`, `title-2`, ... for screenshot URLs.
- **Engagement loop figure** is static: rerun `figs/rudra/figs_loop.R` manually. Needs Source Sans
  Pro as a system font (copied from `/opt/quarto/share/formats/revealjs/reveal/dist/theme/fonts/
  source-sans-pro` to `~/.local/share/fonts/`, then `fc-cache`); otherwise rsvg silently substitutes.
- **Figure scripts leak globals** (`format_path`, `path_data`) when sourced in the console; clean
  up after previewing.
- Absolute-positioned banner images (e.g., `opt_pi_wide.png`) hide text placed near the slide
  bottom; check screenshots after adding lines.
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
