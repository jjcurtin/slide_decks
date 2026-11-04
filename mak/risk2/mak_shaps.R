# OUD v13 day model feature importance; mirrors study_risk2/notebooks/mak_figures.qmd
# (fig-3a). Grouped raw Shapley values from the single 5-fold CV in held-out data.
#   shaps_global.csv:  mean |SHAP| per feature category across all observations
#   shaps_day_max.csv: proportion of observations (days) on which each category
#                      had the highest Shapley value. Paper code uses slice_max(value),
#                      i.e., the largest signed (most risk-increasing) value, with ties kept.
path_out <- "data/risk2"
if(!file.exists(here::here(path_out, "shaps_global.csv")) ||
   !file.exists(here::here(path_out, "shaps_day_max.csv"))){

  library(tidyverse)
  source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_models <- format_path("risk2/models/combined")

  shaps <- read_rds(here::here(path_models, "shapsgrp_1_x_5_day_v13_kfold_full_final_raw.rds"))

  shaps |>
    group_by(variable_grp) |>
    summarize(mean_value = mean(abs(value)), .groups = "drop") |>
    arrange(desc(mean_value)) |>
    write_csv(here::here(path_out, "shaps_global.csv"))

  n_obs <- max(shaps$id_obs)
  shaps |>
    group_by(id_obs) |>
    slice_max(value) |>
    group_by(variable_grp) |>
    summarize(n = n(), prop = n / n_obs, .groups = "drop") |>
    arrange(desc(prop)) |>
    write_csv(here::here(path_out, "shaps_day_max.csv"))

  rm(shaps, n_obs)
}

# Talk version: raw and change ("Diff") categories combined into one category per
# construct (e.g., "Urge, Raw (EMA)" + "Urge, Diff (EMA)" -> "Urge (EMA)").
# Combined at the observation level (sum local SHAPs) before computing either
# metric, the same way the paper formed every category.
if(!file.exists(here::here(path_out, "shaps_global_combined.csv")) ||
   !file.exists(here::here(path_out, "shaps_day_max_combined.csv"))){

  library(tidyverse)
  source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_models <- format_path("risk2/models/combined")

  shaps <- read_rds(here::here(path_models, "shapsgrp_1_x_5_day_v13_kfold_full_final_raw.rds")) |>
    ungroup() |>
    mutate(variable_grp = str_replace(as.character(variable_grp), ", (Raw|Diff) \\(", " ("),
           # dummy features the paper left ungrouped
           variable_grp = case_when(
             str_detect(variable_grp, "^label_day_") ~ "Day of week (other)",
             str_detect(variable_grp, "^gender_") ~ "Gender (demographic)",
             str_detect(variable_grp, "^race_") ~ "Race (demographic)",
             str_detect(variable_grp, "^orientation_") ~ "Sexual orientation (demographic)",
             str_detect(variable_grp, "^employment_") ~ "Employment (demographic)",
             .default = variable_grp)) |>
    group_by(id_obs, variable_grp) |>
    summarize(value = sum(value), .groups = "drop")

  shaps |>
    group_by(variable_grp) |>
    summarize(mean_value = mean(abs(value)), .groups = "drop") |>
    arrange(desc(mean_value)) |>
    write_csv(here::here(path_out, "shaps_global_combined.csv"))

  n_obs <- max(shaps$id_obs)
  shaps |>
    group_by(id_obs) |>
    slice_max(value) |>
    group_by(variable_grp) |>
    summarize(n = n(), prop = n / n_obs, .groups = "drop") |>
    arrange(desc(prop)) |>
    write_csv(here::here(path_out, "shaps_day_max_combined.csv"))

  rm(shaps, n_obs)
}

# Top-feature shares (combined categories) split by predicted risk. On low-risk
# days the "top" feature can win with a tiny SHAP value, so also report shares
# among days with predicted risk >= base rate and >= .2, plus the size of the
# winning SHAP. Risk = raw probability (the paper uses prob_raw for OUD).
# Same approach as AUD (mak/risk/mak_ema_shaps.R, ema_shaps_day_max_risk.csv).
if(!file.exists(here::here(path_out, "shaps_day_max_combined_risk.csv"))){

  library(tidyverse)
  source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_models <- format_path("risk2/models/combined")

  win <- read_rds(here::here(path_models, "shapsgrp_1_x_5_day_v13_kfold_full_final_raw.rds")) |>
    ungroup() |>
    mutate(variable_grp = str_replace(as.character(variable_grp), ", (Raw|Diff) \\(", " ("),
           variable_grp = case_when(
             str_detect(variable_grp, "^label_day_") ~ "Day of week (other)",
             str_detect(variable_grp, "^gender_") ~ "Gender (demographic)",
             str_detect(variable_grp, "^race_") ~ "Race (demographic)",
             str_detect(variable_grp, "^orientation_") ~ "Sexual orientation (demographic)",
             str_detect(variable_grp, "^employment_") ~ "Employment (demographic)",
             .default = variable_grp)) |>
    group_by(id_obs, variable_grp) |>
    summarize(value = sum(value), .groups = "drop") |>
    group_by(id_obs) |>
    slice_max(value, with_ties = FALSE) |>
    ungroup()

  preds <- read_rds(here::here(path_models, "preds_1_x_5_day_v13_kfold_full_final.rds"))

  # In 3_mak_final_model.qmd, preds get id_obs = label_num, while SHAPs get
  # id_obs = row number after arrange(label_num). label_num has gaps (max 89,693
  # for 82,156 days), so map preds to SHAP ids by rank of label_num. Verified
  # 2026-11-03: winning SHAP vs logit(prob_raw) Spearman r = .82 with this
  # mapping vs .13 joining on raw id_obs.
  stopifnot(nrow(preds) == n_distinct(preds$id_obs),
            nrow(preds) == nrow(win))
  preds <- preds |>
    mutate(id_obs = rank(id_obs))
  stopifnot(setequal(preds$id_obs, win$id_obs))

  win <- win |>
    left_join(preds |> select(id_obs, prob = prob_raw, label), by = "id_obs")

  base_rate <- mean(win$label == "Lapse")

  share <- function(d, subset) {
    d |>
      count(variable_grp) |>
      mutate(prop = n / sum(n), subset = subset, n_days = sum(n))
  }

  bind_rows(share(win, "all"),
            share(filter(win, prob >= base_rate), "risk >= base rate"),
            share(filter(win, prob >= .2), "risk >= .2")) |>
    left_join(win |>
                group_by(variable_grp) |>
                summarize(median_winning_shap = median(value),
                          prop_winning_shap_lt_.1 = mean(value < .1), .groups = "drop"),
              by = "variable_grp") |>
    mutate(base_rate = base_rate) |>
    write_csv(here::here(path_out, "shaps_day_max_combined_risk.csv"))

  rm(win, preds, base_rate, share)
}
