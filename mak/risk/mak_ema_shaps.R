path_data <- "data/risk"
if (!file.exists(here::here(path_data, "ema_shaps_global.csv"))){
  library(tidyverse)
  source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_models <- format_path("risk/models/ema")
  
  shap_local_week <- read_rds(file.path(path_models, "outer_shapsgrp_1week_0_v5_nested_main.rds")) |> 
    write_rds(here::here(path_data, "ema_shaps_week.rds"))
  shap_local_day <- read_rds(file.path(path_models, "outer_shapsgrp_1day_0_v5_nested_main.rds")) |> 
    write_rds(here::here(path_data, "ema_shaps_day.rds"))
  shap_local_hour <- read_rds(file.path(path_models, "outer_shapsgrp_1hour_0_v5_nested_main.rds")) |>
    write_rds(here::here(path_data, "ema_shaps_hour.rds"))
  
  shap_global_week <- shap_local_week |> 
    group_by(variable_grp) |> 
    summarize(mean_value = mean(abs(value)), .groups = "drop") |> 
    arrange(mean_value)
  shap_global_day <- shap_local_day |> 
    group_by(variable_grp) |> 
    summarize(mean_value = mean(abs(value)), .groups = "drop") |> 
    arrange(mean_value)
  shap_global_hour <- shap_local_hour |> 
    group_by(variable_grp) |> 
    summarize(mean_value = mean(abs(value)), .groups = "drop") |> 
    arrange(mean_value)
  
  shap_global_all <- shap_global_week |>
    mutate(window = "week") |>
    bind_rows(shap_global_day |>
                mutate(window = "day")) |>
    bind_rows(shap_global_hour |>
                mutate(window = "hour")) |>
    mutate(window = factor(window, levels = c("week", "day", "hour"))) |> 
    write_csv(here::here(path_data, "ema_shaps_global.csv"))
}

# Day model only, with valence and arousal combined into emotional state.
# Combined at the observation level (sum local SHAPs, then mean |value|), the
# same way the paper formed every feature category. Summing the two global
# means would overstate importance.
if (!file.exists(here::here(path_data, "ema_shaps_global_day.csv"))){
  library(tidyverse)

  read_rds(here::here(path_data, "ema_shaps_day.rds")) |>
    ungroup() |>
    mutate(variable_grp = as.character(variable_grp),
           variable_grp = if_else(variable_grp %in% c("valence (EMA item)", "arousal (EMA item)"),
                                  "emotional state (EMA item)", variable_grp)) |>
    group_by(id_obs, variable_grp) |>
    summarize(value = sum(value), .groups = "drop") |>
    group_by(variable_grp) |>
    summarize(mean_value = mean(abs(value)), .groups = "drop") |>
    arrange(desc(mean_value)) |>
    write_csv(here::here(path_data, "ema_shaps_global_day.csv"))
}

# Day model: proportion of observations on which each category had the highest
# (signed, most risk-increasing) SHAP value; same method as OUD (mak/risk2/mak_shaps.R).
# Valence + arousal combined per observation first. Note AUD observations are
# 24-hour windows advancing hourly, not one per calendar day as in OUD.
if (!file.exists(here::here(path_data, "ema_shaps_day_max.csv"))){
  library(tidyverse)

  shaps <- read_rds(here::here(path_data, "ema_shaps_day.rds")) |>
    ungroup() |>
    mutate(variable_grp = as.character(variable_grp),
           variable_grp = if_else(variable_grp %in% c("valence (EMA item)", "arousal (EMA item)"),
                                  "emotional state (EMA item)", variable_grp)) |>
    group_by(id_obs, variable_grp) |>
    summarize(value = sum(value), .groups = "drop")

  n_obs <- n_distinct(shaps$id_obs)
  shaps |>
    group_by(id_obs) |>
    slice_max(value) |>
    group_by(variable_grp) |>
    summarize(n = n(), prop = n / n_obs, .groups = "drop") |>
    arrange(desc(prop)) |>
    write_csv(here::here(path_data, "ema_shaps_day_max.csv"))

  rm(shaps, n_obs)
}

# Same top-feature shares, split by predicted risk. On low-risk windows the
# "top" feature can win with a tiny SHAP value, so also report shares among
# windows with predicted risk >= base rate and >= .2, plus the size of the
# winning SHAP. Risk = beta-calibrated probability (as in fig_cal) averaged
# across the 3 CV repeats, matching how SHAPs were averaged.
if (!file.exists(here::here(path_data, "ema_shaps_day_max_risk.csv"))){
  library(tidyverse)

  win <- read_rds(here::here(path_data, "ema_shaps_day.rds")) |>
    ungroup() |>
    mutate(variable_grp = as.character(variable_grp),
           variable_grp = if_else(variable_grp %in% c("valence (EMA item)", "arousal (EMA item)"),
                                  "emotional state (EMA item)", variable_grp)) |>
    group_by(id_obs, variable_grp) |>
    summarize(value = sum(value), .groups = "drop") |>
    group_by(id_obs) |>
    slice_max(value, with_ties = FALSE) |>
    ungroup() |>
    left_join(read_rds(here::here(path_data, "ema_preds_1day.rds")) |>
                group_by(id_obs) |>
                summarize(prob = mean(prob_beta), label = first(label), .groups = "drop"),
              by = "id_obs")

  base_rate <- mean(win$label == "Lapse")

  share <- function(d, subset) {
    d |>
      count(variable_grp) |>
      mutate(prop = n / sum(n), subset = subset, n_windows = sum(n))
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
    write_csv(here::here(path_data, "ema_shaps_day_max_risk.csv"))

  rm(win, base_rate, share)
}