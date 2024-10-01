####################################
# Set up environment
####################################
library(tidyverse)
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true",
                     sha1 = "a58e57da996d1b70bb9a5b58241325d6fd78890f")
path_models <- format_path("studydata/risk/models/ema")
path_data <- "data/risk"

####################################
# Load and save data
####################################

if (!file.exists(here::here(path_data, "ema_shaps_global.csv"))){
  
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