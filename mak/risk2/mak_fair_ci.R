#################################
# Set Up
#################################

# OLD CODE (pre-2026-11-03): temp file from Kendra for ACNP 2024 (v1 kfold model, 3 subgroups)
# library(tidyverse)
# library(tidyposterior)
# source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
# path_ema <- format_path("risk2/data_processed/ema")
# path_out <- "data/risk2"
# 
# 
# # NOTE: this is temp save of file created by Kendra for ACNP.  Will need to update as
# # analyses of RISK2 continue
# 
# read_csv(here::here(path_ema, "pp_auroc_dem_v1_kfold.csv"),
#                         show_col_types = FALSE)  |> 
#   mutate(model = if_else(model == "Man", "male", model), 
#          model = if_else(model == "Woman", "female", model),
#          model = if_else(model == "non-Hispanic White", "white/non-hispanic", model),
#          model = if_else(model == "not White", "other", model),
#          model = if_else(model == "above $25,000", "above poverty", model),
#          model = if_else(model == "below $25,000", "below poverty", model)) |> 
#   rename(median = pp_median,
#          lower = pp_lower,
#          upper = pp_upper) |>
#   write_csv(here::here(path_out, "auroc_fair_ci.csv"))

# Final v13 models from study_risk2 (same files the paper uses).
# Income split is $25,000 in the paper; labeled above/below poverty for the talk.
path_out <- "data/risk2"
if(!file.exists(here::here(path_out, "auroc_fair_ci.csv")) ||
   !file.exists(here::here(path_out, "auroc_fair_contrast.csv"))){

  library(tidyverse)
  source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_models <- format_path("risk2/models/combined")

  read_csv(here::here(path_models, "pp_fairness_v13_full.csv"),
           show_col_types = FALSE) |>
    mutate(model = recode_values(model,
                              "male" ~ "male",
                              "not male" ~ "not male",
                              "non-Hispanic White" ~ "white/non-hispanic",
                              "Hispanic and/or not white" ~ "other",
                              "above poverty" ~ "above poverty",
                              "below poverty" ~ "below poverty",
                              "urban/suburban" ~ "urban",
                              "small town/rural" ~ "rural",
                              "heterosexual" ~ "heterosexual",
                              "not heterosexual" ~ "not heterosexual")) |>
    rename(median = pp_median,
           lower = pp_lower,
           upper = pp_upper) |>
    write_csv(here::here(path_out, "auroc_fair_ci.csv"))

  # Posterior contrasts (comparison - reference) for the Delta labels
  read_csv(here::here(path_models, "pp_fairness_contrast_v13_full.csv"),
           show_col_types = FALSE) |>
    mutate(group = case_when(str_detect(contrast, "male") ~ "sex",
                             str_detect(contrast, "white|White") ~ "race",
                             str_detect(contrast, "poverty") ~ "income",
                             str_detect(contrast, "rural") ~ "geography",
                             str_detect(contrast, "heterosexual") ~ "orientation")) |>
    write_csv(here::here(path_out, "auroc_fair_contrast.csv"))
}
