#################################
# Set Up
#################################
library(tidyverse)
library(tidyposterior)
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_ema <- format_path(str_c("studydata/risk2/data_processed/ema"))
path_out <- "data/risk2"


# NOTE: this is temp save of file created by Kendra for ACNP.  Will need to update as
# analyses of RISK2 continue

read_csv(here::here(path_ema, "pp_auroc_dem_v1_kfold.csv"),
                        show_col_types = FALSE)  |> 
  mutate(model = if_else(model == "Man", "male", model), 
         model = if_else(model == "Woman", "female", model),
         model = if_else(model == "non-Hispanic White", "white/non-hispanic", model),
         model = if_else(model == "not White", "other", model),
         model = if_else(model == "above $25,000", "above poverty", model),
         model = if_else(model == "below $25,000", "below poverty", model)) |> 
  rename(median = pp_median,
         lower = pp_lower,
         upper = pp_upper) |>
  write_csv(here::here(path_out, "auroc_fair_ci.csv"))
