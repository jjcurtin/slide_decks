#################################
# Set Up
#################################

library(tidyverse)
library(tidyposterior)
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_ema <- format_path(str_c("studydata/risk/models/ema"))
path_out <- "data/risk"

#################################
# transfer files
#################################

if(!file.exists(here::here(path_out, "ema_preds_1hour.rds"))) {

  read_rds(here::here(path_ema, "outer_preds_1hour_0_v5_nested_main.rds")) |> 
    write_rds(here::here(path_out, "ema_preds_1hour.rds"))
  read_rds(here::here(path_ema, "outer_preds_1day_0_v5_nested_main.rds")) |> 
    write_rds(here::here(path_out, "ema_preds_1day.rds"))
  read_rds(here::here(path_ema, "outer_preds_1week_0_v5_nested_main.rds")) |> 
    write_rds(here::here(path_out, "ema_preds_1week.rds"))
  
}
