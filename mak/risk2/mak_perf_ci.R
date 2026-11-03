# Overall auROC for the final v13 OUD day model (same file the study_risk2 paper uses)
path_out <- "data/risk2"
if(!file.exists(here::here(path_out, "auroc_perf_ci.csv"))){

  library(tidyverse)
  source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_models <- format_path("risk2/models/combined")

  read_csv(here::here(path_models, "pp_perf_tibble_v13.csv"),
           show_col_types = FALSE) |>
    rename(median = pp_median,
           lower = pp_lower,
           upper = pp_upper) |>
    write_csv(here::here(path_out, "auroc_perf_ci.csv"))
}
