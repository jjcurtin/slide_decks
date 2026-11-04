# GPS (geolocation) next-day lapse models from study_gps (2 x 5 nested CV, N = 146).
# Same summary file the paper uses for its inline auROC values.
# baseline = demographics only; full = geolocation + demographics
path_data <- "data/risk"
if(!file.exists(here::here(path_data, "gps_ci.csv"))){

  library(tidyverse)
  source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_gps <- format_path("risk/models/gps")

  read_csv(here::here(path_gps, "pp_all_tibble.csv"), show_col_types = FALSE) |>
    write_csv(here::here(path_data, "gps_ci.csv"))
}
