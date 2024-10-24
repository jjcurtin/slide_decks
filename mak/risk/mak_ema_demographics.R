# set up
library(tidyverse)
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true",
                     sha1 = "a58e57da996d1b70bb9a5b58241325d6fd78890f")
path_processed <- format_path("studydata/risk/data_processed/ema")
path_shared <- format_path("studydata/risk/data_processed/shared")
path_out <- "data/risk"

if (!file.exists(here::here(path_out, "ema_demographics.csv"))) {
  
  # open disposition df on server  
  disposition <- read_csv(here::here(path_processed, "disposition.csv"), 
                          col_types = "ccDDcccccccccc")
  
  # open screen df on server
  screen <- read_csv(file.path(path_shared, "screen.csv"), 
                     col_types = cols()) |>
    filter(subid %in% subset(disposition, analysis == "yes")$subid) |> 
    mutate(across(dsm5_1:dsm5_11, ~ if_else(.x == "Yes", 1, 0))) |>  
    rowwise() |>  
    mutate(aud_total = sum(c(dsm5_1, dsm5_2, dsm5_3, dsm5_4, dsm5_5, dsm5_6, dsm5_7, 
                                dsm5_8, dsm5_9, dsm5_10, dsm5_11))) |> 
    ungroup() |> 
    select(age = dem_1, sex = dem_2, white = dem_3, educ = dem_5, 
           income = dem_7, ms = dem_8, aud_total)   |> 
    mutate(white = if_else(white == "White/Caucasian", "White/Non-hispanic", "Other")) |> 
    mutate(educ = case_match(educ, 
                    "2-Year degree" ~ "Some college",
                    "High school or GED" ~ "<= High school", 
                    "Less than high school or GED degree" ~ "<= High school",
                    "College degree" ~ "4 year college degree",
                    .default = educ)) |> 
    mutate(ms = case_match(ms,
                  "Never Married" ~ "Never married",
                  "Separated" ~ "Divorced",
                  .default = ms)) |> 
    mutate(income =  income / 1000)
  
  
  # write to slide decks repo
  screen |> 
    write_csv(here::here(path_out, "ema_demographics.csv"))
}