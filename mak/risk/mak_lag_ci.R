path_data <- "data/risk"
if(!file.exists(here::here(path_data, "lag_ci.csv"))){
  
  library(tidyverse)
  devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_lag <- format_path(str_c("studydata/risk/models/lag"))
  
  q = c(.025, .5, .975)
  ci <- read_csv(here::here(path_lag, "pp_tidy.csv")) |>   
    group_by(model) |> 
    summarize(pp_median = quantile(posterior, probs = q[2]),
              pp_lower = quantile(posterior, probs = q[1]), 
              pp_upper = quantile(posterior, probs = q[3])) |> 
    mutate(model = factor(model, levels = c("lag0", "lag24", "lag72", "lag168", "lag336"),
                          labels = c("0 lag", "24 lag", "72 lag", "168 lag", "336 lag"))) |> 
    arrange(model) |> 
    write_csv(here::here(path_data, "lag_ci.csv"))
} 
