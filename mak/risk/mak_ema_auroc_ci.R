if(!file.exists(here::here(path_data, "ema_auroc_ci.csv"))){
  library(tidyverse)
  devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_ema <- format_path(str_c("studydata/risk/models/ema"))
  path_data <- "data/risk"
  
  q = c(.025, .5, .975)
  ci  <-   read_rds(here::here(path_ema, "posteriors_all_0_v5_nested.rds")) |> 
  tidy(seed = 123) |> 
  group_by(model) |> 
  summarize(median = quantile(posterior, probs = q[2]),
            lower = quantile(posterior, probs = q[1]), 
            upper = quantile(posterior, probs = q[3])) |> 
  mutate(model = factor(model, levels = c("week", "day", "hour"),
                        labels = c("Week", "Day", "Hour"))) |> 
  arrange(model) |> 
  write_csv(here::here(path_data, "ema_auroc_ci.csv"))
} 
