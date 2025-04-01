path_data <- "data/risk"
if(!file.exists(here::here(path_data, "ema_acc_ci.csv"))){
  
  library(tidyverse)
  devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
  path_ema <- format_path(str_c("studydata/risk/models/ema"))
 
  # open preds 
  source(here::here(path_mak, "mak_probability.R"))  # create data if needed
  preds_week <- read_rds(here::here(path_data, "ema_preds_1week.rds"))
  preds_day <- read_rds(here::here(path_data, "ema_preds_1day.rds"))
  preds_hour <- read_rds(here::here(path_data, "ema_preds_1hour.rds")) 
 
  # calc ROC curves 
  roc_week <- preds_week |> 
    yardstick::roc_curve(prob_beta, truth = label) 
  roc_day <- preds_day |> 
    yardstick::roc_curve(prob_beta, truth = label)
  roc_hour <- preds_hour|> 
    yardstick::roc_curve(prob_beta, truth = label) 
  
  
  # Bal acccuracy CIs   
  j_thres_week <- roc_week |> 
    mutate(j = sensitivity + specificity - 1) |> 
    slice_max(j) |> 
    pull(.threshold)
  acc_week <- preds_week |>
    mutate(pred = if_else(prob_beta > j_thres_week, "Lapse", "No lapse"),
           pred = fct(pred, levels = c("Lapse", "No lapse"))) |> 
    group_by(outer_split_num) |>
    summarize(bal_acc = yardstick::bal_accuracy_vec(label, pred)) |> 
    ungroup() |> 
    summarize(mean_bal_acc = mean(bal_acc),
              se = sd(bal_acc) / sqrt(n()),
              lower = mean_bal_acc - 2.045 * se,
              upper = mean_bal_acc + 2.045 * se) |> 
    mutate(model = "Week")
  
  
  j_thres_day <- roc_day |> 
    mutate(j = sensitivity + specificity - 1) |> 
    slice_max(j) |> 
    pull(.threshold)
  acc_day <- preds_day |>
    mutate(pred = if_else(prob_beta > j_thres_day, "Lapse", "No lapse"),
           pred = fct(pred, levels = c("Lapse", "No lapse"))) |> 
    group_by(outer_split_num) |>
    summarize(bal_acc = yardstick::bal_accuracy_vec(label, pred)) |> 
    ungroup() |> 
    summarize(mean_bal_acc = mean(bal_acc),
              se = sd(bal_acc) / sqrt(n()),
              lower = mean_bal_acc - 2.045 * se,
              upper = mean_bal_acc + 2.045 * se) |> 
    mutate(model = "Day")
  
  j_thres_hour <- roc_hour |> 
    mutate(j = sensitivity + specificity - 1) |> 
    slice_max(j) |> 
    pull(.threshold)
  acc_hour <- preds_hour |>
    mutate(pred = if_else(prob_beta > j_thres_hour, "Lapse", "No lapse"),
           pred = fct(pred, levels = c("Lapse", "No lapse"))) |> 
    group_by(outer_split_num) |>
    summarize(bal_acc = yardstick::bal_accuracy_vec(label, pred)) |> 
    ungroup() |> 
    summarize(mean_bal_acc = mean(bal_acc),
              se = sd(bal_acc) / sqrt(n()),
              lower = mean_bal_acc - 2.045 * se,
              upper = mean_bal_acc + 2.045 * se) |>
    mutate(model = "Hour")
  
  ci <- bind_rows(acc_week, acc_day, acc_hour) |> 
    rename(bal_acc = mean_bal_acc) |>
    relocate(model) |> 
    write_csv(here::here(path_data, "ema_acc_ci.csv"))
  
}