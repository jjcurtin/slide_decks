#################################
# Set Up
#################################
library(tidyverse)
library(tidyposterior)
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_fairema <- format_path(str_c("studydata/risk/data_processed/fairema"))
path_out <- "data/risk"

# All of this code is temp to get CIs for CHM.  Not run because csv file already existed so need to be checked 

#################################
# Function for CIs
#################################  

posterior_auroc <- function(df, levels){
  q = c(.025, .5, .975)

  df |> 
    filter(group != "all") |>
    group_by(group) |>
    mutate(repeat_num = trunc((outer_split_num - 1) / 10 + 1),
           fold_num = outer_split_num %% 10) |> 
    select(-outer_split_num) |>
    pivot_wider(values_from = auroc, names_from = group) |>
    rename(id = repeat_num, id2 = fold_num) |>
    perf_mod(formula = statistic ~ model + (1 | id2/id),
             # prior_intercept = rstanarm::student_t(autoscale = TRUE),
             # prior = rstanarm::student_t(autoscale = TRUE),
             # transform = tidyposterior::logit_trans,  # for skewed & bounded AUC
             # iter = 2000, chains = 4,
             adapt_delta = .99,
             # cores = 4, seed = 12345,
             family = gaussian, verbose = FALSE)   |> 
    tidy(seed = 123) |> 
    group_by(model) |> 
    summarize(median = quantile(posterior, probs = q[2]),
              lower = quantile(posterior, probs = q[1]), 
              upper = quantile(posterior, probs = q[3]))
}

if (!file.exists(here::here(path_out, "fairema_ci_1day.csv"))) {

#################################  
# Get Data
#################################  

  df_sex <- read_csv(here::here(path_fairema, "outer_preds_perf_sex_1day_0_v5_nested_main.csv"),
                        col_types = cols()) |> 
    filter(group != "all", n_lapses != 0) |> 
    select(outer_split_num, group, auroc)
  
  df_race <- read_csv(here::here(path_fairema, "outer_preds_perf_race_1day_0_v5_nested_main.csv"),
                        col_types = cols()) |> 
    filter(group != "all", n_lapses != 0) |> 
    select(outer_split_num, group, auroc)
  
  df_income <- read_csv(here::here(path_fairema, "outer_preds_perf_income_1day_0_v5_nested_main.csv"),
                        col_types = cols()) |> 
    filter(group != "all", n_lapses != 0) |> 
    select(outer_split_num, group, auroc)
  
  df_age <- read_csv(here::here(path_fairema, "outer_preds_perf_age_1day_0_v5_nested_main.csv"),
                        col_types = cols()) |> 
    filter(group != "all", n_lapses != 0) |> 
    select(outer_split_num, group, auroc)


#################################
# Calc and save CIs
#################################

  ci_sex <- posterior_auroc(df_sex, c("above_poverty", "below_poverty"))
  ci_race <- posterior_auroc(df_race, c("white", "non_white"))
  ci_income <- posterior_auroc(df_income, c("above_poverty", "below_poverty"))
  ci_age  <-posterior_auroc(df_age, c("younger", "older"))
  
  ci_sex |> 
    bind_rows(ci_race) |> 
    bind_rows(ci_income) |> 
    bind_rows(ci_age) |> 
    write_csv(here::here(path_out, "fairema_ci_1day.csv"))


} # end if for data exists