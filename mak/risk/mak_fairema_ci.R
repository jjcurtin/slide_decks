---
title: "Fair EMA"
author: "John Curtin"
editor_options:
  chunk_output_type: console
params:
  study: "ema"
  window: "1day"
  lead: 0
  version: "v5"
  cv: "nested"
  model: "main" # "main" or "baseline"
---

## Set Up Environment


Function conflicts
```{r, packages_workflow}
options(conflicts.policy = "depends.ok")
library(tidyverse)
# library(tidyposterior)
theme_set(theme_classic()) 

devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_fairema <- format_path(str_c("studydata/risk/data_processed/fairema"))
```


## Temp analyses

All of this code is temp to get CIs to save on server for this presentation

This will evolve as the paper evolves.  code fences removed for easy comment out

### Function for CIs

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

### Get Data

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

### Calc and save CIs

ci_sex <- posterior_auroc(df_sex, c("above_poverty", "below_poverty"))
ci_race <- posterior_auroc(df_race, c("white", "non_white"))
ci_income <- posterior_auroc(df_income, c("above_poverty", "below_poverty"))
ci_age  <-posterior_auroc(df_age, c("younger", "older"))

ci_sex |> 
  bind_rows(ci_race) |> 
  bind_rows(ci_income) |> 
  bind_rows(ci_age) |> 
  write_csv(file.choose())


### Open temp saved CIs


```{r}
ci <- read_csv(here::here(path_fairema, "tmp_ci_jjc.csv")) |> 
  glimpse()
```

Function to plot CIs
```{r}
plot_ci <- function(d, title){
  d |> 
    ggplot(aes(x = model, color = model)) +
    geom_point(aes(y = median), size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
      width = .2, position = position_dodge(.9)) +
    coord_flip() +
    ylab("auROC") +
    xlab("Group") +
    ggtitle(title) +
    xlab("") +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(0.6, 1, 0.10), limits = c(0.6, 1)) +
    scale_color_manual(values = c("green", "blue")) +
    theme(title = element_text(size = 18, color = "orange"),
          axis.text.x = element_text(size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 16, color = "black"),
          axis.title.y = element_text(size = 16, color = "black"))
}
```


```{r}
plot_sex  <- ci |>
  filter(model == "Male" | model == "Female") |>
  mutate(model = fct(model, levels = c("Female", "Male")),
         model = fct_recode(model, male = "Male", female = "Female")) |> 
  plot_ci("Sex")

plot_race  <- ci |>
  filter(model == "white" | model == "non_white") |>
  mutate(model = fct(model, levels = c("non_white", "white")),
         model = fct_recode(model, white = "white", other = "non_white")) |> 
  plot_ci("Race/Ethnicity")

plot_income  <- ci |>
  filter(model == "above_poverty" | model == "below_poverty") |>
  mutate(model = fct(model, levels = c("below_poverty", "above_poverty")),
         model = fct_recode(model, `above poverty` = "above_poverty", `below poverty` = "below_poverty")) |> 
  plot_ci("Income")
  
plot_age  <- ci |>
  filter(model == "younger" | model == "older") |>
  mutate(model = fct(model, levels = c("older", "younger"))) |> 
  plot_ci("Age")
```

```{r}
#| label: fig-fair
#| fig-height: 6
#| fig-width: 8

cowplot::plot_grid(plot_race, plot_income, plot_sex, plot_age)
```