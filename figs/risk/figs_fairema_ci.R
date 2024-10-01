################################
# Set Up Environment
################################

library(tidyverse)
theme_set(theme_classic()) 
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_data <- "data/risk/"
path_mak <- "mak/risk"

source(here::here(path_mak, "mak_fairema_ci.R"))
ci <- read_csv(here::here(path_data, "fairema_ci_1day.csv"))

################################
# Function to plot CIs
################################

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


fig_sex  <- ci |>
  filter(model == "Male" | model == "Female") |>
  mutate(model = fct(model, levels = c("Female", "Male")),
         model = fct_recode(model, male = "Male", female = "Female")) |> 
  plot_ci("Sex")

fig_race  <- ci |>
  filter(model == "white" | model == "non_white") |>
  mutate(model = fct(model, levels = c("non_white", "white")),
         model = fct_recode(model, white = "white", other = "non_white")) |> 
  plot_ci("Race/Ethnicity")

fig_income  <- ci |>
  filter(model == "above_poverty" | model == "below_poverty") |>
  mutate(model = fct(model, levels = c("below_poverty", "above_poverty")),
         model = fct_recode(model, `above poverty` = "above_poverty", `below poverty` = "below_poverty")) |> 
  plot_ci("Income")
  
fig_age  <- ci |>
  filter(model == "younger" | model == "older") |>
  mutate(model = fct(model, levels = c("older", "younger"))) |> 
  plot_ci("Age")