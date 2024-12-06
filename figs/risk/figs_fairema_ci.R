################################
# Set Up Environment
################################

library(tidyverse)
theme_set(theme_classic()) 
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_data <- "data/risk/"
path_mak <- "mak/risk"

source(here::here(path_mak, "mak_fairema_ci.R"))
ci <- read_csv(here::here(path_data, "fairema_ci_1day.csv"),
               show_col_types = FALSE)

################################
# Function to plot CIs
################################

plot_ci <- function(d, title){
  d |> 
    ggplot(aes(x = model, color = model)) +
    geom_point(aes(y = median), size = 4) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
      width = .2, 
      position = position_dodge(.9), 
      linewidth = 2) +
    ylab("auROC") +
    xlab("Group") +
    labs(title = title) +
    xlab("") +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(0.4, 1, 0.10), limits = c(0.4, 1)) +
    scale_color_manual(values = c("orange", "#C5050C")) +
    theme(title = element_text(size = 16, color = "#C5050C"),
          axis.text.x = element_text(size = 14, color = "black", face = "bold"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 16, color = "black"),
          axis.title.y = element_text(size = 16, color = "black"))
}

################################
# Make the plots
################################
fig_sex  <- ci |>
  filter(model == "Male" | model == "Female") |>
  mutate(model = fct(model, levels = c("Male", "Female")),
         model = fct_recode(model, "        male" = "Male", "      female" = "Female")) |> 
  plot_ci("Sex") 

fig_race  <- ci |>
  filter(model == "white" | model == "non_white") |>
  mutate(model = fct(model, levels = c("white", "non_white")),
         model = fct_recode(model, "white/non-hispanic" = "white", "       other" = "non_white")) |> 
  plot_ci("Race/Ethnicity") 

fig_income  <- ci |>
  filter(model == "above_poverty" | model == "below_poverty") |>
  mutate(model = fct(model, levels = c("above_poverty", "below_poverty")),
         model = fct_recode(model, "above poverty" = "above_poverty", "below poverty" = "below_poverty")) |> 
  plot_ci("Income")
  
fig_age  <- ci |>
  filter(model == "younger" | model == "older") |>
  mutate(model = fct(model, levels = c("younger", "older")),  
         model = fct_recode(model, "       older" = "older", "     younger" = "younger")) |> 
  plot_ci("Age")

fig_race_only  <- ci |>
  filter(model == "white" | model == "non_white") |>
  mutate(model = fct(model, levels = c("white", "non_white")),
         model = fct_recode(model, "White/Non-hispanic" = "white", "Other" = "non_white")) |> 
  ggplot(aes(x = model, color = model)) +
    geom_point(aes(y = median), size = 4) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
      width = .2, 
      position = position_dodge(.9),
      linewidth = 2) +
    labs(x = "Race/Ethnicity", y = "auROC") +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(0.4, 1, 0.10), limits = c(0.4, 1)) +
    scale_color_manual(values = c("orange", "#C5050C")) +
    theme(axis.text.x = element_text(size = 14, color = "black", face = "bold"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 16, color = "black"),
          axis.title.y = element_text(size = 16, color = "black")) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
    annotate("text", label = "random", x = 0.75, y = .5, size = 6, color = "gray") +
    annotate("text", label = "perfect", x = 0.75, y = 1, size = 6, color = "gray") 
