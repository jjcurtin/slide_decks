################################
# Set Up Environment
################################

library(tidyverse)
theme_set(theme_classic()) 
path_data <- "data/risk2/"

ci <- read_csv(here::here(path_data, "auroc_fair_ci.csv"),
               show_col_types = FALSE)

################################
# Function to plot CIs
################################

plot_ci <- function(d, title, delta, y = .85){
  the_label <- str_c("Delta = ", sprintf("%.2f", delta))
  d |> 
    ggplot(aes(x = model, color = model)) +
    geom_point(aes(y = median), size = 2.5) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
      width = .2, position = position_dodge(.9),
      linewidth = 1.5) +
    ylab("auROC") +
    labs(title = title) +
    xlab("") +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(0.4, 1, 0.10), limits = c(0.4, 1)) +
    scale_color_manual(values = c("orange", "#C5050C")) +
    theme(title = element_text(size = 16, color = "#C5050C", face = "bold"),
          axis.text.x = element_text(size = 14, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 16, color = "black"),
          axis.title.y = element_text(size = 16, color = "black")) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
    annotate("text", label = "random", x = 0.75, y = .5, size = 6, color = "gray") +
    annotate("text", label = "perfect", x = 0.75, y = 1, size = 6, color = "gray") +
    annotate("text", label = the_label,  x = 2, y = y, size = 6, color = "#C5050C") 
}

################################
# Make the plots
################################

delta <- ci[2, "median"] - ci[1, "median"]
fig_fair_sex_2  <- ci |>
  filter(model == "male" | model == "female") |>
  mutate(model = fct(model, levels = c("male", "female"))) |> 
  plot_ci("Sex", delta)

delta <- ci[4, "median"] - ci[3, "median"]
fig_fair_race_2  <- ci |>
  filter(model == "white/non-hispanic" | model == "other") |>
  mutate(model = fct(model, levels = c("white/non-hispanic", "other"))) |> 
  plot_ci("Race/Ethnicity", delta)

delta <- ci[6, "median"] - ci[5, "median"]
fig_fair_income_2  <- ci |>
  filter(model == "above poverty" | model == "below poverty") |>
  mutate(model = fct(model, levels = c("above poverty", "below poverty"))) |> 
  plot_ci("Income", delta) 
  
#fig_age  <- ci |>
#  filter(model == "younger" | model == "older") |>
#  mutate(model = fct(model, levels = c("younger", "older")),  
#         model = fct_recode(model, "       older" = "older", "     younger" = "younger")) |> 
#  plot_ci("Age")

# fig_race_only  <- ci |>
#   filter(model == "white" | model == "non_white") |>
#   mutate(model = fct(model, levels = c("white", "non_white")),
#          model = fct_recode(model, "White/Non-hispanic" = "white", "Other" = "non_white")) |> 
#   ggplot(aes(x = model, color = model)) +
#     geom_point(aes(y = median), size = 3) +
#     geom_errorbar(aes(ymin = lower, ymax = upper),
#       width = .2, position = position_dodge(.9)) +
#     labs(x = "Race/Ethnicity", y = "auROC") +
#     theme(legend.position = "none") +
#     scale_y_continuous(breaks = seq(0.4, 1, 0.10), limits = c(0.4, 1)) +
#     scale_color_manual(values = c("orange", "#C5050C")) +
#     theme(axis.text.x = element_text(size = 14, color = "black"),
#           axis.text.y = element_text(size = 12, color = "black"),
#           axis.title.x = element_text(size = 16, color = "black"),
#           axis.title.y = element_text(size = 16, color = "black")) +
#     geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
#     geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
#     annotate("text", label = "random", x = 0.75, y = .5, size = 6, color = "gray") +
#     annotate("text", label = "perfect", x = 0.75, y = 1, size = 6, color = "gray") 
