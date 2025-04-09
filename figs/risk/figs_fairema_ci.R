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
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

col_groups <- gg_color_hue(2)

plot_ci <- function(d, title, delta, y = .70){
  the_label <- str_c("Delta = ", sprintf("%.2f", delta))
  d |> 
    ggplot(aes(x = model, color = model)) +
      geom_point(aes(y = median), size = 4) +
      geom_errorbar(aes(ymin = lower, ymax = upper),
        width = .2, 
        position = position_dodge(.9), 
        linewidth = 2) +
      labs(title = title) +
      xlab("") +
      ylab("auROC") +
      theme(legend.position = "none") +
      scale_y_continuous(breaks = seq(0.4, 1, 0.10), limits = c(0.4, 1)) +
      scale_color_manual(values = col_groups) +
      theme(title = element_text(size = 16, color = "#C5050C", face = "bold"),
            axis.text.x = element_text(size = 14, color = "black", face = "bold"),
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

delta <- round(ci[1, "median"] - ci[2, "median"], 2)
fig_fair_sex_1  <- ci |>
  filter(model == "Male" | model == "Female") |>
  mutate(model = fct(model, levels = c("Male", "Female")),
         model = fct_recode(model, "        male" = "Male", "      female" = "Female")) |> 
  plot_ci("Sex", delta, .78) 

delta <- round(ci[3, "median"] - ci[4, "median"], 2)
fig_fair_race_1  <- ci |>
  filter(model == "white" | model == "non_white") |>
  mutate(model = fct(model, levels = c("white", "non_white")),
         model = fct_recode(model, "white/non-hispanic" = "white", "other" = "non_white")) |> 
  plot_ci("Race/Ethnicity", delta, .59) 

delta <- round(ci[6, "median"] - ci[5, "median"], 2)
fig_fair_income_1  <- ci |>
  filter(model == "above_poverty" | model == "below_poverty") |>
  mutate(model = fct(model, levels = c("above_poverty", "below_poverty")),
         model = fct_recode(model, "above poverty" = "above_poverty", "below poverty" = "below_poverty")) |> 
  plot_ci("Income", delta, .65)
  
delta <- ci[7, "median"] - ci[8, "median"]
fig_fair_age_1  <- ci |>
  filter(model == "younger" | model == "older") |>
  mutate(model = fct(model, levels = c("younger", "older")),  
         model = fct_recode(model, "       older" = "older", "     younger" = "younger")) |> 
  plot_ci("Age", delta, .70)

# fig_race_only  <- ci |>
#   filter(model == "white" | model == "non_white") |>
#   mutate(model = fct(model, levels = c("white", "non_white")),
#          model = fct_recode(model, "White/Non-hispanic" = "white", "Other" = "non_white")) |> 
#   ggplot(aes(x = model, color = model)) +
#     geom_point(aes(y = median), size = 4) +
#     geom_errorbar(aes(ymin = lower, ymax = upper),
#       width = .2, 
#       position = position_dodge(.9),
#       linewidth = 2) +
#     labs(x = "Race/Ethnicity", y = "auROC") +
#     theme(legend.position = "none") +
#     scale_y_continuous(breaks = seq(0.4, 1, 0.10), limits = c(0.4, 1)) +
#     scale_color_manual(values = c("orange", "#C5050C")) +
#     theme(axis.text.x = element_text(size = 14, color = "black", face = "bold"),
#           axis.text.y = element_text(size = 12, color = "black"),
#           axis.title.x = element_text(size = 16, color = "black"),
#           axis.title.y = element_text(size = 16, color = "black")) +
#     geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
#     geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
#     annotate("text", label = "random", x = 0.75, y = .5, size = 6, color = "gray") +
#     annotate("text", label = "perfect", x = 0.75, y = 1, size = 6, color = "gray") 
