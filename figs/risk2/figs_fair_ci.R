################################
# Set Up Environment
################################

library(tidyverse)
theme_set(theme_classic()) 
path_data <- "data/risk2/"
path_mak <- "mak/risk2"

source(here::here(path_mak, "mak_fair_ci.R"))  # create data if needed
ci <- read_csv(here::here(path_data, "auroc_fair_ci.csv"),
               show_col_types = FALSE)
contrasts <- read_csv(here::here(path_data, "auroc_fair_contrast.csv"),
                      show_col_types = FALSE)

################################
# Function to plot CIs
################################
# Reference (advantaged) group = gray; comparison group = purple.
# Keep in sync with figs/risk/figs_fairema_ci.R
col_groups <- c("#A6A6A6", "#7B3294")

plot_ci <- function(d, title, delta, y = .85, x = 2){
  the_label <- str_c("Delta = ", sprintf("%.2f", delta))
  d |> 
    ggplot(aes(x = model, color = model)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
      width = .2, position = position_dodge(.9),
      linewidth = 1.25) +
    # hollow point on top; matches auROC/lag figures
    geom_point(aes(y = median), size = 3.5, shape = 21, fill = "white",
               stroke = 1.5) +
    ylab("auROC") +
    labs(title = title) +
    xlab("") +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(0.4, 1, 0.10), limits = c(0.4, 1.05)) +
    scale_color_manual(values = col_groups) +
    theme(title = element_text(size = 16, color = "#C5050C", face = "bold"),
          axis.text.x = element_text(size = 12, color = "black", face = "bold"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 16, color = "black"),
          axis.title.y = element_text(size = 16, color = "black")) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
    annotate("text", label = "random", x = 0.5, y = .49, size = 6, color = "gray",
             hjust = 0, vjust = 1) +
    annotate("text", label = "perfect", x = 0.5, y = 1.01, size = 6, color = "gray",
             hjust = 0, vjust = 0) +
    annotate("text", label = the_label,  x = x, y = y, size = 6, color = "#C5050C") 
}

################################
# Make the plots
################################

# Old version (pre-v13 data; delta = difference of medians, 3 subgroups)
# delta <- ci[2, "median"] - ci[1, "median"]
# fig_fair_sex_2  <- ci |>
#   filter(model == "male" | model == "female") |>
#   mutate(model = fct(model, levels = c("male", "female"))) |> 
#   plot_ci("Sex", delta)
# 
# delta <- ci[4, "median"] - ci[3, "median"]
# fig_fair_race_2  <- ci |>
#   filter(model == "white/non-hispanic" | model == "other") |>
#   mutate(model = fct(model, levels = c("white/non-hispanic", "other"))) |> 
#   plot_ci("Race/Ethnicity", delta)
# 
# delta <- ci[6, "median"] - ci[5, "median"]
# fig_fair_income_2  <- ci |>
#   filter(model == "above poverty" | model == "below poverty") |>
#   mutate(model = fct(model, levels = c("above poverty", "below poverty"))) |> 
#   plot_ci("Income", delta) 

# v13: delta = posterior median of contrast (comparison - reference);
# label centered between groups, just below both CIs (panels are narrow with 4 across)
plot_pair <- function(ref, comp, labels, title, grp) {
  d <- ci |>
    filter(model %in% c(ref, comp)) |>
    mutate(model = factor(model, levels = c(ref, comp), labels = labels))
  delta <- contrasts |> filter(group == grp) |> pull(median)
  plot_ci(d, title, delta, y = min(d$lower) - .05, x = 1.5)
}

fig_fair_race_2 <- plot_pair("white/non-hispanic", "other",
                             c("white/\nnon-hispanic", "other"),
                             "Race/Ethnicity", "race")
fig_fair_income_2 <- plot_pair("above poverty", "below poverty",
                               c("above\npoverty", "below\npoverty"),
                               "Income", "income")
fig_fair_sex_2 <- plot_pair("male", "not male",
                            c("male", "not male"),
                            "Gender", "sex")
fig_fair_geo_2 <- plot_pair("urban", "rural",
                            c("urban", "rural"),
                            "Geography", "geography")

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
