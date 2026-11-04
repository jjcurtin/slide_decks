#####################################
# Set up environment
#####################################

library(tidyverse)
theme_set(theme_classic()) 
source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_data <- "data/opt"
path_mak <- "mak/opt"

####################################
# load data
####################################
# Illustrative (simulated) local SHAP values; see mak_opt_shaps.R
source(here::here(path_mak, "mak_opt_shaps.R"))
shaps_examples <- read_csv(here::here("data/opt", "opt_shaps_examples.csv"), 
                   show_col_types = FALSE)


####################################################
# Local SHAP Examples Plot
####################################################

# Labels, item order, and color match fig_global_day (figs/risk/figs_ema_shaps.R):
# valence + arousal summed into "emotional state", "future efficacy" relabeled,
# and items ordered by AUD day-model global importance (EMA items only)
features <- c("past use", "abstinence confidence", "craving", "past stressful event",
              "future risky situation", "past pleasant event", "past risky situation",
              "emotional state", "future stressful event")

shaps_examples <- shaps_examples |>
  mutate(group = case_when(group %in% c("valence", "arousal") ~ "emotional state",
                           group == "future efficacy" ~ "abstinence confidence",
                           .default = group)) |>
  summarise(shap = sum(shap), .by = c(example, group)) |>
  mutate(group = fct_rev(fct(group, levels = features)))

# Top factor(s) for each example (simulated values >= 1) get red labels
plot_shap_ex <- function(ex, subtitle) {
  d <- shaps_examples |> filter(example == ex)
  top <- levels(d$group) %in% d$group[d$shap >= 1]

  # suppress warning about vector of colors for axis text
  suppressWarnings(
    ggplot(d) +
      geom_col(aes(x = shap, y = group), fill = "#0072B2") +
      # shared scale so bars are comparable across examples
      scale_x_continuous(limits = c(0, 1.7), breaks = seq(0, 1.5, 0.5)) +
      labs(x = "|SHAP| value (in Log-Odds)", y = NULL, subtitle = subtitle) +
      theme(plot.subtitle = element_text(color = "#C5050C", face = "bold", size = 14),
            axis.text.y = element_text(size = 14, face = "bold",
                                       colour = if_else(top, "#C5050C", "black")),
            axis.text.x = element_text(size = 14, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"))
  )
}

fig_shap_ex1 <- plot_shap_ex(1, "Patient 1; Day 30")
fig_shap_ex2 <- plot_shap_ex(2, "Patient 2; Day 30")
fig_shap_ex3 <- plot_shap_ex(3, "Patient 2; Day 70")
