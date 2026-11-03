################################
# auROC for the three AUD models (week, day, hour) plus the OUD day model,
# set apart by a light vertical line. Styling matches plot_ci() in
# figs/risk/figs_ema_probability.R
################################

library(tidyverse)
theme_set(theme_classic())

# Okabe-Ito model colors; keep in sync with figs/risk/figs_ema_probability.R.
# OUD model is a day (24-hour window) model, so it uses the day blue
model_colors <- c(week = "#D55E00", day = "#0072B2", hour = "#009E73")

path_data <- "data/risk"
source(here::here("mak/risk/mak_ema_auroc_ci.R"))  # create data if needed
source(here::here("mak/risk2/mak_perf_ci.R"))      # create data if needed

auroc_aud <- read_csv(here::here("data/risk", "ema_auroc_ci.csv"),
                      show_col_types = FALSE)
auroc_oud <- read_csv(here::here("data/risk2", "auroc_perf_ci.csv"),
                      show_col_types = FALSE) |>
  filter(model == "full") |>
  mutate(model = "Day (OUD)")

auroc_all <- bind_rows(auroc_aud, auroc_oud) |>
  mutate(model = factor(model, levels = c("Week", "Day", "Hour", "Day (OUD)")))

plot_colors <- unname(model_colors[c("week", "day", "hour", "day")])

fig_auroc_ci_oud <- auroc_all |>
  ggplot(aes(x = model)) +
    geom_vline(xintercept = 3.5, color = "gray85", linewidth = 1) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  color = plot_colors,
                  width = .2,
                  linewidth = 1.25) +
    geom_point(aes(y = median), size = 3.5, shape = 21, fill = "white",
               stroke = 1.5, color = plot_colors) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
    annotate("text", label = "random", x = 0.5, y = .49, size = 6, color = "gray",
             hjust = 0, vjust = 1) +
    annotate("text", label = "perfect", x = 0.5, y = 1.01, size = 6, color = "gray",
             hjust = 0, vjust = 0) +
    ylab("auROC") +
    xlab("Prediction Window") +
    scale_y_continuous(
      breaks = seq(0.4, 1, 0.10),
      limits = c(0.4, 1.05)) +
    theme(axis.text.x = element_text(size = 16, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))
