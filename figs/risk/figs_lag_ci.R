# Data verified 2026-11-03 against study_lag (pp_perf_tibble.csv); all lags are 24-hour windows

library(tidyverse)
theme_set(theme_classic()) 
path_data <- "data/risk" 
path_mak <- "mak/risk"

# Lagged models are all day (24-hour window) models; Okabe-Ito blue matches figs_ema_probability.R
day_color <- "#0072B2"

source(here::here(path_mak, "mak_lag_ci.R"))
ci <- read_csv(here::here(path_data, "lag_ci.csv"),
               show_col_types = FALSE) |>
  mutate(model = recode_values(model,
                            "0 lag" ~ "No lag",
                            "24 lag" ~ "1 day",
                            "72 lag" ~ "3 day",
                            "168 lag" ~ "1 week",
                            "336 lag" ~ "2 week")) |> 
  mutate(model = fct(model, 
                     levels = c("No lag", "1 day", "3 day", 
                                "1 week", "2 week"))) 

fig_lag_ci <- ci |>
  ggplot(aes(x = model)) +
  geom_errorbar(aes(ymin = pp_lower, ymax = pp_upper),
    color = day_color,
    width = .2,
    position = position_dodge(.9),
    linewidth = 1.25) +
  # hollow point drawn on top so the median stands out from the CI
  geom_point(aes(y = pp_median), size = 3.5, shape = 21, fill = "white",
             stroke = 1.5, color = day_color) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
    annotate("text", label = "random", x = 0.5, y = .49, size = 6, color = "gray",
             hjust = 0, vjust = 1) +
    annotate("text", label = "perfect", x = 0.5, y = 1.01, size = 6, color = "gray",
             hjust = 0, vjust = 0) +
  # coord_flip() +
  ylab("auROC") +
  xlab("Prediction Lag") +
  theme(legend.position = "none") +
  scale_y_continuous(
    breaks = seq(0.4, 1, 0.10),
    limits = c(0.4, 1.05)
  ) +
  theme(axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))