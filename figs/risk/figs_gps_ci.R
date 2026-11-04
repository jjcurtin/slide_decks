# auROC for demographics-only and geolocation models (study_gps) alongside the
# original EMA day model (same value as the Model Performance slide).
# All three are next-day (24-hour window) models; GPS vs EMA is descriptive only
# (different CV schemes, no formal contrast).

library(tidyverse)
theme_set(theme_classic())
path_data <- "data/risk"
path_mak <- "mak/risk"

# Okabe-Ito blue matches day model in figs_ema_probability.R
day_color <- "#0072B2"

gps_levels <- c("Demographics", "Geolocation\n(passive)", "Daily surveys\n(4x daily)")

source(here::here(path_mak, "mak_gps_ci.R"))
source(here::here(path_mak, "mak_ema_auroc_ci.R"))

ci_gps <- read_csv(here::here(path_data, "gps_ci.csv"), show_col_types = FALSE) |>
  mutate(model = recode_values(model,
                               "baseline" ~ gps_levels[1],
                               "full" ~ gps_levels[2])) |>
  rename(median = pp_median, lower = pp_lower, upper = pp_upper) |>
  bind_rows(read_csv(here::here(path_data, "ema_auroc_ci.csv"), show_col_types = FALSE) |>
              filter(model == "Day") |>
              mutate(model = gps_levels[3])) |>
  mutate(model = factor(model, levels = gps_levels))

# drop = FALSE keeps all three models on the x-axis so the two slides build
plot_gps_ci <- function(d) {
  d |>
    ggplot(aes(x = model)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  color = day_color, width = .2, linewidth = 1.25) +
    # hollow point drawn on top so the median stands out from the CI
    geom_point(aes(y = median), size = 3.5, shape = 21, fill = "white",
               stroke = 1.5, color = day_color) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
    annotate("text", label = "random", x = 0.5, y = .49, size = 6, color = "gray",
             hjust = 0, vjust = 1) +
    annotate("text", label = "perfect", x = 0.5, y = 1.01, size = 6, color = "gray",
             hjust = 0, vjust = 0) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(breaks = seq(0.4, 1, 0.10), limits = c(0.4, 1.05)) +
    labs(x = "Model Inputs", y = "auROC") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 16, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))
}

fig_gps_ci_1 <- ci_gps |>
  filter(model != gps_levels[3]) |>
  plot_gps_ci()

fig_gps_ci_2 <- ci_gps |>
  plot_gps_ci()
