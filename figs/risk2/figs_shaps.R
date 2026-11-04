# OUD v13 day model feature importance, two takes (see mak_shaps.R):
#   fig_global_oud: mean |SHAP| (same metric as AUD fig_global_day)
#   fig_daymax_oud: proportion of days each category was the top feature
# Top n categories by each metric; bars colored by data source.

library(tidyverse)
theme_set(theme_classic())
path_data <- "data/risk2"
path_mak <- "mak/risk2"

n_top <- 15

# Survey = day blue (as in AUD figure); geolocation and other/demographic distinct
source_colors <- c(survey = "#0072B2", geolocation = "#E69F00", other = "#A6A6A6")

source(here::here(path_mak, "mak_shaps.R"))

# "Urge (EMA)" -> "urge". Raw/Diff stripping kept so this also works on the
# paper-faithful caches (where labels can repeat; plotting uses variable_grp).
clean_shap_labels <- function(d) {
  d |>
    mutate(source = str_extract(variable_grp, "(?<=\\()[^)]+(?=\\)$)"),
           source = case_when(source == "EMA" ~ "survey",
                              source == "gps" ~ "geolocation",
                              .default = "other"),
           source = factor(source, levels = names(source_colors)),
           label = str_remove(variable_grp, " \\([^)]+\\)$"),
           label = str_remove(label, ", (Diff|Raw)$"),
           label = str_to_lower(str_sub(label, 1, 1)) |> str_c(str_sub(label, 2)),
           label = if_else(label == "religious", "religious locations", label))
}

# levels = NULL: top n_top by this metric, ordered by it.
# levels supplied (bottom-to-top): show only those categories in that order.
plot_shap_bars <- function(d, metric, x_lab, levels = NULL) {
  if (is.null(levels)) {
    d <- d |>
      slice_max({{ metric }}, n = n_top, with_ties = FALSE) |>
      mutate(variable_grp = fct_reorder(variable_grp, {{ metric }}))
  } else {
    d <- d |>
      filter(variable_grp %in% levels) |>
      mutate(variable_grp = factor(variable_grp, levels = levels))
  }
  d |>
    ggplot(aes(x = {{ metric }}, y = variable_grp, fill = source)) +
    geom_col() +
    scale_y_discrete(labels = setNames(d$label, as.character(d$variable_grp))) +
    scale_fill_manual(values = source_colors, drop = FALSE) +
    labs(x = x_lab, y = NULL, fill = NULL) +
    theme(axis.text.y = element_text(size = 14, face  = "bold"),
          axis.text.x = element_text(size = 14, face  = "bold"),
          axis.title.x = element_text(size = 14, face  = "bold"),
          legend.text = element_text(size = 14),
          legend.position = "inside",
          legend.position.inside = c(.8, .2))
}

# Raw + change combined per construct (see mak_shaps.R); paper-faithful
# 88-category versions remain in shaps_global.csv / shaps_day_max.csv
shaps_global_oud <- read_csv(here::here(path_data, "shaps_global_combined.csv"),
                             show_col_types = FALSE) |>
  clean_shap_labels()

shaps_daymax_oud <- read_csv(here::here(path_data, "shaps_day_max_combined.csv"),
                             show_col_types = FALSE) |>
  clean_shap_labels()

fig_global_oud <- shaps_global_oud |>
  plot_shap_bars(mean_value, "Mean |SHAP| value (in Log-Odds)")

# Own order: top n_top categories by proportion of days
fig_daymax_oud <- shaps_daymax_oud |>
  plot_shap_bars(prop, "Proportion of days as top feature")

# Global order: same categories and order as fig_global_oud (as in the paper's fig-3a).
# Categories never top on any day are absent from the cache, so add them as 0.
global_levels <- shaps_global_oud |>
  slice_max(mean_value, n = n_top, with_ties = FALSE) |>
  arrange(mean_value) |>
  pull(variable_grp)

fig_daymax_oud_global_order <- shaps_global_oud |>
  select(variable_grp, source, label) |>
  left_join(shaps_daymax_oud |> select(variable_grp, prop), by = "variable_grp") |>
  mutate(prop = replace_na(prop, 0)) |>
  plot_shap_bars(prop, "Proportion of days as top feature", levels = global_levels)
