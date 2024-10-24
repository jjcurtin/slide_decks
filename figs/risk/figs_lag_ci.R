# NOTE: THE DATA FOR THIS FIG NEED TO BE CHECKED!


library(tidyverse)
theme_set(theme_classic()) 
path_data <- "data/risk" 
path_mak <- "mak/risk"

source(here::here(path_mak, "mak_lag_ci.R"))
ci <- read_csv(here::here(path_data, "lag_ci.csv"),
               show_col_types = FALSE) |>
  mutate(model = case_match(model,
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
  geom_point(aes(y = pp_median), size = 2) +
  geom_errorbar(aes(ymin = pp_lower, ymax = pp_upper),
    width = .2,
    position = position_dodge(.9)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  # coord_flip() +
  ylab("auROC") +
  xlab("Prediction Lag") +
  theme(legend.position = "none") +
  scale_y_continuous(
    breaks = seq(0.4, 1, 0.10),
    limits = c(0.4, 1)
  ) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))