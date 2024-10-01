# NOTE: THE DATA FOR THIS FIG NEED TO BE CHECKED!


library(tidyverse)
theme_set(theme_classic()) 
path_data <- "data/risk" 
path_mak <- "mak/risk"

source(here::here(path_mak, "mak_lag_ci.R"))


ci <- read_csv(here::here(path_data, "lag_ci.csv")) |>
  mutate(model = case_match(model,
                            "0 lag" ~ "No lag",
                            "24 lag" ~ "1 day lag",
                            "72 lag" ~ "3 day lag",
                            "168 lag" ~ "1 week lag",
                            "336 lag" ~ "2 week lag")) |> 
  mutate(model = fct(model, 
                     levels = c("2 week lag", "1 week lag", "3 day lag", 
                                "1 day lag", "No lag"))) 


fig_lag_ci <- ci |>
  ggplot(aes(x = model)) +
  geom_point(aes(y = pp_median), size = 2) +
  geom_errorbar(aes(ymin = pp_lower, ymax = pp_upper),
    width = .2,
    position = position_dodge(.9)
  ) +
  coord_flip() +
  ylab("auROC") +
  xlab("Prediction Window Lag") +
  theme(legend.position = "none") +
  scale_y_continuous(
    breaks = seq(0.8, 1, 0.05),
    limits = c(0.8, 1)
  ) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))