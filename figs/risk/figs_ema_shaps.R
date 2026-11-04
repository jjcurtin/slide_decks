#####################################
# Set up environment
#####################################

library(tidyverse)
theme_set(theme_classic()) 
source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_data <- "data/risk"
path_mak <- "mak/risk"

####################################
# load data
####################################
source(here::here(path_mak, "mak_ema_shaps.R"))
global <- read_csv(here::here(path_data, "ema_shaps_global.csv"), 
                   show_col_types = FALSE)


####################################################
# Global SHAP Plot
####################################################
fig_global_all <- global |>
  mutate(group = reorder(variable_grp, mean_value, sum)) |>
  mutate(window = fct(window, levels = c("week", "day", "hour"))) |> 
  ggplot() +
  geom_bar(aes(x = group, y = mean_value, fill = window), stat = "identity") +
  ylab("Mean |SHAP| value (in Log-Odds)") +
  xlab("") +
  coord_flip() +
  # Okabe-Ito model colors; keep in sync with figs_ema_probability.R
  scale_fill_manual(values = c(week = "#D55E00", day = "#0072B2", hour = "#009E73"))


# Day model: all categories, valence + arousal combined per observation
# (see mak_ema_shaps.R). Lapse hour dropped (exactly 0 for the day model).
# Labels follow the paper (lapse day -> day of week).
global_day <- read_csv(here::here(path_data, "ema_shaps_global_day.csv"),
                       show_col_types = FALSE)

fig_global_day <- global_day |>
  filter(variable_grp != "lapse hour (other)") |>
  mutate(variable_grp = recode_values(variable_grp,
                                      "lapse day (other)" ~ "day of week (other)",
                                      # matches "Abstinence Confidence" on the EMA slide
                                      "future efficacy (EMA item)" ~ "abstinence confidence (EMA item)",
                                      default = variable_grp),
         variable_grp = str_split_i(variable_grp, " \\(", 1),
         group = fct_reorder(variable_grp, mean_value)) |>
  ggplot() +
  geom_col(aes(x = mean_value, y = group), fill = "#0072B2") +
  labs(x = "Mean |SHAP| value (in Log-Odds)", y = NULL) +
  theme(axis.text.y = element_text(size = 14, face  = "bold"),
        axis.text.x = element_text(size = 14, face  = "bold"),
        axis.title.x = element_text(size = 14, face  = "bold"))

# Day model: proportion of observations each category was the top (most
# risk-increasing) feature; same labels and color as fig_global_day
fig_daymax_day <- read_csv(here::here(path_data, "ema_shaps_day_max.csv"),
                           show_col_types = FALSE) |>
  filter(variable_grp != "lapse hour (other)") |>
  mutate(variable_grp = recode_values(variable_grp,
                                      "lapse day (other)" ~ "day of week (other)",
                                      "future efficacy (EMA item)" ~ "abstinence confidence (EMA item)",
                                      default = variable_grp),
         variable_grp = str_split_i(variable_grp, " \\(", 1),
         group = fct_reorder(variable_grp, prop)) |>
  ggplot() +
  geom_col(aes(x = prop, y = group), fill = "#0072B2") +
  labs(x = "Proportion of days as top feature", y = NULL) +
  theme(axis.text.y = element_text(size = 14, face  = "bold"),
        axis.text.x = element_text(size = 14, face  = "bold"),
        axis.title.x = element_text(size = 14, face  = "bold"))

# Same, restricted to windows with predicted risk at or above the base rate
# (see ema_shaps_day_max_risk.csv for the >= .2 subset and winning SHAP sizes)
fig_daymax_day_risk <- read_csv(here::here(path_data, "ema_shaps_day_max_risk.csv"),
                                show_col_types = FALSE) |>
  filter(subset == "risk >= base rate") |>
  mutate(variable_grp = recode_values(variable_grp,
                                      "lapse day (other)" ~ "day of week (other)",
                                      "future efficacy (EMA item)" ~ "abstinence confidence (EMA item)",
                                      default = variable_grp),
         variable_grp = str_split_i(variable_grp, " \\(", 1),
         group = fct_reorder(variable_grp, prop)) |>
  ggplot() +
  geom_col(aes(x = prop, y = group), fill = "#0072B2") +
  labs(x = "Proportion of higher-risk days as top feature", y = NULL) +
  theme(axis.text.y = element_text(size = 14, face  = "bold"),
        axis.text.x = element_text(size = 14, face  = "bold"),
        axis.title.x = element_text(size = 14, face  = "bold"))

fig_global_hour <- global |>
  filter(window == "hour") |> 
  mutate(variable_grp = if_else(variable_grp == "arousal (EMA item)", "emotional state (EMA item)", variable_grp),
         variable_grp = if_else(variable_grp == "valence (EMA item)", "emotional state (EMA item)", variable_grp)) |>
  mutate(variable_grp = str_split_i(variable_grp, " \\(", 1)) |>
  mutate(group = reorder(variable_grp, mean_value, sum)) |>
  ggplot() +
  geom_bar(aes(x = group, y = mean_value), stat = "identity", fill  = "gray") +
  ylab("Mean |SHAP| value (in Log-Odds)") +
  xlab("") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 14, face  = "bold")) +
  theme(axis.text.x = element_text(size = 14, face  = "bold")) +
  theme(axis.title.x = element_text(size = 14, face  = "bold")) 
