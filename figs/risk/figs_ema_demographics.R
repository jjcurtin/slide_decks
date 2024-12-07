####################################
# Setup
####################################

library(tidyverse)
theme_set(theme_classic()) 

devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true",
                     sha1 = "a58e57da996d1b70bb9a5b58241325d6fd78890f")
path_data <- "data/risk"
path_mak <- "mak/risk"

####################################
# Read in data
####################################
source(here::here(path_mak, "mak_ema_demographics.R")) # make data if needed
d <- read_csv(here::here(path_data, "ema_demographics.csv"), 
                   show_col_types = FALSE)


####################################
# Plots
####################################

# Education
fig_educ <- d |>
  mutate(group = educ) |> 
  mutate(group = fct(group, levels = c(
    "<= High school", "Some college",
    "4 year college degree", "Advanced degree"
  ))) |>
  count(group) |>
  ggplot(aes(x = "", y = n, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  #scale_fill_manual(values = c("red", "green", "blue", "yellow")) +
  geom_text(aes(label = group), position = position_stack(vjust = 0.7),
  size = 3) + 
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("Education") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(title = element_text(size = 16, face = "bold", color = "#C5050C"))

# Race/ethnicity
fig_race <- d |>
  mutate(group = white) |> 
  count(group) |>
  ggplot(aes(x = "", y = n, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  #scale_fill_manual(values = c("red", "green", "blue", "yellow")) +
  scale_fill_manual(values = c("#C5050C", "gold")) +
  geom_text(aes(label = group), position = position_stack(vjust = 0.7),
  size = 6) + 
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("Race/Ethnicity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(title = element_text(size = 16, face = "bold", color = "#C5050C"))

# Sex
fig_sex <- d |>
  mutate(group = sex) |> 
  count(group) |>
  ggplot(aes(x = "", y = n, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  #scale_fill_manual(values = c("red", "green", "blue", "yellow")) +
  geom_text(aes(label = group), position = position_stack(vjust = 0.7),
  size = 6) + 
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("Sex at Birth") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(title = element_text(size = 16, face = "bold", color = "#C5050C"))

# Marital status
fig_ms <- d |>
  mutate(group = ms) |> 
  count(group) |>
  ggplot(aes(x = "", y = n, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  #scale_fill_manual(values = c("red", "green", "blue", "yellow")) +
  geom_text(aes(label = group), position = position_stack(vjust = 0.5),
  size = 3) + 
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("Marital Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(title = element_text(size = 16, face = "bold", color = "#C5050C"))


# Age
fig_age <- d |>
  ggplot(aes(x = age)) +
  geom_histogram(fill = "blue", color = "black", bins = 20) +
  labs(x = "Age", y = "Count") +
  ggtitle("Age") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  theme(title = element_text(size = 16, face = "bold", color = "#C5050C"))

# Income
fig_income <- d |>
  ggplot(aes(x = income)) +
  geom_histogram(fill = "blue", color = "black", bins = 20) +
  labs(x = "Income ($ in thousands)", y = "Count") +
  ggtitle("Income") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  theme(title = element_text(size = 16, face = "bold", color = "#C5050C"))

# AUD Sx
fig_sx <- d |>
  ggplot(aes(x = aud_total)) +
  geom_histogram(bins = 12, fill = "blue", color = "black") +
  labs(x = "Number of AUD Symptoms", y = "Count") +
  ggtitle("AUD Symptom Count") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = seq(0, 11),
                       limits = c(0, 11)) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  theme(title = element_text(size = 16, face = "bold", color = "#C5050C"))