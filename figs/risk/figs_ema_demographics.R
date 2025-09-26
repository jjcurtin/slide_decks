####################################
# Setup
####################################

library(tidyverse)
theme_set(theme_classic()) 

source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_data <- "data/risk"
path_mak <- "mak/risk"

####################################
# Read in data
####################################
source(here::here(path_mak, "mak_ema_demographics.R")) # make data if needed
d <- read_csv(here::here(path_data, "ema_demographics.csv"), 
                   show_col_types = FALSE)


####################################
# Bar/Histogram Plots
####################################

# Education
fig_educ_bar <- d |>
  mutate(educ = fct(educ, levels = c(
    "<= High school", "Some college",
    "4 year college degree", "Advanced degree"))) |>
  count(educ) |>
  ggplot(aes(x = educ, y = n, fill = educ)) +
    geom_bar(stat = "identity", width = 1) +
    labs(title = "Education", x = NULL, y = "count") +
    theme(legend.title = element_blank(), legend.position = "none") +
    theme(axis.text = element_text(size = 12, face = "bold")) + 
    theme(axis.text.x = element_text(size = 10, color = "black", angle = 20, hjust = 1)) + 
    theme(plot.title = element_text(size = 15, face = "bold", color = "#C5050C"))

# Race
fig_race_bar <- d |>
  mutate(white = fct(white, levels = c(
    "White/Non-hispanic", "Other"))) |>
  count(white) |>
  ggplot(aes(x = white, y = n, fill = white)) +
    geom_bar(stat = "identity", width = 1) +
    labs(title = "Race/Ethnicity", x = NULL, y = "count") +
    theme(legend.title = element_blank(), legend.position = "none") +
    theme(axis.text = element_text(size = 12, face = "bold")) + 
    theme(axis.text.x = element_text(size = 10, color = "black", angle = 20, hjust = 1)) + 
    theme(plot.title = element_text(size = 15, face = "bold", color = "#C5050C"))

# Sex
fig_sex_bar <- d |>
  mutate(sex = fct(sex, levels = c(
    "Male", "Female"))) |>
  count(sex) |>
  ggplot(aes(x = sex, y = n, fill = sex)) +
    geom_bar(stat = "identity", width = 1) +
    labs(title = "Sex at Birth", x = NULL, y = "count") +
    theme(legend.title = element_blank(), legend.position = "none") +
    theme(axis.text = element_text(size = 12, face = "bold")) + 
    theme(axis.text.x = element_text(size = 10, color = "black", angle = 20, hjust = 1)) + 
    theme(plot.title = element_text(size = 15, face = "bold", color = "#C5050C"))

# Marital status
fig_ms_bar <- d |>
  count(ms) |>
  ggplot(aes(x = ms, y = n, fill = ms)) +
    geom_bar(stat = "identity", width = 1) +
    labs(title = "Marital Status", x = NULL, y = "count") +
    theme(legend.title = element_blank(), legend.position = "none") +
    theme(axis.text = element_text(size = 12, face = "bold")) + 
    theme(axis.text.x = element_text(size = 10, color = "black", angle = 20, hjust = 1)) + 
    theme(plot.title = element_text(size = 15, face = "bold", color = "#C5050C"))

# Age
fig_age <- d |>
  ggplot(aes(x = age)) +
  geom_histogram(fill = "gray", color = "black", bins = 12) +
  labs(title = "Age (in years)", x = NULL, y = "count") +
  theme(axis.text = element_text(size = 12, face = "bold")) + 
  theme(axis.text.x = element_text(size = 10, color = "black")) + 
  theme(plot.title = element_text(size = 15, face = "bold", color = "#c5050c"))

# Income
fig_income <- d |>
  ggplot(aes(x = income)) +
  geom_histogram(fill = "gray", color = "black", bins = 12) +
  labs(title = "Income (in thousands)", x = NULL, y = "count") +
  theme(axis.text = element_text(size = 12, face = "bold")) + 
  theme(axis.text.x = element_text(size = 10, color = "black")) + 
  theme(plot.title = element_text(size = 15, face = "bold", color = "#c5050c"))

# AUD Sx
fig_sx <- d |>
  ggplot(aes(x = aud_total)) +
  geom_histogram(bins = 12, fill = "gray", color = "black") +
  labs(title = "AUD Symptom Count", x = NULL, y = "count") +
  scale_x_continuous(breaks = seq(0, 11),
                       limits = c(0, 11)) +
  theme(axis.text = element_text(size = 12, face = "bold")) + 
  theme(axis.text.x = element_text(size = 10, color = "black")) + 
  theme(plot.title = element_text(size = 15, face = "bold", color = "#c5050c"))

####################################
#  Pie Plots
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


