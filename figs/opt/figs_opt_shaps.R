#####################################
# Set up environment
#####################################

library(tidyverse)
theme_set(theme_classic()) 
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")
path_data <- "data/risk"
path_mak <- "mak/opt"

####################################
# load data
####################################
source(here::here(path_mak, "mak_opt_shaps.R"))
shaps_examples <- read_csv(here::here(path_data, "opt_shaps_examples.csv"), 
                   show_col_types = FALSE)


####################################################
# Local SHAP Examples Plot
####################################################

features <- rev(c("past use", "future efficacy", "craving", "past stressful event", 
              "future risky situation", "past pleasant event", "past risky situation",
              "future stressful event", "valence", "arousal"))

# suppress warning about vector of colors for axis text
suppressWarnings(
  fig_shap_ex1 <- shaps_examples |>
    filter(example == 1) |> 
    mutate(group = fct(group, levels = features)) |>
    ggplot() +
      geom_bar(aes(x = group, y = shap), stat = "identity", fill  = "gray") +
      labs(x = "", y = "|SHAP value|",
           subtitle = "Patient 1; Day 30") +
      coord_flip() +
      theme(plot.subtitle = element_text(color = "#C5050C", face =  "bold")) +
      theme(axis.text.y = element_text(size = 14, face  = "bold")) +
      theme(axis.text.x = element_text(size = 14, face  = "bold")) +
      theme(axis.title.x = element_text(size = 14, face  = "bold")) +
      theme(axis.text.y = element_text(colour = c(rep("black", 7), "#C5050C", rep("black", 2))))
)

suppressWarnings(
  fig_shap_ex2 <- shaps_examples |>
    filter(example == 2) |> 
    mutate(group = fct(group, levels = features)) |>
    ggplot() +
      geom_bar(aes(x = group, y = shap), stat = "identity", fill  = "gray") +
      labs(x = "", y = "|SHAP value|",
           subtitle = "Patient 2; Day 30") +
      coord_flip() +
      theme(plot.subtitle = element_text(color = "#C5050C", face =  "bold")) +
      theme(axis.text.y = element_text(size = 14, face  = "bold")) +
      theme(axis.text.x = element_text(size = 14, face  = "bold")) +
      theme(axis.title.x = element_text(size = 14, face  = "bold")) +
      theme(axis.text.y = element_text(colour = c(rep("black", 9), "#C5050C")))
)

suppressWarnings(
  fig_shap_ex3 <- shaps_examples |>
    filter(example == 3) |> 
    mutate(group = fct(group, levels = features)) |>
    ggplot() +
      geom_bar(aes(x = group, y = shap), stat = "identity", fill  = "gray") +
      labs(x = "", y = "|SHAP value|",
           subtitle = "Patient 2; Day 70") +
      coord_flip() +
      theme(plot.subtitle = element_text(color = "#C5050C", face =  "bold")) +
      theme(axis.text.y = element_text(size = 14, face  = "bold")) +
      theme(axis.text.x = element_text(size = 14, face  = "bold")) +
      theme(axis.title.x = element_text(size = 14, face  = "bold")) +
      theme(axis.text.y = element_text(colour = c(rep("black", 2), "#C5050C", rep("black", 3),
                                                  "#C5050C", rep("black", 2))))
)