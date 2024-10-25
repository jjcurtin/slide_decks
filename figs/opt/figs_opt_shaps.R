#####################################
# Set up environment
#####################################

library(tidyverse)
theme_set(theme_classic()) 
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true",
                     sha1 = "a58e57da996d1b70bb9a5b58241325d6fd78890f")
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
      geom_bar(aes(x = group, y = shap), stat = "identity", fill  = "green") +
      labs(x = "", y = "|SHAP value|",
           subtitle = "P1; Day 30") +
      coord_flip() +
      theme(plot.subtitle = element_text(color = "blue", face =  "bold")) +
      theme(axis.text.y = element_text(size = 14, face  = "bold")) +
      theme(axis.text.x = element_text(size = 14, face  = "bold")) +
      theme(axis.title.x = element_text(size = 14, face  = "bold")) +
      theme(axis.text.y = element_text(colour = c(rep("black", 7), "red", rep("black", 2))))
)

suppressWarnings(
  fig_shap_ex2 <- shaps_examples |>
    filter(example == 2) |> 
    mutate(group = fct(group, levels = features)) |>
    ggplot() +
      geom_bar(aes(x = group, y = shap), stat = "identity", fill  = "green") +
      labs(x = "", y = "|SHAP value|",
           subtitle = "P2; Day 30") +
      coord_flip() +
      theme(plot.subtitle = element_text(color = "blue", face =  "bold")) +
      theme(axis.text.y = element_text(size = 14, face  = "bold")) +
      theme(axis.text.x = element_text(size = 14, face  = "bold")) +
      theme(axis.title.x = element_text(size = 14, face  = "bold")) +
      theme(axis.text.y = element_text(colour = c(rep("black", 9), "red")))
)

suppressWarnings(
  fig_shap_ex3 <- shaps_examples |>
    filter(example == 3) |> 
    mutate(group = fct(group, levels = features)) |>
    ggplot() +
      geom_bar(aes(x = group, y = shap), stat = "identity", fill  = "green") +
      labs(x = "", y = "|SHAP value|",
           subtitle = "P2; Day 70") +
      coord_flip() +
      theme(plot.subtitle = element_text(color = "blue", face =  "bold")) +
      theme(axis.text.y = element_text(size = 14, face  = "bold")) +
      theme(axis.text.x = element_text(size = 14, face  = "bold")) +
      theme(axis.title.x = element_text(size = 14, face  = "bold")) +
      theme(axis.text.y = element_text(colour = c(rep("black", 2), "red", rep("black", 3),
                                                  "red", rep("black", 2))))
)