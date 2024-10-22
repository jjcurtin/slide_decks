####################################
# Set up environment
####################################
library(tidyverse)
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true",
                     sha1 = "a58e57da996d1b70bb9a5b58241325d6fd78890f")
path_data <- "data/opt"

####################################
# Load and save data
####################################

if (!file.exists(here::here(path_data, "opt_shaps_examples.csv"))){

features <- c("past use", "future efficacy", "craving", "past stressful event", 
              "future risky situation", "past pleasant event", "past risky situation",
              "future stressful event", "valence", "arousal")
set.seed(1234)
shaps_local <- tibble(example = rep(1:3, each = 10), group = rep(features, 3),
                      shap = runif(30, 0, .2)) |> 
  mutate(shap = if_else(example == 1 & group == "craving", 1.4, shap),
          shap = if_else(example == 2 & group == "past use", 1.2, shap),
          shap = if_else(example == 3 & group == "past stressful event", 1.6, shap),
          shap = if_else(example == 3 & group == "future stressful event", 1.3, shap))

shaps_local |> 
  write_csv(here::here(path_data, "opt_shaps_examples.csv"))
}