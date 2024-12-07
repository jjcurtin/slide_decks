library(tidyverse)
theme_set(theme_classic()) 

fig_tx <- tibble(name = c("Past Year AUD", "Received any treatment", "Continuing care"), 
                 proportion = c(1, 0.124496, 0.06),
                 label = c("28 million", "3.4 million", "?"),
                 dashed = c(FALSE, FALSE, TRUE)) |> 
  mutate(name = factor(name, 
         levels = c("Past Year AUD", 
                    "Received any treatment", 
                    "Continuing care"))) |> 
  ggplot(aes(x = name, y = proportion)) +
    geom_bar(stat = "identity",
             fill = c("grey", "grey","white"),
             linetype = c("solid", "solid", "dashed"),
             color = "black",
             linewidth = .7) +
    geom_text(aes(label = label), 
              vjust = -0.5,               
              color = "#C5050C",
              size = 5) +
    labs(x = NULL,
         y = NULL) +
    scale_y_continuous(limits = c(0, 1.05), labels = scales::percent_format()) +
    theme(axis.text = element_text(size = 12, face = "bold"),        
          axis.title = element_text(size = 16),       
          axis.line = element_line(linewidth = .7))  