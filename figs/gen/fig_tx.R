library(tidyverse)
theme_set(theme_classic()) 

fig_tx_1 <- tibble(name = c("Past Year AUD", " ", "  "), 
                 proportion = c(1, 0, 0),
                 label = c("28 million", " ", " ")) |> 
  mutate(name = factor(name, 
         levels = c("Past Year AUD", 
                    " ", 
                    "  "))) |> 
  ggplot(aes(x = name, y = proportion)) +
    geom_bar(stat = "identity",
             fill = c("grey", "white","white"),
             linetype = c("solid", "solid", "solid"),
             color = "black",
             linewidth = .7) +
    geom_text(aes(label = label), 
              vjust = -0.5,               
              color = c("#C5050C", "white", "white"),
              size = 5,
              fontface = "bold") +
    labs(x = NULL,
         y = NULL) +
    scale_y_continuous(limits = c(0, 1.05), labels = scales::percent_format()) +
    theme(axis.text = element_text(size = 12, face = "bold"),        
          axis.text.x = element_text(color = c("#C5050C", "white", "white")),        
          axis.title = element_text(size = 16),       
          axis.line = element_line(linewidth = .7))  

##################################
# Add TX

fig_tx_2 <- tibble(name = c("Past Year AUD", "Received any treatment", " "), 
                 proportion = c(1, 0.124496, 0),
                 label = c("28 million", "3.4 million", "")) |> 
  mutate(name = factor(name, 
         levels = c("Past Year AUD", 
                    "Received any treatment", 
                    " "))) |> 
  ggplot(aes(x = name, y = proportion)) +
    geom_bar(stat = "identity",
             fill = c("grey", "grey","white"),
             linetype = c("solid", "solid", "solid"),
             color = "black",
             linewidth = .7) +
    geom_text(aes(label = label), 
              vjust = -0.5,               
              color = c("black", "#C5050C", "white"),
              size = 5,
              fontface = "bold") +
    labs(x = NULL,
         y = NULL) +
    scale_y_continuous(limits = c(0, 1.05), labels = scales::percent_format()) +
    theme(axis.text = element_text(size = 12, face = "bold"),        
          axis.text.x = element_text(color = c("black", "#C5050C", "white")),        
          axis.title = element_text(size = 16),       
          axis.line = element_line(linewidth = .7))  

##################################
# Add Continuing care

fig_tx_3 <- tibble(name = c("Past Year AUD", "Received any treatment", "Continuing care"), 
                 proportion = c(1, 0.124496, 0.06),
                 label = c("28 million", "3.4 million", "???")) |> 
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
              color = c("black", "black", "#C5050C"),
              size = 5,
              fontface = "bold") +
    labs(x = NULL,
         y = NULL) +
    scale_y_continuous(limits = c(0, 1.05), labels = scales::percent_format()) +
    theme(axis.text = element_text(size = 12, face = "bold"),        
          axis.text.x = element_text(color = c("black", "black", "#C5050C")),        
          axis.title = element_text(size = 16),       
          axis.line = element_line(linewidth = .7))  
