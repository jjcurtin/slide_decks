####################
# Set up and load data
####################

library(tidyverse)
theme_set(theme_classic()) 
path_data <- "data/ema" 

# Load prediction data
preds_week <- read_rds(here::here(path_data, "preds_1week.rds"))
preds_day <- read_rds(here::here(path_data, "preds_1day.rds"))
preds_hour <- read_rds(here::here(path_data, "preds_1hour.rds")) 

# Generate ROC curve data 
roc_week <- preds_week |> 
  yardstick::roc_curve(prob_beta, truth = label) |> 
  mutate(model = "1week")
  
roc_day <- preds_day |> 
  yardstick::roc_curve(prob_beta, truth = label) |> 
  mutate(model = "1day")

roc_hour <- preds_hour|> 
  yardstick::roc_curve(prob_beta, truth = label) |> 
  mutate(model = "1hour")

roc_all <- roc_week |> 
  bind_rows(roc_day) |> 
  bind_rows(roc_hour) |> 
  mutate(thres_avg = round(.threshold, 3)) |> 
  group_by(model, thres_avg) |> 
  reframe(sensitivity = mean(sensitivity), specificity =  mean(specificity)) |> 
  mutate(model = factor(model, levels = c("1week", "1day", "1hour"),
                        labels = c("week", "day", "hour")))

####################
# plot functions
####################

# probability histogram function
# not current used
plot_probs <- function(df_preds, model) {
  bar_color <- 
    case_when(
      tolower(model) == "week" ~ "orange",
      tolower(model) == "day" ~ "green",
      tolower(model) == "hour" ~ "blue",
    )
  df_preds |> 
    ggplot(data = _, aes(x = prob_beta)) + 
     geom_histogram(bins = 15, fill = bar_color, col = "black", alpha = .4) +
     facet_wrap(~label, nrow = 2, scales = "free_y") +
     xlab("P(Lapse | X)") +
    scale_y_continuous(labels = scales::comma)
}
  
plot_roc <- function(df, line_colors){
  df |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
    geom_path(linewidth = 1.25) +
    geom_abline(lty = 3) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = "False Positive Rate",
        y = "True Positive Rate") +
  scale_color_manual(values = line_colors)
}

####################
# Plots
####################

# Histograms
fig_hist_week <- preds_week |> 
  plot_probs("week")

# fig_week +
#   geom_vline(xintercept = .5, color = "red", size = 2)


# Calibration plot
bin_width = 0.10
fig_cal <- preds_day |> 
  mutate(bins = cut(prob_beta, breaks = seq(0, 1, bin_width)),
         lapse = if_else(label == "Lapse", 1, 0)) |> 
  group_by(bins)  |> 
  summarize(mean_lapse = mean(lapse),
            .groups = "drop") |>
  mutate(bins = as.numeric(bins),
         midpoints = bin_width/2 + bin_width * (bins - 1))  |> 
  ggplot(data = _, aes(x = midpoints, y = mean_lapse)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    geom_line() +
    geom_point() +
    xlab("Predicted Lapse Probability (bin mid-point)") +
    ylab("Observed Lapse Probability") +
    scale_x_continuous(breaks = seq(0, 1, bin_width),
                       limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, 1, bin_width),
                       limits = c(0, 1)) +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))

# ROC curve plots
line_colors  <- c("orange", "green", "blue")
fig_roc_week <- roc_all |>
  filter(model == "week") |>
  plot_roc(line_colors) # +
#   geom_text(x = .80, y = .20,
#            label = str_c("AUC = ", auROC_week),
#            show.legend = FALSE, color = "orange")

fig_roc_week_day <- roc_all |>
  filter(model == "week" | model == "day") |>
  plot_roc(line_colors) # +
#   geom_text(x = .80, y = .20,
#            label = str_c("AUC = ", auROC_week),
#            show.legend = FALSE, color = "orange") +
#   geom_text(x = .80, y = .15,
#             label = str_c("AUC = ", auROC_day),
#            show.legend = FALSE, color = "green")

fig_roc_all <- roc_all |>
  plot_roc(line_colors) #  +
#   geom_text(x = .80, y = .20,
#            label = str_c("auROC = ", auROC_week),
#            show.legend = FALSE, color = "orange") +
#  geom_text(x = .80, y = .15,
#            label = str_c("auROC = ", auROC_day),
#            show.legend = FALSE, color = "green") +
#  geom_text(x = .80, y = .10,
#            label = str_c("auROC = ", auROC_hour),
#            show.legend = FALSE, color = "blue")