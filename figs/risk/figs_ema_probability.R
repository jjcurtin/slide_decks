####################
# Set up and load data
####################

library(tidyverse)
theme_set(theme_classic()) 
path_data <- "data/risk" 
path_mak <- "mak/risk"

# Load prediction data
source(here::here(path_mak, "mak_probability.R"))  # create data if needed
preds_week <- read_rds(here::here(path_data, "ema_preds_1week.rds"))
preds_day <- read_rds(here::here(path_data, "ema_preds_1day.rds"))
preds_hour <- read_rds(here::here(path_data, "ema_preds_1hour.rds")) 

source(here::here(path_mak, "mak_ema_auroc_ci.R"))  # create data if needed
auroc_ci <- read_csv(here::here(path_data, "ema_auroc_ci.csv"),
                     show_col_types = FALSE) |>  
  mutate(model = factor(model, levels = c("Week", "Day", "Hour")))

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

# auROC CI plots

plot_ci <- function(d) {
  d |>
    ggplot(aes(x = model)) +
      geom_point(aes(y = median), 
                 size = 4, color = c("red", "green", "blue")) +
      geom_errorbar(aes(ymin = lower, ymax = upper),
                    color = c("red", "green", "blue"),
                    width = .2,
                    linewidth = 2,
                    position = position_dodge(.9)) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
      geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray") +
      annotate("text", label = "random", x = 0.75, y = .5, size = 6, color = "gray") +
      annotate("text", label = "perfect", x = 0.75, y = 1, size = 6, color = "gray") +
      ylab("auROC") +
      xlab("Prediction Window") +
      theme(legend.position = "none") +
      scale_y_continuous(
        breaks = seq(0.4, 1, 0.10),
        limits = c(0.4, 1)) +
      theme(axis.text.x = element_text(size = 16, face = "bold"),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
}

# probability histogram function
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

# CM Plot functions
space_fun <- function(x, adjustment, rescale = FALSE) {
  if (rescale) {
    x <- x / sum(x)
  }

  adjustment <- sum(x) / adjustment

  xmax <- cumsum(x) + seq(0, length(x) - 1) * adjustment
  xmin <- cumsum(x) - x + seq(0, length(x) - 1) * adjustment

  dplyr::tibble(xmin = xmin, xmax = xmax)
}

space_y_fun <- function(data, id, x_data) {
  out <- space_fun(data[, id], 100, rescale = TRUE) * -1

  names(out) <- c("ymin", "ymax")

  out$xmin <- x_data[[id, 1]]
  out$xmax <- x_data[[id, 2]]

  out
}

cm_mosaic <- function(x) {
  `%+%` <- ggplot2::`%+%`

  cm_zero <- (as.numeric(x$table == 0) / 2) + x$table
  x_data <- space_fun(colSums(cm_zero), 200)
  full_data_list <- lapply(
    seq_len(ncol(cm_zero)),
    FUN = function(.x) space_y_fun(cm_zero, .x, x_data))
  full_data <- dplyr::bind_rows(full_data_list)
  y1_data <- full_data_list[[1]]
  tick_labels <- colnames(cm_zero)
  axis_labels <- get_axis_labels(x)
  
  ggplot2::ggplot(full_data) %+%
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ), 
      fill = c("green", "red", "red", "green")
    ) %+%
    ggplot2::scale_x_continuous(
      breaks = (x_data$xmin + x_data$xmax) / 2,
      labels = tick_labels
    ) %+%
    ggplot2::scale_y_continuous(
      breaks = (y1_data$ymin + y1_data$ymax) / 2,
      labels = tick_labels
    ) %+%
    ggplot2::labs(
      y = axis_labels$y,
      x = axis_labels$x
    ) %+%
    ggplot2::theme(panel.background = ggplot2::element_blank())
}

# Note: Always assumes predictions are on the LHS of the table
get_axis_labels <- function(x) {
  table <- x$table
  labels <- names(dimnames(table))

  if (is.null(labels)) {
    labels <- c("Prediction", "Truth")
  }
  list(
    y = labels[[1]],
    x = labels[[2]]
  )
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
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "gray") +
    geom_line(color = "green") +
    geom_point(color = "green") +
    labs(x = "Predicted Lapse Probability (bin mid-point)",
         y = "Observed Lapse Probability") +
    annotate("text", label = "Next day model", x = 0.2, y = 1,
             color = "green", size = 6) +
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

###############################################################################
# auROC CIs

fig_auroc_ci_3 <- auroc_ci |>
  plot_ci()

fig_auroc_ci_2 <- auroc_ci |>
  mutate(median =  if_else(model == "Hour", NA, median),
         lower =  if_else(model == "Hour", NA, lower),
         upper =  if_else(model == "Hour", NA, upper)) |>
  plot_ci()

fig_auroc_ci_1 <- auroc_ci |>
  mutate(median =  if_else(model == "Week", median, NA),
         lower =  if_else(model == "Week", lower, NA),
         upper =  if_else(model == "Week", upper, NA)) |>
  plot_ci() 

##############################################################################
# Confusion matrix 
j_thres_roc <- roc_day |> 
  mutate(j = sensitivity + specificity - 1) |> 
  slice_max(j) |> 
  pull(.threshold)

cm_day <- preds_day |> 
   mutate(estimate = if_else(prob_beta > j_thres_roc, "Lapse", "No lapse"),
          estimate = fct(estimate, levels = c("No lapse", "Lapse")),
          label = fct_relevel(label, "No lapse")) |> 
   yardstick::conf_mat(truth = label, estimate = estimate)

fig_cm_day <- cm_mosaic(cm_day) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24)) +
  geom_text(x = 350000, y = -.4, label = "612,086", color = "black", size = 6) +
  geom_text(x = 350000, y = -.9, label = "147,130", color = "black", size = 6) +
  geom_text(x = 800000, y = -.07, label = "9,244", color = "black", size = 6) +
  geom_text(x = 800000, y = -.55, label = "54,077", color = "black", size = 6)