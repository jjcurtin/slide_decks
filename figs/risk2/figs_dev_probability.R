####################
# Set up and load data
####################

# These are from prelim analyses and should be updated as analyses are improved
library(tidyverse)
theme_set(theme_classic()) 
path_data <- "data/risk2" 
path_mak <- "mak/risk2"

auroc_ci <- read_csv(here::here(path_data, "ci_tmp.csv"),
                     show_col_types = FALSE) |> 
  bind_rows(tibble(median = NA_real_, lower = NA_real_, upper = NA_real_, 
                   model = "Week")) |>
  bind_rows(tibble(median = NA_real_, lower = NA_real_, upper = NA_real_,
                   model = "Hour")) |>  
  mutate(model = factor(model, levels = c("Week", "Day", "Hour")))


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
      tolower(model) == "week" ~ "red",
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


###############################################################################
# auROC CIs

fig_auroc_ci_3 <- auroc_ci |>
  plot_ci()

fig_auroc_ci_2 <- auroc_ci |>
  mutate(median =  if_else(model == "Hour", NA_real_, median),
         lower =  if_else(model == "Hour", NA_real_, lower),
         upper =  if_else(model == "Hour", NA_real_, upper)) |>
  plot_ci()   

fig_auroc_ci_1 <- auroc_ci |>
  mutate(median =  if_else(model == "Week", median, NA_real_),
         lower =  if_else(model == "Week", lower, NA_real_),
         upper =  if_else(model == "Week", upper, NA_real_)) |>
  plot_ci()

# blank plot frame for intro to auroc
fig_auroc_ci_0 <- auroc_ci |>
  mutate(median = NA_real_,
         lower =  NA_real_,
         upper =  NA_real_) |>
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