## Precision - Recall Curves

:::: {.columns}

::: {.column width="50%"}

```{r}
thresholds_2 <- tibble(metric = c("Threshold", "", "Specificity", "PPV"),
                       week = c("0.70", "", "0.67", "0.75"), 
                       day = c("0.88", "", "0.43", "0.75"),
                       hour = c("0.97", "", "0.20", "0.75"))

thresholds_2 |>
  kbl(format = "html", col.names = c("", "Week", "Day", "Hour"),
      digits = 2,
      align = c("r", "r", "r", "r"),
      linesep = "") |> 
  row_spec(row = 0, align = "r") |> 
  kable_styling(full_width = FALSE)
```

:::
  
  ::: {.column width="50%"}
<!--Precision-Recall Curves by Models-->
  ```{r fig_ppv}
#| fig-height: 6
#| fig-width: 6

fig_pr <- pr_all |> 
  mutate(model = factor(model, levels = c("1week", "1day", "1hour"),
                        labels = c("week", "day", "hour"))) |>
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_path(size = 1.25) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Sensitivity (Recall)",
       y = "Positive Predictive Value (Precision)") +
  scale_color_manual(values = c("orange","green","blue")) 


fig_pr +
  geom_hline(yintercept = .75, color = "black", size = 1, linetype = "dotted") +
  geom_segment(x = .67, y = -.05 , xend = .67, yend = .75, 
               color = "orange", size = 1, linetype = "dotted") +
  geom_segment(x = .43, y = -.05 , xend = .43, yend = .75, 
               color = "green", size = 1, linetype = "dotted") +
  geom_segment(x = .20, y = -.05 , xend = .20, yend = .75, 
               color = "blue", size = 1, linetype = "dotted")
```
:::
  
  ::::
  
  ::: {.notes}
But of course, as we increase the decision threshold for labeling a window as a lapse, we will trade off sensitivity.   We can see this trade off directly in the precision-recall curves on the right.  If we decide we need PPV of at least .75, you can see that we still have reasonable sensitivity for the one week window but we start to miss many lapses in the 1day window and more still in the 1hour window.

I’ll return to this a bit more later when we discuss emerging plans for how best to implement these models within a digital therapeutic.

:::