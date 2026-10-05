library(tidyverse)

get_data <- function(n, b = 5){
  x <- rep(c(0,1), n/2)
  # y = b0 + b1*x + e  
  y <- 0 + b*x + rnorm(n = n, mean = 0, sd = 10)
  tibble(x = x, y = y)
}

fit_model <- function(data){
  lm(y ~ x, data = data)
}

extract_stats <- function(model){
  results <- summary(model)
  b <- results$coefficients["x", "Estimate"] 
  p <- results$coefficients["x", "Pr(>|t|)"] 
  sig <- if_else(p < .05, 1, 0) 
  tibble(b, p, sig)
}


# single run
d <- get_data(100, 5)
d |> glimpse()

m <- fit_model(data)
summary(m)

extract_stats(m)

# set up sims 
n_sims <- 1000
b <- 5
n <- c(40, 80, 120, 160)

sims <- expand_grid(b = b, n = n, n_sim = 1:n_sims)
nrow(sims)
head(sims)
tail(sims)

sims <- sims |> 
  mutate(model_b = NA,
         model_p = NA,
         model_sig = NA)

head(sims)



# run the sims
set.seed(12345)
for (i in 1:nrow(sims)){
 
  d <- get_data(sims$n[i], sims$b[i])
  m <- fit_model(d)
  stats <- extract_stats(m) 
  sims$model_b[i] <- stats$b
  sims$model_p[i] <- stats$p
  sims$model_sig[i] <- stats$sig
  
}


sims |> 
  summarise(power = mean(model_sig), .by = n) |> 
  ggplot(aes(x = n, y = power)) +
  geom_line() +
  geom_point() +
  labs(x = "Sample size (n)", y = "Power") +
  theme_minimal()


# plot estimated power (mean of model_sig) by n
sims |> 
  summarise(power = mean(model_sig), .by = n) |> 
  ggplot(aes(x = n, y = power)) +
  geom_line() +
  geom_point() +
  labs(x = "Sample size (n)", y = "Power (P(significant))")


# The tidyverse way...

sims <- sims |> 
  mutate(data = map2(n, b, 
                     \(n, b) get_data(n, b)),
         model = map(data, 
                     \(data) fit_model(data)))
