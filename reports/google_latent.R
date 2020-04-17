# model overall movement changes in Australia, with paramneteric latent factor
# model on Google mobility data

library(dplyr)
library(readr)
library(lubridate)

# load Jono Carroll's scraping of Aus mobility data
file <- "https://raw.githubusercontent.com/jonocarroll/google-location-coronavirus/AUS/2020-04-11-au_state.tsv"
data <- readr::read_tsv(file) %>%
  dplyr::select(state = sub_region_name,
                category = category,
                date = date,
                trend = trend) %>%
  mutate(date_num = date - min(date))

# add on the intervention stage
interventions <- tribble(~date, ~stage, ~text,
                         "2020-03-16", 1, "public gatherings <= 500 people",
                         "2020-03-24", 2, "venues closed and advice to stay home except limited essential activities",
                         "2020-03-29", 3, "public gatherings <= 2 people") %>%
  mutate(date = lubridate::date(date),
         date_num = date - min(data$date))

# data <- data %>% 
#   mutate(stage = case_when(
#     date >= interventions$date[3] ~ 3,
#     date >= interventions$date[2] ~ 2,
#     date >= interventions$date[1] ~ 1,
#     TRUE ~ 0
#   ))

# - subset to all-Australia
# - remove the grocery and pharmacy category
# (affected by panic buying, not interventions)
aus_data <- data %>%
  filter(is.na(state)) %>%
  dplyr::select(-state) %>%
  filter(category != "Grocery & pharmacy")

# get a matrix of days since intervention
n_dates <- max(aus_data$date_num) + 1
date_nums <- seq_len(n_dates) - 1
dates <- min(aus_data$date) + date_nums
n_categories <- n_distinct(aus_data$category)
categories <- unique(aus_data$category)

# model the impacts of those interventions on social distancing factor
library(greta)
props <- uniform(0, 1, dim = 3)
e <- props / sum(props)
k <- 1 / uniform(0, 16, dim = 3)

trend_weights <- normal(0, 10, dim = n_categories)
weekend_weights <- normal(0, 10, dim = n_categories)
weekend_trend_weights <- normal(0, 10, dim = n_categories)
trend_intercepts <- normal(0, 10, dim = n_categories)

sigma_obs <- normal(0, 1, truncation = c(0, Inf))
df_obs <- 1 / normal(0, 1, truncation = c(0, Inf))

# get the social distancing factor epsilon
lags <- outer(date_nums, interventions$date_num, FUN = "-")
lag_delays <- ilogit(sweep(lags, 2, 2 * k, FUN = "*"))
epsilon <- 1 - sum(e) + lag_delays %*% e

# get a vector of weekendiness for each day (lowest on Sundays, average of 0)
doy <- lubridate::wday(dates)
weekendiness <- scale(-exp(abs(doy - 4.5)))

# effect of social distancing on each category
trend_effect <- kronecker(epsilon,
                          t(trend_weights),
                          FUN = "*")

# effect of weekends on each category
weekend_effect <- kronecker(weekendiness,
                            t(weekend_weights),
                            FUN = "*")

# interaction between weekends and social distancing for each category
weekend_trend_effect <- kronecker(weekendiness * epsilon,
                                  t(weekend_trend_weights),
                                  FUN = "*")

intercepts <- kronecker(ones(n_dates),
                        t(trend_intercepts),
                        FUN = "*")

trends <- intercepts + trend_effect + weekend_effect + weekend_trend_effect

# extract expected trend for each observation and define likelihood
rows <- match(aus_data$date_num, date_nums)
cols <- match(aus_data$category, categories)
idx <- cbind(rows, cols)
trends_mean <- trends[idx]
distribution(aus_data$trend) <- student(df = df_obs, mu = trends_mean, sigma = sigma_obs)

# fit model
m <- model(e, k, trend_weights, weekend_weights, weekend_trend_weights)
draws <- mcmc(m, chains = 10)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_effs <- coda::effectiveSize(draws)
max(r_hats)
min(n_effs)

# summarise the posterior for a vector greta array
summarise_vec_posterior <- function(vector, draws) {
  vector_draws <- calculate(vector, values = draws)[[1]]
  vector_mat <- as.matrix(vector_draws)
  posterior_mean <- colMeans(vector_mat)
  posterior_ci <- t(apply(vector_mat, 2, quantile, c(0.025, 0.975)))
  cbind(mean = posterior_mean, posterior_ci)
}

add_ci_poly <- function(posterior_summary,
                        dates,
                        col = grey(0.8),
                        border_col = grey(0.6)) {
  polygon(x = c(dates, rev(dates)),
          y = c(posterior_summary[, 2],
                rev(posterior_summary[, 3])),
          lwd = 0.5,
          col = col,
          border = border_col)
}

# plot fits
par(mfrow = c(3, 2))
for (i in seq_len(n_categories)) {
  category_i <- categories[i]
  plot_data <- aus_data %>%
    filter(category == category_i)
  est <- summarise_vec_posterior(trends[, i], draws)
  ylim <- range(c(plot_data$trend, est))
  plot(est[, 1] ~ dates,
       type = "n",
       ylim = ylim,
       ylab = "trend",
       xlab = "")
  abline(h = 0, col = grey(0.4), lty = 3)
  abline(v = interventions$date, col = grey(0.6))
  add_ci_poly(est, dates)
  lines(est[, 1] ~ dates, lwd = 2, col = grey(0.4))
  points(trend ~ dates,
         data = plot_data,
         pch = 16,
         cex = 0.5,
         col = grey(0.2))
  title(main = category_i)
}

# add social distancing factor plot
est <- summarise_vec_posterior(epsilon, draws)
plot(est[, 1] ~ dates,
     type = "n",
     ylim = c(0, 1),
     ylab = "effect",
     xlab = "")
abline(h = 0, col = grey(0.4), lty = 3)
abline(v = interventions$date, col = grey(0.6))
add_ci_poly(est, dates, col = "darkseagreen1", border_col = "darkseagreen2")
lines(est[, 1] ~ dates,
      lwd = 3,
      col = "darkseagreen"
)
title(main = "Social distancing effect",
      col.main = "darkseagreen")

# do Bayesian inference
# do multiple states (use the mean weights for the national-level data!)


# n_sim <- 100
# sims <- calculate(epsilon, nsim = n_sim)[[1]][, , 1]
# plot(sims[1, ] ~ date_nums, type = "n",
#      ylab = "social distancing effect",
#      xlab = "time",
#      ylim = c(0, 1))
# for (i in 1:n_sim) {
#   lines(sims[i, ] ~ date_nums, lwd = 0.1)
# }
# abline(v = interventions$date_num, lty = 2)
