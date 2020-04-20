# model overall movement changes in Australia, with parameteric latent factor
# model on Google mobility data

library(dplyr)
library(readr)
library(lubridate)
library(greta)
source("R/functions.R")

# load Google mobility data, intervention dates, and public holiday dates
data <- google_mobility()
interventions <- intervention_dates()
holidays <- holiday_dates()
populations <- state_populations()

# # subset to all-Australia
# data <- data %>%
#   filter(is.na(state)) %>%
#   dplyr::select(-state)

# get vectors of date ranges and categories to model
first_date <- min(data$date)
last_date <- max(data$date)
dates <- seq(first_date, last_date, by = 1)
categories <- unique(data$category)
n_categories <- n_distinct(data$category)
n_dates <- n_distinct(dates)
n_interventions <- n_distinct(interventions)
n_states <- n_distinct(na.omit(data$state))

# model the impacts of those interventions on social distancing factor k are
# length of the tails for early- and late-adopters; lambda is the relative
# contribution of the three interventions
k <- 1 / uniform(0, 16, dim = n_interventions)
props <- uniform(0, 1, dim = n_interventions)
lambda <- props / sum(props)

# get regression weights for each category
trend_weights <- normal(0, 10, dim = n_categories)
weekend_weights <- normal(0, 10, dim = n_categories)
weekend_trend_weights <- normal(0, 10, dim = n_categories)
trend_intercepts <- normal(0, 10, dim = n_categories)
holiday_weights <- normal(0, 10, dim = n_categories)

# standard deviation and degrees of freedom on the Student T observation model
sigma_obs <- normal(0, 1, truncation = c(0, Inf))

# get the social distancing factor epsilon
date_num <- dates - first_date
intervention_date_num <- interventions$date - first_date
lags <- outer(date_num, intervention_date_num, FUN = "-")
lag_delays <- ilogit(sweep(lags, 2, 2 * k, FUN = "*"))
epsilon <- 1 - sum(lambda) + lag_delays %*% lambda

# get a vector of weekendiness for each day (lowest on Sundays, average of 0)
doy <- lubridate::wday(dates)
weekendiness <- scale(-exp(abs(doy - 4.5)))

# get a matrix of whether each state has a holiday on the given day
holiday_matrix <- data %>%
  filter(!is.na(state)) %>%
  dplyr::select(-category, -trend) %>%
  left_join(holidays) %>%
  group_by(state, date) %>%
  summarise(holiday = !all(is.na(name))) %>%
  mutate(holiday = as.numeric(holiday)) %>%
  arrange(state) %>%
  tidyr::pivot_wider(names_from = state, values_from = holiday) %>%
  arrange(date) %>%
  dplyr::select(-date) %>%
  as.matrix

# compute the fraction of the national population that had a public holiday
relative_population <- populations %>%
  arrange(state) %>%
  mutate(fraction = population / sum(population)) %>%
  pull(fraction)

population_on_holiday <- holiday_matrix %*% relative_population

# intercept terms, and the effects of social distancing, weekends, and the
# interaction between weekends and social distancing on each category
intercepts <- ones(n_dates) %*% t(trend_intercepts)
trend_effect <- epsilon %*% t(trend_weights)
weekend_effect <- weekendiness %*% t(weekend_weights)
weekend_trend_effect <- (weekendiness * epsilon) %*% t(weekend_trend_weights)
holiday_effect <- population_on_holiday %*% t(holiday_weights)

# get expected trends for each category
trends <- intercepts +
  trend_effect +
  weekend_effect + weekend_trend_effect +
  holiday_effect

aus_data <- data %>%
  filter(is.na(state))

# extract expected trend for each observation and define likelihood
rows <- match(aus_data$date, dates)
cols <- match(aus_data$category, categories)
idx <- cbind(rows, cols)
distribution(aus_data$trend) <- normal(mean = trends[idx],
                                       sd = sigma_obs)

# fit model
m <- model(lambda, k, trend_weights, weekend_weights, weekend_trend_weights)
draws <- mcmc(m, chains = 10)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_effs <- coda::effectiveSize(draws)
max(r_hats)
min(n_effs)

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
      col = "darkseagreen4"
)
title(main = "Social distancing effect",
      col.main = "darkseagreen4")

# - do multiple states with hierarchical weights
# - get national estimate with population weights
