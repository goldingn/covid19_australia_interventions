# model overall movement changes in Australia, with parameteric latent factor
# model on Google mobility data

library(dplyr)
library(readr)
library(lubridate)
library(greta)
source("R/functions.R")

# load Jono Carroll's scraping of Aus mobility data
file <- "https://raw.githubusercontent.com/jonocarroll/google-location-coronavirus/AUS/2020-04-11-au_state.tsv"
data <- readr::read_tsv(file) %>%
  dplyr::select(state = sub_region_name,
                category = category,
                date = date,
                trend = trend)

# add on the intervention stage
interventions <- intervention_dates()

# - subset to all-Australia
# - remove the grocery and pharmacy category
# (affected by panic buying, not interventions)
aus_data <- data %>%
  filter(is.na(state)) %>%
  dplyr::select(-state) %>%
  filter(category != "Grocery & pharmacy")

# get a matrix of days since intervention
first_date <- min(aus_data$date)
last_date <- max(aus_data$date)
dates <- seq(first_date, last_date, by = 1)
n_dates <- length(dates)
n_categories <- n_distinct(aus_data$category)
categories <- unique(aus_data$category)

# model the impacts of those interventions on social distancing factor
props <- uniform(0, 1, dim = 3)
e <- props / sum(props)
k <- 1 / uniform(0, 16, dim = 3)

trend_weights <- normal(0, 10, dim = n_categories)
weekend_weights <- normal(0, 10, dim = n_categories)
weekend_trend_weights <- normal(0, 10, dim = n_categories)
trend_intercepts <- normal(0, 10, dim = n_categories)

# standard deviation and degrees of freedom on the Student T observation model
sigma_obs <- normal(0, 1, truncation = c(0, Inf))
df_obs <- 1 / normal(0, 1, truncation = c(0, Inf))

# get the social distancing factor epsilon
date_num <- dates - first_date
intervention_date_num <- interventions$date - first_date
lags <- outer(date_num, intervention_date_num, FUN = "-")
lag_delays <- ilogit(sweep(lags, 2, 2 * k, FUN = "*"))
epsilon <- 1 - sum(e) + lag_delays %*% e

# get a vector of weekendiness for each day (lowest on Sundays, average of 0)
doy <- lubridate::wday(dates)
weekendiness <- scale(-exp(abs(doy - 4.5)))

# intercept terms, and the effects of social distancing, weekends, and the
# interaction between weekends and social distancing on each category
intercepts <- ones(n_dates) %*% t(trend_intercepts)
trend_effect <- epsilon %*% t(trend_weights)
weekend_effect <- weekendiness %*% t(weekend_weights)
weekend_trend_effect <- (weekendiness * epsilon) %*% t(weekend_trend_weights)

# get expected trends for each category
trends <- intercepts + trend_effect + weekend_effect + weekend_trend_effect

# extract expected trend for each observation and define likelihood
rows <- match(aus_data$date, dates)
cols <- match(aus_data$category, categories)
idx <- cbind(rows, cols)
distribution(aus_data$trend) <- student(df = df_obs,
                                        mu = trends[idx],
                                        sigma = sigma_obs)

# fit model
m <- model(e, k, trend_weights, weekend_weights, weekend_trend_weights)
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
# - account for public holidays in each state
# - get national estiamte with population weights
