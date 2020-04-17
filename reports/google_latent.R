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

# subset to all-australia, and remove the grocery and pharmacy category
# (affected by panic buying, not interventions)
aus_data <- data %>%
  filter(is.na(state)) %>%
  dplyr::select(-state) %>%
  filter(category != "Grocery & pharmacy")

# get a matrix of days since intervention
n_dates <- max(aus_data$date_num) + 1
date_nums <- seq_len(n_dates) - 1
n_categories <- n_distinct(aus_data$category)
categories <- unique(aus_data$category)

# model the impacts of those interventions on social distancing factor
library(greta)
props <- uniform(0, 1, dim = 3)
e <- props / sum(props)
k <- exponential(2, dim = 3)
weights <- normal(0, 10, dim = n_categories)
sigma_obs <- normal(0, 1, truncation = c(0, Inf))
df_obs <- 1 / normal(0, 1, truncation = c(0, Inf))

lags <- outer(date_nums, interventions$date_num, FUN = "-")
lag_delays <- ilogit(sweep(lags, 2, 2 * k, FUN = "*"))
epsilon <- 1 - sum(e) + lag_delays %*% e

# multiply epsilon by weights for each category
trends <- kronecker(epsilon, t(weights), FUN = "*")

# extract expected trend for each observation and define likelihood
rows <- match(aus_data$date_num, date_nums)
cols <- match(aus_data$category, categories)
idx <- cbind(rows, cols)
trends_mean <- trends[idx]
distribution(aus_data$trend) <- student(df = df_obs, mu = trends_mean, sigma = sigma_obs)

# fit model
m <- model(e, k, weights)
o <- opt(m)

# plot fits
par(mfrow = c(3, 2))
dates <- min(aus_data$date) + date_nums
for (i in seq_len(n_categories)) {
  category_i <- categories[i]
  plot_data <- aus_data %>%
    filter(category == category_i)
  mle <- calculate(trends[, i], values = o$par)[[1]]
  ylim <- range(c(plot_data$trend, mle))
  plot(mle ~ dates,
       type = "n",
       ylim = ylim,
       ylab = "trend",
       xlab = "")
  abline(h = 0, col = grey(0.4), lty = 3)
  abline(v = interventions$date, col = grey(0.6))
  points(trend ~ dates,
         data = plot_data,
         pch = 16,
         col = grey(0.8))
  lines(mle ~ dates, lwd = 2)
  title(main = category_i)
}

# add social distancing factor plot
epsilon_est <- calculate(epsilon, values = o$par)[[1]]
plot(epsilon_est ~ dates,
     type = "n",
     ylim = c(0, 1),
     ylab = "effect",
     xlab = "")
abline(h = 0, col = grey(0.4), lty = 3)
abline(v = interventions$date, col = grey(0.6))
lines(epsilon_est ~ dates,
      lwd = 3,
      col = "darkseagreen"
)
title(main = "Social distancing effect",
      col.main = "darkseagreen")

# add weekend effect (interacting with intervention period and category?)
# add a cyclic effect about sunday, with unknown parameters
# include a weekend weight, and a weekend/social-distancing interaction weight

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
