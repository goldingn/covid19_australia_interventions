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

aus_data <- data %>%
  filter(is.na(state)) %>%
  dplyr::select(-state)

# fit the model for one category to start with
aus_resid <- aus_data %>%
  filter(category == "Residential")

# get a matrix of days since intervention
n_dates <- max(aus_resid$date_num) + 1
n_categories <- n_distinct(aus_resid$category)
date_nums <- seq_len(n_dates) - 1
lags <- outer(date_nums, interventions$date_num, FUN = "-")

# model the impacts of those interventions on social distancing factor
library(greta)
props <- uniform(0, 1, dim = 3)
e <- props / sum(props)
k <- exponential(2, dim = 3)
weights <- normal(0, 10, dim = n_categories)
sigma_obs <- normal(0, 1, truncation = c(0, Inf))

lag_delays <- ilogit(sweep(lags, 2, 2 * k, FUN = "*"))
epsilon <- 1 - sum(e) + lag_delays %*% e

# multiply epsilon by weights for each category
trends <- sweep(epsilon, 2, weights, FUN = "*")

# extract expected trend for each observation and define likelihood
idx <- match(aus_resid$date_num, date_nums)
trends_mean <- trends[idx]
distribution(aus_resid$trend) <- normal(trends_mean, sigma_obs)

# fit model
m <- model(e, k, weights)
o <- opt(m)

# plot fit
mle <- calculate(trends, values = o$par)[[1]]
ylim <- range(c(aus_resid$trend, mle))
plot(mle ~ date_nums, type = "l",
     ylim = ylim,
     lwd = 2)
abline(h = 0, lty = 2)
points(trend ~ date_num,
       data = aus_resid,
       pch = 16)

# add weekend effect (interacting with intervention period and category?)
# add multiple categories
# 


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
