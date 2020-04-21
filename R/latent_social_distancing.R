# model overall movement changes in Australia, with parameteric latent factor
# model on Google mobility data

source("R/functions.R")
library(dplyr)
library(lubridate)
library(greta)
library(RColorBrewer)

# load mobility datastreams, and potential drivers
mobility <- all_mobility()
interventions <- intervention_dates()
holidays <- holiday_dates()
populations <- state_populations()

# get vectors of date ranges and datastreams to model
first_date <- min(mobility$date)
last_date <- max(mobility$date)
dates <- seq(first_date, last_date, by = 1)
datastreams <- unique(mobility$datastream)
n_datastreams <- n_distinct(mobility$datastream)
n_dates <- n_distinct(dates)
n_states <- n_distinct(na.omit(mobility$state))

# social distancing latent factor as a function of behavioural switches
# triggered by the major interventions
date_num <- as.numeric(dates - first_date)
trigger_date_num <- as.numeric(interventions$date - first_date)
distancing <- latent_behaviour_switch(date_num, trigger_date_num)

# (add terms for the potential waning of distancing, around an unknown date)

# latent factor for pre-distancing surge in mobility with a prior that it peaks
# around the time of the first restriction
tau_bump <- normal(min(trigger_date_num), 1)
kappa_bump <- normal(3, 1, truncation = c(0, Inf))
bump <- latent_behavioural_event(date_num, tau_bump, kappa_bump)

# behaviour-switching latent factor for back to work period. schools go back
# from Jan 28 to Feb 3-5, so set mean to Feb 1
back_to_school_datenum <- as.numeric(lubridate::date("2020-02-01") - first_date)
tau_back_to_work <- normal(back_to_school_datenum, 5)
back_to_work <- latent_behaviour_switch(date_num, tau_back_to_work)

# spline latent factor for weekly variation and expand out to dates
day_weights <- latent_spline(1:7)
doy <- lubridate::wday(dates)
weekday <- day_weights[doy]

# latent factor for public holidays - IID on each date
is_a_holiday <- dates %in% holidays$date
n_holidays <- sum(is_a_holiday)
holiday_weights <- uniform(0, 1, dim = n_holidays)
holiday_weights <- holiday_weights / max(holiday_weights)
holiday <- zeros(n_dates)
holiday[is_a_holiday] <- holiday_weights

# combine into latent factor matrix
latents <- cbind(distancing,
                 bump,
                 back_to_work,
                 weekday,
                 holiday,
                 distancing * weekday)
n_latents <- ncol(latents)

latent_names <- c("Social distancing",
                  "The Quilton bump",
                  "Back to work",
                  "Weekly variation",
                  "Public holidays",
                  "Week/Social distancing interaction")

# project latent factors onto datastreams - adding an intercept column
loadings <- normal(0, 10, dim = c(n_latents + 1, n_datastreams))
z <- cbind(ones(n_dates), latents)
trends <- z %*% loadings

aus_mobility <- mobility %>%
  filter(is.na(state))

# extract expected trend for each observation and define likelihood
rows <- match(aus_mobility$date, dates)
cols <- match(aus_mobility$datastream, datastreams)
idx <- cbind(rows, cols)

sigma_obs <- normal(0, 1, truncation = c(0, Inf), dim = n_datastreams)
distribution(aus_mobility$trend) <- normal(mean = trends[idx],
                                           sd = sigma_obs[cols])

# fit model
m <- model(trends)
draws <- mcmc(m,
              sampler = hmc(Lmin = 15, Lmax = 20),
              chains = 50)
# draws <- extra_samples(draws, 3000)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_effs <- coda::effectiveSize(draws)
max(r_hats)
min(n_effs)

# ~~~~~~~~~
# plot fits

# plot latent factors
colours <- c("Greens", "Oranges", "Purples", "Blues", "PuRd", "Reds")

png("outputs/figures/latent_factors.png",
    width = 2000, height = 2000,
    pointsize = 40)
par(mfrow = n2mfrow(n_latents))
for (i in 1:n_latents) {
  plot_latent_factor(
    factor = latents[, i],
    draws = draws,
    dates = dates,
    key_dates = interventions$date,
    cols = pal(colours[i]),
    latent_names[i]
  )
}
dev.off()

# plot datastreams and latent factor fit

# simulate with error variance
errors <- normal(0, 1, dim = dim(trends))
latent_fit <- trends + sweep(errors, 2, sigma_obs, FUN = "*")
sim <- calculate(latent_fit, values = draws, nsim = 1000)[[1]]
trend_mean <- apply(sim, 2:3, mean)
trend_lower <- apply(sim, 2:3, quantile, 0.025)
trend_upper <- apply(sim, 2:3, quantile, 0.975)


png("outputs/figures/datastream_model_fit.png",
    width = 3000, height = 2500,
    pointsize = 50)

par(mfrow = n2mfrow(n_datastreams),
    mar = c(2, 2, 4, 2))
for (i in seq_len(n_datastreams)) {
  datastream_i <- datastreams[i]
  plot_data <- aus_mobility %>%
    filter(datastream == datastream_i)
  
  rows <- as.numeric(range(plot_data$date) - first_date)
  date_idx <- seq(rows[1], rows[2]) + 1
  dates_plot <- dates[date_idx]
  est <- cbind(mean = trend_mean[date_idx, i],
               lower = trend_lower[date_idx, i],
               upper = trend_upper[date_idx, i])
  
  ylim <- range(c(plot_data$trend, est))
  plot(est[, 1] ~ dates_plot,
       xlim = range(dates),
       type = "n",
       ylim = ylim,
       ylab = "",
       xlab = "")
  add_gridlines(interventions$date)
  add_mean_ci(est, dates_plot,
              col = grey(0.9),
              border_col = grey(0.8),
              line_col = grey(0.4),
              lwd = 2)
  points(trend ~ date,
         data = plot_data,
         pch = 16,
         cex = 0.5,
         col = "purple")
  title(main = datastream_i)
  
}
dev.off()

# plot the loadings (removing the intercept column)
latent_loadings <- loadings[-1, ]
loadings_sim <- calculate(latent_loadings, values = draws, nsim = 1000)[[1]]
loadings_mean <- apply(loadings_sim, 2:3, mean)
loadings_lower <- apply(loadings_sim, 2:3, quantile, 0.025)
loadings_upper <- apply(loadings_sim, 2:3, quantile, 0.975)
loadings_significant <- sign(loadings_upper) == sign(loadings_lower)

# get labels
latent_id <- as.vector(row(latent_loadings))
datastream_id <- as.vector(col(latent_loadings))

loadings_plot_data <- tibble(
  latent_factor = latent_names[latent_id],
  datastream = datastreams[datastream_id],
  value = as.vector(loadings_mean),
  significant = as.vector(loadings_significant)
)

cols <- brewer.pal(3, "Set2")

library(ggplot2)
library(ggforce)
loadings_plot_data %>%
  mutate(
    col = case_when(
      !significant ~ grey(0.9),
      value > 0 ~ cols[1],
      value < 0 ~ cols[2]
    ),
    latent_factor = factor(
      latent_factor,
      levels = unique(loadings_plot_data$latent_factor)
    )
  ) %>%
  ggplot() +
  geom_circle(aes(x0 = 1,
                  y0 = 1,
                  r = sqrt(abs(value)),
                  fill = col,
                  colour = col),
              show.legend = FALSE) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  facet_grid(latent_factor ~ datastream,
             switch = "y") +
  coord_fixed() +
  theme_void() +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1),
        strip.text.x = element_text(angle = 90, hjust = 0),
        plot.margin = unit(rep(0.5, 4), "cm"))

ggsave("outputs/figures/loadings_datastream.png",
       width = 10,
       height = 5.5)

# - do multiple states with hierarchical weights
# - get national estimate with population weights
# - add separate latent factor for waning social distancing
