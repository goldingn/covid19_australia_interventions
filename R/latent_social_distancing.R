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

# model the social distancing latent factor as a function of behavioural
# switches triggered by the different interventions
date_num <- as.numeric(dates - first_date)
trigger_date_num <- as.numeric(interventions$date - first_date)
distancing <- latent_behaviour_switch(date_num, trigger_date_num)

# (add terms for the potential waning of distancing, around an unknown date)

# build the pre-distancing surge in mobility with a prior that it peaks around
# the time of the first restriction
tau_bump <- normal(min(trigger_date_num), 1)
kappa_bump <- normal(3, 1, truncation = c(0, Inf))
bump <- latent_behavioural_event(date_num, tau_bump, kappa_bump)

# sims <- calculate(bum2, nsim = 100)[[1]][, , 1]
# plot(sims[1, ] ~ dates, type = "n", ylim = range(sims))
# for (i in 1:100) {
#   lines(sims[i, ] ~ dates, lwd = 1)
# }

# add behaviour switching latent factor for back to work period
# schools go back from Jan 28 to Feb 3-5, so set the mean to Feb 1
b2s_datenum <- as.numeric(lubridate::date("2020-02-01") - first_date)
tau_b2w <- normal(b2s_datenum, 5)
b2w <- latent_behaviour_switch(date_num, tau_b2w)

# add latent factor for weekend effect
# add (fixed) latent factor for each weekend
# remove intercept term and separate covariates

# combine into latent factor matrix
z <- cbind(distancing, bump, b2w)
n_latents <- ncol(z)

latent_names <- c("Social distancing",
                  "The Quilton bump",
                  "Back to work")

# get regression weights for each datastream
loadings <- normal(0, 10, dim = c(n_latents, n_datastreams))
weekend_weights <- normal(0, 10, dim = n_datastreams)
weekend_latent_weights <- normal(0, 10, dim = c(n_latents, n_datastreams))
holiday_weights <- normal(0, 10, dim = n_datastreams)

# standard deviation and degrees of freedom on the Student T observation model
sigma_obs <- normal(0, 1, truncation = c(0, Inf), dim = n_datastreams)

# get a vector of weekendiness for each day (lowest on Sundays, average of 0)
doy <- lubridate::wday(dates)
weekendiness <- scale(-exp(abs(doy - 4.5)))

# get a matrix of whether each state has a holiday on the given day
holiday_matrix <- mobility %>%
  filter(!is.na(state)) %>%
  dplyr::select(-datastream, -trend) %>%
  left_join(holidays) %>%
  group_by(state, date) %>%
  summarise(holiday = !all(is.na(name))) %>%
  mutate(holiday = as.numeric(holiday)) %>%
  arrange(state) %>%
  tidyr::pivot_wider(
    names_from = state,
    values_from = holiday,
    values_fill = list(holiday = 0)
  ) %>%
  arrange(date) %>%
  dplyr::select(-date) %>%
  as.matrix

# compute the fraction of the national population that had a public holiday
relative_population <- populations %>%
  arrange(state) %>%
  mutate(fraction = population / sum(population)) %>%
  pull(fraction)

population_on_holiday <- holiday_matrix %*% relative_population

# the effects of social distancing, weekends, and the interaction between
# weekends and social distancing on each datastream
latent_effect <- z %*% loadings
weekend_effect <- weekendiness %*% t(weekend_weights)
weekend_latent_interaction <- sweep(z, 1, weekendiness, FUN = "*")
weekend_trend_effect <- weekend_latent_interaction %*% weekend_latent_weights
holiday_effect <- population_on_holiday %*% t(holiday_weights)

# get expected trends for each datastream
trends <- latent_effect +
  weekend_effect + weekend_trend_effect +
  holiday_effect

aus_mobility <- mobility %>%
  filter(is.na(state))

# extract expected trend for each observation and define likelihood
rows <- match(aus_mobility$date, dates)
cols <- match(aus_mobility$datastream, datastreams)
idx <- cbind(rows, cols)
distribution(aus_mobility$trend) <- normal(mean = trends[idx],
                                           sd = sigma_obs[cols])

# fit model
m <- model(trends)
draws <- mcmc(m, chains = 10)
# draws <- extra_samples(draws, 3000)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_effs <- coda::effectiveSize(draws)
max(r_hats)
min(n_effs)

# ~~~~~~~~~
# plot fits

# plot latent factors
par(mfrow = c(n_latents, 1))
colours <- c("Greens", "Oranges", "Purples")

for (i in 1:n_latents) {
  plot_latent_factor(
    factor = z[, i],
    draws = draws,
    dates = dates,
    key_dates = interventions$date,
    cols = pal(colours[i]),
    latent_names[i]
  )
}

# plot datastreams and latent factor fit

latent_fit <- latent_effect

par(mfrow = n2mfrow(n_datastreams))
for (i in seq_len(n_datastreams)) {
  datastream_i <- datastreams[i]
  plot_data <- aus_mobility %>%
    filter(datastream == datastream_i)
  
  rows <- as.numeric(range(plot_data$date) - first_date)
  date_idx <- seq(rows[1], rows[2]) + 1
  dates_plot <- dates[date_idx]
  est <- summarise_vec_posterior(trends[date_idx, i],
                                 draws,
                                 sigma_obs[i])
  ylim <- range(c(plot_data$trend, est))
  plot(est[, 1] ~ dates_plot,
       xlim = range(dates),
       type = "n",
       ylim = ylim,
       ylab = "trend",
       xlab = "")
  add_gridlines(interventions$date)
  add_mean_ci(est, dates_plot,
              col = grey(0.97),
              border_col = grey(0.6),
              line_col = grey(0.7),
              lwd = 1)

  points(trend ~ date,
         data = plot_data,
         pch = 16,
         cex = 0.5,
         col = grey(0.2))
  title(main = datastream_i)
  
  # superimpose the latent factor component (social distancing and bumps)
  latent_est <- summarise_vec_posterior(latent_fit[date_idx, i],
                                        draws,
                                        sigma_obs[i])
  lines(latent_est[, 1] ~ dates_plot,
        lwd = 3, col = "blue")
  
}

# plot the loadings
loadings_draws <- calculate(loadings, values = draws)
loadings_mat <- as.matrix(loadings_draws)
loadings_mean <- colMeans(loadings_mat)
loadings_ci <- apply(loadings_mat, 2, quantile, c(0.025, 0.975))
loadings_significant <- sign(loadings_ci[1, ]) == sign(loadings_ci[2, ])

# get labels
latent_id <- as.vector(row(loadings))
datastream_id <- as.vector(col(loadings))

loadings_plot_data <- tibble(
  latent_factor = latent_names[latent_id],
  datastream = datastreams[datastream_id],
  value = loadings_mean,
  significant = loadings_significant)

cols <- brewer.pal(3, "Set2")

library(ggplot2)
library(ggforce)
loadings_plot_data %>%
  mutate(
    col = case_when(
      !significant ~ grey(0.9),
      value > 0 ~ cols[1],
      value < 0 ~ cols[2]
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
  
  # scale_fill_brewer(palette="Spectral")
# - visualise loadings (coloured matrix with significance included)




# - do multiple states with hierarchical weights
# - get national estimate with population weights
