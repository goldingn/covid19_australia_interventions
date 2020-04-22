# model overall movement changes in Australia, with parameteric latent factor
# model on Google mobility data

source("R/functions.R")
library(dplyr)
library(lubridate)
library(greta)
library(RColorBrewer)

# load mobility datastreams, keeping only state-level data
mobility <- all_mobility() %>%
  filter(!is.na(state)) %>%
  filter(!is.na(trend)) %>%
  arrange(state, datastream, date) %>%
  mutate(state_datastream = str_c(state, datastream, sep = " "))
interventions <- intervention_dates()
holidays <- holiday_dates()
populations <- state_populations()

# get vectors of date ranges and datastreams to model
first_date <- min(mobility$date)
last_date <- max(mobility$date)
dates <- seq(first_date, last_date, by = 1)
n_dates <- n_distinct(dates)

datastreams <- mobility %>%
  group_by(datastream) %>%
  summarise() %>%
  pull(datastream)
n_datastreams <- length(datastreams)

states <- mobility %>%
  group_by(state) %>%
  summarise() %>%
  pull(state)
n_states <- length(states)

state_datastreams <- mobility %>%
  group_by(state_datastream) %>%
  summarise() %>%
  pull(state_datastream)
n_state_datastreams <- length(state_datastreams)

# social distancing latent factor as a function of behavioural switches
# triggered by the major interventions
date_num <- as.numeric(dates - first_date)
trigger_date_num <- as.numeric(interventions$date - first_date)
distancing <- latent_behaviour_switch(date_num, trigger_date_num)

# add term for recent change to social distancing (some proportion either
# switching back to baseline behaviour or increasing distancing behaviour), at
# some which could be from halfway since the last intervention to now, up to one
# week in the future
last_intervention <- max(trigger_date_num)
tau_distancing_start <- last_intervention + (n_dates - last_intervention) / 2
tau_distancing_change <- uniform(tau_distancing_start, n_dates + 7)
distancing_change <- latent_behaviour_switch(date_num, tau_distancing_change)
distancing_change <- distancing_change / distancing_change[n_dates]

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
  
# combine into latent factor matrix
latents_ntnl <- cbind(bump,
                      distancing,
                      distancing_change,
                      back_to_work,
                      weekday,
                      distancing * weekday)
n_latents_ntnl <- ncol(latents_ntnl)

latent_names <- c("Preparation for distancing",
                  "Social distancing",
                  "Change in distancing",
                  "Back to work",
                  "Weekly variation",
                  "Week/Social distancing interaction")

# get index to state in the combined state-datastreams (not all states have all
# datastreams, so this handles mismatch)
datastream_index <- mobility %>%
  group_by(state_datastream) %>%
  summarise(datastream = first(datastream)) %>%
  pull(datastream) %>%
  match(datastreams)

# hierarchical prior on loadings, so states have similar values, within for each
# latent factor
means_ntnl <- normal(0, 10, dim = c(n_latents_ntnl, n_datastreams))
sds_ntnl <- normal(0, 1, dim = c(n_latents_ntnl, n_datastreams),
                       truncation = c(0, Inf))

# hierarchical decentring with a 3D array squished into two dimensions
loadings_ntnl_raw <- normal(0, 1, dim = c(n_latents_ntnl, n_state_datastreams))
loadings_ntnl <- means_ntnl[, datastream_index] + loadings_ntnl_raw * sds_ntnl[, datastream_index]

trends_ntnl <- latents_ntnl %*% loadings_ntnl

# get state-level latents (only public holidays for now)

# IID effect on each state-holiday, 0s elsewhere
holiday_matrix <- holidays %>%
  right_join(
    tidyr::expand_grid(date = dates,
                       state = states)
  ) %>%
  mutate(is_holiday = !is.na(name)) %>%
  dplyr::select(-name) %>%
  tidyr::pivot_wider(names_from = state, values_from = is_holiday) %>%
  arrange(date) %>%
  dplyr::select(-date) %>%
  as.matrix()

is_a_holiday <- which(holiday_matrix)
n_holidays <- length(is_a_holiday)
holiday_weights <- uniform(0, 1, dim = n_holidays)
holiday <- zeros(nrow(holiday_matrix), ncol(holiday_matrix))
holiday[is_a_holiday] <- holiday_weights
maxes <- apply(holiday, 2, "max")
holiday <- sweep(holiday, 2, maxes, "/")

# holiday is date-by-state - get state-datastream weights and apply them to the correct ones
means_holiday <- normal(0, 10, dim = n_datastreams)
sds_holiday <- normal(0, 1, dim = n_datastreams, truncation = c(0, Inf))

# hierarchical decentring with a 3D array squished into two dimensions
loadings_holiday_raw <- normal(0, 1, dim = n_state_datastreams)
loadings_holiday <- means_holiday[datastream_index] + loadings_holiday_raw * sds_holiday[datastream_index]

# expand out the holiday index to replicate states
state_index <- mobility %>%
  group_by(state_datastream) %>%
  summarise(state = first(state)) %>%
  pull(state) %>%
  match(states)
holiday_latents <- holiday[, state_index]
trends_holiday <- sweep(holiday_latents, 2, loadings_holiday, FUN = "*")

trends <- trends_ntnl + trends_holiday

# extract expected trend for each observation and define likelihood
rows <- match(mobility$date, dates)
cols <- match(mobility$state_datastream, state_datastreams)
idx <- cbind(rows, cols)

sigma_obs <- normal(0, 1, truncation = c(0, Inf), dim = n_state_datastreams)
distribution(mobility$trend) <- normal(mean = trends[idx], 
                                       sd = sigma_obs[cols])

# fit model
m <- model(loadings_ntnl, loadings_holiday)
draws <- mcmc(m,
              sampler = hmc(Lmin = 20, Lmax = 25),
              chains = 20)
# draws <- extra_samples(draws, 3000)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_effs <- coda::effectiveSize(draws)
max(r_hats)
min(n_effs)

# ~~~~~~~~~
# plot fits

# plot (nationwide) latent factors
colours <- c("Purples", "Greens", "Reds", "Blues", "PuRd", "Oranges")

png("outputs/figures/latent_factors.png",
    width = 2000, height = 2000,
    pointsize = 40)
par(mfrow = n2mfrow(n_latents_ntnl))
for (i in 1:n_latents_ntnl) {
  plot_latent_factor(
    factor = latents_ntnl[, i],
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

# loop through states making multipanel model fit plots

for (j in seq_len(n_states)) {
  file <- paste0("outputs/figures/", states[j], "_datastream_model_fit.png")
  png(file,
      width = 3000, height = 2500,
      pointsize = 50)
  
  state_data <- mobility %>%
    filter(state == states[j])
  
  state_datastreams_j <- state_data %>%
    group_by(state_datastream) %>%
    summarise() %>%
    pull(state_datastream)
  n_state_datastreams_j <- length(state_datastreams_j)
  
  par(mfrow = n2mfrow(n_state_datastreams_j),
      mar = c(2, 2, 4, 2))
  for (i in seq_len(n_state_datastreams_j)) {
    datastream_i <- state_datastreams_j[i]
    plot_data <- state_data %>%
      filter(state_datastream == datastream_i)
    
    rows <- as.numeric(range(plot_data$date) - first_date)
    col <- match(datastream_i, state_datastreams)
    date_idx <- seq(rows[1], rows[2]) + 1
    dates_plot <- dates[date_idx]
    est <- cbind(mean = trend_mean[date_idx, col],
                 lower = trend_lower[date_idx, col],
                 upper = trend_upper[date_idx, col])
    
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
  title(outer = states[i])
  dev.off()  
}


# plot the national mean loadings (removing the intercept column)
latent_loadings <- means_ntnl  # [-1, ]
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

cols <- brewer.pal(8, "Set3")
go_stop <- cols[c(1, 4)]
up_down <- cols[c(3, 6)]

pal <- up_down

library(ggplot2)
library(ggforce)
loadings_plot_data %>%
  mutate(
    col = case_when(
      !significant ~ grey(0.9),
      value > 0 ~ pal[1],
      value < 0 ~ pal[2]
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

# - use nicer pastel colours for plots
# - use different colours for loadings (save red/green for waning traffic light) 
# - plot state-by-factor traffic light plots
# - plot state traffic light plot for waning social distancing
# - programatically find Apple download link
