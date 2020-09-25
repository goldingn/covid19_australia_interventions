# model overall movement changes in Australia, with parameteric latent factor
# model on Google mobility data

source("R/functions.R")
library(dplyr)
library(lubridate)
library(greta)
library(RColorBrewer)
library(ggforce)
library(RCurl)

# Google dropped a bunch of previous data from the latest file. Pull a cached
# version from the tidycovid package on GitHub and replace it.
tidycovid_url <- "https://github.com/joachim-gassen/tidycovid19/raw/e4db3ab3007576f34dcb1e8c3299b235cff6198e/cached_data/google_cmr.RDS"

# load mobility datastreams, keeping only state-level data
mobility <- all_mobility() %>%
  append_google_data(tidycovid_url) %>%
  filter(!is.na(state)) %>%
  filter(!is.na(trend)) %>%
  arrange(state, datastream, date) %>%
  mutate(state_datastream = str_c(state, datastream, sep = " "))
saveRDS(mobility, file = "outputs/cached_mobility.RDS")
interventions <- intervention_dates()
holidays <- holiday_dates()
populations <- state_populations()

vic_second_intervention_dates <- as.Date(c("2020-07-01", "2020-07-08", "2020-08-03"))

# get vectors of date ranges and datastreams to model
first_date <- min(mobility$date)
last_date <- max(mobility$date)
dates <- seq(first_date, last_date, by = 1)
n_dates <- n_distinct(dates)

# get a lookup for the state and datastream in the state_datastreams vector
state_datastream_lookup <- mobility %>%
  group_by(state_datastream) %>%
  summarise(state = first(state),
            datastream = first(datastream))

state_datastreams <- unique(state_datastream_lookup$state_datastream)
n_state_datastreams <- length(state_datastreams)

datastreams <- unique(state_datastream_lookup$datastream)
n_datastreams <- length(datastreams)

states <- unique(state_datastream_lookup$state)
n_states <- length(states)

# get indices to datastream and state in the combined state-datastream vector
# (not all states have all datastreams, so this handles mismatch)
datastream_index <- state_datastream_lookup %>%
  pull(datastream) %>%
  match(datastreams)

state_index <- state_datastream_lookup %>%
  pull(state) %>%
  match(states)

# social distancing latent factor as a function of behavioural switches
# triggered by the major interventions
date_num <- as.numeric(dates - first_date)
trigger_date_num <- as.numeric(interventions$date[1:3] - first_date)
distancing <- latent_behaviour_switch(date_num, trigger_date_num)

# add term for recent change to social distancing (some proportion either
# switching back to baseline behaviour or increasing distancing behaviour)
# make this a linear increase since a peak (with prior mean one week after the
# last intervention, 95% interval in the week around that)
last_date_num <- n_dates - 1
last_intervention_date_num <- as.numeric(interventions$date[3] - first_date)
peak_range <- last_date_num - last_intervention_date_num

# when was the peak of distancing?
peak <- normal(last_intervention_date_num + 7,
               3.5 / 1.96,
               truncation = c(last_intervention_date_num, last_date_num))
waning_effect <- latent_hinge(peak, date_num)

# latent factor for pre-distancing surge in mobility with a prior that it peaks
# around the time of the first restriction
tau_bump_mean <- min(trigger_date_num)
tau_bump <- normal(tau_bump_mean, 1,
                   truncation = tau_bump_mean + c(-7, 7))
kappa_bump <- normal(3, 1, truncation = c(0, Inf))
bump <- latent_behavioural_event(date_num, tau_bump, kappa_bump)

# behaviour-switching latent factor for back to work period. schools go back
# from Jan 28 to Feb 3-5, so set mean to Feb 1
back_to_work_datenum <- as.numeric(lubridate::date("2020-02-01") - first_date)
tau_back_to_work <- normal(back_to_work_datenum, 1)
kappa_back_to_work <- normal(1, 0.25, truncation = c(0, Inf))
back_to_work <- latent_behaviour_switch(date_num,
                                        tau_back_to_work,
                                        kappa = kappa_back_to_work)

# fixed covariate for weekly variation, expanded out to dates. This is poorly
# identified in the model when trying to model e.g. a spline on day of the week,
# but the data is pretty much fully crossed so we should be fine to compute it
# up front
day_weights <- mobility %>%
  filter(date > as.Date("2020-05-01")) %>%
  mutate(
    day = lubridate::wday(date),
  ) %>%
  group_by(state_datastream, day) %>%
  summarise(weight = mean(trend)) %>%
  group_by(state_datastream) %>%
  mutate(
    weight = weight - min(weight),
    weight = weight / max(weight)
  ) %>%
  pivot_wider(
    names_from = state_datastream, values_from = weight
  ) %>%
  select(-day) %>%
  as.matrix()

# reorder to match datastreams
id <- match(state_datastreams, colnames(day_weights))
day_weights <- day_weights[, id]

# expand out to dates-by-state-datastreams, and apply hierarchical model to loadings so effects are similar between states
dow <- lubridate::wday(dates)
dow_latent <- day_weights[dow, ]
loadings_dow <- hierarchical_normal(n_state_datastreams, datastream_index)
trends_dow <- sweep(dow_latent, 2, loadings_dow, FUN = "*")

# combine into latent factor matrix
latents_ntnl <- cbind(bump,
                      distancing,
                      waning_effect,
                      back_to_work)
n_latents_ntnl <- ncol(latents_ntnl)

latent_names <- c("Preparation",
                  "Social distancing",
                  "Waning distancing",
                  "Back to work")

# hierarchical prior on loadings, so states have similar values, within for each
# latent factor
# normal with sd 10; rescaled to help sampler

# for identifability, state the sign for time at home as beng the opposite
prior_sign <- ifelse(datastreams == "Google: time at residential", -1, 1)

# doubly hierarchical prior on the national latent-factor loadings:
# top-level parameters for each each latent factor
means_ntnl_mean <- normal(0, 10, dim = n_latents_ntnl)
means_ntnl_sd <- normal(0, 0.5, truncation = c(0, Inf), dim = n_latents_ntnl)

# mid-level parameters for each latent factor and datastream (accounting for
# different sign in mean for some datastreams);
means_ntnl_raw <- normal(0, 1, dim = c(n_latents_ntnl, n_datastreams))
means_ntnl_mean_sign <- kronecker(means_ntnl_mean, t(prior_sign))
means_ntnl <- means_ntnl_mean_sign + sweep(means_ntnl_raw, 1, means_ntnl_sd, FUN = "*")
sds_ntnl <- normal(0, 0.5, dim = c(n_latents_ntnl, n_datastreams),
                   truncation = c(0, Inf))

# bottom-level parameters for each state-datastream
# hierarchical decentring with a 3D array squished into two dimensions
loadings_ntnl_raw <- normal(0, 1, dim = c(n_latents_ntnl, n_state_datastreams))
loadings_ntnl <- means_ntnl[, datastream_index] + loadings_ntnl_raw * sds_ntnl[, datastream_index]

trends_ntnl <- latents_ntnl %*% loadings_ntnl

# get state-level latents (public holidays and VIC interventions)

# IID effect on each state-holiday, 0s elsewhere
holiday_matrix <- holidays %>%
  old_right_join(
    tidyr::expand_grid(date = dates,
                       state = states)
  ) %>%
  mutate(is_holiday = !is.na(name)) %>%
  dplyr::select(-name) %>%
  tidyr::pivot_wider(names_from = state, values_from = is_holiday) %>%
  arrange(date) %>%
  dplyr::select(-date) %>%
  as.matrix()

holiday_idx <- which(holiday_matrix, arr.ind = TRUE)
# find first holiday in each state
match <- outer(holiday_idx[, 2], seq_along(states), FUN = "==")
first_holiday <- apply(match, 2, function(x) which(x)[1])
n_holidays <- nrow(holiday_idx)

n_params <- n_holidays - length(first_holiday)
holiday_params <- normal(1, 1, dim = n_params, truncation = c(0, Inf))
holiday_weights <- ones(n_holidays)
holiday_weights[-first_holiday] <- holiday_params

# need to set the first holiday in each state to 1, set the others to be
# variables
holiday <- zeros(nrow(holiday_matrix), ncol(holiday_matrix))
holiday[holiday_idx] <- holiday_weights

# holiday is date-by-state, get state-datastream weights and apply
loadings_holiday <- hierarchical_normal(n_state_datastreams, state_index)
# state_index
# loadings_holiday <- normal(0, 10, dim = n_state_datastreams)
holiday_latents <- holiday[, state_index]
trends_holiday <- sweep(holiday_latents, 2, loadings_holiday, FUN = "*")

# set informative priors for some inflection dates
inf_infl_date_priors <- tibble::tribble(
  ~state, ~mean_date, ~sd_days,
  # VIC announced reimposed restrictions on June 20 (to start June 22), and saw
  # an uptick in cases for the week preceding that; expect there may be a change
  # around this time.
  # "Victoria", as.Date("2020-06-20"), 7
)

# default uninformative priors
uninf_infl_date_priors <- expand_grid(
  state = states,
  mean_date = as.Date("2020-07-01"),
  sd_days = 14
) 

# in the absence of informative priors, use an uninformative prior with mean
# about halfway from the start of May (earliest point we think it could
# inflect), and the latest date
infl_date_priors <- uninf_infl_date_priors %>%
  filter(!state %in% inf_infl_date_priors$state) %>%
  bind_rows(
    inf_infl_date_priors
  ) %>%
  # convert this into a prior over the time since the peak
  mutate(
    mean_date_num = mean_date - min(dates) + 1,
    mean_date_num = as.numeric(mean_date_num)
  ) %>%
  arrange(state)

# get inflection points for waning in each state - some point after the national
# peak, but no closer than a week from the earliest final datapoint in any
# datastream (otherwise it will try to explain an incomplete weekly trend using
# this)
earliest_latest_date <- mobility %>%
  group_by(state_datastream) %>%
  summarise(latest_date = max(date)) %>%
  ungroup() %>%
  summarise(earliest_date = min(latest_date)) %>%
  pull(earliest_date)
truncation_dates <- c(as.Date("2020-05-01"),
                      earliest_latest_date - 14)
truncation_datenums <- as.numeric(truncation_dates - min(dates) + 1)
inflections <- normal(infl_date_priors$mean_date_num,
                      infl_date_priors$sd_days,
                      truncation = truncation_datenums)
inflection_effect <- latent_hinge(t(inflections), date_num)

# no hierarchical structure on this - it happens to a different extent in each state and datastream
loadings_inflection_raw <- normal(0, 1, dim = n_state_datastreams)
loadings_inflection <- loadings_inflection_raw * 10
inflection_latents <- inflection_effect[, state_index]
trends_inflection <- sweep(inflection_latents, 2, loadings_inflection, FUN = "*")

# trend for VIC due to second phase restrictions
vic_trigger_date_num <- as.numeric(vic_second_intervention_dates - first_date)
vic_distancing <- latent_behaviour_switch(date_num, vic_trigger_date_num)

# only need loadings for VIC and different interventions
loadings_vic_intervention <- normal(0, 1, dim = n_datastreams)
loadings_state_intervention <- loadings_vic_intervention[datastream_index]

state_distancing <- zeros(n_dates, n_states)
state_distancing[, states == "Victoria"]  <- vic_distancing
state_distancing_latents <- state_distancing[, state_index]
trends_intervention <- sweep(state_distancing_latents, 2, loadings_state_intervention, FUN = "*")

trends_state <- trends_holiday + trends_inflection + trends_dow + trends_intervention
trends <- trends_ntnl + trends_state

# extract expected trend for each observation and define likelihood
rows <- match(mobility$date, dates)
cols <- match(mobility$state_datastream, state_datastreams)
idx <- cbind(rows, cols)

sigma_obs <- normal(0, 0.5, truncation = c(0, Inf), dim = n_state_datastreams)

distribution(mobility$trend) <- normal(mean = trends[idx],
                                       sd = sigma_obs[cols])

# fit model
m <- model(tau_back_to_work, kappa_back_to_work,
           tau_bump, kappa_bump,
           peak,
           loadings_dow,
           holiday_params,
           inflections,
           sigma_obs)

draws <- mcmc(m,
              sampler = hmc(Lmin = 35, Lmax = 40),
              chains = 10)

draws <- extra_samples(draws, 2000)

convergence(draws)

# # dig into posterior correlations
# mi <- attr(draws, "model_info")
# free <- as.matrix(mi$raw_draws)
# widths <- apply(free, 2, sd)
# plot(widths)
# free_cor <- abs(cor(free, method = "spearman"))
# diag(free_cor) <- 0
# free_cor[lower.tri(free_cor)] <- 0
# idx <- which(free_cor > 0.9, arr.ind = TRUE)
# eg <- m$dag$example_parameters(free = TRUE)
# n_free <- vapply(eg, length, FUN.VALUE = numeric(1))
# end <- cumsum(n_free)
# start <- end - n_free + 1
# cbind(start, end)
# idx
# m$dag$tf_name(greta:::get_node(log_sigma_obs_raw))

# ~~~~~~~~~
# plot fits

# plot the first 4 (nationwide) latent factors

colours <- c("pink", "green", "red", "blue")
png("outputs/figures/latent_factors.png",
    width = 1000, height = 750,
    pointsize = 25)
par(mfrow = c(2, 2),
    mar = c(3, 4, 3, 1),
    oma = c(0, 0, 2, 0))
for (i in 1:4) {
  plot_latent_factor(
    factor = latents_ntnl[, i],
    draws = draws,
    dates = dates,
    key_dates = interventions$date,
    cols = pal(colours[i]),
    latent_names[i]
  )
}
mtext("Inferred latent factors of mobility",
      side = 3, 
      outer = TRUE, col = grey(0.3), adj = 0.1, cex = 1.3)
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
plot_date_lim <- c(as.Date("2020-03-01"), last_date)

for (j in seq_len(n_states)) {
  file <- paste0("outputs/figures/", states[j], "_datastream_model_fit.png")
  png(file,
      width = 1500, height = 1250,
      pointsize = 25)
  
  state_data <- mobility %>%
    filter(state == states[j])
  
  state_datastreams_j <- state_data %>%
    pull(state_datastream) %>%
    unique()
  datastreams_j <- state_data %>%
    pull(datastream) %>%
    unique()
  n_state_datastreams_j <- length(state_datastreams_j)
  
  par(mfrow = n2mfrow(n_state_datastreams_j),
      mar = c(2, 2, 4, 3),
      oma = c(0, 0, 2, 1),
      las = 1)
  for (i in seq_len(n_state_datastreams_j)) {
    
    state_datastream_i <- state_datastreams_j[i]
    datastream_i <- datastreams_j[i]
    
    plot_data <- mobility %>%
      filter(state_datastream == state_datastream_i)
    
    rows <- as.numeric(range(plot_data$date) - first_date)
    col <- match(state_datastream_i, state_datastreams)
    date_idx <- seq(rows[1], rows[2]) + 1
    dates_plot <- dates[date_idx]
    est <- cbind(mean = trend_mean[date_idx, col],
                 lower = trend_lower[date_idx, col],
                 upper = trend_upper[date_idx, col])
    
    ylim <- range(c(plot_data$trend, est))
    plot(est[, 1] ~ dates_plot,
         type = "n",
         ylim = ylim,
         xlim = plot_date_lim,
         ylab = "",
         xlab = "",
         yaxt = "n")
    axis(4)
    
    # add extra intervention lines for VIC
    if (states[j] == "Victoria") {
      add_gridlines(
        c(interventions$date, vic_second_intervention_dates)
      )
    } else {
      add_gridlines(interventions$date)
    }
    
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
    abline(v = last_date, lty = 2)
    title(main = datastream_i,
          col = grey(0.3))
    
  }
  
  main_title <- paste(states[j],
                      "- data and model fit up to",
                      format(last_date, format = "%B %d"))
  mtext(main_title,
        side = 3,  outer = TRUE,
        col = grey(0.2), cex = 1.3)
  dev.off()  
}

# same for a subset of datastreams, by all states
target_datastreams <- c("Google: time at parks",
                        "Apple: directions for driving",
                        "Google: time at transit stations")

# get the vector of state-datastreams to plot
state_datastreams_plot <- mobility %>%
  filter(datastream %in% target_datastreams) %>%
  pull(state_datastream) %>%
  unique()

# output A4 @ 600DPI
png("outputs/figures/multistate_model_fit.png",
    width = 2481, height = 3507,
    pointsize = 35)

par(mfrow = c(8, 3),
    mar = c(2, 2, 1, 3),
    oma = c(1, 2, 6, 2),
    las = 1)

for(this_state in states) {
  for (this_datastream in target_datastreams) {
    
    # subset data for plotting
    plot_data <- mobility %>%
      filter(state == this_state,
             datastream == this_datastream)
    
    if (nrow(plot_data) == 0) {
      
      plot(1, 1,
           type = "n",
           axes = FALSE,
           ylab = "",
           xlab = "")
      
      text(1, 1,
           labels = "no data",
           cex = 3,
           col = grey(0.8))
      
    } else {
      
      this_state_datastream <- plot_data$state_datastream[1]
      
      rows <- as.numeric(range(plot_data$date) - first_date)
      col <- match(this_state_datastream, state_datastreams)
      date_idx <- seq(rows[1], rows[2]) + 1
      dates_plot <- dates[date_idx]
      est <- cbind(mean = trend_mean[date_idx, col],
                   lower = trend_lower[date_idx, col],
                   upper = trend_upper[date_idx, col])
      
      ylim <- range(c(plot_data$trend, est))
      
      plot(est[, 1] ~ dates_plot,
           xlim = plot_date_lim,
           type = "n",
           ylim = ylim,
           ylab = "",
           xlab = "",
           yaxt = "n")
      axis(4)
      # add extra intervention lines for VIC
      if (this_state == "Victoria") {
        add_gridlines(
          c(interventions$date, vic_second_intervention_dates)
        )
      } else {
        add_gridlines(interventions$date)
      }
      
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
      abline(v = last_date, lty = 2)
      
    }
    
    # add facet labels
    if (this_state == states[1]) {
      
      datastream_name_formatted <- gsub("\n", " ", this_datastream)
      mtext(text = datastream_name_formatted,
            side = 3,
            line = 0.5,
            cex = 1.4,
            xpd = NA)
      
    }
    
    if (this_datastream == target_datastreams[1]) {
      
      state_name_formatted <- abbreviate_states(this_state)
      mtext(text = state_name_formatted,
            side = 2,
            line = 1,
            cex = 1.6,
            xpd = NA,
            las = 0)
      
    }
    
  }
}

mtext(text = paste("Percentage change in selected mobility datastreams up to",
                   format(last_date, format = "%B %d")),
      cex = 2,
      line = 3,
      adj = 0,
      outer = TRUE)

dev.off()


# plot the national mean loadings for the first four latent factors
latent_loadings <- means_ntnl[1:4, ]
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

palette <- up_down

loadings_plot_data %>%
  mutate(
    col = case_when(
      !significant ~ grey(0.9),
      value > 0 ~ palette[1],
      value < 0 ~ palette[2]
    ),
    latent_factor = factor(
      latent_factor,
      levels = unique(loadings_plot_data$latent_factor)
    )
  ) %>%
  ggplot() +
  geom_point(aes(x = 1,
                 y = 1,
                 size = abs(value),
                 fill = col,
                 colour = col)) +
  scale_size_area(
    breaks = c(100, 50, 10),
    labels = c("100%", "50%", "10%"),
    # limits = c(0, 100),
    max_size = 10,
    guide = guide_legend(
      title = "",
      override.aes = list(
        col = grey(0.6)
      ),
      order = 2
    )
  ) +
  scale_fill_identity(
    aesthetics = c("fill", "colour"),
    breaks = c(palette, grey(0.9)),
    labels = c("increase", "decrease", "no evidence"),
    guide = guide_legend(
      title = "",
      order = 1
    )
  ) +
  facet_grid(latent_factor ~ datastream,
             switch = "y") +
  coord_fixed() +
  theme_void() +
  ggtitle("Average change in datastreams due each latent factor",
          "percent change compared to pre-COVID-19 baseline") +
  theme(
    plot.title = element_text(hjust = 0, vjust = 6, size = 16),
    plot.subtitle = element_text(hjust = 0, vjust = 8, size = 10),
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
    strip.text.x = element_text(angle = 90, hjust = 0, size = 8),
    plot.margin = unit(rep(0.5, 4), "cm")
  )

ggsave("outputs/figures/loadings_datastream.png",
       width = 10,
       height = 5,
       dpi = 150)

# create a warning light panel for states and datasets on the change in
# distancing latent factor, turning red when it seems to be pushing it in the
# opposite direction from social distancing

# get the smoothed timeseries of mobility relative to our baseline, for each
# state and datastream (combine distancing, waning and inflection)

loadings_keep <- latent_names %in% c("Social distancing", "Waning distancing")
mobility_ntnl <- latents_ntnl[, loadings_keep] %*% loadings_ntnl[loadings_keep, ]
mobility_trends <- mobility_ntnl + trends_inflection

# direction of the change (e.g. positive for google residential)
direction <- sign(loadings_ntnl[latent_names == "Social distancing", ])
# latest value
latest <- mobility_trends[n_dates, ]
# value at the peak
peak <- t(apply(abs(mobility_trends), 2, "max")) * direction

# what % of the way has it waned back to baseline from the peak
distancing_change_relative <- -(peak - latest) / peak

# get % change at peak, and return relative to peak
# 
# # set direction relative to the direction of the social distancing factor for
# # that state-dataset (negative implies regression of the distancing effect), and
# # scale it to be relative to the amount of social distancing change
# distancing_change <- perc_change * sign(peak_distancing)
# distancing_change_relative <- distancing_change / abs(peak_distancing)

distancing_change_relative_draws <- calculate(distancing_change_relative, values = draws, nsim = 10000)[[1]]

# summarise posterior, and add state and datastream info
est <- summarise_vec_posterior(
  t(distancing_change_relative),
  draws,
  quantiles = c(0.05, 0.995)
)

distancing_change_data <- tibble(
  state_datastream = state_datastreams,
  value = est[, "mean"],
  significant = sign(est[, "5%"]) == sign(est[, "99.5%"])
) %>%
  left_join(state_datastream_lookup)

palette <- go_stop

# write a function to create these plots

# get positive value for proportional reduction in distancing
p <- distancing_change_data %>%
  mutate(
    value = pmax(pmin(value, -0.01), -1),
    value = -value,
    col = case_when(
      !significant ~ grey(0.9),
      TRUE ~ palette[2]
    ),
    outer1 = 1.05,
    outer2 = 1
  ) %>%
  ggplot() +
  # change this back to geom_circle, now there's scale_size?
  # need to map these to get them into the legend
  geom_point(aes(x = 1,
                 y = 1,
                 size = outer1,
                 colour = grey(0.4)),
             show.legend = NA) +
  geom_point(aes(x = 1,
                 y = 1,
                 size = outer2,
                 colour = "white"),
             show.legend = NA) +
  geom_point(aes(x = 1,
                 y = 1,
                 size = abs(value),
                 fill = col,
                 colour = col),
             show.legend = NA) +
  scale_size_area(
    breaks = c(1, 0.5, 0.1),
    labels = c("100%", "50%", "10%"),
    limits = c(0, 1.05),
    guide = guide_legend(
      title = "reduction\nin adherence:",
      title.vjust = 5,
      override.aes = list(
        col = palette[2],
        outer1 = 1,
        outer2 = 2
      ),
      order = 1
    )
  ) +
  scale_fill_identity(
    aesthetics = c("fill", "colour"),
    breaks = grey(0.9),
    labels = "uncertain",
    guide = guide_legend(
      title = "",
      order = 2
    )
  ) +
  facet_grid(state ~ datastream,
             switch = "y") +
  coord_fixed() +
  ggtitle("Indicators of reduced adherence to social distancing") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0, vjust = 6, size = 16),
    plot.subtitle = element_text(hjust = 0, vjust = 8, size = 10),
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
    strip.text.x = element_text(angle = 45,
                                hjust = 0,
                                size = 9,
                                vjust = 2.5),
    plot.margin = unit(rep(0.5, 4), "cm"),
    legend.spacing.y = unit(-0.1, "cm")
  )

## GGPLOT BEND TO MY WILL.
pg <- ggplotGrob(p)
for(i in which(grepl("strip-t-", pg$layout$name))) {
  pg$grobs[[i]]$layout$clip <- "off"
  pg$layout[i, "t"] <- pg$layout[i, "b"] <- 20
}
# grid::grid.draw(pg)

ggsave("outputs/figures/state_distancing_waning_warning.png",
       plot = pg,
       width = 8,
       height = 6,
       dpi = 150)

# output posterior summaries for the different latent factors
for(i in 1:4) {
  filename <- latent_names[i] %>%
    tolower() %>%
    gsub(" ", "_", .) %>%
    paste0("_latent.RDS") %>%
    file.path("outputs", .)
  summarise_vec_posterior(latents_ntnl[, i],
                          draws) %>%
    as_tibble() %>%
    mutate(date = dates) %>%
    saveRDS(filename)
}

save.image("outputs/latent_social_distancing_temp.RData")

# summarise state-level variation in waning
distancing_change_data %>%
  mutate(value = pmin(0, value * 100)) %>%
  mutate(value = pmax(-100, value)) %>%
  group_by(datastream) %>%
  summarise(
    mean = mean(value),
    min = max(value),
    max = min(value),
    min_state = paste(state[value == min], collapse = ", "),
    max_state = paste(state[value == max], collapse = ", ")
  ) %>%
  saveRDS("outputs/mobility_change_summary.RDS")


peak_draws <- calculate(peak, values = draws, nsim = 20000)[[1]][, 1, 1]
peak_mean <- first_date + mean(peak_draws)
peak_ci <- first_date + quantile(peak_draws, c(0.025, 0.975))


# get an index of waned distancing, weighting datastreams according to their
# representativeness of transmission potential ('riskiness')

transmission_potential <- state_datastream_lookup %>%
  mutate(risk = case_when(
    datastream %in% c(
      "Apple: directions for driving",
      "Apple: directions for walking",
      "Google: time at parks",
      "Google: time at residential"
    ) ~ "low",
    datastream %in% c(
      "Google: time at grocery and pharmacy",
      "Google: time at retail and recreation"
    ) ~ "medium",
    datastream %in% c(
      "Citymapper: directions",
      "Apple: directions for transit",
      "Google: time at transit stations",
      "Google: time at workplaces"
    ) ~ "high",
  )) %>%
  group_by(risk) %>%
  mutate(weight = case_when(
    risk == "low" ~ 0.1,
    risk == "medium" ~ 0.3,
    risk == "high" ~ 0.6
  )) %>%
  mutate(weight = weight / n_distinct(state_datastream))

# calculate weighted regression of the overall distancing effect, weighted to
# represent mobility data streams thought to be most representative of
# transmission pontential
regression <- distancing_change_relative %*% as_data(transmission_potential$weight)
regression_vals <- calculate(regression, values = draws, nsim = 10000)[[1]]

# save parameters of posterior on the amount of waning, for use in R_eff model
waning_amount_params <- list(
  mean = mean(regression_vals),
  sd = sd(regression_vals),
  truncation = c(-1, 0)
)
saveRDS(waning_amount_params,
        file = "outputs/waning_amount_parameters.RDS")

# save state-level posteriors over % change in google metrics from just-pre-covid period
datastreams_keep <- grepl("^Google: ", state_datastream_lookup$datastream)
loadings_keep <- latent_names %in% c("Preparation", "Social distancing", "Waning distancing")

perc_change_google_ntnl <- latents_ntnl[,loadings_keep] %*% loadings_ntnl[loadings_keep, datastreams_keep]
perc_change_google_state <- trends_inflection[, datastreams_keep] + trends_intervention[, datastreams_keep]
perc_change_google <- perc_change_google_ntnl + perc_change_google_state
change_google <- 1 + (perc_change_google / 100)

nsim <- coda::niter(draws) * coda::nchain(draws)
change_google_sim <- calculate(change_google, values = draws, nsim = nsim)[[1]]
change_google_means <- apply(change_google_sim, 2:3, mean)
change_google_sds <- apply(change_google_sim, 2:3, sd)
tile <- rep(seq_len(8 * 6), each = n_dates)
google_change <- state_datastream_lookup[datastreams_keep, ][tile, ]
google_change$change <- c(change_google_means)
google_change$date <- rep(dates, 8 * 6)

saveRDS(google_change,
        file = "outputs/google_change_trends.RDS")

# google_change %>%
#   filter(state == "Victoria" &
#            datastream == "Google: time at residential") %>%
#   plot(change ~ date, data = ., type = "l")
