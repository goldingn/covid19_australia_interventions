# analyse change in macrodistancing behaviour (non-household contact rate) by
# state, using a baseline rate, and survey questions from Freya's survey and the
# BETA barometer

source("R/lib.R")

source("R/functions.R")

# informative priors for baseline contact parameters
baseline_contact_params <- baseline_contact_parameters(gi_cdf)

# data for plotting
baseline_point <- tibble::tibble(
  date = as.Date("2020-03-01"),
  estimate = baseline_contact_params$mean_contacts[2],
  sd = baseline_contact_params$se_contacts[2],
  type = "Nowcast"
) %>%
  mutate(
    lower = estimate - sd * 1.96,
    upper = estimate + sd * 1.96
  )

# data, parameters, predictions, and likelihood definition for the model
data <- macrodistancing_data()
params <- macrodistancing_params(baseline_contact_params)
predictions <- macrodistancing_model(data, params)
out <- macrodistancing_likelihood(predictions, data)

# fit model
set.seed(2020-05-30)

m <- model(
  params$OC_0,
  params$mobility_coefs,
  params$weekday_coefs,
  out$sdlog
)

# need to define initial values on the mobility coefs so they don't lead to
# numerical overflow
n_chains <- 10
coefs <- params$mobility_coefs
inits <- replicate(n_chains,
                   initials(coefs = runif(5, 0, 0.1)),
                   simplify = FALSE)

draws <- mcmc(
  m,
  sampler = hmc(Lmin = 10, Lmax = 15),
  initial_values = inits,
  chains = n_chains
)

 draws <- extra_samples(draws, 1000)
convergence(draws)

fitted_model <- module(
  model = m,
  draws,
  data,
  params,
  predictions,
  out
)

# save fitted model
saveRDS(fitted_model, "outputs/fitted_macro_model.RDS")
# fitted_model <- readRDS("outputs/fitted_macro_model.RDS")

# # make predictions using updated data on a day out of sync with standard Monday fit
# fitted_model$data <- data
# fitted_model$predictions <- macrodistancing_model(fitted_model$data, fitted_model$params)

nsim <- coda::niter(fitted_model$draws) * coda::nchain(fitted_model$draws)
nsim <- min(10000, nsim)

# check posterior calibration
sdlog <- fitted_model$out$sdlog
meanlog <- log(fitted_model$out$predictions) - (sdlog ^ 2) / 2
contacts_ga <- discrete_lognormal(
  meanlog = meanlog,
  sdlog = sdlog,
  breaks = fitted_model$data$breaks
)
# contacts_sim <- calculate(contacts_ga, values = fitted_model$draws, nsim = nsim)[[1]][, , 1]
# bayesplot::ppc_ecdf_overlay(
#   fitted_model$data$contacts$contact_num,
#   contacts_sim[1:1000, ],
#   discrete = TRUE
# ) +
#   coord_cartesian(xlim = c(0, 300))
## THIS HASHED OUT AS KILLING GR LAPTOP MEMORY

OC_t_state <- fitted_model$predictions$mean_daily_contacts

# get trend predictions
pred_sim <- calculate(c(OC_t_state), values = fitted_model$draws, nsim = nsim)[[1]][, , 1]
quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

# predicted trends for downstream modelling
pred_trend <- fitted_model$data$location_change_trends %>%
  select(date, state) %>%
  # add predictions
  mutate(mean = colMeans(pred_sim)) %>%
  bind_cols(as_tibble(quants))

saveRDS(pred_trend,
        file = "outputs/macrodistancing_trends.RDS")

# run only up to here for reff update

# estimates at peak and at latest date
pred_summary <- pred_trend %>%
  group_by(state) %>%
  summarise(peak = which.min(mean),
            peak_estimate = mean[peak],
            peak_low = ci_90_lo[peak],
            peak_high = ci_90_hi[peak],
            peak_date = date[peak],
            latest = which.max(date),
            latest_estimate = mean[latest],
            latest_low = ci_90_lo[latest],
            latest_high = ci_90_hi[latest],
            latest_date = date[latest]) %>%
  select(-peak, -latest)

saveRDS(pred_summary,
        file = "outputs/macrodistancing_trend_summary.RDS")

# fit a null-ish model (hierarchical but otherwise independent over
# waves/states) to visualise the data values

# compute day of the week effects from full model to use in null model
log_fraction_weekly_contacts_mean <- fitted_model$predictions$log_fraction_weekly_contacts %>%
  calculate(
    values = fitted_model$draws,
    nsim = 500
  ) %>%
  magrittr::extract2(1) %>%
  apply(2:3, mean)


# null_params <- macrodistancing_params(baseline_contact_params)
null <- macrodistancing_null(fitted_model$data, log_fraction_weekly_contacts_mean)
m_null <- model(null$avg_daily_contacts_wide, null$sdlog)

draws_null <- mcmc(
  m_null,
  chains = 10
)


convergence(draws_null)

#draws_null <- extra_samples(draws_null, 500)

daily_contacts_draws_null <- calculate(
  null$avg_daily_contacts_wide,
  values = draws_null,
  nsim = 2000
)

# summarise fitted values for each date/state combination
sry <- expand_grid(
  state = null$states,
  wave_date = null$wave_dates
  ) %>%
  mutate(
    estimate = c(apply(daily_contacts_draws_null[[1]], 2:3, mean)),
    lower = c(apply(daily_contacts_draws_null[[1]], 2:3, quantile, 0.025)),
    upper = c(apply(daily_contacts_draws_null[[1]], 2:3, quantile, 0.975))
  )

# The width of the horizontal bars for survey data is proportional to the
# duration, but plotted in arbitrary units (which depend on the plot size).
# Rescale it with this tweaking parameter to roughly match the durations

# slim down dataframe to get independent estimates for surveys
survey_points <- fitted_model$data$contacts %>%
  group_by(state, wave_date) %>%
  summarise(
    n = n(),
    wave_duration = first(wave_duration)
  )  %>%
  ungroup() %>%
  right_join(sry) %>%
  mutate(
    width = wave_duration
  ) %>%
  mutate(type = "Nowcast")

# save these fits for plotting later
saveRDS(survey_points, "outputs/macro_data_fit.RDS")

# get holiday dates and subset to where they overlap with surveys
holiday_lines <- survey_points %>%
  mutate(date_start = wave_date - wave_duration / 2,
         date_end = wave_date + wave_duration / 2) %>%
  select(state, date_start, date_end) %>%
  left_join(
    holiday_dates() %>%
      mutate(state = abbreviate_states(state))
  ) %>%
  filter(date < date_end & date > date_start)

holiday_lines <- holiday_dates() %>%
  mutate(
    state = abbreviate_states(state)
  ) %>%
  filter(
    date <= max(data$contacts$date) &
      date >= as.Date("2020-03-01")
  )

type <- 1
states <- unique(fitted_model$data$location_change_trends$state)
dates <- unique(fitted_model$data$location_change_trends$date)
n_states <- length(states)

# mock up data object for plotting
plot_data <- list(
  dates = list(
    infection_project = dates,
    latest_mobility = max(dates)
  ),
  states = states,
  n_states = length(states),
  n_dates_project = length(dates)
)

# non-household contacts
p <- plot_trend(pred_sim,
                data = plot_data,
                multistate = TRUE,
                base_colour = purple,
                max_date = max(data$contacts$date),
                ylim = c(0, 20),
                hline_at = NULL) + 
  ggtitle(label = "Macro-distancing trend",
          subtitle = "Rate of non-household contacts") +
  ylab("Estimated mean number of non-household contacts per day") + 
  
  # add baseline estimate
  geom_point(
    aes(date, estimate),
    data = baseline_point,
    size = 0.5,
    colour = grey(0.5)
  ) +
  geom_errorbar(
    aes(
      date,
      estimate,
      ymin = lower,
      ymax = upper
    ),
    data = baseline_point,
    width = 0,
    colour = grey(0.5)
  ) + 

  # rug marks for holidays
  geom_rug(
    aes(date),
    data = holiday_lines,
    col = green,
    size = 1,
    length = unit(0.05, "npc"),
    sides = "b",
    inherit.aes = FALSE
  ) +

  # add survey results estimate
  geom_point(
    aes(
      wave_date,
      estimate,
    ),
    data = survey_points,
    size = 2,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(
      wave_date,
      estimate,
      ymin = lower,
      ymax = upper,
    ),
    data = survey_points,
    size = 1,
    alpha = 0.2,
    width = 0
  )

p

save_ggplot("macrodistancing_effect.png")
