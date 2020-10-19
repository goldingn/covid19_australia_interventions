# analyse change in macrodistancing behaviour (non-household contact rate) by
# state, using a baseline rate, and survey questions from Freya's survey and the
# BETA barometer
source("R/functions.R")
library(greta)

# informative priors for baseline contact parameters
baseline_contact_params <- baseline_contact_parameters(gi_cdf)

# data, parameters, preedictions, and likleihood definition for the model
data <- macrodistancing_data()
params <- macrodistancing_params(baseline_contact_params)
predictions <- macrodistancing_model(data, params)
out <- macrodistancing_likelihood(predictions, data)

OC_t_state <- predictions$avg_daily_contacts
p_weekend_t_state <- predictions$p_weekend

# fit model
set.seed(2020-05-30)

m <- model(
  params$OC_0,
  params$mobility_coefs,
  params$weekend_intercept,
  params$weekend_coef,
  out$size
)

draws <- mcmc(
  m,
  sampler = hmc(Lmin = 10, Lmax = 20),
  n_samples = 1500,
  chains = 10
)
# draws <- extra_samples(draws, 1000)
convergence(draws)

nsim <- coda::niter(draws) * coda::nchain(draws)
nsim <- min(10000, nsim)

# check posterior calibration
contacts_ga <- negative_binomial(out$size, out$prob)
contacts_sim <- calculate(contacts_ga, values = draws, nsim = nsim)[[1]][, , 1]
bayesplot::ppc_ecdf_overlay(
  data$contacts$contact_num,
  contacts_sim[1:1000, ],
  discrete = TRUE
)

# get trend predictions
pred_sim <- calculate(c(OC_t_state), values = draws, nsim = nsim)[[1]][, , 1]
quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

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


# compute weekend effect weights for INLA, based on fitted weekend_weight
weekend_weights_mean <- out$weekend_weight %>%
  calculate(
    values = draws,
    nsim = 500
  ) %>%
  magrittr::extract2(1) %>%
  magrittr::extract(, , 1) %>%
  colMeans()

# use this as an offset in INLA estimate

# slim down dataframe to get independent estimates for surveys
survey_points <- data$contacts %>%
  group_by(state, wave_date) %>%
  summarise(
    n = n(),
    wave_duration = first(wave_duration)
  )  %>%
  ungroup()

# need to group this by survey periods to plot

library(INLA)
contacts_inla <- data$contacts %>%
  mutate(wave = as.character(wave),
         wave_state = paste(wave, state))

glm <- inla(contact_num ~ f(wave_state, model = "iid"),
            family = "nbinomial",
            data = contacts_inla,
            E = weekend_weights_mean,
            control.predictor = list(link = 1))

# pull out fitted values for each date/state combination
idx <- contacts_inla %>%
  mutate(id = row_number()) %>%
  distinct(wave_date, state, .keep_all = TRUE) %>%
  old_right_join(survey_points) %>%
  pull(id)

sry <- glm$summary.fitted.values[idx, ]

# The width of the horizontal bars for survey data is proportional to the
# duration, but plotted in arbitrary units (which depend on the plot size).
# Rescale it with this tweaking parameter to roughly match the durations
survey_points <- survey_points %>%
  mutate(
    estimate = sry$mean,
    lower = sry$`0.025quant`,
    upper = sry$`0.975quant`,
    width = wave_duration
  ) %>%
  mutate(type = "Nowcast")

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

type <- 1
states <- unique(data$location_change_trends$state)
dates <- unique(data$location_change_trends$date)
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
                ylim = c(0, 15),
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
    length = unit(0.1, "npc"),
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
    size = 4,
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
    size = 4,
    alpha = 0.2,
    width = 0
  )


p

save_ggplot("macrodistancing_effect.png")

# prepare outputs for plotting
pred_trend <- data$location_change_trends %>%
  select(date, state) %>%
  # add predictions
  mutate(mean = colMeans(pred_sim)) %>%
  bind_cols(as_tibble(quants))

# save the model fit
saveRDS(pred_trend,
        file = "outputs/macrodistancing_trends.RDS")

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

