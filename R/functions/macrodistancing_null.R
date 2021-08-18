# a sort of null model (assuming a different rate of contacts per survey/state) for plotting the data 
macrodistancing_null <- function(data, log_fraction_weekly_contacts_mean) {
  
  # extract indices to the survey waves and states for each observation
  
  # add numeric ids for wave dates and states
  wave_dates <- sort(unique(data$contacts$wave_date))
  states <- sort(unique(data$contacts$state))
  dates <- sort(unique(data$location_change_trends$date))
  
  idx <- data$contacts %>%
    mutate(
      wave_id = match(wave_date, wave_dates),
      state_id = match(state, states)
    ) %>%
    select(wave_id, state_id) %>%
    as.matrix()
    
  n_waves <- length(wave_dates)
  n_states <- length(states)

  # hierarchical model for the average number of contacts per survey/state
  log_contacts_mean <- normal(0, 10)
  log_contacts_sd <- normal(0, 1, truncation = c(0, Inf))
  log_contacts_raw <- normal(0, 1, dim = c(n_waves, n_states))
  log_contacts_wide <- log_contacts_mean + log_contacts_raw * log_contacts_raw
  
  # expand these out to match the data
  log_mean_daily_contacts <- log_contacts_wide[idx]
  
  # this has the fractions for each date and state, so pull out the relevant
  # entries
  idx <- data$contacts %>%
    mutate(
      date_id = match(date, dates),
      state_id = match(state, states)
    ) %>%
    select(date_id, state_id) %>%
    as.matrix()
  
  # use this to apply the weekday effect
  log_mean_weekly_contacts <- log_mean_daily_contacts + log(7)
  log_predicted_contacts <- log_mean_weekly_contacts + log_fraction_weekly_contacts_mean[idx]
  
  # get lognormal parameters from mean and standard deviation
  sdlog <- normal(0, 5, truncation = c(0, Inf))
  
  # because mean = exp(meanlog + (sdlog ^ 2) / 2)
  meanlog <- log_predicted_contacts - (sdlog ^ 2) / 2

  distribution(data$contacts$contact_num) <- discrete_lognormal(
    meanlog = meanlog,
    sdlog = sdlog,
    breaks = data$breaks
  )
  
  # return greta arrays to fit model  
  module(
    avg_daily_contacts_wide = exp(log_contacts_wide),
    sdlog,
    wave_dates,
    states
  )
  
}
