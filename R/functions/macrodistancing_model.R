# model for the trend in macrodistancing a weighted sum of time at location
# types, and an overall scaling coefficient, multiplied by a scalar baseline
# contact rate
macrodistancing_model <- function(data, parameters) {
  
  # format data into a date/state by location greta array of log ratios of
  # mobility on baseline
  log_location_change <- data$location_change_trends %>%
    mutate_at(
      vars(public, home, retail, transit, work),
      log
    ) %>%
    # flip log ratio for at home (regression coefficients are all positive)
    mutate(home = -home) %>%
    # turn into a matrix
    select(-state, -date) %>%
    as.matrix() %>%
    # and into a greta array
    as_data()
  
  # expected log change in contacts
  log_change_contacts <- log_location_change %*% parameters$mobility_coefs
  
  # average daily number of contacts
  log_mean_daily_contacts <- log(parameters$OC_0) + log_change_contacts
  mean_daily_contacts <- exp(log_mean_daily_contacts)

  # model the fraction of contacts falling on each day of the week via a multilogit model
  n <- nrow(log_mean_daily_contacts)
  X <- cbind(ones(n), log_mean_daily_contacts)
  eta <- X %*% parameters$weekday_coefs
  
  # imultilogit gives the value for every weekday against each date, so pull out only the
  # actual weekday for that date
  idx <- cbind(
    seq_len(n),
    lubridate::wday(data$location_change_trends$date)
  )
  
  log_fraction_weekly_contacts <- log_imultilogit(eta)[idx]
  
  # this equivalent to:
  #   log_fraction_weekly_contacts_wday <- eta[idx] - log_sum_exp(eta)
  # but shouldn't be much different speed-wise

  # use this to apply the weekday effect
  log_mean_weekly_contacts <- log_mean_daily_contacts + log(7)
  log_mean_daily_contacts_wday <- log_mean_weekly_contacts + log_fraction_weekly_contacts
  
  # convert both to wide (date by state) format for lookups
  mean_daily_contacts_wide <- greta_long_to_date_state(
    mean_daily_contacts,
    data$location_change_trends$date,
    data$location_change_trends$state
  )
  
  log_mean_daily_contacts_wday_wide <- greta_long_to_date_state(
    log_mean_daily_contacts_wday,
    data$location_change_trends$date,
    data$location_change_trends$state
  )
  
  log_fraction_weekly_contacts_wide <- greta_long_to_date_state(
    log_fraction_weekly_contacts,
    data$location_change_trends$date,
    data$location_change_trends$state
  )
  
  # return wide version of all these
  list(
    mean_daily_contacts = mean_daily_contacts_wide,
    log_mean_daily_contacts_wday = log_mean_daily_contacts_wday_wide,
    log_fraction_weekly_contacts = log_fraction_weekly_contacts_wide
  )
  
}
