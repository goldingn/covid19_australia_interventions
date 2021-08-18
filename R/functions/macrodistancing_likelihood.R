# define the likelihood for the macrodistancing model
macrodistancing_likelihood <- function(predictions, data) {
  
  # pull out the expected number of non-household contacts by state and date
  all_dates <- unique(data$location_change_trends$date)
  all_states <- unique(data$location_change_trends$state)
  date_idx <- match(data$contacts$date, all_dates)
  state_idx <- match(data$contacts$state, all_states)
  idx <- cbind(date_idx, state_idx)
  
  # get expected number of contacts per respondent based on their date and state
  # (accounting for day of the week effects)
  log_predicted_contacts <- predictions$log_mean_daily_contacts_wday[idx]
  
  # get lognormal parameters from mean and standard deviation
  sdlog <- normal(0, 5, truncation = c(0, Inf))
  
  # because mean = exp(meanlog + (sdlog ^ 2) / 2)
  meanlog <- log_predicted_contacts - (sdlog ^ 2) / 2

  distribution(data$contacts$contact_num) <- discrete_lognormal(
    meanlog = meanlog,
    sdlog = sdlog,
    breaks = data$breaks
  )
  
  result <- list(
    predictions = exp(log_predicted_contacts),
    sdlog = sdlog
  )
  
  invisible(result)
  
}
