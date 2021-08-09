delay_ecdf <- function(target_date, states, window, delay_data, date_tabulation) {
  
  data <- delay_data %>%
    filter(state %in% states)
  dates <- pull(data, !!date_tabulation)
  delays <- pull(data, delay)
  
  diff <- abs(dates - target_date)
  in_window <- diff <= window
  
  valid_delays <- delays[in_window]
  
  if (length(valid_delays) > 0) {
    distribution <- ecdf(valid_delays)
  } else {
    distribution <- NULL
  }
  
  list(distribution)
  
}
