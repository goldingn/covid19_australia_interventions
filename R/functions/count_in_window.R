count_in_window <- function(target_date, states, delay_data, window, date_tabulation) {
  dates <- delay_data %>%
    filter(state %in% states) %>%
    pull(!!date_tabulation)
  diff <- abs(dates - target_date)
  in_window <- diff <= window
  sum(in_window)
}
