get_window_size <- function(
  target_date,
  states,
  delay_data,
  date_tabulation,
  n_min = 500,
  window_min = 7,
  window_max = 42
) {
  
  dates <- delay_data %>%
    filter(state %in% states) %>%
    pull(!!date_tabulation)
  
  # find the smallest window that yields the required number of counts
  for (window in window_min:window_max) {
    
    diff <- abs(dates - target_date)
    in_window <- diff <= window
    
    if (sum(in_window) >= n_min) {
      break()
    }
    
  }
  
  window
  
}
