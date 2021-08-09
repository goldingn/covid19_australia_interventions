# simulate a number of active cases from prior to the dynamic simulation
sim_initial_cases <- function(initial_expected_infections,
                              nbinom_size,
                              dates,
                              delay_probs_all,
                              gi_matrix) {
  
  n_suburbs <- ncol(initial_expected_infections)
  n_dates_prior <- nrow(initial_expected_infections)
  n_dates <- length(dates)
  
  # simulate numbers of new infections in each suburb on each date  
  initial_infections <- random_initial_infections(
    expected_infections = initial_expected_infections,
    size = nbinom_size
  )
  empty_infections <- matrix(0, n_dates, n_suburbs)
  initial_infections_all <- rbind(initial_infections, empty_infections)
  
  dates_all <- min(dates) + seq(-n_dates_prior, n_dates - 1)
  initial_active_cases <- gi_matrix %*% initial_infections_all
  
  detections_all <- matrix(0, n_dates_prior + n_dates, n_suburbs)
  for (i in seq_len(n_dates_prior)) {
    detections_all <- increment_detections(idx = i,
                                           detections_matrix = detections_all,
                                           infections_matrix = initial_infections_all,
                                           delay_probs = delay_probs_all)
  }

  # return both active cases and detections for the dynamic simulation period
  list(
    active_cases = tail(initial_active_cases, n_dates),
    detections = tail(detections_all, n_dates)
  )
  
}
