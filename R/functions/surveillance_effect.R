# returna date-by-state matrix of reduction in R due to faster detection of cases
surveillance_effect <- function(dates, states, cdf,
                                gi_bounds = c(0, 20),
                                ttd_cdfs = NULL) {
  
  n_dates <- length(dates)
  n_states <- length(states)
  gi_range <- diff(gi_bounds) + 1
  day_vec <- seq_len(gi_range) - 1 + gi_bounds[1]
  day_mat <- col(matrix(0, n_dates, gi_range)) - 1
  
  # generation interval probability on each day post-infection
  gi_days <- gi_probability(cdf, day_vec, bounds = gi_bounds)
  
  date_state_mat <- matrix(1, n_dates, n_states)
  
  for (i in seq_along(states)) {
    
    # times to detection for each date in this states
    ttd_days <- day_mat
    ttd_days[] <- ttd_survival(
      c(day_mat),
      rep(dates, gi_range),
      target_state = states[i],
      cdfs = ttd_cdfs
    )
    
    # weighted sum to get reduction due to impeded transmission
    date_state_mat[, i] <- c(ttd_days %*% gi_days)
    
  }
  
  date_state_mat
  
}
