# return a date-by-state matrix of detection probabilities
detection_probability_matrix <- function(latest_date, infection_dates, states, notification_delay_cdf) {
  
  n_dates <- length(infection_dates)
  n_states <- length(states)
  onset_dates <- infection_dates + 5
  delays <- latest_date - onset_dates
  
  onset_dates_mat <- matrix(
    onset_dates, 
    nrow = n_dates,
    ncol = n_states
  )
  
  delays_mat <- matrix(
    delays, 
    nrow = n_dates,
    ncol = n_states
  )
  
  states_mat <- matrix(
    states,
    nrow = n_dates,
    ncol = n_states,
    byrow = TRUE
  )
  
  # get the detection probability matrix
  detection_prob_mat <- delays_mat * 0
  detection_prob_mat[] <- notification_delay_cdf(
    delays = delays_mat,
    possible_onset_dates = onset_dates_mat,
    states = states_mat
  )
  
  detection_prob_mat
  
}
