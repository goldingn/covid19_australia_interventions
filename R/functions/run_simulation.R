run_simulation <- function(config) {
  
  # set up the initial state
  zeros <- matrix(0, config$n_dates, config$n_suburbs)  
  initial_state <- list(
    infections_matrix = zeros,
    detections_matrix = config$initial_detections,
    lockdown_matrix = config$lockdown_matrix
  )
  
  # iterate the state dynamics
  results <- iterate_state(initial_state, config)
  
  # get summaries
  results$cases <- rowSums(results$detections_matrix)
  results$people_in_lockdown <- (results$lockdown_matrix %*% config$populations)[, 1]
  
  results
  
}
