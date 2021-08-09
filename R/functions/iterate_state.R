# iterate the state for all dates
iterate_state <- function(state, config) {
  
  for (idx in seq_len(config$n_dates)) {
    
    # lockdown decision process
    state$lockdown_matrix <- config$lockdown_policy(idx, state, config)
    
    # infection process  
    state$infections_matrix <- update_infections(idx, state, config)
    
    # detection process
    state$detections_matrix <- update_detections(idx, state, config)
    
  }
  
  state
  
}
