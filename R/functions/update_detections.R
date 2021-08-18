# simulate detection process for new infections and update state object
update_detections <- function(idx, state, config) {
  
  increment_detections(idx = idx,
                       detections_matrix = state$detections_matrix,
                       infections_matrix = state$infections_matrix,
                       delay_probs = config$delay_probs) 
  
}
