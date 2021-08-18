# for each new infection in each suburb, assign a random day of future
# detection
increment_detections <- function(idx,
                                 detections_matrix,
                                 infections_matrix,
                                 delay_probs) {

  new_infections <- infections_matrix[idx, ]
  delay_prob <- delay_probs[[idx]]
  regions <- seq_along(new_infections)
  n_dates <- nrow(infections_matrix) 
  
  delays_list <- lapply(regions,
                        detection_delay,
                        new_infections,
                        delay_prob)
  delays <- do.call(rbind, delays_list)

  if (!is.null(delays)) {
   
    # where to assign detections
    elements <- delays[, 1:2, drop = FALSE]
    elements[, 1] <- elements[, 1] + idx
    valid <- elements[, 1] <= n_dates
    elements <- elements[valid, , drop = FALSE] 

    # numbers of new detections  
    new_detections <- delays[valid, 3]
    
    # update counts
    old_detections <- detections_matrix[elements]
    detections_matrix[elements] <- old_detections + new_detections
    
  }
  

  detections_matrix
  
}
