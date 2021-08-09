  lockdown_policy <- function(idx, state, config) {
    
    # count the number of cases in the last few days
    days <- idx - seq_len(count_days) + 1
    days <- unique(pmax(1, days))
    
    count <- colSums(state$detections_matrix[days, , drop = FALSE])
    
    # from here on, lock down that suburb
    new_lockdown <- count > count_threshold
    old_idx <- pmax(idx - 1, 1)
    current_lockdown <- state$lockdown_matrix[old_idx, ]
    state$lockdown_matrix[idx, ] <- pmax(as.numeric(new_lockdown), current_lockdown)
    
    state$lockdown_matrix
    
  }
