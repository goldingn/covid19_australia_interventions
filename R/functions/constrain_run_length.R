# given a vector of discrete values (e.g. logicals) representing switches
# between states, constrain those switches so that the state may not switch to
# another state unless thge run of the same value must have been at least
# 'min_run_length' long.
constrain_run_length <- function(x, min_run_length = 7) {
  
  # create an empty vector to fill and then iterate (this must be recursive, so
  # can't use e.g. slider::slide())
  result <- rep(FALSE, length(x))
  for (end in seq_along(result)) {
    
    # check whether there was a change recently
    start <- max(1, end - min_run_length) 
    idx <-  seq(start, end - 1)
    previous <- result[idx]
    recently_flipped <- !all(previous == previous[1])
    
    # if there was, then keep the same state as the last iteration. If there
    # wasn't, use the ideal value (the value in x)
    if (recently_flipped) {
      most_recent <- previous[length(previous)]
      result[end] <- most_recent
    } else {
      result[end] <- x[end]
    }
    
  }
  
  result
  
}
