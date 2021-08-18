# modification of the inter-postcode import rate in the face of lockdown
lockdown_import_rate <- function(idx, state, config) {
  
  import_rate <- config$import_rate
  lockdown_status <- state$lockdown_matrix[idx, ]
  
  # when in lockdown, the probability of an infectee being in another suburb is
  # reduced by some value
  old_leaving_probability <- 1 - diag(import_rate)
  reduction <- 1 - lockdown_status * (1 - config$leaving_reduction)
  new_leaving_probability <- old_leaving_probability * reduction
  
  import_rate <- set_leaving_probability(
    import_rate = import_rate,
    leaving_probability = new_leaving_probability
  )
  
  import_rate
  
}
