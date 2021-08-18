# simulate new infections and update state object
update_infections <- function(idx, state, config) {

  # get the number of new active cases, and add on the effect of the initial cases  
  new_active_cases <- config$gi_matrix %*% state$infections_matrix
  active_cases <- config$initial_active_cases + new_active_cases
  
  # get modified Reff and import rate
  r_eff <- lockdown_r_eff(idx, state, config)
  import_rate <- lockdown_import_rate(idx, state, config)
  
  expected_infections <- active_cases[idx, ] * r_eff
  reallocated_infections <- expected_infections %*% import_rate
  n_suburbs <- ncol(active_cases)
  new_infections <- rnbinom(n = n_suburbs,
                            size = config$size,
                            mu = reallocated_infections)
  # cap infections at a ludicrously high level to avoid numerical issues
  new_infections <- pmin(new_infections, 1e4)
  state$infections_matrix[idx, ] <- new_infections
  
  state$infections_matrix
  
}
