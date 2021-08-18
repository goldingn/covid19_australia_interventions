# create a config object for running a simulation
prepare_config <- function (idx,
                            nbinom_size_samples,
                            r_eff_samples,
                            r_eff_reduction_samples,
                            leaving_reduction_samples,
                            initial_expected_infections,
                            import_rate,
                            dates,
                            delay_probs,
                            delay_probs_all,
                            populations,
                            gi_mat,
                            gi_mat_all,
                            lockdown_policy = no_policy,
                            lockdown_matrix = NULL) {
  
  # pull relevant samples
  nbinom_size <- nbinom_size_samples[idx]
  r_eff_reduction <- r_eff_reduction_samples[idx]
  leaving_reduction <- leaving_reduction_samples[idx]
  r_eff <- r_eff_samples[[idx]]
    
  # simulate infections by postcode and compute the number of active cases from
  # before the dynamics
  initial_cases <- sim_initial_cases(
    initial_expected_infections = initial_expected_infections,
    nbinom_size = nbinom_size,
    dates = dates,
    delay_probs_all = delay_probs_all,
    gi_matrix = gi_mat_all
  )
  
  initial_active_cases <- initial_cases$active_cases
  initial_detections <- initial_cases$detections

  n_dates <- length(dates)
  n_suburbs <- ncol(initial_expected_infections)
  
  if (is.null(lockdown_matrix)) {
    lockdown_matrix <- matrix(0, n_dates, n_suburbs)
  }
  
  # return configuration for this simulation
  list(
    n_dates = n_dates,
    n_suburbs = n_suburbs,
    dates = dates,
    size = nbinom_size,
    r_eff_matrix = r_eff,
    r_eff_reduction = r_eff_reduction,
    leaving_reduction = leaving_reduction,
    gi_matrix = gi_mat,
    delay_probs = delay_probs,
    populations = populations,
    import_rate = import_rate,
    initial_active_cases = initial_active_cases,
    initial_detections = initial_detections,
    lockdown_matrix = lockdown_matrix,
    lockdown_policy = lockdown_policy
  )
  
}
