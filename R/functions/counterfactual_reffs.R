# scalar reffs for locally- and overseas-acquired cases under different policy scenarios
counterfactual_reffs <- function(scenario, fitted_model) {
  
  # baseline reff due to household/non-household model
  baseline_local_reff <- reff_distancing(
    fitted_model,
    macro_effect = scenario$mobility_restrictions,
    micro_effect = scenario$physical_distancing
  )
  
  # reduction in reff due to contact tracing
  contact_tracing_effect <- switch(
    scenario$contact_tracing,
    none = 1,
    suboptimal = surveillance_effect(as.Date("2020-01-01"), gi_cdf),
    optimal = surveillance_effect(as.Date("2020-09-01"), gi_cdf)
  )
  
  # overall reff for locally-acquired cases
  local_reff <- baseline_local_reff * contact_tracing_effect
  
  # overall reff for overseas-acquired cases  
  import_reff <- local_reff
  if (scenario$overseas_quarantine) {
    log_q3 <- fitted_model$greta_arrays$log_q[3]
    import_reff <- exp(log_q3)
  }
  
  # return these scalar greta arrays
  module(local_reff, import_reff)
  
}
