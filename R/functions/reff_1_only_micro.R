# reff component 1 if only macrodistancing had changed
reff_1_only_micro <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  household_infections_micro <- de$HC_0 * (1 - de$p_star ^ de$HD_0)
  non_household_infections_micro <- de$OC_0 * infectious_days *
    (1 - de$p_star ^ de$OD_0) * de$gamma_t_state
  hourly_infections_micro <- household_infections_micro +
    non_household_infections_micro
  hourly_infections_micro_extended <- extend(
    hourly_infections_micro,
    fitted_model$data$n_dates_project
  )
  hourly_infections_micro_extended * baseline_surveillance_effect
}
