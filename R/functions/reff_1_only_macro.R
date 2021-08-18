# reff component 1 if only macrodistancing had changed
reff_1_only_macro <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  h_t <- h_t_state(fitted_model$data$dates$mobility)
  HD_t <- de$HD_0 * h_t
  household_infections_macro <- de$HC_0 * (1 - de$p_star ^ HD_t)
  non_household_infections_macro <- de$OC_t_state * infectious_days * (1 - de$p_star ^ de$OD_0)
  hourly_infections_macro <- household_infections_macro + non_household_infections_macro
  hourly_infections_macro_extended <- extend(
    hourly_infections_macro,
    fitted_model$data$n_dates_project
  )
  hourly_infections_macro_extended * baseline_surveillance_effect
}
