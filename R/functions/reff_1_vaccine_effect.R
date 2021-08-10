# reff component 1 if only vaccination
reff_1_vaccine_effect <- function(fitted_model, timeseries){
  
  ga <- fitted_model$greta_arrays
  
  dates <- fitted_model$data$dates$infection_project
  
  df <- full_join(
    tibble(date = dates),
    timeseries
  ) %>%
    fill(
      overall_transmission_effect,
      .direction = "updown"
    )
  
  ote <- df$overall_transmission_effect
  
  
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  household_infections_vacc <- de$HC_0 * (1 - de$p_star ^ de$HD_0)
  non_household_infections_vacc <- de$OC_0 * infectious_days *
    (1 - de$p_star ^ de$OD_0)
  hourly_infections_vacc <- household_infections_vacc +
    non_household_infections_vacc
  hourly_infections_vacc_extended <- extend(
    hourly_infections_vacc,
    fitted_model$data$n_dates_project
  )
  
  reff_non_vac <- hourly_infections_vacc_extended * baseline_surveillance_effect 
  
  sweep(reff_non_vac, 1, ote, "*")
  
}
