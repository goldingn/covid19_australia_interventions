
# reff component 1 under only surveillance changes
reff_1_only_surveillance <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  log_R0 <- ga$log_R0
  reduction <- ga$surveillance_reff_local_reduction
  exp(log_R0 + log(reduction))
}

# reff component 1 if only macrodistancing had changed
reff_1_only_macro <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  h_t <- h_t_state(fitted_model$data$dates$mobility)
  HD_t <- de$HD_0 * h_t
  household_infections_macro <- de$HC_0 * (1 - de$p ^ HD_t)
  non_household_infections_macro <- de$OC_t_state * infectious_days * (1 - de$p ^ de$OD_0)
  hourly_infections_macro <- household_infections_macro + non_household_infections_macro
  hourly_infections_macro_extended <- extend(
    hourly_infections_macro,
    fitted_model$data$n_dates_project
  )
  hourly_infections_macro_extended * baseline_surveillance_effect
}

# reff component 1 if only macrodistancing had changed
reff_1_only_micro <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  household_infections_micro <- de$HC_0 * (1 - de$p ^ de$HD_0)
  non_household_infections_micro <- de$OC_0 * infectious_days *
    (1 - de$p ^ de$OD_0) * de$gamma_t_state
  hourly_infections_micro <- household_infections_micro +
    non_household_infections_micro
  hourly_infections_micro_extended <- extend(
    hourly_infections_micro,
    fitted_model$data$n_dates_project
  )
  hourly_infections_micro_extended * baseline_surveillance_effect
}



reff_1_vaccine_effect <- function(fitted_model, timeseries){
  
  ga <- fitted_model$greta_arrays
  
  dates <- fitted_model$data$dates$infection_project
  
  df <- full_join(
    tibble(date = dates),
    timeseries
  ) %>%
    tidyr::fill(
      overall_transmission_effect,
      .direction = "updown"
    )
  
  ote <- df$overall_transmission_effect
  
#  log_R0 <- ga$log_R0  
  
#  exp(log_R0)*ote
  
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  household_infections_vacc <- de$HC_0 * (1 - de$p ^ de$HD_0)
  non_household_infections_vacc <- de$OC_0 * infectious_days *
    (1 - de$p ^ de$OD_0)
  hourly_infections_vacc <- household_infections_vacc +
    non_household_infections_vacc
  hourly_infections_vacc_extended <- extend(
    hourly_infections_vacc,
    fitted_model$data$n_dates_project
  )
  
  reff_non_vac <- hourly_infections_vacc_extended * baseline_surveillance_effect 
  
  sweep(reff_non_vac, 1, ote, "*")
  
}


vaccination_dates <- function() {
  expand_grid(
    date = c("2021-02-22"),
    state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", 
              "VIC", "WA")
  ) %>%
    mutate(
      date = as.Date(date),
      state = factor(state)
    )
}


