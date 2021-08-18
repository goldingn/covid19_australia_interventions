# reff C1 locals (except surveillance effect) with macro/microdistancing either
# at optimal effect (TRUE) or turned off (FALSE)
reff_distancing <- function(fitted_model, macro_effect = TRUE, micro_effect = TRUE) {
  
  de <- fitted_model$greta_arrays$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  
  # duration in the household
  HD <- de$HD_0
  
  # non-household contacts
  OC <- de$OC_0
  
  # reduction in transmission probability due to hygiene measures
  gamma <- 1
  
  if (macro_effect) {
    
    macro_trends <- trends_date_state(
      "outputs/macrodistancing_trends.RDS",
      fitted_model$data$dates$infection
    )
    
    # optimal date and state for reduction in contacts
    macro_optimum <- which(macro_trends == min(macro_trends), arr.ind = TRUE)[1, , drop = FALSE]
    
    # duration in the household increases at optimum
    h_optimal <- h_t_state(fitted_model$data$dates$infection)[macro_optimum]
    HD <- HD * h_optimal
    
    # number of non-household contacts decreases at optimum
    OC <- macro_trends[macro_optimum]
  }
  
  if (micro_effect) {
    
    # optimal date and state for reduction in contacts
    micro_trends <- trends_date_state(
      "outputs/microdistancing_trends.RDS",
      fitted_model$data$dates$infection
    )
    micro_optimum <- which(micro_trends == max(micro_trends), arr.ind = TRUE)[1, , drop = FALSE]
    
    # transmission probability is reduced at optimum
    gamma <- de$gamma_t_state[micro_optimum]
    
  }
  
  household_infections <- de$HC_0 * (1 - de$p ^ HD)
  non_household_infections <- OC * infectious_days *
    (1 - de$p ^ de$OD_0) * gamma
  
  household_infections + non_household_infections
  
}
