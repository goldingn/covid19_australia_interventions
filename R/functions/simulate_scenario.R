# given a fitted reff model and a scenario, simulate Reffs and the numbers of
# locally-acquired cases nationally
simulate_scenario <- function (index, scenarios, fitted_model, nsim = 5000) {
  
  scenario <- scenarios[index, ]
  
  reffs <- counterfactual_reffs(scenario, fitted_model)
  case_data <- scenario_cases(scenario, fitted_model)
  one <- ones(length(case_data$local_cases))
  
  # need to include 3 weeks of pre-simulation local and imported cases, so pad
  # dates. pass in a single column matrix of these things to do national simulations
  simulation <- forecast_locals(
    local_cases = as.matrix(case_data$local_cases),
    imported_cases = as.matrix(case_data$imported_cases),
    Reff_locals = reffs$local_reff * one,
    Reff_imports = reffs$import_reff * one,
    dates = case_data$dates,
    gi_cdf = gi_cdf,
    simulation_start = case_data$simulation_start
  )
  
  keep <- case_data$dates >= case_data$simulation_start
  local_cases <- simulation$local_cases[keep]
  imported_cases <- case_data$imported_cases[keep]
  dates <- case_data$dates[keep]
  
  reff_local <- reffs$local_reff
  reff_imported <- reffs$import_reff
  
  # get posterior samples
  sims <- calculate(
    local_cases, reff_local, reff_imported,
    values = fitted_model$draws,
    nsim = nsim
  )
  
  # handle the outputting
  local_cases <- tibble(
    sim = rep(seq_len(nsim), each = length(dates)),
    date = rep(dates, nsim),
    cases = as.vector(t(sims$local_cases[, , 1]))
  )
  
  reffs <- tibble(
    sim = seq_len(nsim),
    local = as.vector(sims$reff_local),
    imported = as.vector(sims$reff_imported),
  )
  
  # save these samples
  module(scenario, local_cases, reffs) %>%
    saveRDS(paste0("outputs/counterfactuals/scenario", index, ".RDS"))
  
}
