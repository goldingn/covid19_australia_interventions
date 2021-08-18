# things to do for each:
# 1. compute Reff C1s under each scenario
# 2. project case counts under Reff trajectory
#  - function to create greta array for locally-acquired case trajectories based on two reff trajectories, imported case counts, and initial local case counts
#  - function to set up imported case counts and initial case counts for each phase
#  - wrapper function to take in scenario config, do calculation of posteriors, and save outputs 
# the vectors of case counts to use for each scenario
scenario_cases <- function(scenario, fitted_model) {
  
  all_dates <- fitted_model$data$dates$infection
  all_imported <- rowSums(fitted_model$data$imported$cases)
  all_local <- rowSums(fitted_model$data$local$cases)
  
  scenario_dates <- scenario_dates(scenario)
  scenario_start <- min(scenario_dates)
  n_scenario_dates <- length(scenario_dates)
  
  during <- all_dates %in% scenario_dates
  before <- all_dates < scenario_start & all_dates >= (scenario_start - 21)
  
  list(
    local_cases = c(all_local[before], rep(0, n_scenario_dates)),
    imported_cases = all_imported[before | during],
    dates = all_dates[before | during],
    simulation_start = scenario_start,
    n_dates = n_scenario_dates
  )  
  
}
