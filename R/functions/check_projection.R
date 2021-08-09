# check fit of projected cases against national epi curve
check_projection <- function(fitted_model, start_date = as.Date("2020-02-28")) {
  
  R_eff_local <- fitted_model$greta_arrays$R_eff_loc_12
  R_eff_imported <- fitted_model$greta_arrays$R_eff_imp_12
  gi_vec <- gi_vector(gi_cdf, fitted_model$data$dates$latest, state = "ACT")
  local_infectiousness <- fitted_model$data$local$infectiousness
  imported_infectiousness <- fitted_model$data$imported$infectiousnes
  local_cases <- fitted_model$data$local$cases
  dates <- fitted_model$data$dates$infection
  n_states <- fitted_model$data$n_states
  n_dates <- fitted_model$data$n_dates
  
  # national-level Reff - no clusters and weighted by state populations
  local_weights <- sweep(
    local_infectiousness,
    1,
    rowSums(local_infectiousness),
    FUN = "/"
  )
  local_weights[is.na(local_weights)] <- 1 / n_states
  
  import_weights <- sweep(
    imported_infectiousness,
    1,
    rowSums(imported_infectiousness),
    FUN = "/"
  )
  import_weights[is.na(import_weights)] <- 1 / n_states
  
  R_eff_loc_ntnl <- rowSums(R_eff_local[seq_len(n_dates), ] * local_weights)
  R_eff_imp_ntnl <- rowSums(R_eff_imported[seq_len(n_dates), ] * import_weights)
  
  # subset to from the first of March, when transmission became established (the
  # model is not designed to work with the stochastic extinctions we saw at the beginning of the outbreak)
  start <- which(dates == start_date)
  sub_idx <- start:n_dates
  
  # simulate local-local transmission dynamics, at national level; forced using
  # (observed) case importation and local cases prior to the start of the
  # simulation
  
  # locally-acquired infections present prior to the start of the simulation
  previous_local_cases <- local_cases
  previous_local_cases[sub_idx, ] <- 0
  previous_local_infectiousness <- gi_convolution(
    cases = previous_local_cases,
    dates = dates,
    states = data$states,
    gi_cdf = gi_cdf
  )
  # previous_local_infectiousness <- rowSums(previous_local_infectiousness)
  
  # compute infectious forcing from local cases emerging during this period that
  # were directly infected by imported cases (can't just include the import
  # infectiousness, since they are subject to a different Reff). Get expected
  # number of new local cases from imports, then disaggregate according to their
  # infectiousness profile to get force of local infection
  
  # expected number of new locally-acquired cases during the simulation period due
  # to infection from imports
  import_local_cases <- sweep(imported_infectiousness, 1, R_eff_imp_ntnl[seq_len(n_dates)], FUN = "*")
  import_local_cases_ntnl <- rowSums(import_local_cases)
  import_local_infectiousness <- gi_convolution(
    cases = import_local_cases,
    dates = dates,
    states = data$states,
    gi_cdf = gi_cdf
  )
  
  # combine these to get forcing from existing and import-associated local cases,
  # and disaggregate to get infectiousness of these
  local_infectiousness <- previous_local_infectiousness + import_local_infectiousness
  
  # Given this basic force of infection, R for locally-acquired cases (mean trend,
  # no clusters), and the infectiousness profile, iterate the dynamics to compute
  # the numbers of local cases
  secondary_locals <- project_local_cases(
    infectiousness = rowSums(local_infectiousness)[sub_idx],
    R_local = R_eff_loc_ntnl[sub_idx],
    disaggregation_probs = gi_vec
  )
  
  # compute locally-acquired cases
  local_cases_project_ntnl <- import_local_cases_ntnl[sub_idx] + secondary_locals
  local_cases_project_ntnl_sim <- calculate(local_cases_project_ntnl,
                                            values = fitted_model$draws,
                                            nsim = 1000)[[1]]
  
  data <- fitted_model$data
  data$dates$infection_project <- dates[sub_idx]
  data$n_dates_project <- length(sub_idx)
  
  local_cases_ntnl <- rowSums(local_cases[sub_idx, ])
  plot_trend(local_cases_project_ntnl_sim,
             data = data,
             multistate = FALSE,
             ylim = c(0, 2 * max(local_cases_ntnl)),
             hline_at = NULL,
             min_date = start_date,
             base_colour = green) +
    ggtitle("Projected national locally-acquired cases") +
    ylab("daily infections") +
    geom_line(data = data.frame(mean = local_cases_ntnl,
                                date = dates[sub_idx],
                                type = "Nowcast"))
  
}
