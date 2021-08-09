# function to get a greta array forecasting numbers of locally-acquired cases
# in each state into the future.
# local cases and imported cases should be matrices containing integer (or
# fractional) numbers of observed or assumed cases infected on each date.
# Reff_locals and Reff_imports should be either matrices or 2D greta arrays of
# transmission potential for locally-acquired and imported cases. All four of
# these arguments must have the same number of columns. Reff_locals and
# Reff_imports must have the same number of rows, which should be greater than
# or equal to the numbers of rows in local_cases and imported_cases. Where
# local_cases and imported_cases have fewer rows than the other matrices, they
# will be padded with zeros to represent an assumption of no imported cases or
# other local cases injected into the local population. dates must be a vector
# of dates with as many elements as rows in the Reff matrices, and gi_cdf must
# be a function returning the continuous version of the generation interval
# distribution.
# the function first computes the number of *primary* local cases - those
# infected by imported cases - and then uses a discrete convolution to compute
# the expected number of secondary local infections (local-local) into the
# future. Note this a deterministic simulation of real-valued case counts, so it
# is impossible for a simulated outbreak to go extinct, and a case count of
# close to 0 cases will inevitably lead to a large outbreak if Reff exceeds 1.
forecast_locals <- function (local_cases, imported_cases,
                             Reff_locals, Reff_imports,
                             dates, gi_cdf,
                             simulation_start = dates[nrow(local_cases)],
                             gi_bounds = c(0, 20)) {
  
  n_dates <- length(dates)
  n_states <- ncol(Reff_locals)
  states <- c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
  
  # check inputs
  if (nrow(Reff_locals) != n_dates |
      nrow(Reff_imports) != n_dates) {
    stop("Reff_locals and Reff_imports must have the same number of rows ",
         "as there are elements in dates")
  }
  
  if (ncol(Reff_imports) != n_states |
      ncol(local_cases) != n_states |
      ncol(imported_cases) != n_states) {
    stop("all input matrices must have the same number of columns")
  }
  
  if (nrow(local_cases) > n_dates |
      nrow(imported_cases) > n_dates) {
    stop("local_cases and imported_cases must fewer or equal numbers ",
         "of rows to the Reff matrices")
  }
  
  # pad the cases data if needed
  local_cases <- pad_cases_matrix(local_cases, n_dates, which = "after")
  imported_cases <- pad_cases_matrix(imported_cases, n_dates, which = "after")
  
  # create the generation interval matrix and vector
  # gi_mat <- gi_matrix(gi_cdf, dates, gi_bounds = gi_bounds)
  gi_vec <- gi_vector(gi_cdf, max(dates), gi_bounds = gi_bounds)
  
  # infectiousness of imported cases over time
  imported_infectious <- gi_convolution(
    cases = imported_cases,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  
  # expected number of primary (import-local) locally-acquired cases
  primary_local_cases <- imported_infectious * Reff_imports
  
  # infectiousness of primary locally-acquired cases
  primary_local_infectiousness <- gi_convolution(
    cases = primary_local_cases,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  
  # infectiousness of observed (or assumed) locally-acquired cases
  existing_local_infectiousness <- gi_convolution(
    cases = local_cases,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  
  # sum get local infectiousness not caused by dynamic cases
  local_infectiousness <- existing_local_infectiousness +
    primary_local_infectiousness
  
  # work out where to simulate from
  start_idx <- match(simulation_start, dates)
  sim_idx <- seq(start_idx, n_dates, by = 1)
  
  # simulate the expected numbers of secondary local cases
  secondary_local_cases <- project_local_cases(
    infectiousness = local_infectiousness[sim_idx, ],
    R_local = Reff_locals[sim_idx, ],
    disaggregation_probs = gi_vec
  )
  
  # pad the result with 0s to represent simulated cases
  secondary_local_cases <- pad_cases_matrix(secondary_local_cases,
                                            n_dates,
                                            "before")
  
  # matrix of just the primary local cases after the observed cases
  new_primary_local_cases <- zeros(n_dates, n_states)
  new_primary_local_cases[sim_idx, ] <- primary_local_cases[sim_idx, ]
  
  # combine with primary local cases to get the total number of new
  # locally-acquired cases
  forecast_local_cases <- local_cases + new_primary_local_cases + secondary_local_cases
  
  # get the probability (poisson assumption) of one for more new
  # locally-acquired cases
  forecast_local_infectious <- gi_convolution(
    cases = forecast_local_cases,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  
  expected_transmission <- forecast_local_infectious * Reff_locals +
    primary_local_cases
  p_cases <- 1 - exp(-expected_transmission)
  
  list(
    local_cases = forecast_local_cases,
    secondary_local_cases = secondary_local_cases,
    probability_of_cases = p_cases
  )
  
}
