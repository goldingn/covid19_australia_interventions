# simulate everything from the unconstrained parameters
sim <- function(par, n = 1e5) {
  
  params <- params(par)
  rate_sims <- sim_rates(params, n)
  time_sims <- sim_times(rate_sims, n)

  # return log total durations
  log(time_sims)
}
