objective <- function(par) {
  
  log_time_sims <- sim(par)
  
  # compute log-likelihood across these simulations
  -sum(dnorm(log_time_sims, meanlog, sdlog, log = TRUE))
  
}
