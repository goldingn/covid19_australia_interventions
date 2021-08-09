# simulate time to an infection from these rates
sim_times <- function(rate_sims, n = 1e5) {
  
  # simulate exponential E and I durations
  Es <- matrix(rexp(n * 2, rate_sims$E_rate), ncol = 2)
  Is <- matrix(rexp(n * 2, rate_sims$I_rate), ncol = 2)
  
  # get the infectious period start and end dates
  starts <- rowSums(Es)
  durations <- rowSums(Is) 
  ends <- starts + durations
  times <- runif(n, starts, ends)
  times
  
}
