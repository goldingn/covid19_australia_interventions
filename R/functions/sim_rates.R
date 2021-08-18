# simulate rates from this (without further cholesky decomposition)
sim_rates <- function(params, n = 1e5) {
  
  # simulate log rates from a multivariate normal
  z <- matrix(rnorm(n * 2), n, 2)
  log_rates <- sweep(z %*% params$chol_sigma, 1, params$mu, FUN = "+")
  z_E <- log_rates[, 1]
  z_I <- log_rates[, 2]
  
  E_rate <- exp(z_E + params$E_scaling * z_E ^ 2)
  I_rate <- exp(z_I + params$I_scaling * z_I ^ 2)
  
  # pull out rates for each simulation
  list(
    E_rate = E_rate,
    I_rate = I_rate
  )
  
}
