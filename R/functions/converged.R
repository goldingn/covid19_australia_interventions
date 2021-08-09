# has the sampler converged to our standards?
converged <- function(draws, max_r_hat = 1.1, min_n_eff = 1000) {
  stats <- convergence(draws)
  all(stats$r_hats < max_r_hat) &
    all(stats$n_eff >= min_n_eff)
}
