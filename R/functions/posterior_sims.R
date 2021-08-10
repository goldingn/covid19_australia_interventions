posterior_sims <- function(vector, draws, nsim = NULL) {
  if (is.null(nsim)) {
    nsim <- niter(draws) * nchain(draws)
  }
  calculate(vector, values = draws, nsim = nsim)[[1]][, , 1]
}
