posterior_sims <- function(vector, draws, nsim = NULL) {
  if (is.null(nsim)) {
    nsim <- coda::niter(draws) * coda::nchain(draws)
  }
  calculate(vector, values = draws, nsim = nsim)[[1]][, , 1]
}
