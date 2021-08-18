fit_reff_model <- function(data, max_tries = 3, iterations_per_step = 2000) {
  
  # build the greta model
  model_output <- reff_model(data)
  greta_arrays <- model_output$greta_arrays
  greta_model <- model_output$greta_model
  
  # first pass at model fitting  
  draws <- mcmc(
    greta_model,
    sampler = hmc(Lmin = 25, Lmax = 30),
    chains = 10,
    warmup = 500,
    n_samples = 2000,
    one_by_one = TRUE
  )
  
  # if it did not converge, try extending it a bunch more times
  finished <- converged(draws)
  tries <- 0
  while(!finished & tries < max_tries) {
    draws <- extra_samples(
      draws,
      iterations_per_step,
      one_by_one = TRUE
    )
    tries <- tries + 1
    finished <- converged(draws)
  }
  
  # warn if we timed out before converging successfully
  if (tries == max_tries) {
    warning("sampling did not converge according to benchmarks")
  }
  
  # return a fitted model object
  module(greta_model, greta_arrays, data, draws)
  
}
