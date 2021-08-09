hist_prior_posterior <- function(greta_array, draws, nsim = 1000, ...)  {
  
  prior_sim <- calculate(greta_array, nsim = nsim)[[1]]
  posterior_sim <- calculate(greta_array, values = draws, nsim = nsim)[[1]]
  
  prior_sim <- c(prior_sim)
  posterior_sim <- c(posterior_sim)
  xlim <- range(c(prior_sim, posterior_sim))
  
  op <- par()
  on.exit(par(op))
  par(mfrow = c(1, 2))
  
  hist(c(prior_sim), xlim = xlim, main = "Prior", ...)
  hist(c(posterior_sim), xlim = xlim, main = "Posterior", ...)
  
}
