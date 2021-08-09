R0_prior <- function() {
  # Visually estimated values of the posterior distributions  (means and lower
  # and upper 95% CIs) over R0 in 11 European countries from Flaxman et al.
  # (Imperial report 13). Rather than use the estimates in the main text, we
  # usee those fromm the SI under the shortest serial interval - a mean of 5
  # days, corresponding to a median of 4.49 days. Note, we assumed based on
  # Figure 7 that the SI gamma distribution parameter values given are the mean
  # and the rate parameter of the distribution), since no other combination of
  # mean, sd, shape, scale, or rate could be found to match the figure closely.
  # We use this lower estimate because it is closest to the estimate of Nishiura
  # et al. of a median serial interval of 4 days, or 4.6 days taking only the
  # pairs of cases with greatest confidence. This is also the most conseervative
  # eestimate of R0 and therefore of the impact of social distancing interventions
  prior_estimate <- tibble::tribble(
    ~country, ~lower, ~mean, ~upper,
    "Denmark", 2, 2.6, 3.3,
    "Italy", 2.4, 2.6, 2.8,
    "Germany", 2.5, 3, 3.75,
    "Spain", 2.7, 3.2, 3.6,
    "United Kingdom", 2.4, 2.6, 3.1,
    "France", 2.5, 2.8, 3.2,
    "Norway", 1.7, 2.4, 3.3,
    "Belgium", 2.4, 2.9, 3.7,
    "Austria", 2.3, 2.7, 3.7,
    "Sweden", 2.3, 2.8, 3.6,
    "Switzerland", 2.3, 2.7, 3.4
  ) %>%
    summarise(lower = mean(lower),
              mean = mean(mean),
              upper = mean(upper))
  
  # to find a lognormal prior that matches this, find the parameters of a
  # lognormal distribution that give the minimum mean squared error against
  # these estiamtes
  obj <- function(par) {
    meanlog <- par[1]
    sdlog <- exp(par[2])
    mean <- exp(meanlog + sdlog ^ 2 / 2)
    cis <- qlnorm(c(0.025, 0.975), meanlog, sdlog)
    mean_error <- mean - prior_estimate$mean
    ci_error <- cis - c(prior_estimate$lower, prior_estimate$upper)
    mse <- mean(mean_error ^ 2 + ci_error[1] ^ 2 + ci_error[2] ^ 2)
    mse
  }
  
  par <- optim(par = c(0, 0), obj)$par
  
  list(meanlog = par[1], sdlog = exp(par[2]))
  
}
