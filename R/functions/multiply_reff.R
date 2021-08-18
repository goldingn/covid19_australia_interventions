# modify Reff of a fitted model by multiplying by multiplicative effect, drawn from a distribution
multiply_reff <- function(fitted_model, prior_mean, prior_range, quantile = 0.95) {
  prob <- 1 - (1 - quantile) / 2
  prior_sd <- abs(diff(prior_range)) / (2 * qnorm(prob))
  effect <- normal(prior_mean, prior_sd, truncation = c(0, Inf))
  fitted_model$greta_arrays$R_eff_loc_12 <- fitted_model$greta_arrays$R_eff_loc_12 * effect 
  fitted_model$greta_arrays$R_eff_loc_1 <- fitted_model$greta_arrays$R_eff_loc_1 * effect 
  fitted_model
}
