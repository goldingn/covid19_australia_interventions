# plot visual checks of model posterior calibration against observed data
plot_reff_checks <- function(fitted_model, nsim = 10000) {
  
  cases <- negative_binomial(
    fitted_model$greta_arrays$size,
    fitted_model$greta_arrays$prob_trunc
  )
  cases_sim <- calculate(cases, values = fitted_model$draws, nsim = nsim)[[1]][, , 1]
  
  valid <- which(fitted_model$data$valid_mat, arr.ind = TRUE)
  observed <- fitted_model$data$local$cases[valid]
  
  # overall PPC check
  bayesplot::ppc_ecdf_overlay(
    observed,
    cases_sim[1:1000, ],
    discrete = TRUE
  )
  
  # check by state and time
  plot_fit(observed, cases_sim, fitted_model$data)
  
  # check simulation fit
  check_projection(fitted_model)
  
}
