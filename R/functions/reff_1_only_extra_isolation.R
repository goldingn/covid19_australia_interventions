reff_1_only_extra_isolation <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  log_R0 <- ga$log_R0
  reduction <- ga$extra_isolation_local_reduction
  exp(log_R0 + log(reduction))
}
