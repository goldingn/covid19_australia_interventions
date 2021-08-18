# reff component 1 under only surveillance changes
reff_1_only_surveillance <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  log_R0 <- ga$log_R0
  reduction <- ga$surveillance_reff_local_reduction
  exp(log_R0 + log(reduction))
}
