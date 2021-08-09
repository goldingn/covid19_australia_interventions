# modification of R_ef in the face of lockdown
lockdown_r_eff <- function(idx, state, config) {
  baseline_r_eff <- config$r_eff_matrix[idx, ]
  lockdown_status <- state$lockdown_matrix[idx, ]
  modification <- 1 - lockdown_status * (1 - config$r_eff_reduction)
  baseline_r_eff * modification
}
