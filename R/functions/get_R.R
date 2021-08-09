get_R <- function (transition_matrix, stable_age = NULL, tolerance = 0.001, max_iter = 1000) {
  #Re(eigen(x)$value[1])
  # function from STEPS
  # https://github.com/steps-dev/steps/blob/74c5359dd4470c4056cd799c53ef56d503ba69da/R/growth_transition_functions-class.R#L211
  # compute R from a transition (next generation) matrix
  
  if (is.null(stable_age)) {
    stable_age <- rep(1, ncol(transition_matrix))
  }
  old_stages <- stable_age
  converged <- FALSE
  iter <- 0
  old_Rs <- rep(.Machine$double.eps, ncol(transition_matrix))
  
  while (!converged & iter < max_iter) {
    new_stages <- transition_matrix %*% old_stages
    Rs <- new_stages / old_stages
    errors <- abs(1 - (Rs / old_Rs))
    converged <- all(errors < tolerance)
    old_Rs <- Rs
    old_stages <- new_stages
    iter <- iter + 1
  }
  
  if (!converged) {
    warning(
      "estimation of growth rate did not converge in ",
      max_iter,
      " iterations"
    )
  }
  
  # return the intrinsic growth rate
  Rs[1]
  
}
