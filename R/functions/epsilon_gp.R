# contruct multiple GPs for epsilons in the Reff model
epsilon_gp <- function(
  date_nums,
  n_states,
  kernel,
  inducing_date_nums = date_nums,
  sigma_state = normal(0, 0.5, truncation = c(0, Inf), dim = n_states),
  tol = 1e-6) {
  
  # whitened representation of GP
  n_inducing <- length(inducing_date_nums)
  v_raw <- normal(0, 1, dim = c(n_inducing, n_states))
  v <- sweep(v_raw, 2, sigma_state, FUN = "*")
  
  # GP  
  epsilon <- multi_gp(
    x = date_nums,
    v = v,
    kernel = kernel,
    inducing = inducing_date_nums,
    tol = tol
  )
  
  epsilon
  
}
