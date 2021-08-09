# interpolate proportion of infectious cases that are imports
proportion_imported <- function (local_infectious, imported_infectious) {
  
  all_infectious <- local_infectious + imported_infectious
  prop_imported <- imported_infectious / all_infectious
  
  # transform to vector on unconstrained scale
  prop_imported <- pmax(0.01, prop_imported)
  prop_imported <- pmin(0.99, prop_imported)
  q_imported <- c(qlogis(prop_imported))
  
  n_dates <- nrow(all_infectious)
  n_states <- ncol(all_infectious)
  date_nums <- seq_len(n_dates)
  states <- factor(seq_len(n_states))
  # interpolate state-by-state  
  df <- data.frame(
    date = rep(date_nums, n_states),
    state = rep(states, each = n_dates)
  )
  model <- mgcv::gam(q_imported ~ s(date) +
                       s(date, by = state, k = 30),
                     data = df)
  q_imported <- predict(model, newdata = df)
  
  # transform back, clamp to numerically stable values, and return
  p_imported <- plogis(q_imported)
  eps <- sqrt(.Machine$double.eps)
  p_imported <- pmax(eps, p_imported)
  p_imported <- pmin(1 - eps, p_imported)
  dim(p_imported) <- dim(all_infectious)
  p_imported
  
}
