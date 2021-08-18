# outputting Reff trajectories for Rob M
reff_sims <- function(fitted_model, nsim = 2000, which = "R_eff_loc_12") {
  
  ga <- fitted_model$greta_arrays[[which]]
  ga_vec <- c(ga)
  sim <- calculate(ga_vec, values = fitted_model$draws, nsim = nsim)
  
  samples <- t(sim[[1]][, , 1])
  colnames(samples) <- paste0("sim", 1:2000)
  
  tibble(
    date = rep(fitted_model$data$dates$infection_project, fitted_model$data$n_states),
    state = rep(fitted_model$data$states, each = fitted_model$data$n_dates_project),
  ) %>%
    mutate(date_onset = date + 5) %>%
    cbind(samples)
  
}
