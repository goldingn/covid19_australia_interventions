simulate_variant <- function(
  .fitted_model = fitted_model,
  dir = "outputs/projection/",
  subdir = NA,
  ratio_samples = FALSE,
  variant = c("wt", "alpha", "delta")
){
  
  variant <- match.arg(variant)
  
  if(is.na(subdir)){
    subdir <- variant
  }
 
  data <- .fitted_model$data
  dates <- .fitted_model$data$dates$mobility
  
  de <- .fitted_model$greta_arrays$distancing_effect
  
  p <- de$p
  
  prop_var <- prop_variant(dates = dates)
  prop_alpha <- prop_var$prop_alpha
  prop_delta <- prop_var$prop_delta
  prop_wt    <- prop_var$prop_wt
  
  phi_alpha       <- normal(1.454, 0.055, truncation = c(0, Inf))
  phi_delta_alpha <- normal(1.421, 0.033, truncation = c(0, Inf))
  
  phi_delta <- phi_alpha * phi_delta_alpha
  
  # phi_star <- prop_wt * 1 + prop_alpha * phi_alpha + prop_delta * phi_delta
  # 
  # p <- p_star ^ (1/phi_star)
  
  if(variant == "wt") {
    prop_wt_hat    <- prop_wt    * 0 + 1
    prop_alpha_hat <- prop_alpha * 0 
    prop_delta_hat <- prop_delta * 0
  }else if(variant == "alpha") {
    prop_wt_hat    <- prop_wt    * 0
    prop_alpha_hat <- prop_alpha * 0 + 1
    prop_delta_hat <- prop_delta * 0
  } else if(variant == "delta") {
    prop_wt_hat    <- prop_wt    * 0
    prop_alpha_hat <- prop_alpha * 0
    prop_delta_hat <- prop_delta * 0 + 1
  } 
  
  phi_hat <- prop_wt_hat * 1 + prop_alpha_hat * phi_alpha + prop_delta_hat * phi_delta
  
  p_hat <- p ^ phi_hat
  
  infectious_days <- infectious_period(gi_cdf)
  
  h_t <- h_t_state(dates)
  HD_t <- de$HD_0 * h_t
  
  household_infections <- de$HC_0 * (1 - p_hat ^ HD_t)
  non_household_infections <- de$OC_t_state * de$gamma_t_state *
    infectious_days * (1 - p_hat ^ de$OD_0)
  R_t <- household_infections + non_household_infections
  R_eff_loc_1_no_surv <- extend(R_t, data$n_dates_project)
  
  
  # multiply by the surveillance effect to get component 1
  surveillance_reff_local_reduction <- surveillance_effect(
    dates = data$dates$infection_project,
    cdf = gi_cdf,
    states = data$states
  )
  # 
  
  wt_fitted_model <- .fitted_model
  wt_fitted_model$greta_arrays$R_eff_loc_1 <- R_eff_loc_1_no_surv * surveillance_reff_local_reduction
  
  
  dir.create(dir, showWarnings = FALSE)
  write_reff_sims(wt_fitted_model, paste(dir, subdir, sep = "/"), write_reff_12 = FALSE)
  
  if(ratio_samples) {
    ratio <- wt_fitted_model$greta_arrays$R_eff_loc_1 / .fitted_model$greta_arrays$R_eff_loc_1
    ratio_vec <- c(ratio)
    ratio_sims <- calculate(ratio_vec, values = .fitted_model$draws, nsim = 2000)
    ratio_samples <- t(ratio_sims[[1]][, , 1])
    colnames(ratio_samples) <- paste0("sim", 1:2000)
    
    tibble(
      date = rep(.fitted_model$data$dates$infection_project, .fitted_model$data$n_states),
      state = rep(.fitted_model$data$states, each = .fitted_model$data$n_dates_project),
    ) %>%
      mutate(date_onset = date + 5) %>%
      cbind(ratio_samples) %>%
      write_csv(
        file.path(dir, "r_eff_1_ratio_samples.csv")
      )
    
    # read_csv("outputs/projection/wild_type/r_eff_1_local_samples.csv") %>%
    #   filter(date == as.Date("2020-04-11")) %>%
    #   pivot_longer(
    #     cols = starts_with("sim"),
    #     names_to = "sim"
    #   ) %>%
    #   group_by(state, date) %>%
    #   summarise(
    #     mean = mean(value),
    #     median = median(value),
    #     lower = quantile(value, 0.05),
    #     upper = quantile(value, 0.95),
    #     p_exceedance = mean(value > 1)
    #   )
  }
  
}
