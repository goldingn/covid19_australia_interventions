# output simulations
write_reff_sims <- function(fitted_model,
                            dir = "outputs/projection",
                            write_reff_1 = TRUE,
                            write_reff_12 = TRUE) {
  
  if (write_reff_1) {
    
    reff_1 <- reff_sims(fitted_model, which = "R_eff_loc_1")
    
    reff_1 %>%
      write_csv(
        file.path(dir, "r_eff_1_local_samples.csv")
      )
    
  }
  
  if (write_reff_12) {
    
    # find the dates for clamping into the future (where 50%/95% cases so far detected)
    clip_idx_50 <- (fitted_model$data$detection_prob_mat > 0.5) %>%
      apply(1, all) %>%
      which() %>%
      max()
    
    clip_idx_95 <- (fitted_model$data$detection_prob_mat > 0.95) %>%
      apply(1, all) %>%
      which() %>%
      max()
    
    date_50 <- fitted_model$data$dates$infection[clip_idx_50]
    date_95 <- fitted_model$data$dates$infection[clip_idx_95]
    
    reff_12 <- reff_sims(fitted_model, which = "R_eff_loc_12")
    
    reff_12 %>%
      write_csv(
        file.path(dir, "r_eff_12_local_samples.csv")
      )
    
    reff_12 %>%
      soft_clamp(date_50) %>%
      write_csv(
        file.path(dir, "r_eff_12_local_samples_soft_clamped_50.csv")
      )
    
    reff_12 %>%
      soft_clamp(date_95) %>%
      write_csv(
        file.path(dir, "r_eff_12_local_samples_soft_clamped_95.csv")
      )
  }
  
}
