clean_vaccine_rollout_ts <- function(path){
  
  tidy_vaccination <- tidy_vaccine_rollout_data(path)
  
  tidy_vac_no_totals <- tidy_vaccination %>% filter(age_group != "Totals")
  
  tidy_clean_vac <- tidy_vac_no_totals %>% 
    mutate(count = count_without_suppressed) %>% 
    dplyr::select(-count_without_suppressed)
}