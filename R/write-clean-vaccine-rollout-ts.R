write_clean_vacc_rollout <- function(path, path_write){

  clean_vaccine_rollout <- clean_vaccine_rollout_ts(path)
  
  write_csv(
    x = clean_vaccine_rollout,
    file = path_write
  )
  
}
