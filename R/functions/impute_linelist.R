impute_linelist <- function(linelist, notification_delay_cdf) {
  
  # impute the onset dates (only 0.6% of cases) using expected value from time to
  # detection distribution. Do this outside dplyr to avoid duplicating slow computations
  missing_onset <- is.na(linelist$date_onset)
  imputed_onsets <- impute_onsets(
    linelist$date_confirmation[missing_onset],
    linelist$state[missing_onset],
    notification_delay_cdf,
    method = "random"
  )
  linelist$date_onset[missing_onset] <- imputed_onsets
  
  linelist %>%
    mutate(date = date_onset - 5)
  
}
