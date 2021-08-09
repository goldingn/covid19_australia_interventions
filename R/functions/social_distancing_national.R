# get the overall index of distancing (no waning) on the current dates and
# optionally add extra 1s at the end
social_distancing_national <- function(dates, n_extra = 0) {
  
  distancing_file <- "outputs/social_distancing_latent.RDS"
  distancing_index <- distancing_file %>%
    readRDS() %>%
    select(mean, date) %>%
    old_right_join(tibble(date = dates)) %>%
    replace_na(list(mean = 0)) %>%
    pull(mean)
  
  distancing_index <- c(distancing_index, rep(1, n_extra))
  distancing_index
  
}
