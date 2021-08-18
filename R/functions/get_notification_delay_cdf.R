# return a function to get the CDf of the notification delay distribution for a
# given date and state
get_notification_delay_cdf <- function(linelist) {
  
  delay_data <- linelist %>%
    filter(
      !is.na(date_onset),
      date_confirmation <= (date_linelist - 3)
    ) %>%
    select(
      date_onset,
      date_confirmation,
      state,
      import_status
    ) %>%
    mutate(
      delay = as.numeric(date_confirmation - date_onset),
      group = notification_delay_group(date_confirmation, state)
    ) %>%
    filter(
      delay <= 6 * 7
    ) %>%
    group_by(group) %>%
    mutate(
      lower = quantile(delay, 0.005),
      upper = quantile(delay, 0.995)
    ) %>%
    filter(
      delay >= lower,
      delay <= upper
    )
  
  # get an ecdf for each group
  ecdfs <- delay_data %>%
    mutate(
      ecdf = list(ecdf(delay)),
      id = row_number()
    ) %>%
    filter(id == 1) %>%
    select(group, ecdf)
  
  # return a function to compute the CDF of the delay distribution for that
  # state and those delays and dates
  function(delays, possible_onset_dates, states) {
    
    group <- notification_delay_group(possible_onset_dates, states)
    idx <- match(group, ecdfs$group)
    idx <- replace_na(idx, 1)
    
    cdfs <- ecdfs$ecdf[idx]
    probs <- rep(0, length(group))
    
    for(i in seq_along(group)) {
      probs[i] <- cdfs[[i]](delays[i])
    }
    
    probs
    
  }
  
}
