impute_onsets <- function(confirmation_dates,
                          states,
                          notification_delay_cdf,
                          method = c("expected", "random"),
                          min_days = -10,
                          max_days = 40) {
  
  method <- match.arg(method)
  onset_dates <- mapply(
    impute_one_onset,
    confirmation_date = confirmation_dates,
    state = states,
    MoreArgs = list(
      notification_delay_cdf = notification_delay_cdf,
      method = method,
      min_days = min_days,
      max_days = max_days
    ),
    SIMPLIFY = FALSE
  )
  do.call(c, onset_dates)
  
}
