# get the mean date of symptom onset give a date of detection (using the
# time-varying time to detection distribution)
impute_one_onset <- function(confirmation_date,
                             state,
                             notification_delay_cdf,
                             method = c("expected", "random"),
                             min_days = -10,
                             max_days = 40) {
  
  method <- match.arg(method)
  
  # get possible dates of onset
  delays <- seq(min_days, max_days) 
  possible_onset_dates <- confirmation_date - delays
  
  # probability of being detected this many days later (probability of detection
  # by this day, minus probability of detection by the previous day)
  surv_from <- notification_delay_cdf(delays - 1, possible_onset_dates, state)
  surv_to <- notification_delay_cdf(delays, possible_onset_dates, state)
  prob <- surv_from - surv_to
  
  # normalise to get probabilities of different delays
  prob <- prob / sum(prob)
  
  # compute either the expected time since onset, or draw a random one
  delay <- switch(method,
                  expected = round(sum(delays * prob)),
                  random = sample(delays, 1, prob = prob))
  
  # subtract to get expected date of symptom onset
  onset_date <- confirmation_date - delay
  onset_date
  
}
