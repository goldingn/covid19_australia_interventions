# given a timeseries of cumulative doses (for contiguous consecutive dates), and
# an assumed ideal inter-dose period, return the timeseries of the cumulative
# number of people being fully-vaccinated (having received both doses), assuming
# that second-doses are prioritised for vaccination and are always given at the
# earliest availability after the inter-dose period has elapsed.
model_vaccination_coverage <- function (daily_new_doses, inter_dose_days = 28) {
  
  # empty vectors for the number of first and second doses distributed
  first_doses <- second_doses <- daily_new_doses * 0
  
  # empty scalar for the queue of people due to receive their second dose but
  # who could not yet receive it due to insufficient doses availables
  queueing_for_second_dose <- 0
  
  # iterate through days, 
  for (day in seq_along(daily_new_doses)) {
    
    # the number of new second doses eligible today
    lagged_day <- day - inter_dose_days
    if (lagged_day >= 1) {
      target_second_doses <- first_doses[lagged_day]
    } else {
      target_second_doses <- 0
    }
    
    # add on any waiting from previous days
    target_second_doses <- target_second_doses + queueing_for_second_dose
    
    # calculate the maximum number of second doses that can be given out
    # (assuming these are the priority)
    second_doses[day] <- min(daily_new_doses[day], target_second_doses)
    
    # tally up any second doses in waiting that were not vaccinated today, to
    # wait for the next day
    queueing_for_second_dose <- max(0, target_second_doses - second_doses[day])
    
    # assign any remaining doses as first doses
    first_doses[day] <- daily_new_doses[day] - second_doses[day]
    
  }
  
  # cumulative number of fully-vaccinated people
  cumsum(second_doses)
  
}
