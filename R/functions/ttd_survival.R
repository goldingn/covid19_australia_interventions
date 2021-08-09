# given vectors of dates and numbers of days post infection, and a single state,
# return the fraction of cases *not* being detected by that point
ttd_survival <- function(days, dates, target_state, cdfs = NULL) {
  
  # filter to this state,
  # loop through dates running ecdfs on days (accounting for change of dates from onset to infection!)
  
  # will need to line up dates, but shouldn't need to line up days_idx (ecdf()
  # will take care of it)

  # load empirical CDFs of delay from onset to notificiation (aggregated from
  # date of onset) over time
  if (is.null(cdfs)) {
    cdfs <- readRDS("outputs/delay_from_onset_cdfs.RDS")
  }
  
  # subset to this state
  cdfs <- cdfs %>%
    filter(state == target_state)
  
  # line up dates
  dates <- pmin(dates, max(cdfs$date))
  dates <- pmax(dates, min(cdfs$date))
  dates_idx <- match(dates, cdfs$date)
  
  # convert from days post infection to days post onset (can be up to 5 days
  # negative)
  days_onset <- days - 5
  days_onset_list <- lapply(days_onset, list)

  # apply relevant CDF to each number of days
  ecdfs <- cdfs$ecdf[dates_idx]
  cdf_vec <- mapply(do.call, ecdfs, days_onset_list)
  
  # return probability of not being detected by this point
  1 - cdf_vec
  
}
