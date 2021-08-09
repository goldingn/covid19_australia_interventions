# clean up some weird date encoding in the linelist
clean_date <- function (date, min_date = as.Date("2019-12-01"), max_date = Sys.Date()) {
  # don't use ifelse as it converts to a numeric
  # date <- original_date
  # date[weird] <- corrected_date[weird]
  
  # remove any that are out of bounds
  early <- as.Date(date) < min_date
  late <- as.Date(date) > max_date
  date[early | late] <- NA
  
  date
}
