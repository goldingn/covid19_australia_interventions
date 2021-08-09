# try a bunch of previous days to find the most recent citymapper dataset
citymapper_url <- function(min_delay = 0, max_delay = 14) {
  date <- Sys.Date()
  delay <- min_delay
  while(delay < max_delay) {
    # try various dates to find the citymapper URL
    datestring <- format(date - delay, format = "%Y%m%d")
    
    url <- paste0(
      "https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_",
      datestring,
      ".csv"
    )
    
    # return if successful, or increment
    if (url_exists(url)) {
      return(url)
    } else {
      delay <- delay + 1
    }
    
  }
  
  # error if we hit the timeout
  if (delay == max_delay) {
    stop ("could not find a valid citymapper URL")
  }
  
}
