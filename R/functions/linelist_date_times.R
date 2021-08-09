linelist_date_times <- function(
  dir,
  name_pattern = "^COVID-19 UoM "
) {
  # find the files
  files <- list.files(dir, pattern = ".xlsx$", full.names = TRUE)
  # pull out the date time stamp
  date_time_text <- gsub(name_pattern, "", basename(files)) 
  date_time_text <- gsub(".xlsx$", "", date_time_text)
  date_times <- as.POSIXct(date_time_text, format = "%d%b%Y %H%M")
  # return as a dataframe
  tibble::tibble(
    file = files,
    date_time = date_times
  )
}
