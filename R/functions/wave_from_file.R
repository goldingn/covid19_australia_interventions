# get the survey wave number from a raw survey filename
wave_from_file <- function(filename, previous_waves = 14) {
  
  files <- list.files("data/survey_raw/", pattern = ".csv$", full.names = TRUE)
  lengths <- nchar(files)
  dates <- files %>%
    substr(lengths - 9, lengths - 4) %>%
    as.Date("%d%m%y")
  files <- files[order(dates)]
  waves <- seq_along(files) + previous_waves
  idx <- match(
    basename(filename),
    basename(files)
  )
  
  waves[idx]
  
}
