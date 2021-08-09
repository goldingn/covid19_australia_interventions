# load the expected number of infections by LGA
latest_lga_cases <- function(which = c("local", "imported")) {
  which <- match.arg(which)
  pattern <- paste0("lga_", which, "_infections_")
  files <- list.files("~/not_synced/", pattern = pattern, full.names = TRUE)
  lengths <- nchar(files)
  dates <- files %>%
    substr(nchar(.) - 13, nchar(.) - 4) %>%
    as.Date()
  keep <- which.max(dates)
  readRDS(files[keep])
}
