download_tidycovid <- function() {
  f <- tempfile(fileext = ".RDS")
  download.file(tidycovid_url, destfile = f, method='wget')
  tmp <- readRDS(f)
  saveRDS(tmp, file = "data/google_cmr/tidycovid_cache.RDS")  
}
