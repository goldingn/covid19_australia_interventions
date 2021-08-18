download_tidycovid <- function() {
  
  # Google dropped a bunch of previous data from the latest file. Pull a cached
  # version from the tidycovid package on GitHub and replace it.
  tidycovid_url <- "https://github.com/goldingn/tidycovid19/raw/e4db3ab3007576f34dcb1e8c3299b235cff6198e/cached_data/google_cmr.RDS"
  
  f <- tempfile(fileext = ".RDS")
  download.file(tidycovid_url, destfile = f, method='wget')
  tmp <- readRDS(f)
  saveRDS(tmp, file = "data/google_cmr/tidycovid_cache.RDS")  
}
