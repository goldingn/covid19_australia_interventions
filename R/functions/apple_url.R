# scrape out the URL to apple mobility data
apple_url <- function() {
  base_url <- "https://covid19-static.cdn-apple.com"
  json_data <- base_url %>%
    file.path("covid19-mobility-data/current/v3/index.json") %>%
    fromJSON()
  paste0(base_url, json_data$basePath, json_data$regions$`en-us`$csvPath)
}
