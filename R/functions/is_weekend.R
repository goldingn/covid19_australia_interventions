is_weekend <- function(date) {
  lubridate::wday(date, label = TRUE) %in% c("Sat", "Sun")
}
