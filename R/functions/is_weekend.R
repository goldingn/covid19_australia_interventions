is_weekend <- function(date) {
  wday(date, label = TRUE) %in% c("Sat", "Sun")
}
