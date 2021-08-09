add_gridlines <- function(key_dates, vertical = TRUE, horizontal = TRUE) {
  if (horizontal) {
    abline(h = 0, col = grey(0.4), lty = 3)
  }
  if (vertical) {
    abline(v = key_dates, col = grey(0.6), lwd = 2)
  }
}
