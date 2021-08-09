# dplyr introduced a breaking change whereby the order of entries returned by a
# right_join was changed from the order of y to the order of x. This may work to
# reverse it.
old_right_join <- function(x, y, ...) {
  left_join(y, x, ...)
}
