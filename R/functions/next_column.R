# move columns of this matrix one to the left, and replace the right-most column
# with 1s. Used to define a weights matrix for a piecewise linear curve.
next_column <- function(x) {
  cbind(x[, -1], rep(1, nrow(x)))
}
