# build an upper-triangular circulant matrix of the number of days from one set
# of times to another, set to -999 if the difference is not supported (negative
# or exceeds max_days)
time_difference_matrix <- function (n_days) {
  
  mat <- matrix(0, n_days, n_days)
  row(mat) - col(mat)
  
}
