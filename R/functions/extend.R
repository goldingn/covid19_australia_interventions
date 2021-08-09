# extend (or truncate, or optionally clamp) values in rows of a matrix
# greta_array. I.e. given a matrix `x`, return another matrix with `n_rows` rows
# (by default the same as `x`), and with rows after `clamp_from` taking the
# value of row `clamp_from`. This can be used to extend a matrix, propagating
# the last value (if `n_rows` is increased), shorten a matrix (if n_rows is
# decreased), and simultaneously clamp subsequent values in the matrix at a
# fixed value.
extend <- function(x, n_rows = nrow(x), clamp_from = nrow(x)) {
  if (clamp_from > nrow(x)) {
    stop ("clamp_from must not be higher than the umber of rows in the matrix",
          call. = FALSE)
  }
  index <- seq_len(n_rows)
  clamped_index <- pmin(index, clamp_from)
  x[clamped_index, ]
}
