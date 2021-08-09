gaussian_smooth <- function (values, sd = 1, ...) {
  id <- seq_along(values)
  middle <- round(mean(id))
  diff <- id - middle
  weights <- exp(-0.5 * (diff / sd) ^ 2)
  weights <- weights / sum(weights)
  weighted_mean(values, weights, ...)
}
