weighted_se <- function(x, mu, w, na.rm = TRUE) {
  x_sims <- replicate(10000,
                      sample(x, length(x), replace = TRUE),
                      simplify = FALSE)
  means <- vapply(x_sims,
                  weighted_mean,
                  w,
                  na.rm = na.rm,
                  FUN.VALUE = numeric(1))
  sd(means)
}
