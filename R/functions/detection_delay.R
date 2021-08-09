# simulate n values of days to detection
detection_delay <- function(region, new_infections, probs) {
  n <- new_infections[region]
  result <- NULL
  if (n > 0) {
    count <- rmultinom(1, n, probs)
    pos <- which(count > 0)
    result <- cbind(delay = pos - 1,
                    region = region,
                    count = count[pos])
  }
  result
}
