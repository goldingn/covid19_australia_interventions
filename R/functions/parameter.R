# Given samples 'x' of a parameter, approximate the distribution by a (possibly
# truncated) normal distribution, and return a variable greta array following
# that distribution
parameter <- function(x, truncation = c(-Inf, Inf)) {
  normal(mean(x), sd(x), truncation = truncation)
}
