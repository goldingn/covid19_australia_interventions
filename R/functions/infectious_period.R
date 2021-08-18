# mean infectious period in days
infectious_period <- function(cdf) {
  days <- 0:100
  gi_pmf <- gi_probability(cdf, days)
  infectious_days <- sum(days * gi_pmf)
  infectious_days
}
