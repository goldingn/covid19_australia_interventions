# Discretised generation interval with truncated continuous distribution. Given
# a number of days post-infection, compute the probability of the generation
# interval having that length as the density of a truncated distribution (given
# by an R function for its cumulative density function 'cdf') on time
# over the duration of that day.
gi_probability <- function(cdf,
                           days = seq(bounds[1], bounds[2]),
                           bounds = c(0, 20)) {
  
  # days of infectiousness
  n_days <- length(days)
  # set invalid days to -1 (density 0)
  out_of_bounds <- days < bounds[1] | days > bounds[2]
  days[out_of_bounds] <- -1
  
  # get discretised probability, without accounting for truncation  
  p <- cdf(days + 1) - cdf(days)
  
  # adjust density for truncation
  upper_bound <- cdf(bounds[2] + 1)
  lower_bound <- cdf(bounds[1])
  p <- p / (upper_bound - lower_bound)
  
  p
  
}
