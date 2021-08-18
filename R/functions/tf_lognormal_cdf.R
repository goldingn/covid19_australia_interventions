# tensorflow function for the CDF of a lognormal distribution
tf_lognormal_cdf <- function(x, meanlog, sdlog) {
  
  d <- tfp$distributions$LogNormal(meanlog, sdlog)
  
  # This does not support zero or negative values (returns NA instead of 0), so
  # we need to avoid those. We also can't compute the CDF on negative values
  # then replace the bad values, since that breaks the gradients
  supported <- tf$greater(x, greta:::fl(0))
  ones <- tf$ones_like(x)
  zeros <- tf$zeros_like(x)
  
  x_clean <- tf$where(supported, x, ones)
  cdf_clean <- d$cdf(x_clean)
  mask <- tf$where(supported, ones, zeros)
  cdf_clean * mask
  
}
