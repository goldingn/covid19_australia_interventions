# the serial interval of Nishiura et al.
nishiura_cdf <- function() {
  
  # generation interval distribution; use SI distribution from Nishiura et al.
  nishiura <- nishiura_samples()
  meanlog <- mean(nishiura$param1)
  sdlog <- mean(nishiura$param2)
  
  gi_cdf <- function(days) {
    plnorm(days, meanlog, sdlog)
  }
  
  gi_cdf
  
}
