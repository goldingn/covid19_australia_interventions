# the serial interval distribution of Bi et al. (used in Impeerial Report 13 /
# Flaxman et al.)
bi_cdf <- function() {
  
  gi_cdf <- function(days) {
    pgamma(days, 2.29, 0.36)
  }
  
  gi_cdf
  
}
