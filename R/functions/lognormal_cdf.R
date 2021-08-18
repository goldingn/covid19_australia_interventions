# greta function for the lognormal CDF (equivalent to plnorm(x, meanlog, sdlog))
# x must not be a greta array, though meanlog and sdlog can be. A greta array of
# CDF values is returned, equal to 0 where x was 0 or lower.
# lognormal_cdf <- function(x, meanlog, sdlog) {
#   
#   # filter out any invalid dates, to replace their CDF with 0
#   valid <- x > 0
#   x <- x[valid]
#   
#   p <- iprobit((log(x) - meanlog) / sdlog)
#   
#   # if any were missing, replace their cumulative density with 0
#   if (any(!valid)) {
#     result <- zeros(length(valid))
#     result[valid] <- p
#     p <- result
#   }
#   
#   p
#   
# }
lognormal_cdf <- function(x, meanlog, sdlog) {
  
  op("lognormal_cdf",
     x,
     meanlog,
     sdlog,
     tf_operation = "tf_lognormal_cdf",
     dim = dim(x))
  
}
