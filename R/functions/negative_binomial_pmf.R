# probability mass function of a negative binomial distribution
negative_binomial_pmf <- function(x, size, prob) {
  
  op("negative_binomial_pmf",
     x,
     size,
     prob,
     tf_operation = "tf_negative_binomial_pmf",
     dim = dim(x))
  
}
