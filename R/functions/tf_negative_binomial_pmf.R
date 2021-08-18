tf_negative_binomial_pmf <- function(x, size, prob) {
  
  
  d <- tfp$distributions$NegativeBinomial(
    total_count = size,
    probs = greta:::fl(1) - prob)
  
  d$prob(x)
  
}
