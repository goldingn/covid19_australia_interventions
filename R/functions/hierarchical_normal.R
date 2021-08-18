# define a zero-mean hierarchical normal prior over a vector of length n
hierarchical_normal <- function(n, index = NULL, mean_sd = 10, sd_sd = 0.5) {
  
  indexed <- !is.null(index)
  hyper_n <- ifelse(indexed, max(index), 1)
  
  mean <- normal(0, mean_sd, dim = hyper_n)
  sd <- normal(0, sd_sd, truncation = c(0, Inf), dim = hyper_n)
  raw <- normal(0, 1, dim = n)
  
  if (indexed) {
    mean <- mean[index]
    sd <- sd[index]
  }
  
  mean + raw * sd
  
}
