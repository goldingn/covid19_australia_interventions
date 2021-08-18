# Estimate priors for Rob
# get bivariate normal over untransformed rates
params <- function(free) {
  
  sd <- exp(free[3])
  correl <- plogis(free[4])
  cor <- matrix(c(1, correl, correl, 1), 2, 2)
  cov_chol <- chol(cor) * sd
  
  list(
    mu = free[1:2],
    chol_sigma = cov_chol,
    E_scaling = free[5],
    I_scaling = free[6]
  )
  
}
