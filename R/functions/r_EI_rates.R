r_EI_rates <- function(n) {
  
  mu <- c(0.128560822315797, -0.454247406858292)
  Sigma <- structure(c(0.514168567117846, 0.231332006712942, 0.231332006712942, 
                       0.514168567117846), .Dim = c(2L, 2L))
  scaling <- c(0.307385478370485, 0.286635005095472)
  z <- MASS::mvrnorm(n, mu, Sigma)
  E <- exp(z[, 1] + scaling[1] * z[, 1] ^ 2)
  I <- exp(z[, 2] + scaling[2] * z[, 2] ^ 2)

  cbind(E, I)  

}
