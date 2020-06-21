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

# simulate rates from this (without further cholesky decomposition)
sim_rates <- function(params, n = 1e5) {
  
  # simulate log rates from a multivariate normal
  z <- matrix(rnorm(n * 2), n, 2)
  log_rates <- sweep(z %*% params$chol_sigma, 1, params$mu, FUN = "+")
  z_E <- log_rates[, 1]
  z_I <- log_rates[, 2]
  
  E_rate <- exp(z_E + params$E_scaling * z_E ^ 2)
  I_rate <- exp(z_I + params$I_scaling * z_I ^ 2)
  
  # pull out rates for each simulation
  list(
    E_rate = E_rate,
    I_rate = I_rate
  )
  
}

# simulate time to an infection from these rates
sim_times <- function(rate_sims, n = 1e5) {
  
  # simulate exponential E and I durations
  Es <- matrix(rexp(n * 2, rate_sims$E_rate), ncol = 2)
  Is <- matrix(rexp(n * 2, rate_sims$I_rate), ncol = 2)
  
  # get the infectious period start and end dates
  starts <- rowSums(Es)
  durations <- rowSums(Is) 
  ends <- starts + durations
  times <- runif(n, starts, ends)
  times
  
}

# simulate everything from the unconstrained parameters
sim <- function(par, n = 1e5) {
  
  params <- params(par)
  rate_sims <- sim_rates(params, n)
  time_sims <- sim_times(rate_sims, n)

  # return log total durations
  log(time_sims)
}

objective <- function(par) {
  
  log_time_sims <- sim(par)
  
  # compute log-likelihood across these simulations
  -sum(dnorm(log_time_sims, meanlog, sdlog, log = TRUE))
  
}

# handle bad Cholesky factorizations
safe_objective <- function(par) {
  tryCatch(
    objective(par),
    error = function(e) { Inf }
  )
}

# target SI parameters
# source("R/functions.R")
# nishiura <- nishiura_samples()
# meanlog <- mean(nishiura$param1)
# sdlog <- mean(nishiura$param2)
meanlog <- 1.3757381523
sdlog <- 0.5665298555

# find parameters
set.seed(2020-06-20)
o <- optim(rep(0, 6),
           safe_objective,
           method = "BFGS")
o$convergence

# unpack the parameters
param <- params(o$par)
mu <- param$mu
chol_sigma <- param$chol_sigma
Sigma <- t(chol_sigma) %*% chol_sigma

# simulate from these mean of these parameters to check solution
rates <- sim_rates(param, 1e4)
SI_sims <- sim_times(rates, 1e4)
SI <- rlnorm(1e4, meanlog, sdlog)

par(mfrow = c(2, 1))
hist(SI_sims, xlim = c(0, 35), breaks = 100)
hist(SI, xlim = c(0, 35), breaks = 100)

summary(SI_sims)
summary(SI)

# do the same with just the mean
rates <- list(E_rate = exp(param$mu[1] + param$E_scaling * param$mu[1]),
              I_rate = exp(param$mu[2] + param$I_scaling * param$mu[2]))
              
SI_sims <- sim_times(rates, 1e4)

par(mfrow = c(2, 1))
hist(SI_sims, xlim = c(0, 35), breaks = 100)
hist(SI, xlim = c(0, 35), breaks = 100)

summary(SI_sims)
summary(SI)

dput(Sigma)


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

