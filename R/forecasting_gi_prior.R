# Estimate priors for Rob

# get bivariate normal over log rates
params <- function(free) {
  
  sd <- exp(free[2])
  correl <- plogis(free[3])
  cor <- matrix(c(1, correl, correl, 1), 2, 2)
  cov_chol <- chol(cor) * sd
  
  list(
    mu = rep(free[1], 2),
    chol_sigma = cov_chol
  )
  
}

# simulate rates from this (without further cholesky decomposition)
sim_rates <- function(params, n = 1e5) {
  
  # simulate log rates from a multivariate normal
  z <- matrix(rnorm(n * 2), n, 2)
  log_rates <- sweep(z %*% params$chol_sigma, 1, params$mu, FUN = "+")
  
  # pull out rates for each simulation
  list(
    E_rate = exp(log_rates[, 1]),
    I_rate = exp(log_rates[, 2])
  )
  
}

# simualte durations from these rates
sim_durations <- function(rate_sims, n = 1e5) {
  
  # simulate exponential E and I durations
  Es <- matrix(rexp(n * 2, rate_sims$E_rate), ncol = 2)
  Is <- matrix(rexp(n * 2, rate_sims$I_rate), ncol = 2)
  durations <- rowSums(cbind(Es, Is))
  durations
  
}

# simulate everything from the unconstrained parameters
sim <- function(par, n = 1e5) {
  
  params <- params(par)
  rate_sims <- sim_rates(params, n)
  duration_sims <- sim_durations(rate_sims, n)

  # return log total durations
  log(duration_sims)
}

objective <- function(par) {
  
  log_SI_sims <- sim(par)
  
  # compute log-likelihood across these simulations
  -sum(dnorm(log_SI_sims, meanlog, sdlog, log = TRUE))
  
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
o <- optim(rep(0, 3),
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
SI_sims <- sim_durations(rates, 1e4)
SI <- rlnorm(1e4, meanlog, sdlog)

par(mfrow = c(2, 1))
hist(SI_sims, xlim = c(0, 35), breaks = 100)
hist(SI, xlim = c(0, 35), breaks = 100)

summary(SI_sims)
summary(SI)

# do the same with just the mean
rates <- list(E_rate = exp(param$mu[1]),
              I_rate = exp(param$mu[2]))
              
SI_sims <- sim_durations(rates, 1e4)

par(mfrow = c(2, 1))
hist(SI_sims, xlim = c(0, 35), breaks = 100)
hist(SI, xlim = c(0, 35), breaks = 100)

summary(SI_sims)
summary(SI)


r_EI_rates <- function(n) {
  mu <- c(-0.130909145063366, -0.130909145063366)
  Sigma <- structure(c(0.00915907803127536, 0.000292405830667, 0.000292405830667, 
                       0.00915907803127536), .Dim = c(2L, 2L))
  log_rates <- MASS::mvrnorm(n, mu, Sigma)
  exp(log_rates)
}

