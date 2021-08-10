source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)

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
