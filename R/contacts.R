# sample from the prior(ish) over R_t due to distancing

source("R/functions.R")
dates <- seq(as.Date("2020-01-08"), as.Date("2020-05-11"), by = 1)
distancing_effect <- distancing_effect_model(dates)
R_eff_local_1 <- distancing_effect$R_t

# simulate from prior over R(t) conditioned on survey data (I.e. considering
# that part of the likelihood to be a prior)
m <- model(R_eff_local_1)
draws <- mcmc(m)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_eff <- coda::effectiveSize(draws)
max(r_hats)
min(n_eff)

nsim <- coda::niter(draws) * coda::nchain(draws)

OC_t <- distancing_effect$OC_t
dates <- distancing_effect$dates
freya_survey <- freya_survey_results()

# plot national OC_t
OC_t_sim <- calculate(OC_t, values = draws, nsim = nsim)[[1]]
OC_t_mean <- apply(OC_t_sim, 2:3, mean)
OC_t_lower <- apply(OC_t_sim, 2:3, quantile, 0.025)
OC_t_upper <- apply(OC_t_sim, 2:3, quantile, 0.975)

plot(OC_t_mean ~ dates, type = "l", ylim = c(0, 12))
lines(OC_t_lower ~ dates, lty = 2)
lines(OC_t_upper ~ dates, lty = 2)
points(freya_survey$estimate ~ freya_survey$date)

# calculate state-level OC_t
OC_t_state <- distancing_effect$OC_t_state
OC_t_state_sim <- calculate(OC_t_state, values = draws, nsim = nsim)[[1]]
OC_t_state_mean <- apply(OC_t_state_sim, 2:3, mean)
OC_t_state_sd <- apply(OC_t_state_sim, 2:3, sd)

plot(OC_t_state_mean[, 1] ~ dates,
     type = "n",
     ylim = c(0, 12),
     ylab = "OC_t",
     xlab = "")
for (i in 1:8) {
  lines(OC_t_state_mean[, i] ~ dates, col = i)
  lines(OC_t_state_mean[, i] + 1.96 * OC_t_state_sd[, i] ~ dates, lty = 2, col = i)
  lines(OC_t_state_mean[, i] - 1.96 * OC_t_state_sd[, i] ~ dates, lty = 2, col = i)
}

# very uncertain reduction due to micro-distancing
gamma_t <- distancing_effect$gamma_t
gamma_t_sim <- calculate(gamma_t, values = draws, nsim = nsim)[[1]]
gamma_t_mean <- apply(gamma_t_sim, 2:3, mean)
gamma_t_lower <- apply(gamma_t_sim, 2:3, quantile, 0.025)
gamma_t_upper <- apply(gamma_t_sim, 2:3, quantile, 0.975)

plot(gamma_t_mean ~ dates, type = "l", ylim = c(0, 1))
lines(gamma_t_lower ~ dates, lty = 2)
lines(gamma_t_upper ~ dates, lty = 2)

R_t_sim <- calculate(R_eff_local_1, values = draws, nsim = nsim)[[1]]
R_t_mean <- apply(R_t_sim, 2:3, mean)
R_t_lower <- apply(R_t_sim, 2:3, quantile, 0.025)
R_t_upper <- apply(R_t_sim, 2:3, quantile, 0.975)

plot(R_t_mean[, 1] ~ dates,
     type = "n",
     ylim = c(0, 3),
     ylab = "R_t",
     xlab = "")
for (i in 1:8) {
  lines(R_t_mean[, i] ~ dates, col = i)
  lines(R_t_lower[, i] ~ dates, lty = 2, col = i)
  lines(R_t_upper[, i] ~ dates, lty = 2, col = i)
}
abline(h = 1, lty = 3)

# compute counterfactuals of Reff with only reduced macrodistancing and only
# reduced microdistancing
infectious_days <- infectious_period()

h_t <- h_t_state(dates)
HD_t <- HD_0 * h_t
household_infections_macro <- HC_0 * (1 - p ^ HD_t)
non_household_infections_macro <- OC_t_state * infectious_days * (1 - p ^ OD_0)
hourly_infections_macro <- sweep(household_infections_macro, 1, non_household_infections_macro, FUN = "+")
R_t_macro <- infectious_period() * hourly_infections_macro

household_infections_micro <- HC_0 * (1 - p ^ HD_0)
non_household_infections_micro <- OC_0 * infectious_days * (1 - p ^ OD_0) * gamma_t
hourly_infections_micro <- sweep(household_infections_micro, 1, non_household_infections_micro, FUN = "+")
R_t_micro <- infectious_period() * hourly_infections_micro

