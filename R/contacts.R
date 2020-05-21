# estimate the baseline numbers and durations of contacts with household members
# and non-household members in Australia using data from Rolls et al.

source("R/functions.R")

# modelled change (after/before ratio) in time at types of locations, from Google
location_change_trends <- location_change()

# reshape to state/date x location matrix
location_matrix <- location_change_trends %>%
  select(-state, -date) %>%
  as.matrix()

# prior weights on the relationship between numbers of non-houshold contacts and
# time spent in those locations, from baseline proportions of contacts in those
# locations
location_weights <- location_contacts()
location_idx <- match(colnames(location_matrix), location_weights$location)
relative_weights_prior <- location_weights[location_idx, ]$proportion_contacts

# state population weights, to relate to national survey
states <- unique(location_change_trends$state)
state_weights <- state_populations()$population / sum(state_populations()$population)

# Freya's survey results and index to dates
dates <- unique(location_change_trends$date)
freya_survey <- freya_survey_results()
survey_date_idx <- match(freya_survey$date, dates)

# model for R_t of locally-acquired infections
library(greta)

# informative priors on variables for contacts at t = 0 (Hx = household, Ox =
# non-household, Tx = total, xC = contacts. xD = duration)
baseline_contact_params <- baseline_contact_parameters()
HC_0 <- normal(baseline_contact_params$mean_contacts[1],
               baseline_contact_params$se_contacts[1])
OC_0 <- normal(baseline_contact_params$mean_contacts[2],
               baseline_contact_params$se_contacts[2])
HD_0 <- normal(baseline_contact_params$mean_duration[1],
               baseline_contact_params$se_duration[1])
OD_0 <- normal(baseline_contact_params$mean_duration[2],
               baseline_contact_params$se_duration[2])

# get HD_t in each state
h_t <- h_t_state()
HD_t <- HD_0 * h_t

# relative contribution of time in each location type to the number of
# non-household contacts
relative_weights <- dirichlet(t(relative_weights_prior))

# scaling to account for greater/lower reductions than implied by the mobility
weight_scale <- lognormal(0, 1)

# this is constrained  so that OC_t = OC_0 before distancing
OC_t_state_long <- OC_0 * (location_matrix %*% t(relative_weights)) ^ weight_scale

# reshape to time-by-state matrix
wide_dim <- c(n_distinct(location_change_trends$date),
              n_distinct(location_change_trends$state))
OC_t_state <- greta_array(OC_t_state_long, dim = wide_dim)

# compute likelihood of national survey result
OC_t <- OC_t_state %*% as.matrix(state_weights)
distribution(freya_survey$estimate) <- normal(OC_t[survey_date_idx], freya_survey$sd)

# model gamma_t: reduction in duration and transmission probability of
# non-household contacts over time
d_t <- social_distancing_national(dates)
beta <- uniform(0, 1)
gamma_t <- 1 - beta * d_t

# define prior on p: the probability of *not* transmitting per hour of contact
logit_p_params <- logit_p_prior()
logit_p <- normal(logit_p_params$meanlogit, logit_p_params$sdlogit)
p <- ilogit(logit_p)

# compute component of R_eff for local cases
household_infections <- HC_0 * (1 - p ^ HD_t)
non_household_infections <- OC_t * (1 - p ^ OD_0) * gamma_t
hourly_infections <- sweep(household_infections, 1, non_household_infections, FUN = "+")
R_t <- infectious_period() * hourly_infections

# simulate from prior over R(t) conditioned on survey data (I.e. considering
# that part of the likelihood to be a prior)
m <- model(relative_weights, p, beta, HC_0, HD_0, OC_0, OD_0)
draws <- mcmc(m)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_eff <- coda::effectiveSize(draws)
max(r_hats)
min(n_eff)

nsim <- coda::niter(draws) * coda::nchain(draws)


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

# very reduction due to micro-distancing
gamma_t_sim <- calculate(gamma_t, values = draws, nsim = nsim)[[1]]
gamma_t_mean <- apply(gamma_t_sim, 2:3, mean)
gamma_t_lower <- apply(gamma_t_sim, 2:3, quantile, 0.025)
gamma_t_upper <- apply(gamma_t_sim, 2:3, quantile, 0.975)

plot(gamma_t_mean ~ dates, type = "l", ylim = c(0, 1))
lines(gamma_t_lower ~ dates, lty = 2)
lines(gamma_t_upper ~ dates, lty = 2)

# slightly increased risk of household infections
hi_sim <- calculate(household_infections, values = draws, nsim = nsim)[[1]]
hi_mean <- apply(hi_sim, 2:3, mean)
hi_lower <- apply(hi_sim, 2:3, quantile, 0.025)
hi_upper <- apply(hi_sim, 2:3, quantile, 0.975)

plot(hi_mean[, 1] ~ dates,
     type = "n",
     ylim = range(c(hi_lower, hi_upper)),
     ylab = "household local hourly infections",
     xlab = "")
for (i in 1:8) {
  lines(hi_mean[, i] ~ dates, col = i)
  lines(hi_lower[, i] ~ dates, lty = 2, col = i)
  lines(hi_upper[, i] ~ dates, lty = 2, col = i)
}

R_t_sim <- calculate(R_t, values = draws, nsim = nsim)[[1]]
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


# In the Reff model, input HD_t, OC_t, HC_0, OD_0, duration of infectiousness,
# prior on p, and model using d_t


# to do:
# - compute change in time spent at home from Google residential in model
# - add model for change in non-houseehold contacts (use parameter for OC_0 and
#   add Freya's contact survey as observations?)
# - implement R(t) model for each state