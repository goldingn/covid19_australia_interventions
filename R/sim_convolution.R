# simulate from the convolution model to be used for inferring Reff

# evaluate the probability mass function of a discretised lognormal distribution,
# truncated at max_days
eval_discrete_lognormal_pmf <- function(days, meanlog, sdlog, max_days) {

  lower <- plnorm(
    q = days - 1,
    meanlog = meanlog,
    sdlog = sdlog
  )

  upper <- plnorm(
    q = days,
    meanlog = meanlog,
    sdlog = sdlog
  )

  normalisation <- plnorm(
    q = max_days,
    meanlog = meanlog,
    sdlog = sdlog
  )

  pmf_unnormalised <- upper - lower
  pmf_unnormalised[days > max_days] <- 0
  pmf <- pmf_unnormalised / normalisation

  pmf

}

# constructor for discrete lognormal pmf functions
make_discrete_lognormal_pmf <- function(meanlog, sdlog, max_days = Inf) {
  function(days) {
    eval_discrete_lognormal_pmf(
      days,
      meanlog = meanlog,
      sdlog = sdlog,
      max_days = max_days
    )
  }
}

# get a matrix to use for forward convolution
get_convolution_matrix <- function(vector, mass_function) {

  # get a matrix of time differences between pairs of days
  n <- length(vector)
  day_diff <- matrix(NA, n, n)
  day_diff <- row(day_diff) - col(day_diff)

  # apply the mass function to these delays
  mass_function(day_diff)

}

# convolve a vector forward through time according to a discrete PMF of the
# delay distribution, using the matrix multiply approach
convolve <- function(vector, pmf) {

  # get convolution matrix
  gi_convolution <- get_convolution_matrix(vector, pmf)

  # do the convolution
  (gi_convolution %*% vector)[, 1]

}

# do iterative convolution to compute the expected relative incidence of new
# infections from the reff trajectory and generation interval distribution.
# Unlike the standard convolution where each infection contributes to the tally
# of events on subsequent days, in this one, the new infections on each day
# contribute to subsequent infections. So it must be solved iteratively,
# tracking the state on each day
iterate_relative_incidence_infections <- function(
  reff,
  gi_pmf,
  seed = c(1, rep(0, length(reff) - 1))
) {

  gi_convolution <- get_convolution_matrix(seed, gi_pmf)
  reff_convolution <- sweep(gi_convolution, 2, reff, FUN = "*")

  result <- matrix(NA, n_days, n_days)
  result[1, ] <- state <- seed
  for (i in seq_len(n_days)) {
    result[i, ] <- state
    state <- (reff_convolution %*% state)[, 1]
  }

  colSums(result)

} # greta.dynamics pckage should do this but currently buggy

set.seed(2022-01-14)
days <- 0:180
n_days <- length(days)

# simulate a Reff trajectory (will be unobserved)
reff <- 0.5 + exp(-1 + sin(days / 20) + cos(days / 50) + cumsum(rnorm(n_days, 0, 0.1)))

# define a discretised generation interval distribution
gi_pmf <- make_discrete_lognormal_pmf(
  meanlog = 1.375738,
  sdlog = 0.5665299,
  max_days = 21
)

# simulate predicted relative incidence
expected_infections <- iterate_relative_incidence_infections(reff, gi_pmf)

# simulate ascertainment as a function of incidence
notification_fraction <- plogis(2 + -0.3 * log(expected_infections))
# plot(notification_fraction ~ days, type = "l", ylim = c(0, 1))

# simulate expected and observed notifications
notification_pmf <- make_discrete_lognormal_pmf(
  meanlog = 1.4,
  sdlog = 0.8,
  max_days = 21
)

expected_notifications <- notification_fraction * convolve(expected_infections, notification_pmf)
observed_notifications <- rpois(n_days, expected_notifications)

# simulate expected and observed hospital admissions
hospital_admission_pmf <- make_discrete_lognormal_pmf(
  meanlog = 2.4,
  sdlog = 0.5,
  max_days = 41
)

hospitalisation_fraction <- 0.05

expected_hospital_admissions <- hospitalisation_fraction * convolve(expected_infections, hospital_admission_pmf)
observed_hospital_admissions <- rpois(n_days, expected_hospital_admissions)

# simulate expected and observed deaths
death_pmf <- make_discrete_lognormal_pmf(
  meanlog = 3,
  sdlog = 0.25,
  max_days = 61
)

death_fraction <- 0.01
expected_deaths <- death_fraction * convolve(expected_infections, death_pmf)
observed_deaths <- rpois(n_days, expected_deaths)



par(mfrow = c(1, 1))
plot(
  expected_infections ~ days,
  type = "l",
  col = grey(0.9),
  lwd = 5,
  ylim = range(observed_notifications)
)
lines(observed_notifications ~ days)
lines(observed_hospital_admissions ~ days, col = "red")
lines(observed_deaths ~ days, col = "purple")

# now model prevalences expected in prevalence surveys:

# PCR sensitivity over time since infection (not a PMF, sums to more than 1,
# since each infected person can test positive on multiple days)
pcr_sensitivity <- function(days) {
  plogis(10 + dlnorm(days, 2.1, 0.35, log = TRUE))
}
# plot(pcr_sensitivity, xlim = c(0, 45), ylim = c(0, 1))
# abline(v = 3, lty = 2)

# set a fake population
population <-  3 * sum(expected_infections)

# get the expected population pcr positivity at any one time
expected_pcr_positivity <- convolve(expected_infections, pcr_sensitivity) / population

# simulate prevalence surveys
n_prevalence_surveys <- 10
prevalence_days <- sort(sample(days, n_prevalence_surveys, prob = expected_pcr_positivity))
prevalence_samples <- round(runif(n_prevalence_surveys, 5000, 6000))
expected_prevalences <- expected_pcr_positivity[prevalence_days + 1]

observed_prevalence_positives <- rbinom(n_prevalence_surveys, prevalence_samples, expected_prevalences)
prevalence_round <- factor(seq_len(n_prevalence_surveys))

# glm was wigging out when doing prediction with standard errors, so use mgcv random effect model
prevalence_model <- mgcv::gam(
  cbind(observed_prevalence_positives, prevalence_samples - observed_prevalence_positives) ~ s(prevalence_round, bs = "re") - 1,
  family = stats::binomial
)
prevalence_estimates <- predict(prevalence_model, type = "response", se.fit = TRUE)

plot(expected_pcr_positivity ~ days, type = "l")
points(prevalence_estimates$fit ~ prevalence_days)
arrows(
  x0 = prevalence_days,
  x1 = prevalence_days,
  y0 = prevalence_estimates$fit - 1.96 * prevalence_estimates$se.fit,
  y1 = prevalence_estimates$fit + 1.96 * prevalence_estimates$se.fit,
  angle = 90,
  code = 3,
  length = 0.1
)

#
# # greta version of the iteration
# seed <- as.matrix(c(1, rep(0, n_days - 1)))
# gi_convolution <- get_convolution_matrix(seed, gi_pmf)
# reff_convolution <- sweep(gi_convolution, 2, reff, FUN = "*")
#
# library(greta.dynamics)
# iterations <- greta.dynamics::iterate_matrix(
#   matrix = reff_convolution,
#   initial_state = seed,
#   niter = 300,
#   tol = Inf
# )
# # why is this not iterating correctly?
# states <- calculate(iterations$all_states, nsim = 3)[[1]]
# dim(states)
# head(states)
# seed + colSums(states)
