# model the changing times to case detection throughout the first wave

source("R/functions.R")
set.seed(2020-06-10)

linelist <- readRDS("~/not_synced/nnds/linelist_formatted.RDS")

# make the detection date the earliest of peciment conllection or notification
detection <- linelist %>%
  # remove any cases where first specimen collection is being reported as after
  # case confirmation (data entry error with dates, so detection date could be
  # erroneous)
  filter(
    date_detection <= date_confirmation
  ) %>%
  mutate(
    date_infection = date_onset - 5,
    days_to_detection = date_detection - date_onset,
    days_to_detection = as.numeric(days_to_detection)
  ) %>%
  filter(days_to_detection >= -12) %>%
  # also consider the number tested 1 or 2+ days prior to symptom onset
  mutate(days_to_detection = pmax(days_to_detection, -2)) %>%
  na.omit()

# for those with non-negative ttd, model the distribution of days.
nonneg <- detection %>%
  filter(days_to_detection >= 0)

# start with a Poisson distribution, then check fit and consider NB

# model the log rate with a linear model

# create a date-by-state matrix of lambda
dates <- seq(min(nonneg$date_infection), max(nonneg$date_infection), by = 1)
states <- sort(unique(nonneg$region))
n_dates <- length(dates)
n_states <- length(states)
zeros <- matrix(0, n_dates, n_states)
days <- row(zeros)

# model test positivity in VIC to get shape of a log-logit to match the time to detection curve
cases <- linelist %>%
  select(date = date_confirmation,
         state = region) %>%
  group_by(date, state) %>%
  summarise(count = n())

rolling_average <- function(x, n = 5) {
  stats::filter(x, rep(1 / n, n), sides = 2)
}

# estimate the number of tests per state per day, and add positive tests
tests <- get_tests() %>%
  left_join(cases) %>%
  replace_na(list(count = 0)) %>%
  mutate(daily_tests = rolling_average(daily_tests, 14)) %>%
  na.omit()

# tests %>%
#   mutate(positivity = count / daily_tests) %>%
#   ggplot() +
#   aes(date, 1 / positivity) +
#   facet_wrap(~state, ncol = 2, scales = "free_y") +
#   geom_line()

# the test positivity rate in VIC (which had lots of cases and still has cases)
# appeared to peak around the start of April, and dropped to a plateau around
# the end of May. Define a logit that broadly matches this.

# shape of the shared sigmoid curve
location <- 70 + normal(0, 10)  # hierarchical_normal(n_states)
slope <- normal(0, 1, truncation = c(-Inf, 0)) # hierarchical_normal(n_states)

# relationship between the sigmoid curve and log mean time to detection
intercept_ttd <- exp(normal(0, 10)) # exp(hierarchical_normal(n_states))
slope_ttd <- exp(normal(0, 10)) # exp(hierarchical_normal(n_states))

# relationship between the sigmoid curve and logit probbaility of test
# positivity
intercept_test <- normal(0, 10)
slope_test <- normal(0, 10)

# shared sigmoid curve
sigmoid <- ilogit(slope * (days - location))

# logit probability of a positive test
p_positive <- ilogit(intercept_test + slope_test * sigmoid)

# expected mean time to detection
mean_ttd <- exp(intercept_ttd + slope_ttd * sigmoid)

# test positivity likelihood
test_row_idx <- match(tests$date, dates)
test_col_idx <- match(tests$state, states)
test_idx <- cbind(test_row_idx, test_col_idx)
distribution(tests$count) <- binomial(tests$daily_tests, p_positive[test_idx])

# time to detection likelihood
sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf))
size <- 1 / sqrt(sqrt_inv_size)
prob <- 1 / (1 + mean_ttd / size)
ttd_row_idx <- match(nonneg$date_infection, dates)
ttd_col_idx <- match(nonneg$region, states)
ttd_idx <- cbind(ttd_row_idx, ttd_col_idx)
distribution(nonneg$days_to_detection) <- negative_binomial(size,
                                                            prob[ttd_idx])

m <- model(location, slope,
           intercept_test, slope_test,
           intercept_ttd, slope_ttd)

draws <- mcmc(m)
convergence(draws)

nsim <- coda::niter(draws) * coda::nchain(draws)
nsim <- min(10000, nsim)

# posterior predictive checks
ttd_dist <- negative_binomial(size, prob)
test_dist <- binomial(tests$daily_tests, p_positive[test_idx])
sims <- calculate(ttd_dist[ttd_idx], test_dist, values = draws, nsim = nsim)
ttd_sim <- sims[[1]][, , 1]
test_sim <- sims[[2]][, , 1]

# overall PPC check
bayesplot::ppc_ecdf_overlay(
  nonneg$days_to_detection,
  ttd_sim,
  discrete = TRUE
)

bayesplot::ppc_ecdf_overlay(
  tests$count,
  test_sim,
  discrete = TRUE
)


# plot model fit by date and state
ttd_samples <- calculate(c(ttd_dist), values = draws, nsim = nsim)[[1]]

nonneg_plot <- nonneg %>%
  mutate(
    state = region,
    type = "Nowcast"
  ) %>%
  filter(date_infection > as.Date("2020-03-01"))

p <- plot_trend(ttd_samples,
                dates,
                base_colour = yellow,
                multistate = TRUE,
                hline_at = NULL,
                ylim = range(nonneg$days_to_detection),
                min_date = as.Date("2020-03-01")) +
  
  geom_point(
    aes(date_infection, jitter(days_to_detection)),
    data = nonneg_plot,
    size = 0.25,
    alpha = 0.1
  )

p


# plot model fit by date and state

positive_samples <- calculate(c(p_positive), values = draws, nsim = nsim)[[1]]

tests_plot <- tests %>%
  mutate(mean = count / daily_tests,
         type = "Nowcast") %>%
  filter(date > as.Date("2020-03-01"))
  

p <- plot_trend(positive_samples,
                dates,
                base_colour = yellow,
                multistate = TRUE,
                hline_at = NULL,
                ylim = c(0, 0.02),
                min_date = as.Date("2020-02-01")) +
  
  geom_point(
    data = tests_plot,
    size = 0.25,
    alpha = 0.1
  )

p

