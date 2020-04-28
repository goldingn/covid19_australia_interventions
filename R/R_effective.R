# fit a Bayesian model-based estimate of R_effective over time.

# The model assumes that the number of new locally-acquired cases at time $t$ in
# region $i$ is *conditionally* Poisson-distributed with expectation given by
# the number of infectious people $lambda_{t,i}$ multiplied by the region- and
# time-varying reproduction rate $R_{t,i}$. To account for clusters in numbers
# of cases reported (overdispersion in the data), and fluctuations due to
# unobserved factors affecting transmission (temporal correlation) $R_{t,i}$ has
# a random error structure - either temporally-correlated, temporally
# independent, or some mix of both. This can be expressed via a Gaussian process
# over time in each region on $log(R_{t,i})$. To account for temporal
# fluctations common to all regions, we also include a single temporal Gaussian
# process on $log(\hat{R}_{t})$, where $\hat{R}_{t}$ is the Australia-wide mean
# time-varying effective reproduction rate. Fixed effects for all regions can
# also be included via a linear mean function $m_{t,i}$ for these
# region-specific Gaussian processes. The number of infectious people is
# computed by disaggregating the number of cases (both locally-and
# overseas-acquired) over a subsequent period of time, with probability
# distribution given by a serial interval distribution. The serial interval
# distribution is computed from a linelist of cases, and is represented by a
# discretized cumulative density function $p-serial-interval(t')$, giving the
# probability of a serial interval (average time between subsequent cases) of
# $t'$. The relative contribution of imported versus locally-acquired cases to
# subsequent infections in each region can be accounted for via a time-varying
# contribution rate $import-contribution_{t'}$, which reflects the effectiveness
# of quarantine measures.

#   local-incidence_{t,i} ~ Poisson(lambda_{t,i} * R_{t,i})
#   log(R_{t,i}) ~ GP(m_{t,i}, K) + log(\hat{R}_{t})
#   log(\hat{R}_{t}) ~ GP(0, K)
#   m_{t,i} = a + b * social-distancing-index_{t,i}
#   lambda_{t,i} = sum_{t'=1}^t p-serial-interval(t') (local-incidence_{t',i} + imported-incidence_{t',i} import-contribution_{t'})

# - load in Freya's data
# - get serial interval distribution
# - get case count time series by locals and imports
# - write out model for one region
# - compare fit with David's outputs
# - extend to multiple regions.

library(dplyr)
library(readr)
library(tidyr)
source("R/functions.R")

# return a fake linelist based on the real case counts and samples of the
# reporting delay distribution
linelist <- fake_linelist()

# for now just do single imputation with the mean delay on the date of onset for
# those missing it
delay_samples <- read_csv(
  file = "data/cases/sampled_report_delay.csv",
  col_types = cols(x = col_integer())
)
mean_delay <- round(mean(delay_samples$x))

linelist <- linelist %>%
  mutate(
    date_onset = case_when(
      is.na(date_onset) ~ date_confirmation - mean_delay,
      TRUE ~date_onset
    )
  )

# Later marginalise over the reporting rate properly:
#  - Get the counts of cases with known onsets by onset date, split by local and
#  overseas
#  - Get the continuously disaggregated cases without known onsets, by onset
#  date, split by local and overseas.
# - sum these when computing lambda_ti
# - when defining the likelihood, marginalise over the sum of the known onsets
# and probabilistic unknown onsets

# build date-by-state matrices of the counts of new local and impiorted cases and
# imports by date of onset
linelist <- linelist %>%
  rename(state = region,
         date = date_onset) %>%
  select(-date_confirmation)

import_statuses <- sort(unique(linelist$import_status))
states <- sort(unique(linelist$state))
dates <- seq(min(linelist$date), max(linelist$date), by = 1)

# pad this with full set of dates, states, and import statuses
grid <- expand_grid(
  date = dates,
  import_status = import_statuses,
  state = states)

# widen into matrices of date by state
date_by_state <- linelist %>%
  mutate(cases = 1) %>%
  right_join(grid) %>%
  group_by(import_status, state, date) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = state, values_from = cases) %>%
  select(-date)

imported_cases <- date_by_state %>%
  filter(import_status == "imported") %>%
  select(-import_status) %>%
  as.matrix()

local_cases <- date_by_state %>%
  filter(import_status == "local") %>%
  select(-import_status) %>%
  as.matrix()


library(greta.gp)

idx <- which(states == "VIC")

# summarise nationally to start with
local_cases <- local_cases[, idx, drop = FALSE]
imported_cases <- imported_cases[, idx, drop = FALSE]

# the relative contribution of imported cases (relative to locally-acquired
# cases) to transmission. I.e. one minus the effectiveness of quarantine
# measures.
import_contribution <- 1

# disaggregate imported and local cases according to these probabilities to get
# the expected number of infectious people in each state and time
n_dates <- length(dates)

serial_interval_disaggregation <- disaggregation_matrix(n_dates,
                                                        serial_interval_probability,
                                                        max_days = 46)

case_contribution <- local_cases + imported_cases * import_contribution

lambda <- serial_interval_disaggregation %*% case_contribution

# define a GP on dates, using subset of regressors approximation
lengthscale <- lognormal(4, 0.5)
sigma <- lognormal(-1, 1)
temporal_kernel <- rbf(lengthscale, sigma)
intercept_kernel <- bias(0.5)
kernel <- intercept_kernel + temporal_kernel

n_dates <- length(dates)
date_nums <- seq_len(n_dates)
n_inducing <- 5
inducing_points <- seq(1, n_dates, length.out = n_inducing + 1)[-1]

log_R_eff <- gp(date_nums, kernel, inducing = inducing_points)

# combine with lambda on log scale for numerical stability - since greta can use
# this in evaluating the poisson likelihood
# log_expected_infections <- sweep(log(lambda), 1, log_R_eff, FUN = "+") 
log_expected_infections <- log(lambda) + log_R_eff 
expected_infections <- exp(log_expected_infections)
distribution(local_cases) <- poisson(expected_infections)

m <- model(log_R_eff)

draws <- mcmc(m, one_by_one = TRUE)

r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_eff <- coda::effectiveSize(draws)
max(r_hats)
min(n_eff)

R_eff <- exp(log_R_eff)

R_eff_draws <- calculate(R_eff, values = draws)
R_eff_mat <- as.matrix(R_eff_draws)
mean <- colMeans(R_eff_mat)
ci <- apply(R_eff_mat, 2, quantile, c(0.025, 0.975))

plot(mean ~ dates,
     type = "n",
     ylim = range(c(ci, 0, 1)))
polygon(x = c(dates, rev(dates)),
        y = c(ci[1, ], rev(ci[2, ])),
        lty = 0,
        col = "lightskyblue1")
lines(mean ~ dates,
      col = "blue",
      lwd = 2)
