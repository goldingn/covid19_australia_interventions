# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

# The basic model assumes that the number of new locally-acquired cases
# $N_{ti}$ at time $t$ in region $i$ is *conditionally* Poisson-distributed
# with expectation $lambda_{ti}$ given by the product of the number of
# infectious cases $I_{ti}$ and the time-varying reproduction rate $R_{ti}$:

#   N_{ti} &\sim Poisson(lambda_{ti})
#   lambda_{ti} &= I_{ti} R_{ti}

# We extend this model by considering separate reproduction rates for two groups
# of infectious cases in order to model the effects of different interventions
# targetted at each group: those with locally-acquired cases $I_{ti}^L$, and
# those with overseas acquired cases $I_{ti}^O$, with corresponding reproduction
# rates $R_{ti}^L$ and $R_{ti}^O$:

#   lambda_{ti} &= I_{ti}^L R_{ti}^L + I_{ti}^O R_{ti}^O
#   R_{ti}^L &= R_0 D_t e^{\epsilon_{ti}^L}
#   R_{ti}^O &= R_0 Q_t e^{\epsilon_{ti}^O}

# where each of these reproduction rates is modelled as a product of: the
# reproduction rate under initial conditions and no interventions $R_0$;
# deterministic functions $D_t$ and $Q_t$ that modify $R_0$ over time to
# respectively represent the impacts of physical distancing and quarantine
# interventions at a national level, and correlated timeseries of random effects
# $\epsilon_{ti}^L$ and $\epsilon_{ti}^O$ to represent stochastic fluctuations
# in the reporting rate in each state, for example due to the clusters in
# subpopulations with higher or lower reproduction rates than the general
# population.

# (Not sure about this next bit of the model. It's a bit icky.)

# Whilst we would expect stochastic processes to affect both reproduction rates,
# these two separate stochastic processes are in practice only poorly identified
# apart. We therefore instead model a single combined stochastic process
# $\epsilon_{ti}$, which can be interpreted as a weighted average of these two
# processes, with weights giveen by the porportions of all infectious cases
# belonging to each group:

#  lambda_{ti} &= R_0 (I_{ti}^L D_t + I_{ti}^O Q_t) e^{\epsilon_{ti}}

# We model the effect of $D_t$ as being proportional (on the log scale) to an index of the
# proportional change in population mobility in response to physical distancing
# measures $d_t$, which has initial value 0 before distancing measures were
# implemented and value 1 at its maximum extent:

#   D_t &= e^{\beta d_t}

# We model $Q_t$ via a monotone decreasing step functio with values constrained
# to the unit interval, and with steps at the dates of changes in quarantine policy.

# where respectively encode the impact of social distancing and
# of Quarantine policies on the reproduction rates of locally- and
# overseas-acquired cases respectively

# time-varying reproduction rate $R_{ti}$. To account for clusters in numbers
# of cases reported (overdispersion in the data), and fluctuations due to
# unobserved factors affecting transmission (temporal correlation) $R_{ti}$ has
# a random error structure - either temporally-correlated, temporally
# independent, or some mix of both. This can be expressed via a Gaussian process
# over time in each region on $log(R_{ti})$. To account for temporal
# fluctations common to all regions, we also include a single temporal Gaussian
# process on $log(\hat{R}_{t})$, where $\hat{R}_{t}$ is the Australia-wide mean
# time-varying effective reproduction rate. Fixed effects for all regions can
# also be included via a linear mean function $m_{ti}$ for these
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

#   local-incidence_{ti} ~ Poisson(lambda_{ti} * R_{ti})
#   log(R_{ti}) = log(R_0) + b * social-distancing-index_{ti} + \epsilon_{ti}
#   epsilon_i ~ GP(0, K)
#   lambda_{ti} = sum_{t'=1}^t p-serial-interval(t') (local-incidence_{t',i} + imported-incidence_{t',i} import-contribution_{t'})
#   log(R_0) ~ N(0.723, 0.465)

# this lognormal prior on R0 has a mean of 2.6 and sd of 2

library(dplyr)
library(readr)
library(tidyr)
library(RColorBrewer)
source("R/functions.R")

# return a fake linelist based on the real case counts and samples of the
# reporting delay distribution
set.seed(2020-04-29)
linelist <- readRDS("~/not_synced/nnds/linelist_formatted.RDS")

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

# build date-by-state matrices of the counts of new local and imported cases and
# imports by assumed date of infection (with an incubation period of 5 days)
linelist <- linelist %>%
  rename(state = region,
         date = date_onset) %>%
  mutate(date = date - 5) %>%
  select(-date_confirmation)

import_statuses <- sort(unique(linelist$import_status))
states <- sort(unique(linelist$state))
dates <- seq(min(linelist$date), max(linelist$date), by = 1)

n_states <- length(states)
n_dates <- length(dates)
date_nums <- seq_len(n_dates)

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

# load the social distancing factor
distancing_file <- "outputs/social_distancing_latent.RDS"
social_distancing_index <- distancing_file %>%
  readRDS() %>%
  select(mean, date) %>%
  right_join(tibble(date = dates)) %>%
  replace_na(list(mean = 0)) %>%
  pull(mean)
  
library(greta.gp)

# lognormal prior on R0 (normal prior on log(R0)) based on estimates for
# Northern Europe from Flaxman et al. (Imperial report 13, lowest R0s from their
# sensitivity analysis)
R0_europe <- R0_prior()
log_R0 <- normal(R0_europe$meanlog, R0_europe$sdlog)

# the reduction from R0 down to R_eff for imported cases due to different
# quarantine measures each measure applied during a different period. Q_t is
# R_eff_t / R0 for each time t, modelled as a monotone decreasing step function
# over three periods with increasingly strict policies
quarantine_dates <- as.Date(c("2020-03-15", "2020-03-28"))

q_index <- case_when(
  dates < quarantine_dates[1] ~ 1,
  dates < quarantine_dates[2] ~ 2,
  TRUE ~ 3,
)

q_index_vec <- rep(q_index, n_states)
# q_raw <- uniform(0, 1, dim = 3)
# q <- cumprod(q_raw)
# log_q <- log(q)
log_q_raw <- -exponential(1, dim = 3)
log_q <- cumsum(log_q_raw)
log_Qt_vec <- log_q[q_index_vec]
# log_Qt <- log_q[q_index]

# the reduction from R0 down to R_eff for locally-acquired cases due to social
# distancing behaviour, modelled as a being proportional to the change in
# mobility measured from the mobility datastreams
beta <- normal(0, 1)
log_Dt_vec <- beta * rep(social_distancing_index, n_states)
# log_Dt <- beta * social_distancing_index

# temporally correlated errors in R_eff for local cases - representing all the
# stochastic transmission dynamics in the community, such as outbreaks in
# communities with higher or lower tranmission rates.

# variation in R_eff for locally-acquired cases over time
alpha_time <- lognormal(2, 0.5)
lengthscale_time <- lognormal(4, 0.5)
sigma_time <- normal(0, 0.5, truncation = c(0, Inf))

time_kernel <- rational_quadratic(
  lengthscales = lengthscale_time,
  variance = sigma_time ^ 2,
  alpha = alpha_time,
  columns = 1)

# variation in this timeseries between the states and territories
sigma_state <- normal(0, 0.5, truncation = c(0, Inf))
state_variation_kernel <- iid(sigma_state ^ 2, columns = 2)

# IID noise in log(R_eff) per day (overdispersion, or case clustering)
sigma_noise <- normal(0, 0.5, truncation = c(0, Inf))
noise_kernel <- white(sigma_noise ^ 2)

kernel <- noise_kernel + time_kernel * state_variation_kernel

# data for the GP to act on
X <- cbind(date = rep(date_nums, n_states),
           state = rep(seq_len(n_states), each = n_dates))

# build a matrix of inducing points, regularly spaced over time but with one on
# the most recent date
inducing_date_nums <- seq(n_dates, 1, by = -5)
inducing_index <- which(X[, 1] %in% inducing_date_nums)
X_inducing <- X[inducing_index, ]

# evaluate GP for epsilon
epsilon <- gp(X, kernel, inducing = X_inducing, tol = 0)

# combine these components
# N ~ Poisson(R0 * exp(epsilon_t) * (I_t^L * D_t  + I_t^O * Q_t))
# log_R0 + epsilon + log(exp(log(I_L) + log_Dt) + exp(log(I_O) + log_Qt))

# disaggregate imported and local cases according to the serial interval
# probabilities to get the expected number of infectious people in each state
# and time. Fixing the SI parameters at their prior means for now
local_infectious <- apply_serial_interval(local_cases, fixed = TRUE) 
imported_infectious <- apply_serial_interval(imported_cases, fixed = TRUE) 

# combine everything as vectors
log_loc_infectious_vec <- c(log(local_infectious))
log_imp_infectious_vec <- c(log(imported_infectious))

# work out which ones to exclude (because there were no infectious people)
valid <- which(is.finite(log_loc_infectious_vec + log_imp_infectious_vec))

# log_Reff_local <- log_R0 + log_Dt_vec
# log_Reff_imported <- log_R0 + log_Qt_vec
# 
# # get log expected number of new cases due to infectious people in each group
# new_from_local <- exp(log_loc_infectious_vec[valid] + log_Reff_local[valid])
# new_from_imports <- exp(log_imp_infectious_vec[valid] + log_Reff_imported[valid])
# expected_infections <- new_from_local + new_from_local

log_rel_new_from_loc <- log_loc_infectious_vec[valid] + log_Dt_vec[valid]
log_rel_new_from_imp <- log_imp_infectious_vec[valid] + log_Qt_vec[valid]
rel_new <- exp(log_rel_new_from_loc) + exp(log_rel_new_from_imp)
log_expected_infections <- log_R0 + epsilon[valid] + log(rel_new)
expected_infections <- exp(log_expected_infections)

distribution(local_cases[valid]) <- poisson(expected_infections)

m <- model(beta, log_q, epsilon)
draws <- mcmc(m, chains = 10, one_by_one = TRUE)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_eff <- coda::effectiveSize(draws)
max(r_hats)
min(n_eff)




# estimate the overall R_eff by weighting using the (interpolated) proportion of
# infectious cases that are imports

p_imported <- proportion_imported(local_infectious, imported_infectious, X)

# log_Dt_vec
# log_Qt_vec
# log_R0 + epsilon + log(exp())


log_Dt_weighted <- log_Dt_vec + log(1 - c(p_imported))
log_Qt_weighted <- log_Qt_vec + log(c(p_imported))
log_rel_R_eff <- log(exp(log_Dt_weighted) + exp(log_Qt_weighted))
log_R_eff <- log_R0 + log_rel_R_eff + epsilon
R_eff <- exp(log_R_eff)

# simulate from posterior for quantitites of interest

# R_eff of all cases
R_eff_sim <- posterior_sims(R_eff, draws)

# # R_eff of locally-acquired cases
# R_eff_loc_sim <- posterior_sims(R_eff_loc, draws)

# average R_eff of locally-acquired cases
log_Dt <- log_Dt_vec[1:n_dates]
R_eff_loc_trend <- exp(log_R0 + log_Dt)
R_eff_loc_trend_sim <- posterior_sims(R_eff_loc_trend, draws)

# average R_eff of overseas-acquired cases
log_Qt <- log_Qt_vec[1:n_dates]
R_eff_imp_trend <- exp(log_R0 + log_Qt)
R_eff_imp_trend_sim <- posterior_sims(R_eff_imp_trend, draws)

# random effect trends in Rt for locally-acquired cases (by state)
error_effect_sim <- posterior_sims(epsilon, draws)


blue <- "steelblue3"
green <- brewer.pal(8, "Set2")[1]
orange <- brewer.pal(8, "Set2")[2]
pink <- brewer.pal(8, "Set2")[4]


# overall R_eff
plot_trend(R_eff_sim,
           multistate = TRUE,
           base_colour = blue) +
  ggtitle(label = "Transmission by all cases") +
  ylab(expression(R["eff"]))

ggsave("outputs/figures/R_eff_all.png",
       width = 10,
       height = 16, scale = 0.8)

# # local R_eff
# plot_trend(R_eff_loc_sim,
#            multistate = TRUE,
#            base_colour = blue) +
#   ggtitle(label = "Transmission by locally-acquired cases") +
#   ylab(expression(R["eff"]~of~"locally-acquired"~cases))
#   
# ggsave("outputs/figures/R_eff_local.png",
#        width = 10,
#        height = 16, scale = 0.8)

# local average R_eff 
plot_trend(R_eff_loc_trend_sim,
           multistate = FALSE,
           base_colour = green,
           vline_at = intervention_dates()$date) + 
  ggtitle(label = "Impact of social distancing",
          subtitle = expression(Average~R["eff"]~of~"locally-acquired"~cases)) +
  ylab(expression(Average~R["eff"]))
  
ggsave("outputs/figures/R_eff_trend_local.png",
       width = 5,
       height = 4, scale = 1)

# imported average R_eff
plot_trend(R_eff_imp_trend_sim,
           multistate = FALSE,
           base_colour = orange,
           vline_at = quarantine_dates) + 
  ggtitle(label = "Impact of quarantine",
          subtitle = expression(Average~R["eff"]~of~"overseas-acquired"~cases)) +
  ylab(expression(Average~R["eff"]))

ggsave("outputs/figures/R_eff_trend_import.png",
       width = 5,
       height = 4)

# error trends
plot_trend(error_effect_sim,
           multistate = TRUE,
           base_colour = pink,
           hline_at = 0,
           ylim = NULL) + 
  ggtitle(label = "Trends not explained by interventions",
          subtitle = expression(Deviation~from~average~ln(R["eff"])~of~"locally-acquired"~cases)) +
  ylab("Deviation")

ggsave("outputs/figures/R_eff_error.png",
       width = 10,
       height = 16, scale = 0.8)


# posterior summary of R0
R0_draws <- R_eff_trend_sim[, 1]
mean(R0_draws)
sd(R0_draws)

# posterior summary of R_eff for the latest date
R_eff_now_draws <- R_eff_trend_sim[, ncol(R_eff_trend_sim)]
mean(R_eff_now_draws)
sd(R_eff_now_draws)
max(dates)
# - add a custom greta function for lognormal CDF (using TFP) and try
#   sampling with that
