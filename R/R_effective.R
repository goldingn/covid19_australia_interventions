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
#   log(R_{t,i}) = R_0 + b * social-distancing-index_{t,i} + \epsilon_{t,i} + \gamma_t
#   epsilon_i ~ GP(0, K1)
#   gamma ~ GP(0, K2)
#   lambda_{t,i} = sum_{t'=1}^t p-serial-interval(t') (local-incidence_{t',i} + imported-incidence_{t',i} import-contribution_{t'})
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


library(greta.gp)

# lognormal prior on R0 (normal prior on log(R0))
R0_prior <- lognormal_prior(2.6, 2)

# the relative contribution of imported cases (relative to locally-acquired
# cases) to transmission. I.e. one minus the effectiveness of quarantine
# measures. Assume it varies over time
quarantine_effectiveness <- case_when(
  dates < as.Date("2020-03-15") ~ 0.2,
  dates <= as.Date("2020-03-27") ~ 0.5,
  TRUE ~ 0.99,
)
import_contribution <- 1 - quarantine_effectiveness

# disaggregate imported and local cases according to these probabilities to get
# the expected number of infectious people in each state and time
case_contribution <- local_cases + sweep(imported_cases, 1, import_contribution, FUN = "*")
lambda <- apply_serial_interval(case_contribution, fixed = TRUE)

# NOTE: fixing the SI parameters at their prior means just for testing

# define a GP to reflect the function:
#   log(R_eff) <- log(R_0) + b * social-distancing-index_{t,i} + \epsilon_{t,i} + \gamma_t
#   epsilon_i ~ GP(0, K1)
#   gamma ~ GP(0, K2)
#   log(R_0) ~ N(0.723, 0.465)

# but since this implies a MVN prior over log(R_eff), we can marginalise all of
# these separate parameters and make the posterior much more nicely behaved:

#  log(R0) ~ R0_mean + GP(0, white())
#  gamma ~ GP(0, rbf(time))
#  epsilon_i ~ GP(0, rbf1(time))
#  epsilon ~ GP(0, rbf2(time) * iid(state))
#  social-distancing-index * b ~ GP(0, lin(social-distancing-index))

# These GPs can be combined as:

#  log(R0) + b * social-distancing-index + gamma + epsilon = GP(0.723, K)
#  K = white() +
#      lin(social-distancing-index)
#      rbf1(time) +
#      rbf2(time) * iid(state) +

# need to first put the data into a vector, and take this opportunity to exclude those with no infectious cases
lambda_vec <- c(lambda)

date_vec <- rep(date_nums, n_states)
state_vec <- rep(seq_len(n_states), each = n_dates)
social_distancing_index_vec <- rep(0, n_dates * n_states)

X <- cbind(date_vec,
           state_vec,
           social_distancing_index_vec)

# define a GP on dates, using subset of regressors approximation
l_ntnl <- lognormal(1, 0.5)
l_state <- lognormal(1, 0.5)

# magnitude of the national error trend
sigma_ntnl <- normal(0, 1, truncation = c(0, Inf))
# magnitude of the state-level (difference from the national) error trend
sigma_state <- normal(0, 1, truncation = c(0, Inf))
# degree of variation between state timeseries (shrink towards 0)
sigma_state_iid <- normal(0, 1, truncation = c(0, Inf))

# standard deviation of IID noise associated with each day (case clustering)
# we want the prior marginal variance to approximately match prior on log(R0),
# but also to shrink towards 0, so use a half normal prior with mean matching
# the required standard deviation
sigma_noise_sd <- R0_prior$sd * sqrt(pi) / sqrt(2)
sigma_noise <- normal(0, sigma_noise_sd, truncation = c(0, Inf))
# summary(calculate(sigma_noise, nsim = 10000)[[1]])

noise_kernel <- white(sigma_noise ^ 2)
distancing_kernel <- linear(1, columns = 3)
state_kernel <- rbf(l_state, sigma_state ^ 2, columns = 1) * iid(sigma_state_iid ^ 2, columns = 2)
national_kernel <- rbf(l_ntnl, sigma_ntnl ^ 2, columns = 1)

kernel <- noise_kernel + distancing_kernel + state_kernel + national_kernel

# build a matrix of inducing points, one every 7 days but with one at the end
inducing_date_nums <- seq(n_dates, 1, by = -7)
inducing_index <- which(X[, 1] %in% inducing_date_nums)
X_inducing <- X[inducing_index, ]

zero_mean_gp <- gp(X, kernel, inducing = X_inducing, tol = 0)

log_R_eff <- R0_prior$mean + zero_mean_gp

# work out which ones to exclude (because there were no infectious people)
valid <- which(lambda_vec > 0)

# combine with lambda on log scale for numerical stability - since greta can use
# this in evaluating the poisson likelihood
# log_expected_infections <- sweep(log(lambda), 1, log_R_eff, FUN = "+") 
log_expected_infections <- log(lambda_vec[valid]) + log_R_eff[valid]
expected_infections <- exp(log_expected_infections)
distribution(local_cases[valid]) <- poisson(expected_infections)

m <- model(log_R_eff)

draws <- mcmc(m, chains = 10, one_by_one = TRUE)

r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_eff <- coda::effectiveSize(draws)
max(r_hats)
min(n_eff)

R_eff <- exp(log_R_eff)

R_eff_draws <- calculate(R_eff, values = draws)
R_eff_mat <- as.matrix(R_eff_draws)
mean <- colMeans(R_eff_mat)
ci90 <- apply(R_eff_mat, 2, quantile, c(0.05, 0.95))
ci50 <- apply(R_eff_mat, 2, quantile, c(0.25, 0.75))

df <- tibble(date = rep(dates, n_states),
             state = rep(states, each = n_dates),
             R_eff_mean = mean,
             R_eff_50_lo = ci50[1, ],
             R_eff_50_hi = ci50[2, ],
             R_eff_90_lo = ci90[1, ],
             R_eff_90_hi = ci90[2, ])


base_colour <- "steelblue3"
library(ggplot2)
df %>%
  filter(date >= as.Date("2020-03-01")) %>%
  mutate(type = "Nowcast") %>%
  
  ggplot() + 
  aes(date, R_eff_mean, fill = type) +
  
  ylab(expression(R["eff"])) +
  xlab("Date") +
  
  scale_y_continuous(limits = c(0, 3), position = "right") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_ribbon(aes(ymin = R_eff_90_lo,
                  ymax = R_eff_90_hi),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = R_eff_50_lo,
                  ymax = R_eff_50_hi),
              alpha = 0.5) +
  geom_line(aes(y = R_eff_90_lo),
            colour = base_colour,
            alpha = 0.8) + 
  geom_line(aes(y = R_eff_90_hi),
            colour = base_colour,
            alpha = 0.8) + 
  
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_hline(yintercept = 2.6, linetype = "dashed") +
  
  facet_wrap(~ state, ncol = 2, scales = "free") +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 0))

ggsave("outputs/figures/R_eff.png",
       width = 10,
       height = 16, scale = 0.8)

# plot fixed effect trend in Rt (common to all states)
X_one_state <- X[1:n_dates, ]
distancing_effect <- project(zero_mean_gp,
                             x_new = X_one_state,
                             kernel = distancing_kernel)

noise <- normal(0, sigma_noise, dim = n_dates)
log_R_eff_trend <- R0_prior$mean + noise + distancing_effect
R_eff_trend <- exp(log_R_eff_trend)

R_eff_trend_sim <- calculate(R_eff_trend, values = draws, nsim = 10000)[[1]][, , 1]
mean <- colMeans(R_eff_trend_sim)
ci90 <- apply(R_eff_trend_sim, 2, quantile, c(0.05, 0.95))
ci50 <- apply(R_eff_trend_sim, 2, quantile, c(0.25, 0.75))

df_trend <- tibble(date = dates,
                   R_eff_mean = mean,
                   R_eff_50_lo = ci50[1, ],
                   R_eff_50_hi = ci50[2, ],
                   R_eff_90_lo = ci90[1, ],
                   R_eff_90_hi = ci90[2, ])

base_colour <- brewer.pal(8, "Set2")[1]
y_max <- max(c(3, df_trend$R_eff_50_hi))
df_trend %>%
  filter(date >= as.Date("2020-03-01")) %>%
  mutate(type = "Nowcast") %>%
  
  ggplot() + 
  aes(date, R_eff_mean, fill = type) +
  
  ylab(expression(R["eff"])) +
  xlab("Date") +
  
  coord_cartesian(ylim = c(0, y_max)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_ribbon(aes(ymin = R_eff_90_lo,
                  ymax = R_eff_90_hi),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = R_eff_50_lo,
                  ymax = R_eff_50_hi),
              alpha = 0.5) +
  geom_line(aes(y = R_eff_90_lo),
            colour = base_colour,
            alpha = 0.8) + 
  geom_line(aes(y = R_eff_90_hi),
            colour = base_colour,
            alpha = 0.8) + 
  
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_hline(yintercept = 2.6, linetype = "dashed") +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 0))

ggsave("outputs/figures/R_eff_trend.png",
       width = 5,
       height = 4, scale = 0.8)


# plot random effect trends in Rt (different in each state)
error_kernel <- state_kernel + national_kernel
error_effect <- project(zero_mean_gp,
                        x_new = X,
                        kernel = error_kernel)

error_effect_sim <- calculate(error_effect, values = draws, nsim = 10000)[[1]][, , 1]
mean <- colMeans(error_effect_sim)
ci90 <- apply(error_effect_sim, 2, quantile, c(0.05, 0.95))
ci50 <- apply(error_effect_sim, 2, quantile, c(0.25, 0.75))

df_error <- tibble(date = rep(dates, n_states),
                   state = rep(states, each = n_dates),
                   R_eff_mean = mean,
                   R_eff_50_lo = ci50[1, ],
                   R_eff_50_hi = ci50[2, ],
                   R_eff_90_lo = ci90[1, ],
                   R_eff_90_hi = ci90[2, ])

base_colour <- brewer.pal(8, "Set2")[4]
y_lim <- range(c(df_error$R_eff_90_lo, df_error$R_eff_90_hi))
df_error %>%
  filter(date >= as.Date("2020-03-01")) %>%
  mutate(type = "Nowcast") %>%
  
  ggplot() + 
  aes(date, R_eff_mean, fill = type) +
  
  ylab(expression(Temporal~variation~around~trend~of~ln(R["eff"]))) +
  xlab("Date") +
  
  coord_cartesian(ylim = y_lim) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_ribbon(aes(ymin = R_eff_90_lo,
                  ymax = R_eff_90_hi),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = R_eff_50_lo,
                  ymax = R_eff_50_hi),
              alpha = 0.5) +
  geom_line(aes(y = R_eff_90_lo),
            colour = base_colour,
            alpha = 0.8) + 
  geom_line(aes(y = R_eff_90_hi),
            colour = base_colour,
            alpha = 0.8) + 
  
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~ state, ncol = 2, scales = "free") +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90))

ggsave("outputs/figures/R_eff_error.png",
       width = 10,
       height = 16, scale = 0.8)



# - output plots of GP components:
#    - prior on Rt (mean & white kernel & distancing factor)
#    - state-level deviation from those trends

# - run with social distancing index as a covariate in the mean function

# - add a custom greta function for lognormal CDF (using TFP) and try
#   sampling with that

# # run epinow too
# epinow_cases <- linelist %>%
#   rename(date = date_confirmation) %>%
#   group_by(date, region, import_status) %>%
#   summarise(cases = n())
# 
# epinow_linelist <- linelist %>%
#   rename(date_confirm = date_confirmations)
# 
# EpiNow::regional_rt_pipeline(
#   cases = epinow_cases,
#   linelist = epinow_linelist,
#   case_limit = 20
# )

