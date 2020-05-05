# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

# The basic model assumes that the number of new locally-acquired cases
# $N_{t,i}^L$ at time $t$ in region $i$ is *conditionally* Poisson-distributed
# with expectation $lambda_{t,i}$ given by the product of the number of
# infectious cases $I_{t,i}$ and the time-varying reproduction rate $R_{t,i}$:

#   N_{t,i}^L &\sim Poisson(lambda_{t,i})
#   lambda_{t,i} &= I_{t,i} R_{t,i}

# We extend this model by considering separate reproduction rates for two groups
# of infectious cases in order to model the effects of different interventions
# targetted at each group: those with locally-acquired cases $I_{t,i}^L$, and
# those with overseas acquired cases $I_{t,i}^O$, with corresponding reproduction
# rates $R_{t,i}^L$ and $R_{t,i}^O$:

#   lambda_{t,i} &= I_{t,i}^L R_{t,i}^L + I_{t,i}^O R_{t,i}^O
#   R_{t,i}^L &= R_0 D_t e^{\epsilon_{t,i}^L}
#   R_{t,i}^O &= R_0 Q_t e^{\epsilon_{t,i}^O}

# where each of these reproduction rates is modelled as a product of: the
# reproduction rate under initial conditions and no interventions $R_0$;
# deterministic functions $D_t$ and $Q_t$ that modify $R_0$ over time to
# respectively represent the impacts of physical distancing and quarantine
# interventions at a national level, and correlated timeseries of random effects
# $\epsilon_{t,i}^L$ and $\epsilon_{t,i}^O$ to represent stochastic fluctuations
# in the reporting rate in each state, for example due to the clusters in
# subpopulations with higher or lower reproduction rates than the general
# population

# We model the effect of $D_t$ as being proportional (on the log scale) to an
# index of the proportional change in population mobility in response to
# physical distancing measures $d_t$ (estimated from multiple datastreams of
# poplulation mobility), which has initial value 0 before distancing measures
# were implemented and value 1 at its maximum extent:

#   D_t &= e^{\beta d_t}

# We model $Q_t$ via a monotone decreasing step function with values constrained
# to the unit interval, and with steps at the known dates $\tau_1$ and $\tau_2$ of
# changes in quarantine policy:

# \[ Q_t = \begin{cases}
#    q1 & t < \tau_1 \\
#    q2 & \tau_1 \leq t < \tau_2 \\
#    q3 & \tau_2 \leq t
#    \end{cases}
# \]

# where $q1 > q2 > q3$ and all parameters are constrained to the unit interval.

# The correlated timeseries of errors in the log of the effective reproduction
# rate for each group $\epsilon_{t,i}^L$ and $\epsilon_{t,i}^O$ are each modelled
# as a zero-mean Gaussian processes with covariance structured to reflect
# temporal correlation in errors within each state (but independent between
# states), plus IID Gaussian error on each observation. The IID Gaussian errors
# account for overdispersion in the numbers of new infections arising on a given
# day. This is equivalent to a Poisson-lognormal observation distribution
# conditional on the timeseries component of the model, which is very similar to
# a negative binomial distribution (which can be viewed as a Poisson-gamma
# distribution).

# The number of infectious people in each group is computed by disaggregating
# the number of cases (assigned to their assumed date of infection) over a
# subsequent period of time, with probability distribution given by a serial
# interval distribution. The serial interval distribution is computed from a
# linelist of cases, and is represented by a discretized cumulative density
# function $p-serial-interval(t')$, giving the probability of a serial interval
# (average time between subsequent cases) of $t'$.

#   I_{t,i}^L = sum_{t'=1}^t p-serial-interval(t') N_{t',i}^L
#   I_{t,i}^O = sum_{t'=1}^t p-serial-interval(t') N_{t',i}^O

# where $N_{t,i}^O$ is the number of cases with overseas-acquired infections in
# the linelist in state $i$ and with infection date on time $t$.

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

library(greta.gp)

# build the social distancing index, with fixed shape for main effect, and informative prior on the amount of waning
distancing_file <- "outputs/social_distancing_latent.RDS"
social_distancing_main_index <- distancing_file %>%
  readRDS() %>%
  select(mean, date) %>%
  right_join(tibble(date = dates)) %>%
  replace_na(list(mean = 0)) %>%
  pull(mean)

waning_file <- "outputs/waning_distancing_latent.RDS"
waning_index <- waning_file %>%
  readRDS() %>%
  select(mean, date) %>%
  right_join(tibble(date = dates)) %>%
  replace_na(list(mean = 0)) %>%
  pull(mean)
  
waning_amount_params <- readRDS("outputs/waning_amount_parameters.RDS")
waning_amount <- do.call(normal, waning_amount_params)

social_distancing_index <- social_distancing_main_index + waning_index * waning_amount

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

# q_index_vec <- rep(q_index, n_states)
# q_raw <- uniform(0, 1, dim = 3)
# q <- cumprod(q_raw)
# log_q <- log(q)
log_q_raw <- -exponential(1, dim = 3)
log_q <- cumsum(log_q_raw)
# log_Qt_vec <- log_q[q_index_vec]
log_Qt <- log_q[q_index]

# the reduction from R0 down to R_eff for locally-acquired cases due to social
# distancing behaviour, modelled as a being proportional to the reduction in
# mobility (and therefore contacts) measured from the mobility datastreams. The
# social distanding index is proportional to the percentage change in mobility,
# but we can express is as proportional reduction in mobility with: 1 - beta * sdi
beta <- uniform(0, 1)
# log_Dt_vec <- log1p(-beta * rep(social_distancing_index, n_states))
log_Dt <- log1p(-beta * social_distancing_index)

# temporally correlated errors in R_eff for local cases - representing all the
# stochastic transmission dynamics in the community, such as outbreaks in
# communities with higher or lower tranmission rates.

# variation in R_eff for locally-acquired cases over time
alpha_time_L <- lognormal(2, 0.5)
alpha_time_O <- lognormal(2, 0.5)
lengthscale_time_L <- lognormal(4, 0.5)
lengthscale_time_O <- lognormal(4, 0.5)
sigma_time_L <- normal(0, 0.5, truncation = c(0, Inf))
sigma_time_O <- normal(0, 0.5, truncation = c(0, Inf))

time_kernel_L <- rational_quadratic(
  lengthscales = lengthscale_time_L,
  variance = sigma_time_L ^ 2,
  alpha = alpha_time_L,
  columns = 1)

time_kernel_O <- rational_quadratic(
  lengthscales = lengthscale_time_O,
  variance = sigma_time_O ^ 2,
  alpha = alpha_time_O,
  columns = 1)

# variation in this timeseries between the states and territories
sigma_state_L <- normal(0, 0.5, truncation = c(0, Inf))
sigma_state_O <- normal(0, 0.5, truncation = c(0, Inf))
state_variation_kernel_L <- iid(sigma_state_L ^ 2, columns = 2)
state_variation_kernel_O <- iid(sigma_state_O ^ 2, columns = 2)

# IID noise in log(R_eff) per day (overdispersion, or case clustering)
sigma_noise_L <- normal(0, 0.5, truncation = c(0, Inf))
noise_kernel_L <- white(sigma_noise_L ^ 2)
sigma_noise_O <- normal(0, 0.5, truncation = c(0, Inf))
noise_kernel_O <- white(sigma_noise_O ^ 2)

kernel_L <- noise_kernel_L + time_kernel_L * state_variation_kernel_L
kernel_O <- noise_kernel_O + time_kernel_O * state_variation_kernel_O

# # data for the GP to act on
# X <- cbind(date = rep(date_nums, n_states),
#            state = rep(seq_len(n_states), each = n_dates))

# build a matrix of inducing points, regularly spaced over time but with one on
# the most recent date
inducing_date_nums <- seq(n_dates, 1, by = -5)
# inducing_index <- which(X[, 1] %in% inducing_date_nums)
# X_inducing <- X[inducing_index, ]

# evaluate GP for epsilon
epsilon_L <- gp(date_nums, kernel_L, inducing = inducing_date_nums, n = n_states, tol = 0)
epsilon_O <- gp(date_nums, kernel_O, inducing = inducing_date_nums, n = n_states, tol = 0)

# combine these components
# N ~ Poisson(R0 * exp(epsilon_t) * (I_t^L * D_t  + I_t^O * Q_t))
# log_R0 + epsilon + log(exp(log(I_L) + log_Dt) + exp(log(I_O) + log_Qt))

# disaggregate imported and local cases according to the serial interval
# probabilities to get the expected number of infectious people in each state
# and time. Fixing the SI parameters at their prior means for now
local_infectious <- apply_serial_interval(local_cases, fixed = TRUE) 
imported_infectious <- apply_serial_interval(imported_cases, fixed = TRUE) 

# combine everything as vectors
# log_loc_infectious_vec <- c(log(local_infectious))
# log_imp_infectious_vec <- c(log(imported_infectious))
log_loc_infectious <- log(local_infectious)
log_imp_infectious <- log(imported_infectious)

# work out which ones to exclude (because there were no infectious people)
valid <- which(is.finite(log_loc_infectious + log_imp_infectious))

log_rel_Reff_loc <- sweep(epsilon_L, 1, log_Dt, FUN = "+") 
log_rel_Reff_imp <- sweep(epsilon_O, 1, log_Qt, FUN = "+") 

log_rel_new_from_loc_vec <- log_loc_infectious[valid] + log_rel_Reff_loc[valid]
log_rel_new_from_imp_vec <- log_imp_infectious[valid] + log_rel_Reff_imp[valid]
rel_new_vec <- exp(log_rel_new_from_loc_vec) + exp(log_rel_new_from_imp_vec)

log_expected_infections_vec <- log_R0 + log(rel_new_vec)
expected_infections_vec <- exp(log_expected_infections_vec)

distribution(local_cases[valid]) <- poisson(expected_infections_vec)

m <- model(beta, log_q, epsilon_L, epsilon_O)
draws <- mcmc(m, chains = 10, one_by_one = TRUE)
draws <- extra_samples(draws, 2000)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_eff <- coda::effectiveSize(draws)
max(r_hats)
min(n_eff)

# estimate the overall R_eff by weighting using the (interpolated) proportion of
# infectious cases that are imports
p_imported <- proportion_imported(local_infectious, imported_infectious)
log_Dt_weighted <- log_rel_Reff_loc + log1p(-p_imported)
log_Qt_weighted <- log_rel_Reff_imp + log(p_imported)
log_rel_R_eff <- log(exp(log_Dt_weighted) + exp(log_Qt_weighted))
log_R_eff <- log_R0 + log_rel_R_eff
R_eff_vec <- c(exp(log_R_eff))

# R_eff of locally-acquired and overseas-acquired cases
R_eff_loc_vec <- c(exp(log_R0 + log_rel_Reff_loc))
R_eff_imp_vec <- c(exp(log_R0 + log_rel_Reff_imp))

# average R_eff of locally-acquired and overseas-acquired cases
R_eff_loc_trend <- exp(log_R0 + log_Dt)
R_eff_imp_trend <- exp(log_R0 + log_Qt)

# simulate from posterior for quantitities of interest
nsim <- coda::niter(draws) * coda::nchain(draws)
sims <- calculate(
  R_eff_vec,
  R_eff_loc_vec,
  R_eff_imp_vec,
  R_eff_loc_trend,
  R_eff_imp_trend,
  c(epsilon_L),
  c(epsilon_O),
  values = draws,
  nsim = nsim
)

R_eff_sim <- sims$R_eff_vec
R_eff_loc_sim <- sims$R_eff_loc_vec
R_eff_imp_sim <- sims$R_eff_imp_vec
R_eff_loc_trend_sim <- sims$R_eff_loc_trend
R_eff_imp_trend_sim <- sims$R_eff_imp_trend
error_effect_L_sim <- sims$`c(epsilon_L)`
error_effect_O_sim <- sims$`c(epsilon_O)`


blue <- "steelblue3"
green <- brewer.pal(8, "Set2")[1]
orange <- brewer.pal(8, "Set2")[2]
pink <- brewer.pal(8, "Set2")[4]

# david does 8.27 x 11.69 (landscape A4) for 3x2 panels
# aspect ratio of 0.707:1 h:w
# want A4 *portrait* width (8.27) with same aspect ratio

# get required aspect ratio
panel_width <- 11.69 / 2
panel_height <- 8.27 / 3
panel_ratio <- panel_height / panel_width

# work out dimensions for 4x2 panels for reports
multi_mfrow <- c(4, 2)
multi_width <- 8.27
multi_height <- (multi_width / multi_mfrow[2]) * panel_ratio * multi_mfrow[1]
# add a bit of space for the title
multi_height <- multi_height * 1.2

# overall R_eff
plot_trend(R_eff_sim,
           multistate = TRUE,
           base_colour = blue,
           vline_at = quarantine_dates) +
  ggtitle(label = "Transmission by all cases") +
  ylab(expression(R["eff"]))

ggsave("outputs/figures/R_eff_all.png",
       width = multi_width,
       height = multi_height,
       scale = 0.8)

# local R_eff
plot_trend(R_eff_loc_sim,
           multistate = TRUE,
           base_colour = green,
           vline_at = intervention_dates()$date) +
  ggtitle(label = "Transmission by locally-acquired cases") +
  ylab(expression(R["eff"]~of~"locally-acquired"~cases))

ggsave("outputs/figures/R_eff_local.png",
       width = multi_width,
       height = multi_height,
       scale = 0.8)

# imported R_eff
plot_trend(R_eff_imp_sim,
           multistate = TRUE,
           base_colour = orange,
           vline_at = quarantine_dates) +
  ggtitle(label = "Transmission by overseas-acquired cases") +
  ylab(expression(R["eff"]~of~"overseas-acquired"~cases))

ggsave("outputs/figures/R_eff_imported.png",
       width = multi_width,
       height = multi_height,
       scale = 0.8)


# local average R_eff 
plot_trend(R_eff_loc_trend_sim,
           multistate = FALSE,
           base_colour = green,
           vline_at = intervention_dates()$date) + 
  ggtitle(label = "Impact of social distancing",
          subtitle = expression(Component~of~R["eff"]~due~to~social~distancing)) +
  ylab(expression(R["eff"]~component))
  
ggsave("outputs/figures/R_eff_trend_local.png",
       width = panel_width,
       height = panel_height * 1.25,
       scale = 1)

# imported average R_eff
plot_trend(R_eff_imp_trend_sim,
           multistate = FALSE,
           base_colour = orange,
           vline_at = quarantine_dates) + 
  ggtitle(label = "Impact of quarantine of overseas arrivals",
          subtitle = expression(Component~of~R["eff"]~due~to~quarantine~of~overseas~arrivals)) +
  ylab(expression(R["eff"]~component))

ggsave("outputs/figures/R_eff_trend_import.png",
       width = panel_width,
       height = panel_height * 1.25,
       scale = 1)

# error trends
plot_trend(error_effect_L_sim,
           multistate = TRUE,
           base_colour = pink,
           hline_at = 0,
           vline_at = intervention_dates()$date,
           ylim = NULL) + 
  ggtitle(label = "Trend in local cases not explained by social distancing",
          subtitle = expression(Deviation~from~average~ln(R["eff"])~of~"locally-acquired"~cases)) +
  ylab("Deviation")

ggsave("outputs/figures/R_eff_error_local.png",
       width = multi_width,
       height = multi_height,
       scale = 0.8)

# error trends
plot_trend(error_effect_O_sim,
           multistate = TRUE,
           base_colour = pink,
           hline_at = 0,
           vline_at = quarantine_dates,
           ylim = NULL) + 
  ggtitle(label = "Trend in imported cases not explained by quarantine of overseas arrivals",
          subtitle = expression(Deviation~from~average~ln(R["eff"])~of~"overseas-acquired"~cases)) +
  ylab("Deviation")

ggsave("outputs/figures/R_eff_error_import.png",
       width = multi_width,
       height = multi_height,
       scale = 0.8)

# posterior summary of R0
R0_draws <- R_eff_loc_trend_sim[, 1, 1]
mean(R0_draws)
sd(R0_draws)

# posterior summary of R_eff for the latest date
R_eff_now_draws <- R_eff_loc_trend_sim[, ncol(R_eff_loc_trend_sim), 1]
mean(R_eff_now_draws)
sd(R_eff_now_draws)
max(dates)

# posterior summary of R_eff for the peak of distancing
peak_distancing <- max(which(waning_index == 0))
R_eff_peak_draws <- R_eff_loc_trend_sim[, peak_distancing, 1]
mean(R_eff_peak_draws)
sd(R_eff_peak_draws)
min(dates) + peak_distancing - 1

# summary of the inferred waning amount
waning_amount_draws <- calculate(waning_amount, values = draws, nsim = 20000)[[1]][, 1, 1]
mean(waning_amount_draws * -100)
sd(waning_amount_draws * -100)
waning_amount_params


post_intervention_start <- as.numeric(max(intervention_dates()$date) - dates[1])
sum(local_infectious[post_intervention_start:n_dates, ])
sum(local_cases[post_intervention_start:n_dates, ])


# Ratio of R_eff at minimum to R0 to

# - switch to stacked version of GP (smaller covaraince matrix, faster inference)
# - add a custom greta function for lognormal CDF (using TFP) and try
#   sampling with that
