# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

# see the manuscript for an explanation of the model that may or may not be out
# of date.

library(dplyr)
library(readr)
library(tidyr)
library(RColorBrewer)
source("R/functions.R")

staging <- FALSE

set.seed(2020-04-29)
linelist <- get_linelist()

# impute the onset dates (only 0.6% of cases) using expected value from time to
# detection distribution. Do this outside dplyr to avoid duplicating slow computations
missing_onset <- is.na(linelist$date_onset)
imputed_onsets <- impute_onsets(
  linelist$date_detection[missing_onset],
  method = "random"
)
linelist$date_onset[missing_onset] <- imputed_onsets

# build date-by-state matrices of the counts of new local and imported cases and
# imports by assumed date of infection (with an incubation period of 5 days)
linelist <- linelist %>%
  rename(state = region,
         date = date_onset) %>%
  mutate(date = date - 5) %>%
  select(-date_confirmation)

import_statuses <- sort(unique(linelist$import_status))
states <- sort(unique(linelist$state))

# dates in the linelist (used for fitting)
latest_date <- max(linelist$date)
dates <- seq(min(linelist$date), latest_date, by = 1)

# last date in the mobility data (used for plotting)
google_change_data <- readRDS("outputs/google_change_trends.RDS")
last_mobility_date <- max(google_change_data$date)
mobility_dates <- seq(min(dates), last_mobility_date, by = 1)
change_date <- last_mobility_date + 1
linelist_date <- linelist$date_linelist[1]

# save these for Freya and Rob to check
tibble(
  linelist_date = linelist_date,
  latest_infection_date = max(dates),
  latest_reff_date = last_mobility_date,
  forecast_reff_change_date = change_date
) %>%
  write_csv("outputs/output_dates.csv")

n_states <- length(states)
n_dates <- length(dates)
n_extra <- as.numeric(Sys.Date() - max(dates)) + 7 * 6
date_nums <- seq_len(n_dates + n_extra)

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

# use Nishiura's serial interval as a generation interval
gi_cdf <- nishiura_cdf()

# circulant matrix of generation interval discrete probabilities
gi_mat <- gi_matrix(gi_cdf, dates, gi_bounds = c(0, 20))

# disaggregate imported and local cases according to the generation interval
# probabilities to get the expected number of infectious people in each state
# and time
local_infectious <- gi_mat %*% local_cases
imported_infectious <- gi_mat %*% imported_cases

library(greta.gp)

# reduction in R due to surveillance detecting and isolating infectious people
dates_long <- min(dates) + seq_along(date_nums) - 1
surveillance_reff_local_reduction <- surveillance_effect(
  dates = dates_long,
  cdf = gi_cdf
)

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
q_index <- c(q_index, rep(3, n_extra))

# q_raw <- uniform(0, 1, dim = 3)
log_q_raw <- -exponential(1, dim = 3)
log_q <- cumsum(log_q_raw)
log_Qt <- log_q[q_index]

# The change in R_t for locally-acquired cases due to social distancing
# behaviour, modelled as a sum of household R_t and non-household R_t
# Non-household Reff is modelled as a function of the number of non-household
# contacts per 24h (itself modelled from mobility data, calibrated against
# contact surveys) and the relative transmission probability per contact,
# inferred from surveys on micro-distancing behaviour.
distancing_effect <- distancing_effect_model(mobility_dates, gi_cdf)

# pull out R_t component due to distancing for locally-acquired cases, and
# extend to correct length
extend_idx <- pmin(seq_along(date_nums), nrow(distancing_effect$R_t))
R_eff_loc_1_no_surv <- distancing_effect$R_t[extend_idx, ]

# multiply by the surveillance effect
R_eff_loc_1 <- sweep(
  R_eff_loc_1_no_surv,
  1,
  surveillance_reff_local_reduction,
  FUN = "*"
)

log_R_eff_loc_1 <- log(R_eff_loc_1)

# extract R0 from this model and estimate R_t component due to quarantine for
# overseas-acquired cases
log_R0 <- log_R_eff_loc_1[1, 1]
log_R_eff_imp_1 <- log_R0 + log_Qt
R_eff_imp_1 <- exp(log_R_eff_imp_1)

# build a matrix of inducing points, regularly spaced over time but with one on
# the most recent date
n_date_nums <- length(date_nums)
inducing_date_nums <- rev(seq(n_date_nums, 1, by = -3))
n_inducing <- length(inducing_date_nums)

# temporally correlated errors in R_eff for local and imported cases - representing all the
# stochastic transmission dynamics in the community, such as outbreaks in
# communities with higher or lower tranmission rates, and interstate and
# temporal variation in quarantine effectiveness not captured by the step
# function

kernel_O <- rbf(
  lengthscales = lognormal(3, 1),
  variance = normal(0, 0.5, truncation = c(0, Inf)) ^ 2,
)

epsilon_O <- epsilon_gp(
  date_nums = date_nums,
  n_states = n_states,
  kernel = kernel_O,
  inducing_date_nums = inducing_date_nums
)

kernel_L <- rational_quadratic(
  lengthscales = lognormal(3, 1),
  variance = normal(0, 0.5, truncation = c(0, Inf)) ^ 2,
  alpha = lognormal(3, 1)
)

epsilon_L <- epsilon_gp(
  date_nums = date_nums,
  n_states = n_states,
  kernel = kernel_L,
  inducing_date_nums = inducing_date_nums
)

# work out which elements to exclude (because there were no infectious people)
# local_infectious_sim <- calculate(local_infectious, nsim = 1)[[1]][1, , ]
# imported_infectious_sim <- calculate(imported_infectious, nsim = 1)[[1]][1, , ]
local_valid <- is.finite(local_infectious) & local_infectious > 0
import_valid <- is.finite(imported_infectious) & imported_infectious > 0
valid <- which(local_valid & import_valid, arr.ind = TRUE)

# log Reff for locals and imports
log_R_eff_loc <- log_R_eff_loc_1 + epsilon_L

log_R_eff_imp <- sweep(
  epsilon_O,
  1,
  log_R_eff_imp_1,
  FUN = "+"
)

# combine everything as vectors, excluding invalid datapoints (remove invalid
# elements here, otherwise it causes a gradient issue)
log_new_from_loc_vec <- log(local_infectious[valid]) + log_R_eff_loc[1:n_dates, ][valid]
log_new_from_imp_vec <- log(imported_infectious[valid]) + log_R_eff_imp[1:n_dates, ][valid]
expected_infections_vec <- exp(log_new_from_loc_vec) + exp(log_new_from_imp_vec)

# negative binomial likelihood for number of cases
sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf), dim = n_states)
size <- 1 / sqrt(sqrt_inv_size[valid[, 2]])
prob <- 1 / (1 + expected_infections_vec / size)

# Account for right truncation; underreporting of recent infections which have
# had less time to be detected. Given the number of cases N_t infected on day t
# (that will ever be detected), the number of cases N^*_t infected on that day
# that are known about so far is drawn from a binomial sample with probability
# p, from the time-to-detection distribution. Since N_t is drawn from a negative
# binomial,  N^*_t is drawn from a compound binomial/negative binomial mixture
# distribution. Fortunately that turns out to be a negative binomial with
# modified probability parameter (NB is poisson-gamma, so binomial-NB is
# binomial-poisson-gamma, but binomial-poisson is poisson with rate lambda * p and gamma times a constant is gamma,
# so it's a poisson-gamma, which is NB).

# There is an average of one day from specimen collection to confirmation, and
# the linelist covers the previous day, so the date by which they need to have
# been detected two days prior to the linelist date.
latest_detection_date <- linelist_date - 2
delays <- as.numeric(latest_detection_date - dates)
detection_prob <- 1 - ttd_survival(delays, dates)
detection_prob_vec <- detection_prob[valid[, 1]]

# Modify the probability to account for truncation. When detection_prob_vec = 1,
# this collapses to prob
prob_trunc <- 1 / (1 + detection_prob_vec * (1 - prob) / prob)

distribution(local_cases[valid]) <- negative_binomial(size, prob_trunc)

m <- model(expected_infections_vec)

draws <- mcmc(
  m,
  sampler = hmc(Lmin = 10, Lmax = 15),
  chains = 10,
  one_by_one = TRUE
)
# draws <- extra_samples(draws, 1000, one_by_one = TRUE)

convergence(draws)

# check fit of observation model against data 
nsim <- coda::niter(draws) * coda::nchain(draws)
nsim <- min(10000, nsim)
cases <- negative_binomial(size, prob_trunc)
cases_sim <- calculate(cases, values = draws, nsim = nsim)[[1]][, , 1]

# overall PPC check
bayesplot::ppc_ecdf_overlay(
  local_cases[valid],
  cases_sim[1:1000, ],
  discrete = TRUE
)

# check by state and time
plot_fit(local_cases[valid], cases_sim, valid)

# R_eff for local-local and import-local among active cases per state
# (components 1 and 2)
R_eff_loc_12 <- exp(log_R_eff_loc)
R_eff_imp_12 <- exp(log_R_eff_imp)

# vector of generation interval probabilities
gi_vec <- gi_vector(gi_cdf, max(dates))

# check fit of projected cases against national epi curve
check_projection(draws,
                 R_eff_local = R_eff_loc_12,
                 R_eff_imported = R_eff_imp_12,
                 gi_mat = gi_mat,
                 gi_vec = gi_vec,
                 local_infectious = local_infectious,
                 imported_infectious = imported_infectious,
                 local_cases = local_cases,
                 dates = dates,
                 start_date = as.Date("2020-02-28"))

# Reff local component one under only micro- and only macro-distancing
de <- distancing_effect

# include the effect of surveillance at baseline (no improvements yet, but not nothing)
baseline_surveillance_effect <- surveillance_reff_local_reduction[1]

infectious_days <- infectious_period(gi_cdf)

# microdistancing
household_infections_micro <- de$HC_0 * (1 - de$p ^ de$HD_0)
non_household_infections_micro <- de$OC_0 * infectious_days *
  (1 - de$p ^ de$OD_0) * de$gamma_t_state
hourly_infections_micro <- household_infections_micro +
  non_household_infections_micro
R_eff_loc_1_micro <- hourly_infections_micro[extend_idx, ] * baseline_surveillance_effect

# macrodistancing
h_t <- h_t_state(mobility_dates)
HD_t <- de$HD_0 * h_t
household_infections_macro <- de$HC_0 * (1 - de$p ^ HD_t)
non_household_infections_macro <- de$OC_t_state * infectious_days * (1 - de$p ^ de$OD_0)
hourly_infections_macro <- household_infections_macro + non_household_infections_macro
R_eff_loc_1_macro <- hourly_infections_macro[extend_idx, ] * baseline_surveillance_effect

# Reff for locals compnent under only surveillance improvements
R_eff_loc_1_surv <- exp(log_R0 + log(surveillance_reff_local_reduction))

# make 5 different versions of the plots and outputs:
# 1. to the latest date of mobility data
# 2. 6 weeks into the future
# 3. 6 weeks into the future, with increase in mean Reff to 1.1
# 4. 6 weeks into the future, with increase in mean Reff to 1.2
# 5. 6 weeks into the future, with increase in mean Reff to 1.5

output_directories <- c("",
                        "projection",
                        "scenario_1_half_distancing",
                        "scenario_2_full_distancing",
                        "scenario_3_isolation_no_distancing",
                        "scenario_4_isolation_and_distancing")

# put in a separate directory if testing something
if (staging) {
  output_directories <- file.path(output_directories, "staging")
}

# types <- seq_along(output_directories)
types <- 5:6

for (type in types) {
  
  dir <- file.path("outputs", output_directories[type])
  dir.create(file.path(dir, "figures"),
             recursive = TRUE,
             showWarnings = FALSE)
  
  # save local case data, dates, and detection probabilities for Rob
  cbind(
    data.frame(
      date_onset = dates + 5,
      detection_probability = detection_prob
    ),
    local_cases
  ) %>%
    pivot_longer(
      cols = c(-date_onset, -detection_probability),
      names_to = "state",
      values_to = "count"
    ) %>%
    write.csv(file.path(dir, "local_cases_input.csv"), row.names = FALSE)
  
  # subset or extend projections based on type of projection
  if (type == 1) {
    # for the nowcast, estimate up to the latest mobility data
    last_date <- last_mobility_date
    projection_date <- NA
  } else {
    last_date <- min(dates) + n_date_nums - 1
    projection_date <- last_mobility_date + 1
  }
  n_projected <- n_dates + as.numeric(last_date - max(dates))
  rows <- pmin(n_dates + n_extra, seq_len(n_projected))
  dates_type <- min(dates) - 1 + seq_along(rows)
  
  # duplicate these so they can be modified for scenarios
  R_eff_loc_1_proj <- R_eff_loc_1
  R_eff_loc_12_proj <- R_eff_loc_12
  R_eff_loc_1_micro_proj <- R_eff_loc_1_micro
  R_eff_loc_1_macro_proj <- R_eff_loc_1_macro
  R_eff_loc_1_surv_proj <- R_eff_loc_1_surv
  epsilon_L_proj <- epsilon_L
  
  # for counterfactuals, relevel the Reffs in VIC to predetermined minima
  if (type > 2) {
    
    # component 1 minimum = 2020-04-13
    # component 2 minimum = 2020-03-29
    
    # either type of distancing (half or full) or full distancing plus isolation
    if (type %in% c(3, 4, 6)) {
      
      # if it's half distancing, use the date that relaxation of restrictions
      # was implemented (at which point macro and micro had already waned),
      # otherwise, use the date of maximum distancing (minimum of component 1)
      scenario_date <- ifelse(type == 3,
                             as.Date("2020-05-13"),
                             as.Date("2020-04-13"))
      
      # after the projection date, set the component 1 value to the one form this date
      state_idx <- which(states == "VIC")
      duplicate_idx <- seq_along(dates_type)
      duplicate_idx[dates_type >= projection_date] <- which(dates == scenario_date)
      R_eff_loc_1_proj[, state_idx] <- R_eff_loc_1_proj[duplicate_idx, state_idx]
      R_eff_loc_1_micro_proj[, state_idx] <- R_eff_loc_1_micro_proj[duplicate_idx, state_idx]
      R_eff_loc_1_macro_proj[, state_idx] <- R_eff_loc_1_macro_proj[duplicate_idx, state_idx]
      R_eff_loc_1_surv_proj <- R_eff_loc_1_surv_proj[duplicate_idx]
    }
    
    # if the scenario includes full isolation, reduce component 2 to match
    # minimum distribution
    if (type %in% c(5, 6)) {
      
      # after the projection date, set the epsilon_L value to this one
      state_idx <- which(states == "VIC")
      duplicate_idx <- seq_along(dates_type)
      duplicate_idx[dates_type >= projection_date] <- which(dates == as.Date("2020-03-29"))
      epsilon_L_proj[, state_idx] <- epsilon_L_proj[duplicate_idx, state_idx]
      
    }
    
    R_eff_loc_12_proj <- exp(log(R_eff_loc_1_proj) + epsilon_L_proj)
    
  }
  
  
  R_eff_loc_1_vec <- c(R_eff_loc_1_proj[rows, ])
  R_eff_imp_1_vec <- c(R_eff_imp_1[rows, ])
  R_eff_imp_12_vec <- c(R_eff_imp_12[rows, ])
  R_eff_loc_12_vec <- c(R_eff_loc_12_proj[rows, ])
  
  epsilon_L_vec <- c(epsilon_L_proj[rows, ])
  epsilon_O_vec <- c(epsilon_O[rows, ])
  
  R_eff_loc_1_micro_vec <- c(R_eff_loc_1_micro_proj[rows, ])
  R_eff_loc_1_macro_vec <- c(R_eff_loc_1_macro_proj[rows, ])
  R_eff_loc_1_surv_vec <- c(R_eff_loc_1_surv_proj[rows])
  
  OC_t_state_vec <- c(de$OC_t_state)

  # make sure the seeds are the same for each type of prediction, so the samples
  # match
  set.seed(2020-06-02)
  
  # simulate from posterior for quantitities of interest
  sims <- calculate(
    R_eff_loc_1_vec,
    R_eff_imp_1_vec,
    R_eff_loc_12_vec,
    R_eff_imp_12_vec,
    epsilon_L_vec,
    epsilon_O_vec,
    R_eff_loc_1_micro_vec,
    R_eff_loc_1_macro_vec,
    R_eff_loc_1_surv_vec,
    OC_t_state_vec,
    values = draws,
    nsim = nsim
  )
  
  R_eff_loc_1_sim <- sims$R_eff_loc_1_vec
  R_eff_imp_1_sim <- sims$R_eff_imp_1_vec
  R_eff_loc_12_sim <- sims$R_eff_loc_12_vec
  R_eff_imp_12_sim <- sims$R_eff_imp_12_vec
  epsilon_L_sim <- sims$epsilon_L_vec
  epsilon_O_sim <- sims$epsilon_O_vec
  R_eff_loc_1_micro_sim <- sims$R_eff_loc_1_micro_vec
  R_eff_loc_1_macro_sim <- sims$R_eff_loc_1_macro_vec
  R_eff_loc_1_surv_sim <- sims$R_eff_loc_1_surv_vec
  OC_t_state_sim <- sims$OC_t_state_vec
  
  # Component 1 for national / state populations
  
  # microdistancing only
  plot_trend(R_eff_loc_1_micro_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = purple,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of micro-distancing",
            subtitle = expression(R["eff"]~"if"~only~"micro-distancing"~behaviour~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_micro.png", dir)

  # macrodistancing only
  plot_trend(R_eff_loc_1_macro_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = blue,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of macro-distancing",
            subtitle = expression(R["eff"]~"if"~only~"macro-distancing"~behaviour~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_macro.png", dir)
  
  # improved surveilance only
  plot_trend(R_eff_loc_1_surv_sim,
             dates = dates_type,
             multistate = FALSE,
             base_colour = yellow,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of improved surveillance",
            subtitle = expression(R["eff"]~"if"~only~surveillance~effectiveness~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_surv.png", dir, multi = FALSE)
  
  # Component 1 for national / state populations
  plot_trend(R_eff_loc_1_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = green,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of social distancing",
            subtitle = expression(Component~of~R["eff"]~due~to~social~distancing)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local.png", dir)
  
  plot_trend(R_eff_imp_1_sim,
             dates = dates_type,
             multistate = FALSE,
             base_colour = orange,
             ylim = c(0, 0.4),
             vline_at = quarantine_dates,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of quarantine of overseas arrivals",
            subtitle = expression(Component~of~R["eff"]~due~to~quarantine~of~overseas~arrivals)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_import.png", dir, multi = FALSE)
  
  # Reff for active cases
  p <- plot_trend(R_eff_loc_12_sim,
                  dates = dates_type,
                  multistate = TRUE,
                  base_colour = green,
                  vline_at = intervention_dates()$date,
                  vline2_at = projection_date) +
    ggtitle(label = "Local to local transmission potential",
            subtitle = "Average across active cases") +
    ylab(expression(R["eff"]~from~"locally-acquired"~cases))
  
  if (type == 1) {
    p <- p + annotate("rect",
                      xmin = latest_date,
                      xmax = last_mobility_date,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  p
  
  save_ggplot("R_eff_12_local.png", dir)
  
  plot_trend(R_eff_imp_12_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = orange,
             ylim = c(0, 0.4),
             vline_at = quarantine_dates,
             vline2_at = projection_date) +
    ggtitle(label = "Import to local transmission potential",
            subtitle = "Average across active cases") +
    ylab(expression(R["eff"]~from~"overseas-acquired"~cases))
  
  save_ggplot("R_eff_12_imported.png", dir)
  
  # component 2 (noisy error trends)
  p <- plot_trend(epsilon_L_sim,
                  dates = dates_type,
                  multistate = TRUE,
                  base_colour = pink,
                  hline_at = 0,
                  vline_at = intervention_dates()$date,
                  vline2_at = projection_date,
                  ylim = NULL) + 
    ggtitle(label = "Short-term variation in local to local transmission rates",
            subtitle = expression(Deviation~from~log(R["eff"])~of~"local-local"~transmission)) +
    ylab("Deviation")
  
  if (type == 1) {
    p <- p + annotate("rect",
                      xmin = latest_date,
                      xmax = last_mobility_date,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  p
  
  save_ggplot("R_eff_2_local.png", dir)
  
  plot_trend(epsilon_O_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = pink,
             hline_at = 0,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date,
             ylim = NULL) + 
    ggtitle(label = "Short-term variation in import to local transmission rates",
            subtitle = expression(Deviation~from~log(R["eff"])~of~"import-local"~transmission)) +
    ylab("Deviation")
  
  save_ggplot("R_eff_2_imported.png", dir)

  if (type == 1) {
    
    # represent simulations as matrix and lop off extra dates
    R_eff_loc_1_sim_mat <- R_eff_loc_1_sim
    dim(R_eff_loc_1_sim_mat) <- c(nsim, length(rows), n_states)
    R_eff_loc_1_sim_mat <- R_eff_loc_1_sim_mat[, seq_len(n_dates), ]
    
    # find the minimum  (over the average over states) Reff (peak of distancing)
    R_eff_mean <- apply(R_eff_loc_1_sim_mat, 2, mean)
    min_reff <- which.min(R_eff_mean)
    peak_date <- dates[min_reff]
    
    # posterior summary of R0 (same in all states, so first element)
    R0_draws <- rowMeans(R_eff_loc_1_sim_mat[, 1, ])
    cat(sprintf("\nR0 %.2f (%.2f)\n",
                mean(R0_draws),
                sd(R0_draws)))
    
    # posterior summary of R_eff for the peak of distancing
    peak_idx <- which(dates == peak_date)
    R_eff_peak_draws <- rowMeans(R_eff_loc_1_sim_mat[, peak_idx, ])
    cat(sprintf("\nminimum Reff %.2f (%.2f) on %s\n",
                mean(R_eff_peak_draws),
                sd(R_eff_peak_draws),
                format(peak_date, "%d %b")))
    
    # posterior summary of R_eff for the latest date
    last_date_idx <- which(dates == max(dates))
    R_eff_now_draws <- rowMeans(R_eff_loc_1_sim_mat[, last_date_idx, ])
    cat(sprintf("\nReff %.2f (%.2f) on %s\n",
                mean(R_eff_now_draws),
                sd(R_eff_now_draws),
                format(max(dates), "%d %b")))
    
    # covariance of these estimates
    covar <- cov(cbind(R0_draws, R_eff_peak_draws))
    cat("\nCovariance of R0 and minimum Reff:\n")
    print(covar)
    
  }
  
  # output 2000 posterior samples of R_eff for active local cases
  R_eff_12_samples <- t(R_eff_loc_12_sim[1:2000, , 1])
  colnames(R_eff_12_samples) <- paste0("sim", 1:2000)
  
  df_base <- tibble(
    date = rep(dates_type, n_states),
    state = rep(states, each = length(dates_type)),
  ) %>%
    mutate(date_onset = date + 5)
  
  # CSV of R_eff local posterior samples
  df_samples <- df_base %>%
    cbind(R_eff_12_samples)
  
  write_csv(
    df_samples,
    file.path(dir, "r_eff_12_local_samples.csv")
  )
  
  # output 2000 posterior samples of R_eff for statewide local cases
  R_eff_1_samples <- t(R_eff_loc_1_sim[1:2000, , 1])
  colnames(R_eff_1_samples) <- paste0("sim", 1:2000)
  
  df_base <- tibble(
    date = rep(dates_type, n_states),
    state = rep(states, each = length(dates_type)),
  ) %>%
    mutate(date_onset = date + 5)
  
  # CSV of R_eff local posterior samples
  df_samples <- df_base %>%
    cbind(R_eff_1_samples)
  
  write_csv(
    df_samples,
    file.path(dir, "r_eff_1_local_samples.csv")
  )
  
  # make forecasts
  if (type >= 2) {
    
    # forecast locally-acquired cases
    
    # drop last two days of case data, because detection probabilities are very low
    keep_idx <- seq_len(nrow(local_cases) - 2)
    
    
    # add constant rate of imported cases - model numbers of imports per day from
    # two weeks after the last quarantine date (mandatory hotel quarantine
    # introduced), using a poisson model with offset of the population size; add
    # those expectations to the imported cases data.
    
    
    forecast_list <- forecast_locals(local_cases = local_cases[keep_idx, ],
                                     imported_cases = imported_cases[keep_idx, ],
                                     Reff_locals = R_eff_loc_12_proj,
                                     Reff_imports = R_eff_imp_12,
                                     dates = dates_type,
                                     gi_cdf = gi_cdf,
                                     simulation_start = latest_date,
                                     gi_bounds = c(0, 20))
    
    forecast <- forecast_list$local_cases
    
    # is the probability of any new cases very small, and is it after the
    # projection period? if so sthen set the number of new cases from this point onwards to 0?
    n_forecast <- nrow(forecast) - nrow(local_cases)
    
    # in the forecasting period?
    projection_mask <- rbind(
      matrix(0,
             nrow(local_cases),
             n_states),
      matrix(1,
             n_forecast,
             n_states)
    )
    
    # small enough probbability of more cases to round to 0
    small_mask <- forecast_list$probability_of_cases < 0.01
    
    # invert this (small and forecasting gets 0)
    forecast_mask <- 1 - (projection_mask * small_mask)
    
    # tmp <- calculate(forecast_mask, values = draws, nsim = 1)[[1]][1, , ]
    
    # set all subsequent dates to 0
    forecast_mask <- apply(forecast_mask, 2, "cumprod")
    # cap forecasts by this
    forecast_capped <- forecast * forecast_mask
    
    forecast_sim <- calculate(c(forecast),
                              values = draws,
                              nsim = nsim)[[1]]
    
    forecast_capped_sim <- calculate(c(forecast_capped),
                                     values = draws,
                                     nsim = nsim)[[1]]
    
    plot_trend(forecast_sim,
               dates = dates_type,
               multistate = TRUE,
               base_colour = blue,
               hline_at = NULL,
               ylim = c(0, 200),
               vline_at = quarantine_dates,
               vline2_at = max(dates)) + 
      ggtitle(label = "Forecast numbers of locally-acquired cases") +
      ylab("New infections per day")
    
    save_ggplot("forecast.png", dir)
    
    plot_trend(forecast_sim,
               dates = dates_type,
               multistate = TRUE,
               base_colour = blue,
               hline_at = NULL,
               ylim = c(0, 10),
               vline_at = quarantine_dates,
               vline2_at = max(dates)) + 
      ggtitle(label = "Forecast numbers of locally-acquired cases") +
      ylab("New infections per day")
    
    save_ggplot("forecast_low.png", dir)
    
    plot_trend(forecast_capped_sim,
               dates = dates_type,
               multistate = TRUE,
               base_colour = blue,
               hline_at = NULL,
               ylim = c(0, 200),
               vline_at = quarantine_dates,
               vline2_at = max(dates)) + 
      ggtitle(label = "Forecast numbers of locally-acquired cases") +
      ylab("New infections per day")
    
    save_ggplot("forecast_capped.png", dir)
    
  }
  
}

# summarise the proportion of local cases assumed to have been infected by imports
expected_from_imports <- imported_infectious * R_eff_imp_12[1:n_dates, ]
expected_from_locals <- local_infectious * R_eff_loc_12[1:n_dates, ]
expected_total <- expected_from_imports + expected_from_locals
prop_from_imports <- expected_from_imports / expected_total

prop_from_imports_sim <- calculate(c(prop_from_imports),
                                   values = draws,
                                   nsim = nsim)[[1]]

mean <- colMeans(prop_from_imports_sim)
median <- apply(prop_from_imports_sim, 2, FUN = stats::median, na.rm = TRUE)
ci90 <- apply(prop_from_imports_sim, 2, quantile, c(0.05, 0.95), na.rm = TRUE)
ci50 <- apply(prop_from_imports_sim, 2, quantile, c(0.25, 0.75), na.rm = TRUE)

# CSV of proportion of locally-acquired cases that were infected by imports
prop_imports_output <- tibble(
  date = rep(dates, n_states),
  state = rep(states, each = length(dates)),
  bottom = ci90[1, ],
  top = ci90[2, ],
  lower = ci50[1, ],
  upper = ci50[2, ],
  median = median,
  mean = mean
) %>%
  mutate(date_onset = date + 5)

write_csv(
  prop_imports_output,
  "outputs/prop_local_from_imports_estimates.csv"
)

base_colour <- grey(0.5)
ggplot(prop_imports_output) +
  aes(date, mean) +
  facet_wrap(~state, ncol = 2) +
  geom_ribbon(aes(ymin = bottom,
                  ymax = top),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = 0.5) +
  geom_line(aes(y = bottom),
            colour = base_colour,
            alpha = 0.8) + 
  geom_line(aes(y = top),
            colour = base_colour,
            alpha = 0.8) +
  ggtitle("Proportion of local cases infected by imported cases") +
  ylab("Proportion") +
  xlab(element_blank()) +
  theme_minimal()

expected_from_imports <- prop_from_imports * local_cases


exp_from_imports_sim <- calculate(c(expected_from_imports), values = draws, nsim = nsim)[[1]]

mean <- colMeans(exp_from_imports_sim)
median <- apply(exp_from_imports_sim, 2, FUN = stats::median, na.rm = TRUE)
ci90 <- apply(exp_from_imports_sim, 2, quantile, c(0.05, 0.95), na.rm = TRUE)
ci50 <- apply(exp_from_imports_sim, 2, quantile, c(0.25, 0.75), na.rm = TRUE)

# CSV of proportion of locally-acquired cases that were infected by imports
exp_imports_output <- tibble(
  date = rep(dates, n_states),
  state = rep(states, each = length(dates)),
  bottom = ci90[1, ],
  top = ci90[2, ],
  lower = ci50[1, ],
  upper = ci50[2, ],
  median = median,
  mean = mean
) %>%
  mutate(date_onset = date + 5)

base_colour <- grey(0.5)
exp_imports_output %>%
  filter(date >= as.Date("2020-03-01")) %>%
ggplot() +
  aes(date, mean) +
  facet_wrap(~state, ncol = 2) +
  geom_ribbon(aes(ymin = bottom,
                  ymax = top),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = 0.5) +
  geom_line(aes(y = bottom),
            colour = base_colour,
            alpha = 0.8) + 
  geom_line(aes(y = top),
            colour = base_colour,
            alpha = 0.8) +
  ggtitle("Total number of local cases infected by imported cases") +
  ylab("Number") +
  xlab(element_blank()) +
  theme_minimal()

save_ggplot("number_of_import_local_infections.png")

# # make counterfactual predictions of case counts if quarantine had never been
# # extended to all arrivals (imports get local first-stage quarantine Reff)
# 
# # do this at national level, and start at a time when there were more cases, as
# # in the check
# 
# # Compute local cases directly caused by imports under assumption about Reff for
# # imports, then disaggregate those locally-acquired (from imports) cases
# # according to their infectiousness profile to get force of local infection
# R_eff_imp_first <- exp(log_R0 + log_q[3])
# first_locals <- imported_infectious * R_eff_imp_first
# local_infectiousness <- gi_mat %*% first_locals
# 
# # Given this basic force of infection, R for locally-acquired cases (mean trend,
# # no clusters), and the infectiousness profile, iterate the dynamics to compute
# # the numbers of local cases
# cases_basic_quarantine <- project_local_cases(
#   infectiousness = local_infectiousness,
#   R_local = R_eff_loc_1[seq_len(n_dates), ],
#   disaggregation_probs = gi_vec
# )
# 
# cases_basic_quarantine_ntnl <- rowSums(cases_basic_quarantine)
# cumul_cases_basic_quarantine_ntnl <- cumsum(cases_basic_quarantine_ntnl)
# cumul_cases_basic_quarantine_sim <- calculate(cumul_cases_basic_quarantine_ntnl,
#                                               values = draws,
#                                               nsim = 1000)[[1]]
# 
# plot_trend(cumul_cases_basic_quarantine_sim,
#            multistate = FALSE,
#            ylim = c(0, 7500),
#            hline_at = 7226,
#            dates = dates,
#            base_colour = "red",
#            vline_at = intervention_dates()$date,
#            min_date = min(dates)) +
#   ggtitle("Counterfactual: no blanket quarantine of overseas arrivals",
#           "Assumes both travel restrictions and social distancing took place") +
#   ylab("Cumulative infections")
# 
# mn <- colMeans(cumul_cases_basic_quarantine_sim)
# head(mn, 50)


# when was the minnimum of the posterior mean of component 1 and of component 2 in VIC?
minimum_dates_vic <- read_csv("outputs/r_eff_1_local_samples.csv") %>%
  mutate(reff = "1") %>%
  bind_rows(
    read_csv("outputs/r_eff_12_local_samples.csv") %>%
      mutate(reff = "12")
  ) %>%
  filter(state == "VIC") %>%
  pivot_longer(cols = starts_with("sim"),
               names_to = "sim",
               values_to = "value") %>%
  group_by(date, reff) %>%
  summarise(mean = mean(value)) %>%
  group_by(reff) %>%
  mutate(min = mean == min(mean)) %>%
  filter(min)

minimum_dates_vic
# component 1 minimum = 2020-04-13
# component 2 minimum = 2020-03-29



