# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

# see the manuscript for an exaplanation of the model that may or may not be out
# of date.

library(dplyr)
library(readr)
library(tidyr)
library(RColorBrewer)
source("R/functions.R")

set.seed(2020-04-29)
linelist <- readRDS("~/not_synced/nnds/linelist_formatted.RDS")

# impute with the mean delay on the date of onset for those missing it
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

library(greta.gp)

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
distancing_effect <- distancing_effect_model(mobility_dates)

# pull out R_t component due to distancing for locally-acquired cases, and
# extend to correct length
extend_idx <- pmin(seq_along(date_nums), nrow(distancing_effect$R_t))
R_eff_loc_1 <- distancing_effect$R_t[extend_idx, ]
log_R_eff_loc_1 <- log(R_eff_loc_1)

# extract R0 from this model and estimate R_t component due to quarantine for
# overseas-acquired cases
log_R0 <- log_R_eff_loc_1[1, 1]
log_R_eff_imp_1 <- log_R0 + log_Qt
R_eff_imp_1 <- exp(log_R_eff_imp_1)

# temporally correlated errors in R_eff for local cases - representing all the
# stochastic transmission dynamics in the community, such as outbreaks in
# communities with higher or lower tranmission rates.

# variation in R_eff for locally-acquired cases over time
alpha_time_noise_L <- lognormal(2, 0.5)
lengthscale_time_noise_L <- lognormal(2, 0.5)
sigma_time_noise_L <- normal(0, 0.25, truncation = c(0, Inf))

time_noise_kernel_L <- rational_quadratic(
  lengthscales = lengthscale_time_noise_L,
  variance = sigma_time_noise_L ^ 2,
  alpha = alpha_time_noise_L)

# combine noise kernels
noise_kernel_L <- time_noise_kernel_L #+ iid_noise_kernel_L

# for for local-local cases, use smooth term for state-level trends, for
# import-local, use random intercepts
lengthscale_time_signal_L <- lognormal(4, 0.5)
sigma_time_signal_L <- normal(0, 0.25, truncation = c(0, Inf))

signal_kernel_L  <- rbf(
  lengthscales = lengthscale_time_signal_L,
  variance = sigma_time_signal_L ^ 2
)

sigma_bias_O <- normal(0, 0.5, truncation = c(0, Inf))
signal_kernel_O <- bias(sigma_bias_O ^ 2)

kernel_L <- signal_kernel_L + noise_kernel_L
kernel_O <- signal_kernel_O

# variation in the Reff timeseries between the states and territories
sigma_state_L <- normal(0, 0.25, truncation = c(0, Inf), dim = n_states)
sigma_state_O <- normal(0, 0.5, truncation = c(0, Inf), dim = n_states)

# build a matrix of inducing points, regularly spaced over time but with one on
# the most recent date
n_date_nums <- length(date_nums)
inducing_date_nums <- rev(seq(n_date_nums, 1, by = -7))
n_inducing <- length(inducing_date_nums)

# evaluate epsilon GP for imports, with different error variance for each state
v_O_raw <- normal(0, 1, dim = c(n_inducing, n_states))
v_O <- sweep(v_O_raw, 2, sigma_state_O, FUN = "*")

epsilon_O <- multi_gp(
  x = date_nums,
  v = v_O,
  kernel = kernel_O,
  inducing = inducing_date_nums,
  tol = 1e-6
)

# and for locals, with different variances
v_L_raw <- normal(0, 1, dim = c(n_inducing, n_states))
v_L <- sweep(v_L_raw, 2, sigma_state_L, FUN = "*")

epsilon_L <- multi_gp(
  x = date_nums,
  v = v_L,
  kernel = kernel_L,
  inducing = inducing_date_nums,
  tol = 1e-6
)

# disaggregate imported and local cases according to the serial interval
# probabilities to get the expected number of infectious people in each state
# and time. Fixing the SI parameters at their prior means for now
local_infectious <- apply_serial_interval(local_cases, fixed = TRUE) 
imported_infectious <- apply_serial_interval(imported_cases, fixed = TRUE) 

# combine everything as vectors
log_loc_infectious <- log(local_infectious)
log_imp_infectious <- log(imported_infectious)

# work out which ones to exclude (because there were no infectious people)
valid <- which(is.finite(log_loc_infectious + log_imp_infectious))

log_R_eff_loc <- log_R_eff_loc_1 + epsilon_L
log_R_eff_imp <- sweep(epsilon_O, 1, log_R_eff_imp_1, FUN = "+") 

log_new_from_loc_vec <- log_loc_infectious[valid] + log_R_eff_loc[1:n_dates, ][valid]
log_new_from_imp_vec <- log_imp_infectious[valid] + log_R_eff_imp[1:n_dates, ][valid]
expected_infections_vec <- exp(log_new_from_loc_vec) + exp(log_new_from_imp_vec)

# try negative binomial likelihood
valid <- which(is.finite(log_loc_infectious + log_imp_infectious), arr.ind = TRUE)
sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf), dim = n_states)
size <- 1 / sqrt(sqrt_inv_size[valid[, 2]])
prob <- 1 / (1 + expected_infections_vec / size)
distribution(local_cases[valid]) <- negative_binomial(size, prob)

# add correlated errors on the quarantine model too

m <- model(expected_infections_vec)
draws <- mcmc(
  m,
  sampler = hmc(Lmin = 20, Lmax = 25),
  chains = 10,
  one_by_one = TRUE
)
# draws <- extra_samples(draws, 1000, one_by_one = TRUE)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_eff <- coda::effectiveSize(draws)
max(r_hats)
min(n_eff)

# check fit
nsim <- coda::niter(draws) * coda::nchain(draws)
nsim <- min(10000, nsim)
cases <- negative_binomial(size, prob)
cases_sim <- calculate(cases, values = draws, nsim = nsim)[[1]][, , 1]

# overall PPC check
bayesplot::ppc_ecdf_overlay(
  local_cases[valid],
  cases_sim,
  discrete = TRUE
)

# check by state and time
plot_fit(local_cases[valid], cases_sim, valid)

# split epsilons into signal (2) and noise (3) Reff components
epsilon_L_2 <- project(epsilon_L, x_new = date_nums, kernel = signal_kernel_L)
epsilon_L_3 <- project(epsilon_L, x_new = date_nums, kernel = noise_kernel_L)
epsilon_O_2 <- project(epsilon_O, x_new = date_nums, kernel = signal_kernel_O)

# R_eff for local-local and import-local among state populations
# (components 1 and 2)
log_R_eff_loc_12 <- log_R_eff_loc_1 + epsilon_L_2 
R_eff_loc_12 <- exp(log_R_eff_loc_12)
log_R_eff_imp_12 <- sweep(epsilon_O_2, 1, log_R_eff_imp_1, FUN = "+") 
R_eff_imp_12 <- exp(log_R_eff_imp_12)

# R_eff ofor local-local and import-local among active cases per state
# (components 1, 2, and 3)
R_eff_loc_123 <- exp(log_R_eff_loc)
R_eff_imp_123 <- exp(log_R_eff_imp)


# Reff local component one under only micro- and only macro-distancing
de <- distancing_effect

infectious_days <- infectious_period()

household_infections_micro <- de$HC_0 * (1 - de$p ^ de$HD_0)
non_household_infections_micro <- de$OC_0 * infectious_days *
  (1 - de$p ^ de$OD_0) * de$gamma_t_state
hourly_infections_micro <- household_infections_micro +
  non_household_infections_micro
R_eff_loc_1_micro <- hourly_infections_micro
R_eff_loc_1_micro <- R_eff_loc_1_micro[extend_idx, ]

h_t <- h_t_state(mobility_dates)
HD_t <- de$HD_0 * h_t
household_infections_macro <- de$HC_0 * (1 - de$p ^ HD_t)
non_household_infections_macro <- de$OC_t_state * infectious_days * (1 - de$p ^ de$OD_0)
hourly_infections_macro <- household_infections_macro + non_household_infections_macro
R_eff_loc_1_macro <- hourly_infections_macro
R_eff_loc_1_macro <- R_eff_loc_1_macro[extend_idx, ]

# make 4 different versions of the plots and outputs:
# 1. up to latest date of infection
# 2. up to June 8
# 3. up to June 8, with increase in mean Reff to 1.1 after May 11
# 3. up to June 8, with increase in lower bound of Reff to 1.1 after May 11

output_directories <- c("",
                        "projection",
                        "counterfactual_1",
                        "counterfactual_2",
                        "counterfactual_3")

for (type in 1:5) {
  
  dir <- file.path("outputs", output_directories[type])
  
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

  
  R_eff_loc_1_type_vec <- c(R_eff_loc_1[rows, ])
  R_eff_imp_1_type_vec <- c(R_eff_imp_1[rows, ])
  R_eff_imp_12_vec_type <- c(R_eff_imp_12[rows, ])
  R_eff_loc_12_vec_type <- c(R_eff_loc_12[rows, ])
  R_eff_imp_123_vec_type <- c(R_eff_imp_123[rows, ])
  R_eff_loc_123_vec_type <- c(R_eff_loc_123[rows, ])
  
  epsilon_L_2_vec_type <- c(epsilon_L_2[rows, ])
  epsilon_L_3_vec_type <- c(epsilon_L_3[rows, ])
  epsilon_O_2_vec_type <- c(epsilon_O_2[rows, ])
  # epsilon_O_3_vec_type <- c(epsilon_O_3[rows, ])
  
  R_eff_loc_1_micro_type_vec <- c(R_eff_loc_1_micro[rows, ])
  R_eff_loc_1_macro_type_vec <- c(R_eff_loc_1_macro[rows, ])
  
  # simulate from posterior for quantitities of interest
  sims <- calculate(
    R_eff_loc_1_type_vec,
    R_eff_imp_1_type_vec,
    R_eff_loc_12_vec_type,
    R_eff_imp_12_vec_type,
    R_eff_loc_123_vec_type,
    R_eff_imp_123_vec_type,
    epsilon_L_2_vec_type,
    epsilon_L_3_vec_type,
    epsilon_O_2_vec_type,
    # epsilon_O_3_vec_type,
    R_eff_loc_1_micro_type_vec,
    R_eff_loc_1_macro_type_vec,
    values = draws,
    nsim = nsim
  )
  
  R_eff_loc_1_sim <- sims$R_eff_loc_1_type_vec
  R_eff_imp_1_sim <- sims$R_eff_imp_1_type_vec
  R_eff_loc_12_sim <- sims$R_eff_loc_12_vec_type
  R_eff_imp_12_sim <- sims$R_eff_imp_12_vec_type
  R_eff_loc_123_sim <- sims$R_eff_loc_123_vec_type
  R_eff_imp_123_sim <- sims$R_eff_imp_123_vec_type
  epsilon_L_2_sim <- sims$epsilon_L_2_vec_type
  epsilon_L_3_sim <- sims$epsilon_L_3_vec_type
  epsilon_O_2_sim <- sims$epsilon_O_2_vec_type
  # epsilon_O_3_sim <- sims$epsilon_O_3_vec_type
  R_eff_loc_1_micro_sim <- sims$R_eff_loc_1_micro_type_vec
  R_eff_loc_1_macro_sim <- sims$R_eff_loc_1_macro_type_vec

  dates_type <- min(dates) - 1 + seq_along(rows)
  
  # for counterfactuals, relevel the R0s after calculating them
  if (type > 2) {
    
    counterfactual_Reff <- switch(as.character(type),
                                  "3" = 1.1,
                                  "4" = 1.2,
                                  "5" = 1.5)
    
    change_date <- last_mobility_date + 1
    dates_long <- rep(dates_type, n_states)
    projected_dates_long <- dates_long >= change_date
    projected_dates <- dates_type >= change_date
    
    mean_Reff <- mean(R_eff_loc_1_sim[, projected_dates, ])
    add_Reff <- counterfactual_Reff - mean_Reff
    R_eff_loc_1_sim[, projected_dates, ] <- R_eff_loc_1_sim[, projected_dates, ] + add_Reff
    R_eff_loc_12_sim[, projected_dates, ] <- R_eff_loc_12_sim[, projected_dates, ] + add_Reff
    R_eff_loc_123_sim[, projected_dates, ] <- R_eff_loc_123_sim[, projected_dates, ] + add_Reff
  }
  
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

  # Component 1 for national / state populations
  plot_trend(R_eff_loc_1_micro_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = purple,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of micro-distancing",
            subtitle = expression(Component~of~R["eff"]~due~to~"micro-distancing")) +
    ylab(expression(R["eff"]~component))
  
  ggsave(file.path(dir, "figures/R_eff_1_local_micro.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  # Component 1 for national / state populations
  plot_trend(R_eff_loc_1_macro_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = blue,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of macro-distancing",
            subtitle = expression(Component~of~R["eff"]~due~to~"macro-distancing")) +
    ylab(expression(R["eff"]~component))
  
  ggsave(file.path(dir, "figures/R_eff_1_local_macro.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  
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
  
  ggsave(file.path(dir, "figures/R_eff_1_local.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  plot_trend(R_eff_imp_1_sim,
             dates = dates_type,
             multistate = FALSE,
             base_colour = orange,
             vline_at = quarantine_dates,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of quarantine of overseas arrivals",
            subtitle = expression(Component~of~R["eff"]~due~to~quarantine~of~overseas~arrivals)) +
    ylab(expression(R["eff"]~component))
  
  ggsave(file.path(dir, "figures/R_eff_1_import.png"),
         width = panel_width,
         height = panel_height * 1.25,
         scale = 1)
  
  # R_eff for state populations
  plot_trend(R_eff_loc_12_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = green,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Local to local transmission potential",
            subtitle = "Average across the population") +
    ylab(expression(R["eff"]~of~"local-local"~transmission))
  
  ggsave(file.path(dir, "figures/R_eff_12_local.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  plot_trend(R_eff_imp_12_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = orange,
             vline_at = quarantine_dates,
             vline2_at = projection_date) + 
    ggtitle(label = "Import to local transmission potential",
            subtitle = "Average across the population") +
    ylab(expression(R["eff"]~of~"import-local"~transmission))
  
  ggsave(file.path(dir, "figures/R_eff_12_import.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  # Reff for active cases
  plot_trend(R_eff_loc_123_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = green,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) +
    ggtitle(label = "Local to local transmission potential",
            subtitle = "Average across active cases") +
    ylab(expression(R["eff"]~from~"locally-acquired"~cases))
  
  ggsave(file.path(dir, "figures/R_eff_123_local.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  plot_trend(R_eff_imp_123_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = orange,
             vline_at = quarantine_dates,
             vline2_at = projection_date) +
    ggtitle(label = "Import to local transmission potential",
            subtitle = "Average across active cases") +
    ylab(expression(R["eff"]~from~"overseas-acquired"~cases))
  
  ggsave(file.path(dir, "figures/R_eff_123_imported.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  

  # component 2 (smooth error trends)
  plot_trend(epsilon_L_2_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = pink,
             hline_at = 0,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date,
             ylim = NULL) + 
    ggtitle(label = "Long-term variation in local to local transmission rates",
            subtitle = expression(Deviation~from~log(R["eff"])~of~"local-local"~transmission)) +
    ylab("Deviation")
  
  ggsave(file.path(dir, "figures/R_eff_2_local.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  plot_trend(epsilon_O_2_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = pink,
             hline_at = 0,
             vline_at = quarantine_dates,
             vline2_at = projection_date,
             ylim = NULL) + 
    ggtitle(label = "Long-term variation in import to local transmission rates",
            subtitle = expression(Deviation~from~log(R["eff"])~of~"import-local"~transmission)) +
    ylab("Deviation")
  
  ggsave(file.path(dir, "figures/R_eff_2_import.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  
  # component 3 (noisy error trends)
  plot_trend(epsilon_L_3_sim,
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
  
    ggsave(file.path(dir, "figures/R_eff_3_local.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  if (type == 1) {
    
    # posterior summary of R0
    R0_draws <- R_eff_loc_1_sim[, 1, 1]
    cat(sprintf("\nR0 %.2f (%.2f)\n",
                  mean(R0_draws),
                  sd(R0_draws)))
    
    # posterior summary of R_eff for the peak of distancing
    peak_date <- as.Date("2020-04-11")
    peak_idx <- which(dates == peak_date)
    R_eff_peak_draws <- R_eff_loc_1_sim[, peak_idx, 1]
    cat(sprintf("\nminimum Reff %.2f (%.2f) on %s\n",
                mean(R_eff_peak_draws),
                sd(R_eff_peak_draws),
                format(peak_date, "%d %b")))
    
    # posterior summary of R_eff for the latest date
    R_eff_now_draws <- R_eff_loc_1_sim[, ncol(R_eff_loc_1_sim), 1]
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
  R_eff_123_samples <- t(R_eff_loc_123_sim[1:2000, , 1])
  colnames(R_eff_123_samples) <- paste0("sim", 1:2000)

  df_base <- tibble(
    date = rep(dates_type, n_states),
    state = rep(states, each = length(dates_type)),
  ) %>%
    mutate(date_onset = date + 5)
  
  # CSV of R_eff local posterior samples
  df_samples <- df_base %>%
    cbind(R_eff_123_samples)
  
  write_csv(
    df_samples,
    file.path(dir, "r_eff_123_local_samples.csv")
  )
  
  
  # output 2000 posterior samples of R_eff for statewide local cases
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
  
}

# summarise the proportion of local cases assumed to have been infected by imports
expected_from_imports <- imported_infectious * R_eff_imp_123[1:n_dates, ]
expected_from_locals <- local_infectious * R_eff_loc_123[1:n_dates, ]
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
ggplot(exp_imports_output) +
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




# # posterior summary of R_eff for the peak of distancing
# peak_distancing <- max(which(waning_index == 0))
# R_eff_peak_draws <- R_eff_loc_1_sim[, peak_distancing, 1]
# mean(R_eff_peak_draws)
# sd(R_eff_peak_draws)
# min(dates) + peak_distancing - 1

# # summary of the inferred waning amount
# waning_amount_draws <- calculate(waning_amount, values = draws, nsim = 20000)[[1]][, 1, 1]
# mean(waning_amount_draws * -100)
# sd(waning_amount_draws * -100)
# waning_amount_params
# 
# 
# post_intervention_start <- as.numeric(max(intervention_dates()$date) - dates[1])
# sum(local_infectious[post_intervention_start:n_dates, ])
# sum(local_cases[post_intervention_start:n_dates, ])

# - add a custom greta function for lognormal CDF (using TFP) and try
#   sampling with that
