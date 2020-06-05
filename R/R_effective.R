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

# build a matrix of inducing points, regularly spaced over time but with one on
# the most recent date
n_date_nums <- length(date_nums)
inducing_date_nums <- rev(seq(n_date_nums, 1, by = -5))
n_inducing <- length(inducing_date_nums)

# temporally correlated errors in R_eff for local cases - representing all the
# stochastic transmission dynamics in the community, such as outbreaks in
# communities with higher or lower tranmission rates.
epsilon_O <- epsilon_gp(date_nums, n_states, inducing_date_nums)
epsilon_L <- epsilon_gp(date_nums, n_states, inducing_date_nums)

# log Reff for both types
log_R_eff_loc <- log_R_eff_loc_1 + epsilon_L
log_R_eff_imp <- sweep(epsilon_O, 1, log_R_eff_imp_1, FUN = "+")

# get priors over the generation interval distribution parameters. There is some
# consensus of the SI (and GI) mean being around 4 (Nishiura et al., Du et al. EID).
# The variance of the GI is smaller than the SI, so R down would be biased up
# with the SI sd. 

gi_prior <- ganyani_gi()
# gi_mean <- normal(gi_prior$mean$est, gi_prior$mean$sd, truncation = c(0, Inf))
# gi_sd <- normal(gi_prior$sd$est, gi_prior$sd$sd, truncation = c(0, Inf))
gi_mean <- gi_prior$mean$est
gi_sd <- gi_prior$sd$est

# convert these to lognormal parameters to estimate the distribution
gi_params <- lognormal_prior(gi_mean, gi_sd)
meanlog <- gi_params$mean
sdlog <- gi_params$sd

# circulant matrix of generation interval discrete probabilities
# lower bound of 1 day so cases can't infect others on the day of infection
day_diff <- time_difference_matrix(n_dates)
day_diff[upper.tri(day_diff)] <- -1
gi_mat <- gi_probability(day_diff, meanlog, sdlog, bounds = c(0, 20))

# disaggregate imported and local cases according to the generation interval
# probabilities to get the expected number of infectious people in each state
# and time
local_infectious <- gi_mat %*% local_cases
imported_infectious <- gi_mat %*% imported_cases

# work out which elements to exclude (because there were no infectious people)
# local_infectious_sim <- calculate(local_infectious, nsim = 1)[[1]][1, , ]
# imported_infectious_sim <- calculate(imported_infectious, nsim = 1)[[1]][1, , ]
local_valid <- is.finite(local_infectious) & local_infectious > 0
import_valid <- is.finite(imported_infectious) & imported_infectious > 0
valid <- which(local_valid & import_valid, arr.ind = TRUE)

# combine everything as vectors, excluding invalid datapoints (remove invalid
# elements here, otherwise it causes a gradient issue)
log_new_from_loc_vec <- log(local_infectious[valid]) + log_R_eff_loc[1:n_dates, ][valid]
log_new_from_imp_vec <- log(imported_infectious[valid]) + log_R_eff_imp[1:n_dates, ][valid]
expected_infections_vec <- exp(log_new_from_loc_vec) + exp(log_new_from_imp_vec)

# try negative binomial likelihood
sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf), dim = n_states)
size <- 1 / sqrt(sqrt_inv_size[valid[, 2]])
prob <- 1 / (1 + expected_infections_vec / size)
distribution(local_cases[valid]) <- negative_binomial(size, prob)

m <- model(expected_infections_vec)

draws <- mcmc(
  m,
  sampler = hmc(Lmin = 10, Lmax = 15),
  chains = 10,
  one_by_one = TRUE
)
# draws <- extra_samples(draws, 1000)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_eff <- coda::effectiveSize(draws)
max(r_hats)
min(n_eff)

# check fit of obsrvation model against data 
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

# R_eff for local-local and import-local among active cases per state
# (components 1 and 2)
R_eff_loc_12 <- exp(log_R_eff_loc)
R_eff_imp_12 <- exp(log_R_eff_imp)


# check fit of projected cases against national epi curve

# national-level Reff - no clusters and weighted by state populations
local_weights <- sweep(local_infectious, 1, rowSums(local_infectious), FUN = "/")
local_weights[is.na(local_weights)] <- 1 / n_states
import_weights <- sweep(imported_infectious, 1, rowSums(imported_infectious), FUN = "/")
import_weights[is.na(import_weights)] <- 1 / n_states

R_eff_loc_ntnl <- rowSums(R_eff_loc_12[seq_len(n_dates), ] * local_weights)
R_eff_imp_ntnl <- rowSums(R_eff_imp_12[seq_len(n_dates), ] * import_weights)

# subset to from the first of March, when transmission became established (the
# model is not designed to work with the stochastic extinctions we saw at the beginning of the outbreak)
start <- which(dates == as.Date("2020-02-28"))
sub_idx <- start:n_dates

# simulate local-local transmission dynamics, at national level; forced using
# (observed) case importation and local cases prior to the start of the
# simulation

# locally-acquired infections present prior to the start of the simulation
previous_local_cases <- rowSums(local_cases)
previous_local_cases[sub_idx] <- 0
previous_local_infectiousness <- gi_mat %*% as.matrix(previous_local_cases)

# compute infectious forcing from local cases emerging during this period that
# were directly infected by imported cases (can't just include the import
# infectiousness, since they are subject to a different Reff). Get expected
# number of new local cases from imports, then disaggregate according to their
# infectiousness profile to get force of local infection

# expected number of new locally-acquired cases during the simulation period due
# to infection from imports
import_local_cases <- rowSums(imported_infectious) * R_eff_imp_ntnl[seq_len(n_dates)]
import_local_infectiousness <- gi_mat %*% import_local_cases

# combine these to get forcing from existing and import-associated local cases,
# and disaggregate to get infectiousness of these
local_infectiousness <- previous_local_infectiousness + import_local_infectiousness

# Given this basic force of infection, R for locally-acquired cases (mean trend,
# no clusters), and the infectiousness profile, iterate the dynamics to compute
# the numbers of local cases
gi_vec <- gi_probability(0:20, meanlog, sdlog, bounds = c(0, 20))
secondary_locals <- project_local_cases(
  infectiousness = local_infectiousness[sub_idx],
  R_local = R_eff_loc_ntnl[sub_idx],
  disaggregation_probs = gi_vec
)

# compute locally-acquired cases
local_cases_project_ntnl <- import_local_cases[sub_idx] + secondary_locals
local_cases_project_ntnl_sim <- calculate(local_cases_project_ntnl,
                                          values = draws,
                                          nsim = 1000)[[1]]

local_cases_ntnl <- rowSums(local_cases[sub_idx, ])
plot_trend(local_cases_project_ntnl_sim,
           multistate = FALSE,
           ylim = c(0, 3 * max(local_cases_ntnl)),
           hline_at = NULL,
           dates = dates[sub_idx],
           base_colour = "red",
           vline_at = intervention_dates()$date,
           min_date = min(dates)) +
  ggtitle("Projected national locally-acquired cases") +
  ylab("daily infections") +
  geom_line(data = data.frame(mean = local_cases_ntnl,
                              date = dates[sub_idx],
                              type = "Nowcast"))

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

  R_eff_loc_1_vec <- c(R_eff_loc_1[rows, ])
  R_eff_imp_1_vec <- c(R_eff_imp_1[rows, ])
  R_eff_imp_12_vec <- c(R_eff_imp_12[rows, ])
  R_eff_loc_12_vec <- c(R_eff_loc_12[rows, ])
  
  epsilon_L_vec <- c(epsilon_L[rows, ])
  epsilon_O_vec <- c(epsilon_O[rows, ])
  
  R_eff_loc_1_micro_vec <- c(R_eff_loc_1_micro[rows, ])
  R_eff_loc_1_macro_vec <- c(R_eff_loc_1_macro[rows, ])
  
  OC_t_state_vec <- c(de$OC_t_state)
  
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
  OC_t_state_sim <- sims$OC_t_state_vec

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

  if (type == 1) {
    # non-household contacts estimated from Freya's surveys
    freya_survey <- freya_survey_results() %>%
      mutate(type = "Nowcast") %>%
      mutate(lower = estimate - sd * 1.96,
             upper = estimate + sd * 1.96)
    
    # non-household contacts
    plot_trend(OC_t_state_sim,
               dates = dates_type,
               multistate = TRUE,
               base_colour = purple,
               vline_at = intervention_dates()$date,
               ylim = c(0, 15),
               hline_at = NULL,
               vline2_at = projection_date) + 
      ggtitle(label = "Macro-distancing trend",
              subtitle = "Rate of non-household contacts") +
      ylab("Estimated number of non-household contacts per day") + 
      
      # add survey results
      geom_point(
        aes(date, estimate),
        data = freya_survey,
        size = 0.5,
        colour = grey(0.5)
      ) +
      
      geom_errorbar(
        aes(
          date,
          estimate,
          ymin = lower,
          ymax = upper
        ),
        data = freya_survey,
        width = 0,
        colour = grey(0.5)
      )
    
    ggsave(file.path(dir, "figures/macrodistancing_effect.png"),
           width = multi_width,
           height = multi_height,
           scale = 0.8)
    
    # print out the estimates at peak and waning
    cols <- ncol(OC_t_state_sim) / n_states
    dim(OC_t_state_sim) <- c(nsim, cols, n_states)
    OC_t_state_means <- apply(OC_t_state_sim, 2:3, FUN = "mean")
    overall_mean <- rowMeans(OC_t_state_means)
    peak <- which.min(overall_mean)
    peak_OC <- OC_t_state_means[peak, ]
    recent_OC <- OC_t_state_means[nrow(OC_t_state_means), ]
    
    cat(sprintf("\nminimum Reff %.2f (%.2f in %s to %.2f in  %s) on %s\n",
                mean(peak_OC),
                min(peak_OC),
                states[which.min(peak_OC)],
                max(peak_OC),
                states[which.max(peak_OC)],
                format(dates[peak], "%d %b")))
    
    cat(sprintf("\nlatest Reff %.2f (%.2f in %s to %.2f in %s)",
                mean(recent_OC),
                min(recent_OC),
                states[which.min(recent_OC)],
                max(recent_OC),
                states[which.max(recent_OC)]))
    
  }
  
  # Component 1 for national / state populations
  plot_trend(R_eff_loc_1_micro_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = purple,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of micro-distancing",
            subtitle = expression(R["eff"]~"if"~only~"micro-distancing"~behaviour~had~changed)) +
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
            subtitle = expression(R["eff"]~"if"~only~"macro-distancing"~behaviour~had~changed)) +
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
  
  # Reff for active cases
  plot_trend(R_eff_loc_12_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = green,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) +
    ggtitle(label = "Local to local transmission potential",
            subtitle = "Average across active cases") +
    ylab(expression(R["eff"]~from~"locally-acquired"~cases))
  
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
            subtitle = "Average across active cases") +
    ylab(expression(R["eff"]~from~"overseas-acquired"~cases))
  
  ggsave(file.path(dir, "figures/R_eff_12_imported.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
  # component 3 (noisy error trends)
  plot_trend(epsilon_L_sim,
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
  
  ggsave(file.path(dir, "figures/R_eff_2_local.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
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
  
  ggsave(file.path(dir, "figures/R_eff_2_imported.png"),
         width = multi_width,
         height = multi_height,
         scale = 0.8)
  
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


# make counterfactual predictions of case counts if quarantine had never been
# extended to all arrivals (imports get local first-stage quarantine Reff)

# Compute local cases directly caused by imports under assumption about Reff for
# imports, then disaggregate those locally-acquired (from imports) cases
# according to their infectiousness profile to get force of local infection
R_eff_imp_first <- exp(log_R0 + log_q[3])
first_locals <- imported_infectious * R_eff_imp_first
local_infectiousness <- gi_mat %*% first_locals

# Given this basic force of infection, R for locally-acquired cases (mean trend,
# no clusters), and the infectiousness profile, iterate the dynamics to compute
# the numbers of local cases
gi_vec <- gi_probability(0:20, meanlog, sdlog)
cases_basic_quarantine <- project_local_cases(
  infectiousness = local_infectiousness,
  R_local = R_eff_loc_1[seq_len(n_dates), ],
  disaggregation_probs = gi_vec
)

# why is this so big? are cases being counted multiple times?
# is the SI distribution somehow being shortened?
# are infectiousness and cases being put in the wrong way round?

cases_basic_quarantine_ntnl <- rowSums(cases_basic_quarantine)
cumul_cases_basic_quarantine_ntnl <- cumsum(cases_basic_quarantine_ntnl)
cumul_cases_basic_quarantine_sim <- calculate(cumul_cases_basic_quarantine_ntnl,
                                              values = draws,
                                              nsim = 1000)[[1]]

plot_trend(cumul_cases_basic_quarantine_sim,
           multistate = FALSE,
           ylim = c(0, 7500),
           hline_at = 7226,
           dates = dates,
           base_colour = "red",
           vline_at = intervention_dates()$date,
           min_date = min(dates)) +
  ggtitle("Counterfactual: no blanket quarantine of overseas arrivals",
          "Assumes both travel restrictions and social distancing took place") +
  ylab("Cumulative infections")

mn <- colMeans(cumul_cases_basic_quarantine_sim)
head(mn, 50)

