# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

set.seed(2020-04-29)
source("R/functions.R")

# sync up the case data
sync_nndss()

# prepare data for Reff modelling
data <- reff_model_data()

data$dates$linelist

# save the key dates for Freya and David to read in, and tabulated local cases
# data for the Robs
write_reff_key_dates(data)
write_local_cases(data)

# format and write out any new linelists to the past_cases folder for Rob H
update_past_cases()

# define the model (and greta arrays) for Reff, and sample until convergence
fitted_model <- fit_reff_model(data)

# save the fitted model object
saveRDS(fitted_model, "outputs/fitted_reff_model.RDS")
# fitted_model <- readRDS("outputs/fitted_reff_model.RDS")

# output Reff trajectory draws for Rob M
write_reff_sims(fitted_model, dir = "outputs/projection")
  
# visual checks of model fit
plot_reff_checks(fitted_model)

# do plots for main period
reff_plotting(fitted_model, dir = "outputs")

# and for projected part
reff_plotting(fitted_model,
              dir = "outputs/projection",
              max_date = fitted_model$data$dates$latest_project,
              mobility_extrapolation_rectangle = FALSE,
              projection_date = fitted_model$data$dates$latest_mobility)

# model C1 under UK strain, under two modelled estimates of relative transmissability

# Imperial estimate with a long GI (6.5 days); 50-75%. Assuming centred at 62.5%.
imperial_fitted_model <- multiply_reff(fitted_model, 1.625, c(1.5, 1.75))
imperial_dir <- "outputs/projection/b117_imperial_long"
dir.create(imperial_dir, showWarnings = FALSE)
write_reff_sims(imperial_fitted_model, imperial_dir)

# LSHTM estimate with a short GI (mean 3.6); 31% (27%-34%)
lshtm_fitted_model <- multiply_reff(fitted_model, 1.31, c(1.27, 1.34))
lshtm_dir <- "outputs/projection/b117_lshtm_short"
dir.create(lshtm_dir, showWarnings = FALSE)
write_reff_sims(lshtm_fitted_model, lshtm_dir)

# Results with Oz analysis - use estimate of the relative per-unit-contact-time
# infection probability and reconstruct component 1 timeseries
phi <- normal(0.852, 0.021, truncation = c(0, Inf))

dates <- data$dates$mobility
de <- fitted_model$greta_arrays$distancing_effect
infectious_days <- infectious_period(gi_cdf)
h_t <- h_t_state(dates)
HD_t <- de$HD_0 * h_t
p_star <- 1 - (1 - de$p) ^ phi

household_infections <- de$HC_0 * (1 - p_star ^ HD_t)
non_household_infections <- de$OC_t_state * de$gamma_t_state *
  infectious_days * (1 - p_star ^ de$OD_0)
R_t <- household_infections + non_household_infections
R_eff_loc_1_no_surv <- extend(R_t, data$n_dates_project)

# multiply by the surveillance effect to get component 1
surveillance_reff_local_reduction <- surveillance_effect(
  dates = data$dates$infection_project,
  cdf = gi_cdf,
  states = data$states
)

oz_fitted_model <- fitted_model
oz_fitted_model$greta_arrays$R_eff_loc_1 <- R_eff_loc_1_no_surv * surveillance_reff_local_reduction

oz_dir <- "outputs/projection/b117_oz_style"
dir.create(oz_dir, showWarnings = FALSE)
write_reff_sims(oz_fitted_model, oz_dir, write_reff_12 = FALSE)

# also calculate and write out the equivalent multiplicative factor over time
ratio <- oz_fitted_model$greta_arrays$R_eff_loc_1 / fitted_model$greta_arrays$R_eff_loc_1
ratio_vec <- c(ratio)
ratio_sims <- calculate(ratio_vec, values = fitted_model$draws, nsim = 2000)
ratio_samples <- t(ratio_sims[[1]][, , 1])
colnames(ratio_samples) <- paste0("sim", 1:2000)

tibble(
  date = rep(fitted_model$data$dates$infection_project, fitted_model$data$n_states),
  state = rep(fitted_model$data$states, each = fitted_model$data$n_dates_project),
) %>%
  mutate(date_onset = date + 5) %>%
  cbind(ratio_samples) %>%
  write_csv(
    file.path(oz_dir, "r_eff_1_ratio_samples.csv")
  )

read_csv("outputs/projection/b117_oz_style/r_eff_1_local_samples.csv") %>%
  filter(date == as.Date("2020-04-11")) %>%
  pivot_longer(
    cols = starts_with("sim"),
    names_to = "sim"
  ) %>%
  group_by(state, date) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    p_exceedance = mean(value > 1)
  )

