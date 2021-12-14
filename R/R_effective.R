# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

source("R/lib.R")

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
#update_past_cases()

# define the model (and greta arrays) for Reff, and sample until convergence
fitted_model <- fit_reff_model(data)

# save the fitted model object
saveRDS(fitted_model, "outputs/fitted_reff_model.RDS")
# fitted_model <- readRDS("outputs/fitted_reff_model.RDS")


# visual checks of model fit
plot_reff_checks(fitted_model)


# output Reff trajectory draws for Rob M
write_reff_sims(fitted_model, dir = "outputs/projection")


vaccine_effect_timeseries <- readRDS(file = "outputs/vaccine_effect_timeseries.RDS")

# write sims of C1 without vaccine effect
write_reff_sims_novax(
  fitted_model,
  vaccine_timeseries = vaccine_effect_timeseries
)

# generatge sims for plotting
# (saves repeat generation of sims in each reff_plotting call and keeps them consistent)
sims <- reff_plotting_sims(fitted_model)

# do plots for main period
reff_plotting(
  fitted_model,
  dir = "outputs",
  sims = sims
)

# most recent six months
reff_plotting(
  fitted_model,
  dir = "outputs",
  subdir = "figures/six_month",
  min_date = NA,
  sims = sims
)

# most recent month
reff_plotting(
  fitted_model,
  dir = "outputs",
  subdir = "figures/one_month",
  min_date = fitted_model$data$dates$latest_mobility - months(1),
  sims = sims
)


# projection plots 
reff_plotting(
  fitted_model,  
  dir = "outputs/projection",
  max_date = fitted_model$data$dates$latest_project,
  mobility_extrapolation_rectangle = FALSE,
  projection_date = fitted_model$data$dates$latest_mobility,
  sims = sims
)

# 6-month projection plots
reff_plotting(
  fitted_model,
  dir = "outputs/projection",
  subdir = "figures/six_month",
  min_date = NA,
  max_date = fitted_model$data$dates$latest_project,
  mobility_extrapolation_rectangle = FALSE,
  projection_date = fitted_model$data$dates$latest_mobility,
  sims = sims
)


# produce simulations where proportion of variant is constant
simulate_variant(variant = "wt")
simulate_variant(variant = "alpha")
simulate_variant(variant = "delta")
