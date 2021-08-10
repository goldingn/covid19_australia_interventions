# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

source("R/lib.R")
source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)
source("./objects_and_settings.R")

set.seed(2020-04-29)

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

timeseries <- readRDS(file = "outputs/vaccine_timeseries.RDS")

# do plots for main period
reff_plotting(fitted_model, dir = "outputs")

# most recent six months
reff_plotting(
  fitted_model,
  dir = "outputs",
  subdir = "figures/six_month",
  min_date = NA
)


reff_plotting(
  fitted_model,
  dir = "outputs/projection",
  max_date = fitted_model$data$dates$latest_project,
  mobility_extrapolation_rectangle = FALSE,
  projection_date = fitted_model$data$dates$latest_mobility
)

# and for projected part
reff_plotting(
  fitted_model,
  dir = "outputs/projection",
  subdir = "figures/six_month",
  min_date = NA,
  max_date = fitted_model$data$dates$latest_project,
  mobility_extrapolation_rectangle = FALSE,
  projection_date = fitted_model$data$dates$latest_mobility
)


# produce simulations where proportion VOC is zero throughout period for reporting
# in common operating picture table
simulate_variant(variant = "wt")
simulate_variant(variant = "alpha")
simulate_variant(variant = "delta")
