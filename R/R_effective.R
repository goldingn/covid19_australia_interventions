# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

set.seed(2020-04-29)
source("R/functions.R")

# prepare the output directories, in a separate directory if testing something
output_directories <- get_output_directories(staging = FALSE)

# load NNDSS linelist
linelist <- load_linelist()

# load modelled google mobility trends
google_change_data <- readRDS("outputs/google_change_trends.RDS")

# prepare data for Reff modelling
data <- reff_model_data(linelist, google_change_data)

# save the key dates for Freya and David to read in, and tabulated local cases
# data for the Robs
write_reff_key_dates(data)
write_local_cases(data)

# define the model (and greta arrays) for Reff, and sample until convergence
fitted_model <- fit_reff_model(model, data)

# save
saveRDS(fitted_model, "outputs/fitted_reff_model.RDS")
# fit_reff_model should take in data and return a fitted_model object (module(draws, model, data)), and everything else should take in a fitted model object

fitted_model <- readRDS("outputs/fitted_reff_model.RDS")
# model <- fitted_model$model
# data <- fitted_model$data
# draws <- fitted_model$draws
# greta_model <- model$greta_model
# greta_arrays <- model$greta_arrays
# fitted_model <-  module(greta_model, greta_arrays, data, draws)

# output Reff trajectory draws
write_reff_sims(fitted_model, dir = "outputs/projection/staging")

# visual checks of model fit
plot_reff_checks(fitted_model)

# do plots for main period
reff_plotting(fitted_model, dir = "outputs/staging")

# and for projected part
reff_plotting(fitted_model,
              dir = "outputs/projection/staging",
              max_date = fitted_model$data$dates$latest_project,
              mobility_extrapolation_rectangle = FALSE,
              projection_date = fitted_model$data$dates$latest_mobility)

