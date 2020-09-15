# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

set.seed(2020-04-29)
source("R/functions.R")

# prepare data for Reff modelling
data <- reff_model_data()

# save the key dates for Freya and David to read in, and tabulated local cases
# data for the Robs
write_reff_key_dates(data)
write_local_cases(data)

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

