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

# define the model (and greta arrays) for Reff, and sample until adequate convergence
model <- reff_model(data)
draws <- fit_reff_model(model)

# save these objects
write_fitted_reff(model, draws) 
  
object <- readRDS("outputs/fitted_reff.RDS")
model <- object$model
draws <- object$draws

# output Reff trajectory draws
write_reff_sims(draws, model, dir = "outputs/projection/staging")

# visual checks of model fit
plot_reff_ppc_checks(draws, model)

# check fit of projected cases against national epi curve
check_projection(draws, model)

# do plots for main period
reff_plotting(draws, model, dir = "outputs/staging")

# and for projected part
reff_plotting(draws,
              model,
              dir = "outputs/projection/staging",
              max_date = model$data$dates$latest_project,
              mobility_extrapolation_rectangle = FALSE,
              projection_date = data$dates$latest_mobility)

# model$data$n_dates_project <- model$data$n_date_nums
# model$data$dates$infection_project <- model$data$dates$earliest + model$data$dates$date_nums - 1
# model$data$dates$latest_project <- max(model$data$dates$infection_project)
