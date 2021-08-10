source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)

# define the model (and greta arrays) for Reff, and sample until convergence
fitted_model_voc <- fit_reff_model(data)


saveRDS(fitted_model_voc, "outputs/fitted_reff_model_voc.RDS")
# fitted_model <- readRDS("outputs/fitted_reff_model.RDS")

# visual checks of model fit
#plot_reff_checks(fitted_model_voc)

# do plots for main period
reff_plotting(fitted_model, dir = "long_figs", washout_cutoff = 5)


###
