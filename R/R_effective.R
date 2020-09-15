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

# visual checks of model fit
plot_reff_ppc_checks(draws, model)

# check fit of projected cases against national epi curve
check_projection(draws, model)

# add counterfactuals to the model object: Reff for locals component 1 under
# only micro/macro/surveillance improvements
model$greta_arrays <- c(
  model$greta_arrays,
  list(
    R_eff_loc_1_macro = reff_1_only_macro(model),
    R_eff_loc_1_micro = reff_1_only_micro(model),
    R_eff_loc_1_surv = reff_1_only_surveillance(model)
  ) 
)

# model$data$n_dates_project <- model$data$n_date_nums
# model$data$dates$infection_project <- model$data$dates$earliest + model$data$dates$date_nums - 1

# function to calculate, plot, and save all the outputs (with flags for plot
# types) - pass in an optional maximum date argument
reff_plotting <- function(
  draws,
  model,
  dir = "outputs",
  latest_date = model$data$dates$latest_mobility,
  projection_date = NA
) {
  
  # flatten all relevant greta array matrices to vectors before calculating
  vector_list <- lapply(
    model$greta_arrays[
      c(
        "R_eff_loc_1",
        "R_eff_imp_1",
        "R_eff_loc_12",
        "R_eff_imp_12",
        "epsilon_L",
        "epsilon_O",
        "R_eff_loc_1_micro",
        "R_eff_loc_1_macro",
        "R_eff_loc_1_surv"
      )
    ],
    c
  )
  
  # make sure the seeds are the same for each type of prediction, so the samples
  # match
  set.seed(2020-06-02)
  
  # simulate from posterior for quantitities of interest
  args <- c(vector_list, list(values = draws, nsim = 10000))
  sims <- do.call(calculate, args)

  
}


# need to do away with dates_type, and set the x axis date limits instead
write_reff_sims(draws, model, dir = "outputs/projection/staging")





types <- seq_along(output_directories)

for (type in types) {
  
  dir <- output_directories[type]
  
  # subset or extend projections based on type of projection
  if (type == 1) {
    # for the nowcast, estimate up to the latest mobility data
    last_date <- data$dates$latest_mobility
    projection_date <- NA
  } else {
    last_date <- data$dates$earliest + data$n_date_nums - 1
    projection_date <- data$dates$latest_mobility
  }
  n_projected <- data$n_dates + as.numeric(last_date - data$dates$latest)
  rows <- pmin(data$n_date_nums, seq_len(n_projected))
  dates_type <- data$dates$earliest - 1 + seq_along(rows)
  
  # duplicate these so they can be modified for scenarios
  R_eff_loc_1_proj <- R_eff_loc_1
  R_eff_loc_12_proj <- R_eff_loc_12 * 1
  R_eff_loc_1_micro_proj <- R_eff_loc_1_micro
  R_eff_loc_1_macro_proj <- R_eff_loc_1_macro
  R_eff_loc_1_surv_proj <- R_eff_loc_1_surv
  epsilon_L_proj <- epsilon_L
  
  # for counterfactuals, relevel the Reffs in VIC to specific ratios of the
  # values the projection date
  if (type > 2) {
    
    # either type of distancing (half or full) or full distancing plus isolation
    if (type %in% c(3, 4, 5)) {

      # set amount of reduction      
      multiplier <- switch(as.character(type),
                           "3" = 1,
                           "4" = 0.85,
                           "5" = 0.7)
      
      # after the projection date, set the component 1 value to the one form this date
      state_idx <- which(data$states == "VIC")
      duplicate_idx <- seq_along(dates_type)
      latest_value <- R_eff_loc_12[dates_type == latest_mobility_date, state_idx]
      scenario_idx <- dates_type >= latest_mobility_date
      scenario_value <- latest_value * multiplier
      R_eff_loc_12_proj[scenario_idx, state_idx] <- scenario_value
    }
    
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

  # Component 1 for national / state populations
  
  # microdistancing only
  plot_trend(sims$R_eff_loc_1_micro,
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
  plot_trend(sims$R_eff_loc_1_macro,
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
  plot_trend(sims$R_eff_loc_1_surv,
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
  plot_trend(sims$R_eff_loc_1,
             dates = dates_type,
             multistate = TRUE,
             base_colour = green,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of social distancing",
            subtitle = expression(Component~of~R["eff"]~due~to~social~distancing)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local.png", dir)
  
  plot_trend(sims$R_eff_imp_1,
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
  p <- plot_trend(sims$R_eff_loc_12,
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
                      xmin = data$dates$latest_infection,
                      xmax = data$dates$latest_mobility,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  p
  
  save_ggplot("R_eff_12_local.png", dir)
  
  plot_trend(sims$R_eff_imp_12,
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
  p <- plot_trend(sims$epsilon_L,
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
                      xmin = data$dates$latest_infection,
                      xmax = data$dates$latest_mobility,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  p
  
  save_ggplot("R_eff_2_local.png", dir)
  
  plot_trend(sims$epsilon_O,
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
  
}

