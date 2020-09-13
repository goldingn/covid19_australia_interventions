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

# save the key dates for Freya and David to read in
write_reff_key_dates(data)

# define the model (and greta arrays) for Reff, and sample until adequate convergence
model <- reff_model(model_data)
draws <- fit_reff_model(model)

write_fitted_reff(model_data, model, draws) 
  
# check fit of observation model against data 
nsim <- coda::niter(draws) * coda::nchain(draws)
nsim <- min(10000, nsim)
cases <- negative_binomial(size, prob_trunc)
cases_sim <- calculate(cases, values = draws, nsim = nsim)[[1]][, , 1]

# overall PPC check
bayesplot::ppc_ecdf_overlay(
  data$local$cases[valid],
  cases_sim[1:1000, ],
  discrete = TRUE
)

# check by state and time
plot_fit(data$local$cases[valid], cases_sim, valid)

# R_eff for local-local and import-local among active cases per state
# (components 1 and 2)
R_eff_loc_12 <- exp(log_R_eff_loc)
R_eff_imp_12 <- exp(log_R_eff_imp)

# vector of generation interval probabilities
gi_vec <- gi_vector(gi_cdf, data$dates$latest)

# check fit of projected cases against national epi curve
check_projection(draws,
                 R_eff_local = R_eff_loc_12,
                 R_eff_imported = R_eff_imp_12,
                 gi_mat = data$gi_mat,
                 gi_vec = gi_vec,
                 local_infectiousness = data$local$infectiousness,
                 imported_infectiousness = data$imported$infectiousness,
                 local_cases = data$local$cases,
                 dates = data$dates$infection,
                 start_date = as.Date("2020-02-28"))

# Reff local component one under only micro- and only macro-distancing
de <- distancing_effect

# include the effect of surveillance at baseline (no improvements yet, but not nothing)
baseline_surveillance_effect <- surveillance_reff_local_reduction[1]

infectious_days <- infectious_period(gi_cdf)

# microdistancing
household_infections_micro <- de$HC_0 * (1 - de$p ^ de$HD_0)
non_household_infections_micro <- de$OC_0 * infectious_days *
  (1 - de$p ^ de$OD_0) * de$gamma_t_state
hourly_infections_micro <- household_infections_micro +
  non_household_infections_micro
R_eff_loc_1_micro <- hourly_infections_micro[extend_idx, ] * baseline_surveillance_effect

# macrodistancing
h_t <- h_t_state(data$dates$mobility)
HD_t <- de$HD_0 * h_t
household_infections_macro <- de$HC_0 * (1 - de$p ^ HD_t)
non_household_infections_macro <- de$OC_t_state * infectious_days * (1 - de$p ^ de$OD_0)
hourly_infections_macro <- household_infections_macro + non_household_infections_macro
R_eff_loc_1_macro <- hourly_infections_macro[extend_idx, ] * baseline_surveillance_effect

# Reff for locals component under only surveillance improvements
R_eff_loc_1_surv <- exp(log_R0 + log(surveillance_reff_local_reduction))

# make 5 different versions of the plots and outputs:
# 1. to the latest date of mobility data
# 2. 6 weeks into the future
# 3. 6 weeks into the future, with increase in mean Reff to 1.1
# 4. 6 weeks into the future, with increase in mean Reff to 1.2
# 5. 6 weeks into the future, with increase in mean Reff to 1.5

# types <- seq_along(output_directories)
types <- 1:2

for (type in types) {
  
  dir <- output_directories[type]
  
  # save local case data, dates, and detection probabilities for Rob
  tibble::tibble(
      date_onset = rep(data$dates$onset, data$n_states),
      detection_probability = as.vector(data$detection_prob_mat),
      state = rep(data$states, each = data$n_dates),
      count = as.vector(data$local$cases_infectious),
      acquired_in_state = as.vector(data$local$cases)
  ) %>%
    write.csv(file.path(dir, "local_cases_input.csv"), row.names = FALSE)
  
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
  plot_trend(R_eff_loc_1_micro_sim,
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
  plot_trend(R_eff_loc_1_macro_sim,
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
  plot_trend(R_eff_loc_1_surv_sim,
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
  plot_trend(R_eff_loc_1_sim,
             dates = dates_type,
             multistate = TRUE,
             base_colour = green,
             vline_at = intervention_dates()$date,
             vline2_at = projection_date) + 
    ggtitle(label = "Impact of social distancing",
            subtitle = expression(Component~of~R["eff"]~due~to~social~distancing)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local.png", dir)
  
  plot_trend(R_eff_imp_1_sim,
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
  p <- plot_trend(R_eff_loc_12_sim,
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
  
  plot_trend(R_eff_imp_12_sim,
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
  p <- plot_trend(epsilon_L_sim,
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
  
  save_ggplot("R_eff_2_imported.png", dir)

  # output 2000 posterior samples of R_eff for active local cases
  R_eff_12_samples <- t(R_eff_loc_12_sim[1:2000, , 1])
  colnames(R_eff_12_samples) <- paste0("sim", 1:2000)
  
  # output 2000 posterior samples of R_eff for statewide local cases
  R_eff_1_samples <- t(R_eff_loc_1_sim[1:2000, , 1])
  colnames(R_eff_1_samples) <- paste0("sim", 1:2000)
  
  df_base <- tibble(
    date = rep(dates_type, data$n_states),
    state = rep(data$states, each = length(dates_type)),
  ) %>%
    mutate(date_onset = date + 5)
  
  # CSV of R_eff local posterior samples
  df_base %>%
    cbind(R_eff_12_samples) %>%
    write_csv(
      file.path(dir, "r_eff_12_local_samples.csv")
    )
  
  # CSV of R_eff local posterior samples
  df_base %>%
    cbind(R_eff_1_samples) %>%
    write_csv(
      file.path(dir, "r_eff_1_local_samples.csv")
    )
  
  # # make forecasts
  # if (type >= 2) {
  #   
  #   # forecast locally-acquired cases
  #   
  #   # drop last two days of case data, because detection probabilities are very low
  #   keep_idx <- seq_len(nrow(data$local$cases) - 2)
  #   
  #   # add constant rate of imported cases - model numbers of imports per day from
  #   # two weeks after the last quarantine date (mandatory hotel quarantine
  #   # introduced), using a poisson model with offset of the population size; add
  #   # those expectations to the imported cases data.
  #   
  #   forecast_list <- forecast_locals(local_cases = data$local$cases[keep_idx, ],
  #                                    imported_cases = imported_cases[keep_idx, ],
  #                                    Reff_locals = R_eff_loc_12_proj,
  #                                    Reff_imports = R_eff_imp_12,
  #                                    dates = dates_type,
  #                                    gi_cdf = gi_cdf,
  #                                    simulation_start = data$dates$latest_infection,
  #                                    gi_bounds = c(0, 20))
  #   
  #   forecast <- forecast_list$local_cases
  #   
  #   # is the probability of any new cases very small, and is it after the
  #   # projection period? if so sthen set the number of new cases from this point onwards to 0?
  #   n_forecast <- nrow(forecast) - nrow(data$local$cases)
  #   
  #   # in the forecasting period?
  #   projection_mask <- rbind(
  #     matrix(0,
  #            nrow(data$local$cases),
  #            data$n_states),
  #     matrix(1,
  #            n_forecast,
  #            data$n_states)
  #   )
  #   
  #   # small enough probbability of more cases to round to 0
  #   small_mask <- forecast_list$probability_of_cases < 0.01
  #   
  #   # invert this (small and forecasting gets 0)
  #   forecast_mask <- 1 - (projection_mask * small_mask)
  #   
  #   # set all subsequent dates to 0
  #   forecast_mask <- apply(forecast_mask, 2, "cumprod")
  #   # cap forecasts by this
  #   forecast_capped <- forecast * forecast_mask
  #   
  #   forecast_sim <- calculate(c(forecast),
  #                             values = draws,
  #                             nsim = nsim)[[1]]
  #   
  #   forecast_capped_sim <- calculate(c(forecast_capped),
  #                                    values = draws,
  #                                    nsim = nsim)[[1]]
  #   
  #   plot_trend(forecast_sim,
  #              dates = dates_type,
  #              multistate = TRUE,
  #              base_colour = blue,
  #              hline_at = NULL,
  #              ylim = c(0, 200),
  #              vline_at = quarantine_dates,
  #              vline2_at = data$dates$latest) + 
  #     ggtitle(label = "Forecast numbers of locally-acquired cases") +
  #     ylab("New infections per day")
  #   
  #   save_ggplot("forecast.png", dir)
  #   
  #   plot_trend(forecast_sim,
  #              dates = dates_type,
  #              multistate = TRUE,
  #              base_colour = blue,
  #              hline_at = NULL,
  #              ylim = c(0, 10),
  #              vline_at = quarantine_dates,
  #              vline2_at = data$dates$latest) + 
  #     ggtitle(label = "Forecast numbers of locally-acquired cases") +
  #     ylab("New infections per day")
  #   
  #   save_ggplot("forecast_low.png", dir)
  #   
  #   plot_trend(forecast_capped_sim,
  #              dates = dates_type,
  #              multistate = TRUE,
  #              base_colour = blue,
  #              hline_at = NULL,
  #              ylim = c(0, 200),
  #              vline_at = quarantine_dates,
  #              vline2_at = data$dates$latest) + 
  #     ggtitle(label = "Forecast numbers of locally-acquired cases") +
  #     ylab("New infections per day")
  #   
  #   save_ggplot("forecast_capped.png", dir)
  #   
  #   # save forecast draws
  #   forecast_sim_mat <- t(forecast_sim[, , 1])
  #   colnames(forecast_sim_mat) <- paste0("sim", seq_len(ncol(forecast_sim_mat)))
  #   df_base %>%
  #     cbind(
  #       forecast_sim_mat
  #     ) %>%
  #     write_csv(
  #       file.path(dir, "forecast_samples.csv")
  #     )
  #   
  # }
  
}

