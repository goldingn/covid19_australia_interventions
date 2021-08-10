source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)

fitted_model <- readRDS("outputs/fitted_reff_model.RDS")

timeseries <- readRDS("outputs/vaccine_timeseries.RDS")

timeseries_1_dose <- timeseries

# simple 2 dose model with lag

ages <- age_classes(80)
age_populations <- phase_age_populations()
age_distribution <- get_age_distribution(final_age_bin = 80)
#timeseries <- scrape_doses_timeseries()
timeseries <- vaccination_coverage()
ifr <- get_ifr()



# fake data
n_days <- length(timeseries$doses)
time_to_protection <- 21
cumulative_vaccinees <- timeseries$doses/2
daily_vaccinees <- timeseries$new_doses/2
# construct linear weights
dummy_matrix <- matrix(0, n_days, n_days)
lags <- row(dummy_matrix) - col(dummy_matrix)
weights <- lags / time_to_protection
weights[] <- pmax(0, weights[])
weights[] <- pmin(1, weights[])
# apply them to the daily number of cases, to get the cumulative effect
cumulative_vaccinees_effective <- c(weights %*% daily_vaccinees)
# compare plot
plot(cumulative_vaccinees, type = "l")
lines(cumulative_vaccinees_effective, lty = 2)


average_transmission_efficacy <- function() {
  average_efficacy(
    efficacy_pf_2_dose = 0.9685,
    efficacy_az_2_dose = 0.93,
    #efficacy_pf_1_dose = efficacy_pf_2_dose/2,
    #efficacy_az_1_dose = efficacy_az_2_dose/2,
    efficacy_pf_1_dose = 0.8317,
    efficacy_az_1_dose = 0.892,
    proportion_pf = 0.5,
    proportion_2_dose = 1
  )
}

all_effects <- lapply(
  cumulative_vaccinees_effective,
  summarise_effect,
  age_populations = age_populations,
  age_distribution = age_distribution,
  next_generation_matrix = next_generation_matrix,
  ifr = ifr
)

timeseries$overall_transmission_effect <- vapply(
  X = all_effects,
  extract_overall_transmission_effect,
  FUN.VALUE = numeric(1)
)

timeseries_2_dose <- timeseries


ggplot(
  data = bind_rows(
    timeseries_1_dose %>%
      mutate(dose_model = "one"),
    timeseries_2_dose %>%
      mutate(dose_model = "two")
  )
) +
  geom_line(
    aes(
      x = date,
      y = overall_transmission_effect,
      col = dose_model
    )
  )


# plot for single dose

dir = "outputs"
min_date = as.Date("2020-03-01")
max_date = fitted_model$data$dates$latest_mobility
mobility_extrapolation_rectangle = TRUE
projection_date = NA
washout_cutoff = 0
vaccine_timeseries = timeseries_1_dose



# add counterfactuals to the model object:
# add fitted_model_extended obect because fitted_model is modified
fitted_model_extended1 <- fitted_model
# Reff for locals component 1 under
# only micro/macro/surveillance improvements
fitted_model_extended1$greta_arrays <- c(
  fitted_model$greta_arrays,
  list(
    R_eff_loc_1_macro = reff_1_only_macro(fitted_model_extended1),
    R_eff_loc_1_micro = reff_1_only_micro(fitted_model_extended1),
    R_eff_loc_1_surv = reff_1_only_surveillance(fitted_model_extended1),
    R_eff_loc_1_vaccine_effect = reff_1_vaccine_effect(fitted_model_extended1, vaccine_timeseries)
  ) 
)

# flatten all relevant greta array matrices to vectors before calculating
trajectory_types <- c(
  "R_eff_loc_1",
  "R_eff_imp_1",
  "R_eff_loc_12",
  "R_eff_imp_12",
  "epsilon_L",
  "R_eff_loc_1_micro",
  "R_eff_loc_1_macro",
  "R_eff_loc_1_surv",
  "R_eff_loc_1_vaccine_effect"
)
vector_list <- lapply(fitted_model_extended1$greta_arrays[trajectory_types], c)

# simulate from posterior for these quantities of interest
args <- c(vector_list, list(values = fitted_model_extended1$draws, nsim = 10000))
sims1 <- do.call(calculate, args)

# vaccine effect only
plot_trend(sims1$R_eff_loc_1_vaccine_effect[,1:fitted_model_extended1$data$n_date_nums,], # clunky fix
           data = fitted_model_extended1$data,
           min_date = min_date,
           max_date = max_date,
           multistate = FALSE,
           base_colour = fifo,
           projection_at = projection_date,
           ylim = c(0, 5),
           intervention_at = vaccination_dates(),
           plot_voc = TRUE
) + 
  ggtitle(label = "Impact of vaccination",
          subtitle = expression(R["eff"]~"if"~only~vaccination~had~occurred)) +
  ylab(expression(R["eff"]~component))

save_ggplot("R_eff_1_local_vaccine_effect_single_dose.png", dir, multi = FALSE)


# plot for two dose

dir = "outputs"
min_date = as.Date("2020-03-01")
max_date = fitted_model$data$dates$latest_mobility
mobility_extrapolation_rectangle = TRUE
projection_date = NA
washout_cutoff = 0
vaccine_timeseries = timeseries_2_dose



# add counterfactuals to the model object:
# add fitted_model_extended obect because fitted_model is modified
fitted_model_extended2 <- fitted_model
# Reff for locals component 1 under
# only micro/macro/surveillance improvements
fitted_model_extended2$greta_arrays <- c(
  fitted_model$greta_arrays,
  list(
    R_eff_loc_1_macro = reff_1_only_macro(fitted_model_extended2),
    R_eff_loc_1_micro = reff_1_only_micro(fitted_model_extended2),
    R_eff_loc_1_surv = reff_1_only_surveillance(fitted_model_extended2),
    R_eff_loc_1_vaccine_effect = reff_1_vaccine_effect(fitted_model_extended2, vaccine_timeseries)
  ) 
)

# flatten all relevant greta array matrices to vectors before calculating
trajectory_types <- c(
  "R_eff_loc_1",
  "R_eff_imp_1",
  "R_eff_loc_12",
  "R_eff_imp_12",
  "epsilon_L",
  "R_eff_loc_1_micro",
  "R_eff_loc_1_macro",
  "R_eff_loc_1_surv",
  "R_eff_loc_1_vaccine_effect"
)
vector_list <- lapply(fitted_model_extended2$greta_arrays[trajectory_types], c)

# simulate from posterior for these quantities of interest
args <- c(vector_list, list(values = fitted_model_extended2$draws, nsim = 10000))
sims2 <- do.call(calculate, args)

# vaccine effect only
plot_trend(sims2$R_eff_loc_1_vaccine_effect[,1:fitted_model_extended2$data$n_date_nums,], # clunky fix
           data = fitted_model_extended2$data,
           min_date = min_date,
           max_date = max_date,
           multistate = FALSE,
           base_colour = fifo,
           projection_at = projection_date,
           ylim = c(0, 5),
           intervention_at = vaccination_dates(),
           plot_voc = TRUE
) + 
  ggtitle(label = "Impact of vaccination",
          subtitle = expression(R["eff"]~"if"~only~vaccination~had~occurred)) +
  ylab(expression(R["eff"]~component))

save_ggplot("R_eff_1_local_vaccine_effect_two_dose.png", dir, multi = FALSE)

