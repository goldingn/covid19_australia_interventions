source("R/lib.R")

source("R/functions.R")


age_distribution_state <- get_age_distribution_by_state()

vax_data <- load_vax_data()

dose_dates <- unique(vax_data$date)

dose_data <- vax_data %>%
  full_join(
    y = expand_grid(
      date = dose_dates,
      vaccine = c("az", "pf"),
      dose_number = 1:2,
      age_distribution_state
    )
  ) %>%
  mutate(doses = ifelse(is.na(doses), 0, doses)) %>%
  dplyr::select(-fraction) %>%
  arrange(state, age_class, vaccine, dose_number, date) %>%
  group_by(state, age_class, vaccine, dose_number) %>% 
  mutate(
    correction = slider::pslide_dbl(
      .l = list(
        date,
        doses,
        dose_number
      ),
      .f = immunity_lag_correction,
      .before = Inf
    ) %>%
      unlist,
    effective_doses = correction * doses
  ) %>%
  ungroup %>%
  group_by(state, age_class, date) %>%
  mutate(
    any_vaccine = sum(doses),
    effective_any_vaccine = sum(effective_doses),
  ) %>%
  ungroup %>%
  mutate(
    fraction = doses / any_vaccine,
    effective_fraction = effective_doses / effective_any_vaccine,
    coverage_any_vaccine = any_vaccine / pop,
    effective_coverage_any_vaccine = effective_any_vaccine / pop,
  ) %>%
  arrange(state, age_class, date) 

efficacy_data <- dose_data %>%
  pivot_wider(
    names_from = c(vaccine, dose_number),
    values_from = c(doses, correction, effective_doses, fraction, effective_fraction)
  ) %>%
  mutate(
    average_efficacy_transmission = average_efficacy(
      efficacy_az_2_dose = combine_efficacy(0.60, 0.65),
      efficacy_pf_2_dose = combine_efficacy(0.79, 0.65),
      efficacy_pf_1_dose = combine_efficacy(0.30, 0.46),
      efficacy_az_1_dose = combine_efficacy(0.18, 0.48),    
      proportion_pf_2_dose = fraction_pf_2,
      proportion_az_2_dose = fraction_az_2,
      proportion_pf_1_dose = fraction_pf_1,
      proportion_az_1_dose = fraction_az_1
    ),
    effective_average_efficacy_transmission = average_efficacy(
      efficacy_az_2_dose = combine_efficacy(0.60, 0.65),
      efficacy_pf_2_dose = combine_efficacy(0.79, 0.65),
      efficacy_pf_1_dose = combine_efficacy(0.30, 0.46),
      efficacy_az_1_dose = combine_efficacy(0.18, 0.48),    
      proportion_pf_2_dose = effective_fraction_pf_2,
      proportion_az_2_dose = effective_fraction_az_2,
      proportion_pf_1_dose = effective_fraction_pf_1,
      proportion_az_1_dose = effective_fraction_az_1
    ),
    average_efficacy_transmission = replace_na(average_efficacy_transmission, 0),
    effective_average_efficacy_transmission = replace_na(effective_average_efficacy_transmission, 0)
  ) %>%
  left_join(
    age_lookup,
    by = c("age_class" = "age_5y")
  )  %>%
  dplyr::select(
    -age
  ) %>%
  rename(
    age = age_class
  )

efficacy_data
efficacy_data %>% glimpse

vaccination_effect <- efficacy_data %>%
  group_by(
    state, date
  ) %>%
  summarise(
    vaccination_transmission_multiplier = vaccination_transmission_effect(
      age_coverage = coverage_any_vaccine,
      efficacy_mean = average_efficacy_transmission,
      next_generation_matrix = baseline_matrix()
    )$overall,
    effective_vaccination_transmission_multiplier = vaccination_transmission_effect(
      age_coverage = effective_coverage_any_vaccine,
      efficacy_mean = effective_average_efficacy_transmission,
      next_generation_matrix = baseline_matrix()
    )$overall,
    .groups = "drop"
  ) %>%
  mutate(
    vaccination_transmission_reduction_percent =
      100 * (1 - vaccination_transmission_multiplier),
    effective_vaccination_transmission_reduction_percent =
      100 * (1 - effective_vaccination_transmission_multiplier)
  ) %>%
  mutate(
    dubious = (date - min(date)) < 21,
    across(
      starts_with("effective_"),
      ~ ifelse(dubious, NA, .)
    )
  ) %>%
  select(
    -dubious
  )


earliest_effect_date <- min(vaccination_effect$date[which(!is.na(vaccination_effect$effective_vaccination_transmission_multiplier))])
last_effect_date    <- max(vaccination_effect$date[which(!is.na(vaccination_effect$effective_vaccination_transmission_multiplier))])

earliest_vaccination_effect <- vaccination_effect %>%
  filter(date == earliest_effect_date) %>% 
  dplyr::select(state, date, effective_vaccination_transmission_multiplier)


public_timeseries <- readRDS("outputs/vaccine_timeseries.RDS")

state_effect_ratios <- public_timeseries %>% 
  dplyr::select(date, overall_transmission_effect) %>% 
  inner_join(earliest_vaccination_effect) %>%
  mutate(
    multiplier = (1-effective_vaccination_transmission_multiplier)/(1-overall_transmission_effect)
  ) %>%
  dplyr::select(state, multiplier)

scaled_public_timeseries <- public_timeseries %>%
  dplyr::select(date, overall_transmission_effect) %>% 
  expand_grid(state_effect_ratios) %>%
  filter(date <= earliest_effect_date) %>%
  group_by(state) %>%
  mutate(
    datenum = as.numeric(date - min(date)),
    #scaled_effect = overall_transmission_effect - (overall_transmission_effect * (1-ratio)* datenum/  max(datenum))
    scaled_effect = 1 - (1 - overall_transmission_effect)*datenum/max(datenum)*multiplier
  ) %>% 
  dplyr::select(state, date, effect = scaled_effect)




interpolated_effect <- vaccination_effect %>%
  rename(effect = effective_vaccination_transmission_multiplier) %>%
  dplyr::select(state, date, effect) %>%
  filter(!is.na(effect)) %>%
  full_join(
    y = expand_grid(
      date = seq.Date(
        from = earliest_effect_date,
        to = last_effect_date,
        by = 1
      ),
      state = states
    ),
    by = c("state", "date")
  ) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(
    effect = ifelse(
      is.na(effect),
      approx(date, effect, date)$y,
      effect
    )
  )

vaccine_effect_timeseries <- bind_rows(
  scaled_public_timeseries %>%
    filter(date < earliest_effect_date),
  interpolated_effect
)

# 
# ggplot(vaccine_effect_timeseries) +
#   geom_line(
#     aes(
#       x = date,
#       y = effect,
#       colour = state
#     )
#   )



effective_dose_data <- dose_data %>%
  dplyr::select(
    state,
    age_class,
    vaccine,
    dose_number,
    doses,
    effective_doses,
    date
  ) %>%
  mutate(
    dubious = (date - min(date)) < 21,
    across(
      starts_with("effective_"),
      ~ ifelse(dubious, NA, .)
    )
  ) %>%
  dplyr::select(
    -dubious
  )

data_date <- max(effective_dose_data$date)

write_csv(
  vaccine_effect_timeseries,
  file = sprintf(
    "outputs/vaccine_effect_timeseries_%s.csv",
    data_date
  )
)


write_csv(
  effective_dose_data,
  file = sprintf(
    "outputs/effective_dose_data_%s.csv",
    data_date
  )
)


#############
# code to read in Tierney processed data of wide timeseries
# from Jul-Aug of weekly data to process 
# read_csv("~/not_synced/vaccination/2021-08-16-1559-tidy-not-clean-vaccine-rollout-ts.csv") %>%
#   rename(
#     age_class = age_group,
#     dose_number = dose
#   ) %>% 
#   mutate(
#     vaccine = case_when(
#       vaccine_type == "astra_zeneca" ~ "az",
#       vaccine_type == "pfizer_comirnaty" ~ "pf"
#     ),
#     state = case_when(
#       state == "act" ~ "ACT",
#       state == "nt"  ~ "NT",
#       state == "nsw" ~ "NSW",
#       state == "qld" ~ "QLD",
#       state == "sa"  ~ "SA",
#       state == "tas" ~ "TAS",
#       state == "vic" ~ "VIC",
#       state == "wa"  ~ "WA",
#       state == "unknown" ~ "unk"
#     ),
#     date_doses = ifelse(is.na(count), 0, count)
#   ) %>%
#   dplyr::select(state, date, age_class, vaccine, dose_number, date_doses) %>%
#   arrange(state, date, age_class, vaccine, dose_number) %>%
#   rowwise %>%
#   mutate(
#     age_class = case_when(
#       any(age_class == c(
#         "80+",
#         "80-84",
#         "85+",
#         "85-89",
#         "90+",
#         "90-94",
#         "95+",
#         "95-99",
#         "100+"
#       )) ~ "80+",
#       TRUE ~ age_class
#     )
#   ) %>%
#   group_by(state, date, age_class, vaccine, dose_number) %>%
#   summarise(date_doses = sum(date_doses)) %>%
#   arrange(date, age_class, vaccine, dose_number) %>%
#   group_by(date, age_class, vaccine, dose_number) %>%
#   mutate(
#    unknown = date_doses[7], # this is the unknown dose row,
#    aus_doses = sum(date_doses) - unknown,
#    date_doses_adjusted = date_doses + date_doses/aus_doses * unknown,
#    date_doses_adjusted = ifelse(is.nan(date_doses_adjusted), 0, date_doses_adjusted)
#   ) %>%
#   group_by(
#     state, date, age_class
#   )

#################
# code to read in and process covidlive data.
# will cease regular read in and store historic set for scaling as
# "outputs/vaccine_timeseries.RDS"
# keeping this code in case need to reactivate
################
# 
# 
# # get the baseline next generation matrix
# next_generation_matrix <- baseline_matrix(
#   R0 = 3
# )
# 
# # check R0
# get_R(next_generation_matrix)
# 
# ages <- age_classes(80)
# age_populations <- phase_age_populations()
# age_distribution <- get_age_distribution(final_age_bin = 80)
# #timeseries <- scrape_doses_timeseries()
# timeseries <- vaccination_coverage()
# ifr <- get_ifr()
# 
# # get effects for all timepoints
# all_effects <- lapply(
#   timeseries$doses,
#   summarise_effect,
#   age_populations = age_populations,
#   age_distribution = age_distribution,
#   next_generation_matrix = next_generation_matrix,
#   ifr = ifr
# )
# 
# timeseries$overall_transmission_effect <- vapply(
#   X = all_effects,
#   extract_overall_transmission_effect,
#   FUN.VALUE = numeric(1)
# )
# 
# timeseries$overall_ifr_effect <- vapply(
#   X = all_effects,
#   extract_overall_ifr_effect,
#   FUN.VALUE = numeric(1),
#   which = "odriscoll"
# )
# 
# timeseries$over_70_ifr_effect <- vapply(
#   X = all_effects,
#   extract_over_70_ifr_effect,
#   FUN.VALUE = numeric(1),
#   which = "odriscoll"
# )
# 
# png(
#   filename = "outputs/figures/vacc_effect_tp_reduction.png",
#   width = 11.69 / 2,
#   height = 8.27 / 3,
#   #scale = 1,
#   units = "in",
#   res = 150
# )
# 
# plot(
#   I(100 * (1 - overall_transmission_effect)) ~ date,
#   data = timeseries,
#   type = "l",
#   ylab = "Percentage reduction in Reff",
#   xlab = "",
#   lwd = 2,
#   las = 1,
#   main = "Vaccination effect on population-wide\nCOVID-19 transmission potential"
# )
# 
# dev.off()
# 
# png(
#   filename = "outputs/figures/vacc_ifr_population.png",
#   width = 11.69 / 2,
#   height = 8.27 / 3,
#   #scale = 1,
#   units = "in",
#   res = 150
# )
# 
# plot(
#   overall_ifr_effect ~ date,
#   data = timeseries,
#   type = "l",
#   ylab = "Population-wide IFR (%)",
#   xlab = "",
#   lwd = 2,
#   col = "forestgreen",
#   las = 1,
#   ylim = c(0, 1.2),
#   main = "Change in population-wide infection fatality risk"
# )
# 
# dev.off()
# 
# range(timeseries$overall_ifr_effect)
# 
# png(
#   filename = "outputs/figures/vacc_ifr_70plus.png",
#   width = 11.69 / 2,
#   height = 8.27 / 3,
#   #scale = 1,
#   units = "in",
#   res = 150
# )
# 
# plot(
#   over_70_ifr_effect ~ date,
#   data = timeseries,
#   type = "l",
#   ylab = "IFR (%)",
#   xlab = "",
#   lwd = 2,
#   col = "deepskyblue",
#   las = 1,
#   ylim = c(0, 8),
#   main = "Change in infection fatality risk among over-70s"
# )
# range(timeseries$over_70_ifr_effect)
# 
# dev.off()
# 
# 
# # plot current vaccination coverage by age
# current_coverage_by_age <- all_effects[[length(all_effects)]]$coverage_by_age
# 
# png(
#   filename = "outputs/figures/vacc_coverage.png",
#   width = 11.69 / 2,
#   height = 8.27 / 3,
#   #scale = 1,
#   units = "in",
#   res = 150
# )
# 
# barplot(100 * current_coverage_by_age,
#         names.arg = age_distribution$age_class,
#         axes = FALSE,
#         border = "white",
#         xlab = "age group",
#         main = "Assumed current vaccine coverage\n(with only single doses)",
#         ylim = c(0, 100))
# y_axis_ticks <- seq(0, 100, by = 20) 
# axis(2,
#      at = y_axis_ticks,
#      labels = paste0(y_axis_ticks, "%"),
#      las = 1)
# 
# dev.off()
# 
# # # The same, but assuming a complete phase 1 roll out
# # n_doses_phase_1 <- sum(age_populations$phase_1A, age_populations$phase_1B)
# # coverage_phase_1 <- doses_by_age(n_doses_phase_1, age_populations) / age_distribution$pop
# # barplot(100 * coverage_phase_1,
# #         names.arg = age_distribution$age_class,
# # axes = FALSE,
# # xlab = "age group",
# #         main = "Assumed vaccination coverage\nafter complete phase 1 roll out",
# #         ylim = c(0, 100))
# # y_axis_ticks <- seq(0, 100, by = 20) 
# # axis(2,
# #      at = y_axis_ticks,
# #      labels = paste0(y_axis_ticks, "%"),
# #      las = 1)
# # abline(h = 100, lty = 3)
# 
# # population-wide baseline IFRs based on these numbers
# ifr_baseline_odriscoll <- sum(age_distribution$fraction * ifr$odriscoll)
# ifr_baseline_brazeau <- sum(age_distribution$fraction * ifr$brazeau)
# 
# # apply this to vaccine coverage (single dose) estimates to get new IFRs by age
# # and overall
# 
# 
# # # check against tables
# # lapply(populations_1A, sum)
# # lapply(populations_1B, sum)
# # 
# # # check these look sensible
# # 
# # # populations by age in each phase
# # par(mfrow = c(2, 1))
# # barplot(age_populations_1A, ylim = range(age_populations_1B))
# # barplot(age_populations_1B)
# # 
# # # proportional coverage with single doses in each age
# # par(mfrow = c(1, 1))
# # barplot(age_coverage)
# # 
# # # check this is true
# # identical(
# #   sum(age_doses_1A, age_doses_1B),
# #   n_doses
# # )
# 
# 
# 
# 
# 
# library(ggplot2)
# 
# timeseries %>%
#   select(date, fully_vaccinated, partially_vaccinated) %>%
#   pivot_longer(
#     cols = -date,
#     names_to = "status",
#     values_to = "population"
#   ) %>%
#   # mutate(status = factor(status))
#   ggplot() +
#   aes(date, population, fill = status) +
#   geom_area() +
#   scale_y_continuous(
#     label = scales::comma
#   ) +
#   xlab("") +
#   theme_minimal()
# 
# 
# # simple 1 dose model with lag
# ages <- age_classes(80)
# age_populations <- phase_age_populations()
# age_distribution <- get_age_distribution(final_age_bin = 80)
# #timeseries <- scrape_doses_timeseries()
# timeseries <- vaccination_coverage()
# ifr <- get_ifr()
# 
# 
# # fake data
# n_days <- length(timeseries$doses)
# time_to_protection <- 21
# cumulative_vaccinees <- timeseries$doses
# daily_vaccinees <- timeseries$new_doses
# # construct linear weights
# dummy_matrix <- matrix(0, n_days, n_days)
# lags <- row(dummy_matrix) - col(dummy_matrix)
# weights <- lags / time_to_protection
# weights[] <- pmax(0, weights[])
# weights[] <- pmin(1, weights[])
# # apply them to the daily number of cases, to get the cumulative effect
# cumulative_vaccinees_effective <- c(weights %*% daily_vaccinees)
# # compare plot
# plot(cumulative_vaccinees, type = "l")
# lines(cumulative_vaccinees_effective, lty = 2)
# 
# 
# average_transmission_efficacy <- function() {
#   average_efficacy(
#     efficacy_pf_2_dose = 0.9685,
#     efficacy_az_2_dose = 0.93,
#     #efficacy_pf_1_dose = efficacy_pf_2_dose/2,
#     #efficacy_az_1_dose = efficacy_az_2_dose/2,
#     efficacy_pf_1_dose = 0.8317,
#     efficacy_az_1_dose = 0.892,
#     proportion_pf = 0.5,
#     proportion_2_dose = 0
#   )
# }
# 
# all_effects <- lapply(
#   cumulative_vaccinees_effective,
#   summarise_effect,
#   age_populations = age_populations,
#   age_distribution = age_distribution,
#   next_generation_matrix = next_generation_matrix,
#   ifr = ifr
# )
# 
# timeseries$overall_transmission_effect <- vapply(
#   X = all_effects,
#   extract_overall_transmission_effect,
#   FUN.VALUE = numeric(1)
# )
# 
# 
# saveRDS(
#   object = timeseries,
#   file = "outputs/vaccine_timeseries.RDS"
# )
