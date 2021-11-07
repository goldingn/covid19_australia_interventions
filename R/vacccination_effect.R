source("R/lib.R")

source("R/functions.R")

# load data on number of individuals who have had first and second doses
cumulative_doses <- load_cumulative_doses()


ggplot(cumulative_doses) +
  geom_bar(
    aes(
      x = date,
      y = doses,
      fill = age_class
    ),
    stat = "identity"
  ) +
  facet_grid(
    state ~ vaccine + dose_number,
    scales = "free_y"
  )


individuals_on_dose <- cumulative_doses %>%
  pivot_wider(
    names_from = dose_number,
    values_from = doses
  ) %>%
  mutate(
    `1` = unlist(`1`),
    `2` = unlist(`2`),
    `1` = `1` - `2`
  ) %>%
  pivot_longer(
    cols = `1`:`2`,
    names_to = "dose_number",
    values_to = "doses"
  ) %>%
  mutate(
    dose_number = as.integer(dose_number)
  )


age_distribution_state <- get_age_distribution_by_state()

dose_dates <- unique(individuals_on_dose$date)

dose_data <- individuals_on_dose %>%
  # hack to set all moderna doses to pfizer because
  # assumed equal efficacy
  # fix into pipeline properly but should give same result for now
  mutate(
    vaccine = case_when(
      vaccine == "mo" ~ "pf",
      TRUE ~ vaccine
    )
  ) %>%
  group_by(state, vaccine, age_class, date, dose_number) %>%
  summarise(doses = sum(doses)) %>%
  # end hack
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
      efficacy_az_1_dose = combine_efficacy(0.46, 0.02),
      efficacy_az_2_dose = combine_efficacy(0.67, 0.36),
      efficacy_pf_1_dose = combine_efficacy(0.57, 0.13),
      efficacy_pf_2_dose = combine_efficacy(0.80, 0.65),
      proportion_pf_2_dose = fraction_pf_2,
      proportion_az_2_dose = fraction_az_2,
      proportion_pf_1_dose = fraction_pf_1,
      proportion_az_1_dose = fraction_az_1
    ),
    effective_average_efficacy_transmission = average_efficacy(
      efficacy_az_1_dose = combine_efficacy(0.46, 0.02),
      efficacy_az_2_dose = combine_efficacy(0.67, 0.36),
      efficacy_pf_1_dose = combine_efficacy(0.57, 0.13),
      efficacy_pf_2_dose = combine_efficacy(0.80, 0.65),
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
  mutate(
    age_group = age %>%
      factor(
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80+"
        )
      )
  ) %>%
  arrange(
    state, date, age_group
  ) %>%
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
  ) %>% 
  mutate(
    effective_vaccination_transmission_multiplier = ifelse(
      is.na(effective_vaccination_transmission_multiplier),
      1,
      effective_vaccination_transmission_multiplier
    ),
    effective_vaccination_transmission_reduction_percent = ifelse(
      is.na(effective_vaccination_transmission_reduction_percent),
      0,
      effective_vaccination_transmission_reduction_percent
    )
  )

vaccine_effect_timeseries <- bind_rows(
  vaccination_effect[1,],
  vaccination_effect %>%
    mutate(
      date = date + 6
    )
) %>%
  dplyr::select(
    state,
    date,
    effect = effective_vaccination_transmission_multiplier,
    percent_reduction = effective_vaccination_transmission_reduction_percent
  ) %>%
  full_join(
    y = expand_grid(
      date = seq.Date(
        from = min(dose_dates),
        to = max(dose_dates) + 6,
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
    ),
    percent_reduction = ifelse(
      is.na(percent_reduction),
      approx(date, percent_reduction, date)$y,
      percent_reduction
    )
  ) %>%
  ungroup
    
    
    
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
      ~ ifelse(dubious, 0, .)
    )
  ) %>%
  dplyr::select(
    -dubious
  ) %>% 
  arrange(
    state,
    age_class,
    vaccine,
    dose_number,
    date
  )

data_date <- max(vaccine_effect_timeseries$date)

saveRDS(
  object = vaccine_effect_timeseries %>%
    dplyr::select(-percent_reduction),
  file = "outputs/vaccine_effect_timeseries.RDS"
)

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

dpi <- 150
font_size <- 16

ggplot(vaccine_effect_timeseries) +
  geom_line(
    aes(
      x = date,
      y = effect,
      colour = state
    ),
    size = 1.5,
    alpha = 1
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in transmission potential",
    col = "State"
  ) +
  scale_x_date(
    breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  ggtitle(
    label = "Vaccination effect",
    subtitle = "Change in transmission potential due to vaccination"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90, size = font_size),
    legend.position = c(0.02, 0.135),
    legend.text = element_text(size = font_size),
    axis.text = element_text(size = font_size),
    plot.title = element_text(size = font_size + 8),
    plot.subtitle = element_text(size = font_size)
  ) +
  scale_colour_manual(
    values = c(
      "darkgray",
      "cornflowerblue",
      "chocolate1",
      "violetred4",
      "red1",
      "darkgreen",
      "darkblue",
      "gold1"
    )
  ) +
  scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  )

ggsave(
  filename = "outputs/figures/vaccination_effect.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)


vaccine_effect_timeseries %>%
  group_by(state) %>%
  mutate(
    delta_week = slider::slide(
      .x = -percent_reduction,
      .f = function(x){
        x[1] - x[7]
      },
      .before = 7
    ) %>%
      unlist
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = delta_week,
      col = state
    ),
    size = 2
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in percentage reduction of transmission potential",
    col = "State"
  ) +
  scale_x_date(
    breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  ggtitle(
    label = "Vaccination effect",
    subtitle = "Change in weekly average percentage reduction in transmission potential due to vaccination"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90),
    panel.spacing = unit(1.2, "lines")
  ) +
  scale_colour_manual(
    values = c(
      "darkgray",
      "cornflowerblue",
      "chocolate1",
      "violetred4",
      "red1",
      "darkgreen",
      "darkblue",
      "gold1"
    )
  )


ggsave(
  filename = "outputs/figures/vaccination_weekly_percent_change_in_tp.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)



# fix this and need to split out moderna from pfizer
ggplot(
  effective_dose_data
  ) +
  geom_bar(
    aes(
      x = date,
      y = doses,
      fill = age_class
    ),
    stat = "identity"
  ) +
  facet_grid(
    state ~ vaccine + dose_number,
    scales = "free_y"
  )



q_age   <- read_csv(file = "data/vaccinatinon/quantium_vaccination_Rollout/Rollout/dim_age_band.csv")
q_sa4   <- read_csv(file = "data/vaccinatinon/quantium_vaccination_Rollout/Rollout/dim_sa4.csv")
q_time  <- read_csv(file = "data/vaccinatinon/quantium_vaccination_Rollout/Rollout/dim_time.csv")
q_brand <- read_csv(file = "data/vaccinatinon/quantium_vaccination_Rollout/Rollout/dim_vaccine.csv")
q_vacc  <- read_csv(file = "data/vaccinatinon/quantium_vaccination_Rollout/Rollout/vaccinations.csv")


# 
# effective_dose_data %>%
#   group_by(state, age_class, vaccine, date) %>%
#   summarise(total_vaccinees = sum(doses)) %>%
#   ggplot() +
#   geom_bar(
#     aes(
#       x = date,
#       y = total_vaccinees,
#       fill = age_class
#     ),
#     stat = "identity"
#   ) +
#   facet_grid(
#     state ~ vaccine,
#     scales = "free_y"
#   )
# 
# 
# effective_dose_data %>%
#   filter(state == "ACT", date >= "2021-08-30", vaccine == "pf") %>%
#   ggplot() +
#   geom_bar(
#     aes(
#       x = date,
#       y = doses,
#       fill = age_class
#     ),
#     stat = "identity"
#   ) +
#   facet_grid(
#     age_class ~ dose_number,
#     scales = "free_y"
#   )
# 
# 
# effective_dose_data %>%
#   filter(state == "ACT", date >= "2021-08-30", vaccine == "pf", dose_number == "2") %>%
#   ggplot() +
#   geom_bar(
#     aes(
#       x = date,
#       y = doses,
#       fill = age_class
#     ),
#     stat = "identity"
#   ) +
#   facet_grid(
#     age_class ~ state,
#     scales = "free_y"
#   )
# 
# effective_dose_data %>%
#   filter((state == "ACT" | state == "NT"), date >= "2021-08-30", vaccine == "pf", dose_number == "2") %>%
#   ggplot() +
#   geom_line(
#     aes(
#       x = date,
#       y = doses,
#       col = age_class
#     )
#   ) +
#   facet_wrap(
#     ~state,
#     scales = "free_y"
#   )




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
