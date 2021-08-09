source("R/lib.R")
source("R/functions.R")

doses <- nsw_vaccinations()

# manual check against published total doses
doses %>%
  filter(date == max(date)) %>%
  summarise(
    across(
      c(dose_1, dose_2, only_dose_1),
      sum
    )
  )

# manually check LGA populations against web searches to make sure they are in
# the right ball park
doses %>%
  filter(
    date == max(date),
    vaccine == "AstraZeneca"
  ) %>%
  group_by(lga, age) %>%
  summarise(
    population = first(population)
  ) %>%
  group_by(lga) %>%
  summarise(
    population = sum(population)
  )

# lookup to disaggregate coverages to 5y age groups
age_lookup <- tibble::tribble(
  ~age_5y, ~age, ~proportion_of_group,
  "0-4", "0-14", 5/15,
  "5-9", "0-14", 5/15,   
  "10-14", "0-14", 5/15,
  "15-19", "15-29", 5/15,
  "20-24", "15-29", 5/15,
  "25-29", "15-29", 5/15,
  "30-34", "30-39", 5/10,
  "35-39", "30-39", 5/10,
  "40-44", "40-49", 5/10,
  "45-49", "40-49", 5/10,
  "50-54", "50-59", 5/10,
  "55-59", "50-59", 5/10,
  "60-64", "60-69", 5/10,
  "65-69", "60-69", 5/10,
  "70-74", "70-79", 5/10,
  "75-79", "70-79", 5/10,
  "80+", "80+", 1/1
)

# check these proportions all sum to 1
age_lookup %>% group_by(age) %>%
  summarise(
    sum(proportion_of_group)
  )

# compute fractional coverage by type and number of doses, then compute average efficacy against delta
coverage <- doses %>%
  # remove places for which we don't have populations (rest of greater sydney,
  # rest of greater NSW)
  filter(
    !is.na(population)
  ) %>%
  # compute fractional coverage by dose and vaccine type
  pivot_wider(
    names_from = vaccine,
    values_from = c(dose_1, dose_2, only_dose_1)
  ) %>%
  # compute corrections to get the effective number/fraction vaccinated, accounting
  # for a delay to acquire immunity after vaccination
  arrange(
    lga, age, date
  ) %>%
  group_by(
    lga, age
  ) %>%
  mutate(
    across(
      c(only_dose_1_AstraZeneca, only_dose_1_Pfizer),
      .fns = list(
        correction = ~slider::slide2_dbl(
          date, .,
          .f = immunity_lag_correction,
          weeks_increase = 2,
          weeks_wait = 1,
          .before = Inf
        )
      )
    ),
    across(
      c(dose_2_AstraZeneca, dose_2_Pfizer),
      .fns = list(
        correction = ~slider::slide2_dbl(
          date, .,
          .f = immunity_lag_correction,
          weeks_increase = 2,
          weeks_wait = 0,
          .before = Inf
        )
      )
    )
  ) %>%
  # compute (fractional) coverage, and effective (fractional) coverage
  mutate(
    effective_only_dose_1_Pfizer = only_dose_1_Pfizer * only_dose_1_Pfizer_correction,
    effective_only_dose_1_Pfizer = only_dose_1_Pfizer * only_dose_1_Pfizer_correction,
    effective_only_dose_1_AstraZeneca = only_dose_1_AstraZeneca * only_dose_1_AstraZeneca_correction,
    effective_dose_2_Pfizer = dose_2_Pfizer * dose_2_Pfizer_correction,
    effective_dose_2_AstraZeneca = dose_2_AstraZeneca * dose_2_AstraZeneca_correction,
    any_vaccine = dose_1_Pfizer + dose_1_AstraZeneca,
    effective_any_vaccine = effective_only_dose_1_Pfizer +
      effective_only_dose_1_AstraZeneca +
      effective_dose_2_Pfizer +
      effective_dose_2_AstraZeneca,
    coverage_any_vaccine = any_vaccine / population,
    effective_coverage_any_vaccine = effective_any_vaccine / population,
    .after = population
  ) %>%
  mutate(
    across(
      c(
        only_dose_1_AstraZeneca,
        dose_2_AstraZeneca,
        only_dose_1_Pfizer,
        dose_2_Pfizer
      ),
      .fns = c(
        fraction = ~ . / any_vaccine
      ),
      .names = "{.fn}_{.col}"
    ),
    across(
      c(
        effective_only_dose_1_AstraZeneca,
        effective_dose_2_AstraZeneca,
        effective_only_dose_1_Pfizer,
        effective_dose_2_Pfizer
      ),
      .fns = c(
        fraction = ~ . / effective_any_vaccine
      ),
      .names = "{.fn}_{.col}"
    )
  ) %>%
  # combine this into a reduction in any-dose coverage 
  mutate(
    average_efficacy_transmission = average_efficacy(
      efficacy_az_2_dose = combine_efficacy(0.60, 0.65),
      efficacy_pf_2_dose = combine_efficacy(0.79, 0.65),
      efficacy_pf_1_dose = combine_efficacy(0.30, 0.46),
      efficacy_az_1_dose = combine_efficacy(0.18, 0.48),    
      proportion_pf_2_dose = fraction_dose_2_Pfizer,
      proportion_az_2_dose = fraction_dose_2_AstraZeneca,
      proportion_pf_1_dose = fraction_only_dose_1_Pfizer,
      proportion_az_1_dose = fraction_only_dose_1_AstraZeneca
    ),
    effective_average_efficacy_transmission = average_efficacy(
      efficacy_az_2_dose = combine_efficacy(0.60, 0.65),
      efficacy_pf_2_dose = combine_efficacy(0.79, 0.65),
      efficacy_pf_1_dose = combine_efficacy(0.30, 0.46),
      efficacy_az_1_dose = combine_efficacy(0.18, 0.48),    
      proportion_pf_2_dose = fraction_effective_dose_2_Pfizer,
      proportion_az_2_dose = fraction_effective_dose_2_AstraZeneca,
      proportion_pf_1_dose = fraction_effective_only_dose_1_Pfizer,
      proportion_az_1_dose = fraction_effective_only_dose_1_AstraZeneca
    ),
    average_efficacy_transmission = replace_na(average_efficacy_transmission, 0),
    effective_average_efficacy_transmission = replace_na(effective_average_efficacy_transmission, 0),
    .after = coverage_any_vaccine
  ) %>%
  ungroup() %>%
  right_join(
    age_lookup,
    by = "age"
  ) %>%
  relocate(
    age_5y, .after = age
  ) %>%
  select(
    -age,
    -population
  ) %>%
  rename(
    age = age_5y
  )

# estimate effects of vaccination on transmission
vaccination_effect <- coverage %>%
  group_by(
    lga, date
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

vaccination_effect %>%
  filter(date == max(date)) %>%
  select(
    -vaccination_transmission_multiplier,
    -date,
    -effective_vaccination_transmission_multiplier
  )

vaccination_effect %>%
  filter(date == Sys.Date() - 14) %>%
  select(
    -vaccination_transmission_multiplier,
    -date,
    -effective_vaccination_transmission_multiplier,
    -effective_vaccination_transmission_reduction_percent
  )

vaccination_effect %>%
  filter(date == max(date)) %>%
  select(
    lga,
    effective_vaccination_transmission_reduction_percent
  )

write_csv(vaccination_effect, "~/Desktop/nsw_lgas_vaccination_effect.csv")  
