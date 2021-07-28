source("R/lib.R")
source("R/functions.R")

format_nsw_vaccination_data <- function (
  file, sheet,
  skip_rows = 4,
  min_date = as.Date("2021-06-26"),
  age_names = c("15-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
  vaccines = c("AstraZeneca", "Pfizer"),
  deliverers = c("NSW Health","GP", "Commercial", "Other")
) {

  # read oin the sheet, and skip the bumpf at the top
  readxl::read_excel(
    path = file,
    sheet = sheet,
    skip = skip_rows
  ) %>%
    # keep the cumulative count up to this date
    rename(
      previous_dates = `...2`
    ) %>%
    # remove total columns on the RHS
    select(
      -starts_with("...")
    ) %>%
    # remove the first regional grouping label
    filter(
      Group != "Special Interest LGAs"
    ) %>%
    # work out what level of grouping each row indicates
    mutate(
      grouping_level = case_when(
        Group %in% age_names ~ "age",
        Group %in% vaccines ~ "vaccine",
        Group %in% deliverers ~ "deliverer",
        TRUE ~ "lga" 
      ),
      .after = Group
    ) %>%
    # remove other two regional grouping labels (need to do it here as they duplicate the LGA names)
    filter(
      !(duplicated(.) & grouping_level == "lga")
    ) %>%
    # unpack the grouping levels
    mutate(
      lga = case_when(
        grouping_level == "lga" ~ Group,
        TRUE ~ NA_character_
      ),
      deliverer = case_when(
        grouping_level == "deliverer" ~ Group,
        TRUE ~ NA_character_
      ),
      vaccine = case_when(
        grouping_level == "vaccine" ~ Group,
        TRUE ~ NA_character_
      ),
      age = case_when(
        grouping_level == "age" ~ Group,
        TRUE ~ NA_character_
      ),
      .after = Group
    ) %>%
    # fill down to label each age-specific coverafge with the other groupings
    fill(
      c(lga, deliverer, vaccine, age)
    ) %>%
    # throw out grouping totals and extraneous columns
    filter(
      grouping_level == "age"
    ) %>%
    select(
      -Group,
      -grouping_level
    ) %>%
    # convert to long format
    pivot_longer(
      cols = -c(lga, deliverer, vaccine, age),
      values_to = "count",
      names_to = "date"
    ) %>%
    # rename the date columns
    mutate(
      date = case_when(
        date == "previous_dates" ~ min_date - 1,
        # stringr::str_starts(date, "...") ~ NA,
        TRUE ~ as.Date(
          suppressWarnings(
            as.numeric(
              date
            )
            ),
          origin = "1899-12-30"
        )
      )
    ) %>%
    # convert to cumulative vaccinations
    group_by(
      lga, deliverer, vaccine, age
    ) %>%
    arrange(
      lga, deliverer, vaccine, age, date
    ) %>%
    mutate(
      cumulative_count = cumsum(count)
    ) %>%
    # collapse the over 80 age group
    mutate(
      age = case_when(
        age == "80-89" ~ "80+",
        age == "90+" ~ "80+",
        TRUE ~ age
      )
    ) %>%
    group_by(
      lga, vaccine, age, date
    ) %>%
    summarise(
      across(
        c(count, cumulative_count),
        sum
      ),
      .groups = "drop"
    ) %>%
    select(
      -count
    )
  
}

nsw_lga_populations <- function(
  file = "data/vaccinatinon/nsw/Coverage_data_July27/AIR Data 20210727 rates - v2.xlsx",
  sheet = "LGA Sex x Age Dose 1 rate"
) {
  readxl::read_excel(
    path = file,
    sheet = sheet,
    skip = 5
  ) %>%
    select(
      lga = `...1`,
      sex = `...2`,
      age = `...3`,
      population = `Population...5`
    ) %>%
    filter(
      sex == "Total",
      age != "15+",
      age != "All ages"
    ) %>%
    select(
      -sex
    ) %>%
    mutate(
      age = case_when(
        age == "80-84" ~ "80+",
        age == "85+" ~ "80+",
        TRUE ~ age
      )
    ) %>%
    group_by(
      lga, age
    ) %>%
    summarise(
      population = sum(population),
      .groups = "drop"
    )
}

nsw_vaccinations <- function(
  file = "data/vaccinatinon/nsw/Coverage_data_July27/AIR Data 20210727 Special Interest LGAs -v2.xlsx"
) {
  
  # lookup to remove (C), (A) etc from lga in doses
  lga_lookup <- tibble::tribble(
    ~lga_long, ~lga_short,
    "Blacktown (C)", "Blacktown",
    "Canterbury-Bankstown (A)", "Canterbury-Bankstown",
    "Cumberland (A)", "Cumberland",
    "Fairfield (C)", "Fairfield",
    "Liverpool (C)", "Liverpool",
    "Rest of Greater Sydney", NA_character_,
    "Rest of NSW", NA_character_
  )
  
  # load and combine doses 1 and 2
  doses <- full_join(
    file %>%
      format_nsw_vaccination_data(
        sheet = "Dose 1"
      ) %>%
      rename(
        dose_1 = cumulative_count
      ),
    file %>%
      format_nsw_vaccination_data(
        sheet = "Dose 2"
      ) %>%
      rename(
        dose_2 = cumulative_count
      ),
    by = c("lga", "vaccine", "age", "date")
  ) %>%
    mutate(
      only_dose_1 = dose_1 - dose_2
    ) %>%
    # shorten LGA names for population join
    left_join(
      lga_lookup,
      by = c("lga" = "lga_long")
    ) %>%
    mutate(
      lga = lga_short
    ) %>%
    select(
      -lga_short
    )
  
  pops <- nsw_lga_populations()
  
  combinations <- expand_grid(
    lga = unique(doses$lga),
    vaccine = unique(doses$vaccine),
    age = unique(pops$age),
    date = unique(doses$date)
  )
  
  # join on populations including empty values for 0-15s 
  combinations %>%
    left_join(
      doses,
      by = c("lga", "vaccine", "age", "date")
    ) %>%
    left_join(
      pops,
      by = c("lga", "age")
    ) %>%
    mutate(
      across(
        c(dose_1, dose_2, only_dose_1),
        ~replace_na(., 0)
      )
    )
    
}

# Account for lag in vaccination effect.

# For those with only 1 dose, no increase for 1 week, then linear increase from
# 0% to 100% over the next two weeks. For those with 2 doses, linear increase
# from 0% to 100% over the next two weeks.

# Compute a correction factor to downgrade the coverage (by number of doses)
# based on the fraction of all people with that number of doses that received it
# at each point in time the last 2-3 weeks.
immunity_lag_correction <- function(date, coverage,
                                    weeks_increase = 2,
                                    weeks_wait = 1) {
  
  # compute the current coverage (by dose/vaccine)
  max_date <- which.max(date)
  latest_coverage <- coverage[max_date]
  
  # compute the diff of coverages for all dates to get the proportion of the
  # population added on/by that date
  new_coverages <- diff(c(0, coverage))
  
  # compute the ratio of the proportion added on each previous date to the
  # current coverage (should sum to 1)
  date_weights <- new_coverages / latest_coverage
  
  # multiply each of those ratios by the relative effect based on the date difference¿
  week_diff <- as.numeric(date[max_date] - date) / 7
  relative_effect <- pmax(0, pmin(1, (week_diff - weeks_wait) / weeks_increase))
  
  # sum the ratios to get the correction multiplier
  correction <- sum(relative_effect * date_weights)
  
  if (is.na(correction)) {
    correction <- 0
  }
  
  correction
  
}

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




