# Compute under-vaccination of cases relative to case-weighted LGA averages. Ie.
# the degree to which cases are concentrated in undervaccinated groups within
# LGAs.
source("R/functions.R")

# convert an age into an age range
assign_age <- function(age, grouping = "age_5y") {
  
  age_class_5y <- age_classes()
  lookup <- age_lookup()
  
  grouping <- match.arg(grouping, names(lookup))
  
  # assign 5y age bands to ages
  lower <- outer(age, age_class_5y$lower, FUN = ">=")
  upper <- outer(age, age_class_5y$upper, FUN = "<=")
  idx <- apply(lower & upper, 1, which)
  age_5y <- age_class_5y$classes[idx]
  
  lookup_idx <- match(age_5y, lookup$age_5y)
  lookup[[grouping]][lookup_idx]
  
}

# compute the expected fraction of cases unvaccinated at each time point, given
# the age-specific coverage with any vaccine, and average efficacies on
# susceptibility and on onward transmission
fraction_cases_unvaccinated <- function(
  efficacy_susceptibility,
  efficacy_onward,
  coverage_any_vaccine
) {
  
  # transmission between unvaccinated people, no effect of vaccines and scale down
  # to vaccinated population
  unvax_unvax <- baseline_matrix() %>%
    sweep(1, 1 - coverage_any_vaccine, FUN = "*")
  
  # transmission between vaccinated people, susceptibility and onward transmission
  # effects and scale down to vaccinated population
  vax_vax <- baseline_matrix() %>%
    sweep(1, 1 - efficacy_susceptibility, FUN = "*") %>%
    sweep(2, 1 - efficacy_onward, FUN = "*") %>%
    sweep(1, coverage_any_vaccine, FUN = "*")
  
  # transmission from unvaccinated to vaccinated people (account for
  # susceptibility effects on rows) and scale down to vaccinated population
  # fraction
  unvax_vax <- baseline_matrix() %>%
    sweep(1, 1 - efficacy_susceptibility, FUN = "*") %>%
    sweep(1, coverage_any_vaccine, FUN = "*")
  
  # transmission from vaccinated to unvaccinated people (account for transmission
  # effects) and scale down to unvaccinated population
  vax_unvax <- baseline_matrix() %>%
    sweep(2, 1 - efficacy_onward, FUN = "*") %>%
    sweep(1, 1 - coverage_any_vaccine, FUN = "*")
  
  vax_structured_matrix <- rbind(
    cbind(unvax_unvax, vax_unvax),
    cbind(unvax_vax, vax_vax)
  )
  
  stable_state <- Re(eigen(vax_structured_matrix)$vectors[, 1])
  fraction_cases_unvaccinated <- stable_state[1:17] / (stable_state[1:17] + stable_state[18:34])
  
  fraction_cases_unvaccinated
  
}

# get expected rate of vaccination among cases (if they were a random sample of
# their LGA population within each age group), by computing the age distribution
# of cases in each LGA, then computing the case-age-weighted coverage of first
# and second doses

# load case data to get age distribution in each LGA and time period
cases <- read_csv(
  "~/not_synced/vaccination/nsw/cases_29.csv",
  col_types = cols(
    .default = col_character(),
    AGE_AT_EVENT_YEARS = col_double(),
    SA2_2016_CODE = col_double(),
    EARLIEST_CONFIRMED_OR_PROBABLE = col_date(format = ""),
    CALCULATED_ONSET_DATE = col_date(format = ""),
    SYMPTOM_ONSET_DATE = col_date(format = ""),
    LAST_CONTACT_WITH_CASE_DATE = col_date(format = ""),
    INITIAL_CLOSE_CONTACT_INTERVIEW_DATE = col_date(format = ""),
    INTERVIEWED_DATE = col_date(format = ""),
    DATE_ISOLATION_BEGAN = col_date(format = ""),
    SETTING_OF_TRANSMISSION_DATE = col_date(format = ""),
    earliest_detected = col_date(format = ""),
    link_CALCULATED_ONSET_DATE = col_date(format = ""),
    link_EARLIEST_CONFIRMED_OR_PROBABLE = col_date(format = ""),
    link_AGE_AT_EVENT_YEARS = col_double(),
    link_SA2_2016_CODE = col_double(),
    link_earliest_detected = col_date(format = "")
  )
)

sa2_lga_concordance <- read_csv(
  "data/spatial/abs/CG_SA2_2016_LGA_2018 - All.csv",
  col_types = cols(
    SA2_MAINCODE_2016 = col_double(),
    SA2_NAME_2016 = col_character(),
    LGA_CODE_2018 = col_double(),
    LGA_NAME_2018 = col_character(),
    RATIO_FROM_TO = col_double(),
    INDIV_TO_REGION_QLTY_INDICATOR = col_character(),
    OVERALL_QUALITY_INDICATOR = col_character(),
    BMOS_NULL_FLAG = col_double()
  )
) %>%
  select(
    sa2 = SA2_MAINCODE_2016,
    lga = LGA_NAME_2018,
    weight = RATIO_FROM_TO
  )

# load the vaccination coverages by LGA, age and period
lga_coverage <- read_csv(
  "outputs/nsw/nsw_lgas_vaccination_coverage.csv",
  col_types = cols(
    X1 = col_double(),
    scenario = col_character(),
    coverage_scenario = col_character(),
    date = col_date(format = ""),
    lga = col_character(),
    age_air_80 = col_character(),
    age = col_character(),
    forecast = col_logical(),
    coverage_any_vaccine = col_double(),
    average_efficacy_transmission = col_double(),
    fraction_dose_1_AstraZeneca = col_double(),
    fraction_dose_2_AstraZeneca = col_double(),
    fraction_dose_1_Pfizer = col_double(),
    fraction_dose_2_Pfizer = col_double(),
    population = col_double()
  )
) %>%
  select(-X1) %>%
  filter(
    scenario == "baseline",
    coverage_scenario == "max 100% coverage",
    !forecast
  ) %>%
  mutate(
    fraction_dose_1 = fraction_dose_1_AstraZeneca + fraction_dose_1_Pfizer,
    fraction_dose_2 = fraction_dose_2_AstraZeneca + fraction_dose_2_Pfizer,
    coverage_dose_1 = coverage_any_vaccine * fraction_dose_1,
    coverage_dose_2 = coverage_any_vaccine * fraction_dose_2,
  ) %>%
  select(
    date,
    lga,
    age,
    coverage_dose_1,
    coverage_dose_2,
    coverage_any_vaccine,
    average_efficacy_transmission,
    starts_with("fraction")
  )

# compute the population vaccination coverage (by 1/2 doses) for the age, lga,
# and date of each case
case_coverage_pop <- cases %>%
  select(
    date = EARLIEST_CONFIRMED_OR_PROBABLE,
    age = AGE_AT_EVENT_YEARS,
    sa2 = SA2_2016_CODE
  ) %>%
  left_join(
    sa2_lga_concordance,
    by = "sa2"
  ) %>%
  filter(
    date <= as_date("2021-08-14") & date >= as_date("2021-07-18"),
    !is.na(sa2)
  ) %>%
  mutate(
    # convert specific age to age group
    age = assign_age(age, "age_5y")
  ) %>%
  left_join(
    lga_coverage,
    by = c("date", "lga", "age")
  )


# get the expected fraction of cases vaccinated under an assumption of random
# mixing between vaccinated and unvaccinated people

efficacy_az_1_dose_susceptibility <- 0.18
efficacy_pf_1_dose_susceptibility <- 0.30
efficacy_az_2_dose_susceptibility <- 0.60
efficacy_pf_2_dose_susceptibility <- 0.79

efficacy_az_1_dose_onward <- 0.48
efficacy_pf_1_dose_onward <- 0.46
efficacy_az_2_dose_onward <- 0.65
efficacy_pf_2_dose_onward <- 0.65

expected_case_coverage <- lga_coverage %>%
  filter(
    date %in% case_coverage_pop$date,
    lga %in% case_coverage_pop$lga
  ) %>%
  mutate(
    age = factor(
      age,
      levels = colnames(
        baseline_matrix()
      )
    )
  ) %>%
  arrange(age) %>%
  mutate(
    susceptibility = average_efficacy(
      efficacy_az_1_dose = efficacy_az_1_dose_susceptibility,    
      efficacy_pf_1_dose = efficacy_pf_1_dose_susceptibility,
      efficacy_az_2_dose = efficacy_az_2_dose_susceptibility,
      efficacy_pf_2_dose = efficacy_pf_2_dose_susceptibility,
      proportion_pf_2_dose = fraction_dose_2_Pfizer,
      proportion_az_2_dose = fraction_dose_2_AstraZeneca,
      proportion_pf_1_dose = fraction_dose_1_Pfizer,
      proportion_az_1_dose = fraction_dose_1_AstraZeneca
    ),
    onward = average_efficacy(
      efficacy_az_1_dose = efficacy_az_1_dose_onward,    
      efficacy_pf_1_dose = efficacy_pf_1_dose_onward,
      efficacy_az_2_dose = efficacy_az_2_dose_onward,
      efficacy_pf_2_dose = efficacy_pf_2_dose_onward,
      proportion_pf_2_dose = fraction_dose_2_Pfizer,
      proportion_az_2_dose = fraction_dose_2_AstraZeneca,
      proportion_pf_1_dose = fraction_dose_1_Pfizer,
      proportion_az_1_dose = fraction_dose_1_AstraZeneca
    )
  ) %>%
  group_by(lga, date) %>%
  mutate(
    fraction_unvaccinated = fraction_cases_unvaccinated(
      efficacy_susceptibility = susceptibility,
      efficacy_onward = onward,
      coverage_any_vaccine = coverage_any_vaccine)
  )

# compute the expected vaccination coverage among all cases in the date and time
case_expected_coverages <- case_coverage_pop %>%
  select(
    date,
    age,
    lga,
    weight
  ) %>%
  left_join(
    expected_case_coverage,
    by = c("date", "age", "lga")
  ) %>%
  mutate(
    # assign the time period
    period = case_when(
      date > as_date("2021-08-06") ~ "week ending August 14",
      date > as_date("2021-07-31") ~ "week ending August 7",
      date > as_date("2021-07-24") ~ "week ending July 31",
      date > as_date("2021-07-17") ~ "week ending July 24",
      TRUE ~ NA_character_
    ),
  ) %>%
  # within each period, compute the LGA/age case weights, fo computing expected coverages
  group_by(period) %>%
  summarise(
    across(
      "fraction_unvaccinated",
      ~ weighted.mean(.x, weight)
    )
  ) %>%
  ungroup()

# get the population vaccination coverages in the periods for which we have
# vaccination coverage data
period_coverages <- case_coverage_pop %>%
  mutate(
    # assign the time period
    period = case_when(
      date > as_date("2021-08-06") ~ "week ending August 14",
      date > as_date("2021-07-31") ~ "week ending August 7",
      date > as_date("2021-07-24") ~ "week ending July 31",
      date > as_date("2021-07-17") ~ "week ending July 24",
      TRUE ~ NA_character_
    ),
  ) %>%
  # within each period, compute the LGA/age case weights, fo computing expected coverages
  group_by(period) %>%
  summarise(
    across(
      starts_with("coverage"),
      ~ weighted.mean(.x, weight)
    )
  ) %>%
  ungroup() %>%
  mutate(
    no_dose = 1 - (coverage_dose_1 + coverage_dose_2)
  )

# observed vaccination statuses among cases
case_coverages <- tibble::tribble(
                  ~period,  ~cases_no_dose,  ~cases_dose_1,  ~cases_dose_2,
  "week ending August 14",            1864,            272,             95,
  "week ending August 7",             1349,            174,             43,
  "week ending July 31",              1071,            133,             30,
  "week ending July 24",              789,             50,              10
)

# get the ratio between the observed coverages and what we would have expected
# given given the LGA/age group coverages
fractions_vaccinated <- case_coverages %>%
  mutate(
    cases_known_status = cases_no_dose + cases_dose_1 + cases_dose_2,
    fraction_no_dose = cases_no_dose / cases_known_status,
    fraction_dose_1 = cases_dose_1 / cases_known_status,
    fraction_dose_2 = cases_dose_2 / cases_known_status,
  ) %>%
  left_join(
    period_coverages,
    by = "period"
  ) %>%
  left_join(
    case_expected_coverages
  ) %>%
  mutate(
    case_fraction_vaccinated = 1 - fraction_no_dose,
    matched_population_fraction_vaccinated = 1 - no_dose,
    expected_case_fraction_vaccinated = 1 - fraction_unvaccinated
  ) %>%
  select(
    period,
    ends_with("_vaccinated")
  )


fractions_vaccinated




# Stretch goal: consider a model structured by vaccinated, partially vaccinated,
# unvaccinated, and age, to see whether the neutral dynamics explain this, or
# whether there's a demographic effect as well (stratify the Prem matrix by
# vaccination type, and weight contacts accordingly)