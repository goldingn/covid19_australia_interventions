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
    coverage_dose_2
  )

# add on LGA concordance; and summarise by period, lga, and age
period_coverages <- cases %>%
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
ratios <- case_coverages %>%
  mutate(
    cases_known_status = cases_no_dose + cases_dose_1 + cases_dose_2,
    fraction_no_dose = cases_no_dose / cases_known_status,
    fraction_dose_1 = cases_dose_1 / cases_known_status,
    fraction_dose_2 = cases_dose_2 / cases_known_status,
  ) %>%
  select(
    period,
    starts_with("fraction")
  ) %>%
  left_join(
    period_coverages,
    by = "period"
  ) %>%
  mutate(
    ratio_any_doses = (1 - fraction_no_dose) / (1 - no_dose),
    ratio_dose_1 = fraction_dose_1 / coverage_dose_1,
    ratio_dose_2 = fraction_dose_2 / coverage_dose_2,
  ) %>%
  select(
    period,
    starts_with("ratio")
  )


ratios 

# Stretch goal: consider a model structured by vaccinated, partially vaccinated,
# unvaccinated, and age, to see whether the neutrall dynamics explain this, or
# whether there's a demographic effect as well (stratify the Prem matrix by
# vaccination type, and weight contacts accordingly)