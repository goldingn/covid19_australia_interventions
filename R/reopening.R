# compute TP for difference PHSM scenarios from an Reff run with TTIQ in component1
source("R/lib.R")
source("R/functions.R")

# load TP timeseries estimates for delta
# tp_delta_sims <- read.csv("~/Dropbox/covid_output/ttiq/projection/delta/r_eff_1_local_samples.csv")
tp_delta_sims <- read.csv("~/Desktop/delta_r_eff_1_local_samples.csv")

# load Reff model object (to get TTIQ and surveillance effects)
obj <- readRDS("~/Dropbox/covid_output/ttiq/fitted_reff_model.RDS")
reff_data <- obj$data
greta_arrays <- obj$greta_arrays

surveillance_effect <- greta_arrays$surveillance_reff_local_reduction %>%
  `colnames<-`(reff_data$states) %>%
  as_tibble() %>%
  mutate(
    date = reff_data$dates$infection_project
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "state",
    values_to = "surveillance_effect"
  )

extra_ttiq_effect <- greta_arrays$extra_isolation_local_reduction %>%
  `colnames<-`(reff_data$states) %>%
  as_tibble() %>%
  mutate(
    date = reff_data$dates$infection_project
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "state",
    values_to = "extra_ttiq_effect"
  )

effects <- surveillance_effect %>%
  left_join(
    extra_ttiq_effect,
    by = c("date", "state")
  )

# pull out the maximum effects
maximum_effects <- effects %>%
  summarise(
    surveillance = min(surveillance_effect),
    extra_ttiq = min(extra_ttiq_effect)
  )

# compute the mean of 'observed' TP over time in each state, and add on the
# surveillance and TTIQ effects, then compute TP with these at their maximum values
tp_delta <- tp_delta_sims %>%
  mutate(
    date = as_date(date)
  ) %>%
  pivot_longer(
    cols = starts_with("sim"),
    names_to = "sim"
  ) %>%
  group_by(state, date) %>%
  summarise(
    observed_tp = mean(value),
    .groups = "drop"
  ) %>%
  left_join(
    effects,
    by = c("date", "state")
  ) %>%
  mutate(
    # divide out the surveillance and ttiq effects to get the Delta TP in the absence of these measures
    tp_no_surveillance_ttiq = observed_tp / (extra_ttiq_effect * surveillance_effect),
    # add in the maximal surveillance effect
    tp_no_ttiq = tp_no_surveillance_ttiq * maximum_effects$surveillance,
    # add in the maximal extra TTIQ effect
    tp_with_ttiq = tp_no_ttiq * maximum_effects$extra_ttiq,
  )

# make a look up table of reference periods for PHSMs
phsm_periods <- bind_rows(
    # VIC vs NSW at the peak of the Vic stage 4 lockdown are high and low
    tibble::tribble(
      ~phsm_scenario, ~state, ~date,
      "high", "VIC", as_date("2020-08-23"),
      "low", "NSW", as_date("2020-08-23")
    ),
    # Various snap lockdowns as medium
    # excluding those since the start of June and NT (less data) and QLD in
    # January (confounded with public holidays) &
    # taking the behaviour 2 days after the lockdown is imposed
    tibble::tribble(
      ~phsm_scenario, ~state, ~date,
      "medium", "SA", as_date("2020-11-19"),
      "medium", "QLD", as_date("2021-03-29"),
      "medium", "VIC", as_date("2021-02-13"),
      "medium", "VIC", as_date("2021-05-28"),
      "medium", "WA", as_date("2021-01-31"),
      "medium", "WA", as_date("2021-04-24")
    ) %>%
      mutate(
        date = date + 2
      ),
    # NSW in March 2021 is baseline
    tibble::tibble(
      phsm_scenario = "baseline",
      state = "NSW",
      date = seq(
        as_date("2021-03-01"),
        as_date("2021-03-31"),
        by = 1
      )
    ),
    # WA in March 2021 is an alternative baseline
    tibble::tibble(
      phsm_scenario = "baseline_low",
      state = "WA",
      date = seq(
        as_date("2021-03-01"),
        as_date("2021-03-31"),
        by = 1
      )
    )
    
)

# pull out the mean TPs for each PHSM scenario
phsm_scenarios <- tp_delta %>%
  left_join(
    phsm_periods, by = c("date", "state")
  ) %>%
  filter(
    !is.na(phsm_scenario)
  ) %>%
  group_by(
    phsm_scenario
  ) %>%
  summarise(
    tp_no_ttiq = mean(tp_no_ttiq),
    tp_with_ttiq = mean(tp_with_ttiq),
  ) %>%
  # reshape to have TTIQ/baseline scenarios as rows
  pivot_longer(
    cols = -phsm_scenario,
    names_to = "ttiq",
    values_to = "tp"
  ) %>%
  mutate(
    phase = ifelse(
      ttiq == "tp_with_ttiq",
      "A",
      "B"
    )
  ) %>%
  select(
    -ttiq
  ) %>%
  # now reshape to have baseline as a column
  pivot_wider(
    names_from = phsm_scenario,
    values_from = tp
  ) %>%
  pivot_longer(
    cols = starts_with("baseline"),
    names_to = "baseline_type",
    values_to = "baseline"
  ) %>%
  mutate(
    baseline_type = case_when(
      baseline_type == "baseline" ~ "standard",
      baseline_type == "baseline_low" ~ "low (WA)",
      baseline_type == "baseline_low2" ~ "low (SA)"
    )
  ) %>%
  rename_at(
    vars(baseline, low, medium, high),
    ~paste0("tp_", .)
  ) %>%
  relocate(
    phase,
    baseline_type,
    tp_baseline,
    tp_low,
    tp_medium,
    tp_high
  )
    
# tp_delta %>%
#   ggplot(
#     aes(
#       y = tp_no_ttiq,
#       x = date
#     )
#   ) +
#   geom_line() +
#   geom_vline(
#     xintercept = as_date("2020-08-23"),
#     linetype = 2
#   ) +
#   geom_hline(
#     yintercept = 1,
#     linetype = 3
#   ) +
#   facet_wrap(
#     ~state,
#     ncol = 2
#   ) +
#   scale_y_continuous(
#     breaks = 1:8,
#     # trans = "log"
#   ) +
#   ylab("TP (no contact tracing)") +
#   theme_minimal()

# load vaccination scenarios and compute completion dates
completion_file <- "data/vaccinatinon/quantium_simulations/20210716 Completion rates over time.xlsx"
completion <- completion_file %>%
  readxl::read_xlsx(sheet = 4) %>%
  rename(
    `Scenario 1` = `Senario 1`,
    week = `Week starting`
  ) %>%
  mutate(
    week = as_date(week)
  ) %>%
  pivot_longer(
    cols = -week,
    names_to = "vacc_scenario",
    values_to = "coverage"
  ) %>%
  mutate(
    vacc_scenario = gsub("^Scenario ", "", vacc_scenario),
    vacc_scenario = as.integer(vacc_scenario)
  ) %>%
  right_join(
    expand_grid(
      vacc_scenario = unique(.$vacc_scenario),
      target_coverage = c(0.5, 0.6, 0.7, 0.8)
    ),
    by = "vacc_scenario"
  ) %>%
  filter(
    coverage > target_coverage
  ) %>%
  group_by(
    vacc_scenario,
    target_coverage
  ) %>%
  summarise(
    week = min(week),
    .groups = "drop"
  )

# for each of these, pull out the relevant age/dose/type distributions from the simulations
vaccinations <- read.csv("data/vaccinatinon/quantium_simulations/vaccinations.csv")

# combine this with 'completion' to flag get the final dates of administration for aggregation.
week_lookup <- read.csv("data/vaccinatinon/quantium_simulations/dim_time.csv") %>%
  mutate(
    week = as_date(week_starting)
  ) %>%
  select(
    -week_starting
  )

vacc_total <- vaccinations %>%
  group_by(
    scenario,
    age_band_id,
    vaccine,
    time_dose_1,
    time_dose_2
  ) %>%
  summarise(
    num_people = sum(num_people),
    .groups = "drop"
  ) 

vacc_cohorts <- expand_grid(
  scenario = unique(completion$vacc_scenario),
  coverage = unique(completion$target_coverage),
  time_dose_1 = unique(vacc_total$time_dose_1),
  time_dose_2 = unique(vacc_total$time_dose_2),
  age_band_id = unique(vacc_total$age_band_id),
  vaccine = unique(vacc_total$vaccine)
) %>%
  left_join(
    vacc_total,
    by = c("scenario", "time_dose_1", "time_dose_2", "age_band_id", "vaccine")
  ) %>%
  filter(
    !is.na(num_people)
  ) %>%
  # convert week numbers to dates
  left_join(
    rename(week_lookup, week_dose_1 = week),
    by = c("time_dose_1" = "time")
  ) %>%
  left_join(
    rename(week_lookup, week_dose_2 = week),
    by = c("time_dose_2" = "time")
  ) %>%
  pivot_longer(
    cols = starts_with("week_dose_"),
    names_to = "dose",
    values_to = "week"
  ) %>%
  mutate(
    dose = gsub("week_dose_", "", dose)
  )

quantium_age_lookup <- read_csv(
  "data/vaccinatinon/quantium_simulations/dim_age_band.csv",
  col_types = cols(
    age_band_id = col_double(),
    age_band = col_character()
  )
)

age_lookup <- tibble::tribble(
  ~age_lower, ~age_upper, ~age_band_quantium, ~age_band_5y,
  0,         4, "0-9", "0-4",
  5,         9, "0-9", "5-9",
  10,        10, "10-19", "10-14",
  11,        11, "10-19", "10-14",
  12,        12, "10-19", "10-14",
  13,        13, "10-19", "10-14",
  14,        14, "10-19", "10-14",
  15,        15, "10-19", "15-19",
  16,        16, "10-19", "15-19",
  17,        17, "10-19", "15-19",
  18,        19, "10-19", "15-19",
  20,        24, "20-29", "20-24",
  25,        29, "20-29", "25-29",
  30,        34, "30-39", "30-34",
  35,        39, "30-39", "35-39",
  40,        44, "40-49", "40-44",
  45,        49, "40-49", "45-49",
  50,        54, "50-59", "50-54",
  55,        59, "50-59", "55-59",
  60,        64, "60-69", "60-64",
  65,        69, "60-69", "65-69",
  70,        74, "70-79", "70-74",
  75,        79, "70-79", "75-79",
  80,        84, "80+", "80+",
  85,        89, "80+", "80+",
  90,        94, "80+", "80+",
  95,        99, "80+", "80+",
  100,       999, "80+",  "80+"
) %>%
  left_join(
    quantium_age_lookup,
    by = c("age_band_quantium" = "age_band")
  )


pop_data <- read_csv(
  "data/vaccinatinon/2021-07-13-census-populations.csv",
  col_types = cols(
    ste_name16 = col_character(),
    sa3_name16 = col_character(),
    sa2_name16 = col_character(),
    mmm2019 = col_double(),
    age_lower = col_double(),
    age_upper = col_double(),
    is_indigenous = col_logical(),
    is_comorbidity = col_logical(),
    vaccine_segment = col_character(),
    population = col_double()
  )
)

# compute vaccine-eligible population in each age bin, for age-specific eligible coverage estimates
age_pops_eligible <- pop_data %>%
  filter(
    age_lower >= 16
  ) %>%
  left_join(
    age_lookup,
    by = c("age_lower", "age_upper")
  ) %>%
  group_by(
    age_band_quantium, age_band_id
  ) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  )

# compute total and eligible (16+) populations
eligible_population <- pop_data %>%
  filter(
    age_lower >= 16
  ) %>%
  summarise(
    population = sum(population)
  ) %>%
  pull(
    population
  )

total_population <- pop_data %>%
  summarise(
    population = sum(population)
  ) %>%
  pull(
    population
  )

# join to dates of targets being achieved, filter, and sum to get 1 dose and 2
# dose counts and age-bin coverages
vacc_coverage <- vacc_cohorts %>%
  # combine moderna and Pfizer for computing vaccine effectiveness
  mutate(
    vaccine = case_when(
      vaccine == 1 ~ "mRNA",
      vaccine == 2 ~ "AZ",
      vaccine == 3 ~ "mRNA"
    )
  ) %>%
  left_join(
    rename(
      completion,
      target_week = week
    ),
    by = c(
      "scenario" = "vacc_scenario",
      "coverage" = "target_coverage"
    )
  ) %>%
  # remove vaccinations after the target week
  filter(
    week <= target_week
  ) %>%
  # compute cumulative vaccinations by the the groups we need (note dose 1 is *
  # any* dose 1, so need subtract dose 2s to get population with *only* dose 1)
  group_by(
    scenario,
    coverage,
    age_band_id,
    vaccine,
    dose
  ) %>%
  summarise(
    num_people = sum(num_people),
    .groups = "drop"
  ) %>%
  mutate(
    dose = paste0("dose_", dose)
  ) %>%
  pivot_wider(
    names_from = "dose",
    values_from = "num_people"
  ) %>%
  mutate(
    dose_1 = replace_na(dose_1, 0),
    dose_2 = replace_na(dose_2, 0),
    only_dose_1 = dose_1 - dose_2
  ) %>%
  # collapse down to 1 row per scenario/age group
  pivot_wider(
    names_from = "vaccine",
    values_from = c("dose_1", "dose_2", "only_dose_1")
  ) %>%
  left_join(
    age_pops_eligible,
    by = "age_band_id"
  ) %>%
  mutate(
    any_dose_2 = dose_2_AZ + dose_2_mRNA,
    any_dose_1 = dose_1_AZ + dose_1_mRNA,
    any_only_dose_1 = only_dose_1_AZ + only_dose_1_mRNA
  ) %>%
  mutate(
    coverage_any_dose_2 = any_dose_2 / population,
    coverage_any_dose_1 = any_dose_1 / population,
    coverage_any_only_dose_1 = any_only_dose_1 / population,
    across(
      c(only_dose_1_AZ, dose_2_AZ, only_dose_1_mRNA, dose_2_mRNA),
      .fns = list(fraction = ~ . / any_dose_1),
      .names = "{.fn}_{.col}"
    )
  )

# check the fractions for each age group sum to one
vacc_coverage %>%
  mutate(
    fraction_sum = fraction_only_dose_1_AZ + fraction_dose_2_AZ + fraction_only_dose_1_mRNA + fraction_dose_2_mRNA,
    fractions_sum_to_one = abs(1 - fraction_sum) < 1e-6
  ) %>%
  # check this is correct for all scenarios and coverages
  summarise(
    across("fractions_sum_to_one", all)
  )

# check the coverage dates match these numbers, when calculated 3 ways:
# weighted mean on group level eligible population coverages;
# summing group level counts and summing by group-level eligible populations;
# summing group level counts and using external sum of eligible populations
# also check that the coverage estimates are identical using these methods
vacc_coverage %>%
  group_by(
    scenario,
    coverage
  ) %>%
  summarise(
    coverage_any_dose_2 = weighted_mean(x = coverage_any_dose_2, w = population),
    across(c(contains("dose"), population), sum),
    .groups = "drop"
  ) %>%
  mutate(
    observed_coverage_group_count = any_dose_2 / population,
    observed_coverage_total = any_dose_2 / eligible_population
  ) %>%
  mutate(
    target_met_groups = coverage_any_dose_2 >= coverage,
    target_met_group_count = observed_coverage_group_count >= coverage,
    target_met_total = observed_coverage_total >= coverage
  ) %>%
  mutate(
    identical_total = observed_coverage_total == coverage_any_dose_2,
    identical_group_count = observed_coverage_group_count == coverage_any_dose_2
  ) %>%
  # check this is correct for all scenarios and coverages
  summarise(
    across(starts_with("target_met"), all),
    across(starts_with("identical"), all)
  )

# create a lookup from quantium 10y age bins to the 5 year age bins in the NGM
# for computing the vaccination effect
age_lookup_quantium_5y <- age_lookup %>%
  mutate(
    age_band_5y = factor(
      age_band_5y,
      levels = age_classes()$classes
    )
  ) %>%
  group_by(
    age_band_5y
  ) %>%
  summarise(
    age_band_quantium = first(age_band_quantium),
    .groups = "drop"
  )

age_combinations <- expand_grid(
  scenario = unique(vacc_coverage$scenario),
  coverage = unique(vacc_coverage$coverage),
  age_band_5y = unique(age_lookup_quantium_5y$age_band_5y)
) %>%
  left_join(
    age_lookup_quantium_5y,
    by = "age_band_5y"
  )

# compute age-structured vaccination coverages, fractions of each type/dose,
# and disaggregate to 5-year age bins for estimating the vaccination effect
vacc_coverage_5y <- vacc_coverage %>%
  right_join(
    age_combinations,
    by = c("scenario", "coverage", "age_band_quantium")
  ) %>%
  select(
    scenario,
    target_coverage = coverage,
    age_band_5y,
    proportion_vaccinated = coverage_any_dose_1,
    proportion_fully_vaccinated = coverage_any_dose_2,
    starts_with("fraction_")
  ) %>%
  mutate(
    proportion_vaccinated = replace_na(proportion_vaccinated, 0),
    proportion_fully_vaccinated = replace_na(proportion_fully_vaccinated, 0),
    across(
      starts_with("fraction_"),
      ~ replace_na(., 0.25)
    ),
    # average_efficacy_transmission = 
  ) %>%
  arrange(
    scenario,
    target_coverage,
    age_band_5y
  )

# compute the average efficacy for each row from the fractions

# need to adjust the average efficacy function to handle the 2x2 table of
# fractions


# for now, compute fake vaccination coverages for Treasury
fake_vaccine_effect <- function(two_dose_coverage, remainder_one_dose_coverage = 0.1) {
  
  one_dose_coverage <- (1 - two_dose_coverage) * remainder_one_dose_coverage
  total_coverage <- two_dose_coverage + one_dose_coverage
  
  next_generation_matrix <- baseline_matrix()
  
  combine_efficacy <- function(infection, transmission) {
    1 - ((1 - infection) * (1 - transmission)) 
  }
  
  efficacy <- average_efficacy(
    efficacy_pf_2_dose = combine_efficacy(0.79, 0.65),
    efficacy_az_2_dose = combine_efficacy(0.60, 0.65),
    efficacy_pf_1_dose = combine_efficacy(0.30, 0.46),
    efficacy_az_1_dose = combine_efficacy(0.18, 0.48),
    proportion_pf = 0.5,
    proportion_2_dose = two_dose_coverage / total_coverage
  )
  
  coverage <- c(rep(0, 3), rep(total_coverage, 14))
  
  effect <- vaccination_transmission_effect(
    age_coverage = coverage,
    efficacy_mean = efficacy,
    next_generation_matrix = next_generation_matrix
  )
  
  effect$overall
  
}

vaccination_scenarios <- expand_grid(
  vacc_scenario = 1:2,
  vacc_coverage = c(50, 60, 70, 80)
) %>%
  group_by(
    vacc_scenario,
    vacc_coverage
  ) %>%
  mutate(
    vacc_effect = fake_vaccine_effect(vacc_coverage / 100)
  ) %>%
  ungroup()

# assuming restrictions pulse between two states: baseline and lockdown, with R
# for baseline greater than 1 (cases grow) and R for lockdown less than 1 (cases
# shrink), and the aim is to keep the long-term average of R at 1 (maintain case
# counts below a critical threshold), compute the fraction of the time that
# would need to be in the lockdown state.
fraction_lockdown <- function(
  R_baseline,
  R_lockdown
) {
  
  # compute the fraction of the time we would need to be in lockdown to maintain
  # an average R of 1
  fraction <- -log(R_baseline) / (log(R_lockdown) - log(R_baseline))
  # if the baseline TP is not above 1, the fraction is 0 as no lockdowns are needed
  fraction[R_baseline <= 1] <- 0
  # if the lockdown TP is not below 1, there is no fraction that can keep R at average 1
  fraction[R_lockdown >= 1] <- NA
  
  fraction
}

# combine all scenarios
scenarios <-
  expand_grid(
    phase = unique(phsm_scenarios$phase),
    baseline_type = unique(phsm_scenarios$baseline_type),
    vacc_scenario = unique(vaccination_scenarios$vacc_scenario),
    vacc_coverage = unique(vaccination_scenarios$vacc_coverage)
  ) %>%
  left_join(
    phsm_scenarios,
    by = c("phase", "baseline_type")
  ) %>%
  left_join(
    vaccination_scenarios,
    by = c("vacc_scenario", "vacc_coverage")
  ) %>%
  # compute the post-vaccination TPs
  mutate(
    across(
      starts_with("tp_"),
      .fns = list(vacc = ~ . * vacc_effect)
    )
  ) %>%
  # compute the fraction of time in lockdown
  mutate(
    p_low_vacc_vs_baseline_vacc = fraction_lockdown(tp_baseline_vacc, tp_low_vacc),
    p_medium_vacc_vs_baseline_vacc = fraction_lockdown(tp_baseline_vacc, tp_medium_vacc),
    p_high_vacc_vs_baseline_vacc = fraction_lockdown(tp_baseline_vacc, tp_high_vacc),
    p_medium_vacc_vs_low_vacc = fraction_lockdown(tp_low_vacc, tp_medium_vacc),
    p_high_vacc_vs_low_vacc = fraction_lockdown(tp_low_vacc, tp_high_vacc)
  )

write.csv(
  scenarios,
  file = paste0(
    "~/Desktop/tp_scenarios_FAKE_",
    Sys.Date(),
    ".csv"
  ),
  row.names = FALSE
)


scenarios %>%
  filter(
    phase == "B",
    baseline_type == "standard",
    vacc_scenario == 1
  ) %>%
  as.data.frame()

