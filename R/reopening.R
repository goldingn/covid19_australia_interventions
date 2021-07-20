# compute TP for difference PHSM scenarios from an Reff run with TTIQ in component1
source("R/lib.R")
source("R/functions.R")

# load TP timeseries estimates for delta
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

# pull out the optimal effects of contact tracing
optimal_effects <- effects %>%
  summarise(
    surveillance = min(surveillance_effect),
    extra_ttiq = min(extra_ttiq_effect)
  )

# and the effects on the case peak of the Victorian 2nd wave
partial_effects <- effects %>%
  filter(
    state == "VIC",
    date == as_date("2020-08-04")
  ) %>%
  select(
    surveillance = surveillance_effect,
    extra_ttiq = extra_ttiq_effect
  )

# output the distributions for times to isolation at these points for Eamon's model
tti_cdfs <- effects %>%
  filter(
    surveillance_effect == min(surveillance_effect),
  ) %>%
  filter(
    date == first(date)
  ) %>%
  select(
    date,
    state
  ) %>%
  mutate(
    ttiq = "optimal"
  ) %>%
  bind_rows(
    tibble(
      date = as_date("2020-08-04"),
      state = "VIC",
      ttiq = "partial"
    )
  ) %>%
  left_join(
    readRDS("outputs/isolation_cdfs.RDS"),
    by = c("date", "state")
  ) %>%
  select(
    -date, -state
  ) %>%
  pivot_wider(
    names_from = ttiq,
    values_from = ecdf
  )

tti_distributions <- tibble(
  days = environment(tti_cdfs$partial[[1]])$x,
  partial = environment(tti_cdfs$partial[[1]])$pdf
) %>%
  left_join(
    tibble(
      days = environment(tti_cdfs$optimal[[1]])$x,
      optimal = environment(tti_cdfs$optimal[[1]])$pdf
    ),
    by = "days"
  ) %>%
  mutate(
    optimal = replace_na(optimal, 0),
    optimal = optimal / sum(optimal),
    partial = partial / sum(partial)
  )

saveRDS(tti_distributions,
        file = "~/Desktop/tti_distributions.rds")

# barplot(tti_distributions$partial, names.arg = tti_distributions$days)
# barplot(tti_distributions$optimal, names.arg = tti_distributions$days)


# compute the mean of 'observed' TP over time in each state, and add on the
# surveillance and TTIQ effects, then compute TP with these at their optimal 
# and partial values
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
    tp_no_ttiq = observed_tp / (extra_ttiq_effect * surveillance_effect),
    # add in the optimal surveillance effect, but no extra TTIQ effect
    tp_minimal_ttiq = tp_no_ttiq * optimal_effects$surveillance,
    # add in the partial surveillance and extra TTIQ effect
    tp_partial_ttiq = tp_no_ttiq * partial_effects$surveillance * partial_effects$extra_ttiq,
    # add in the optimal surveillance and extra TTIQ effect
    tp_optimal_ttiq = tp_no_ttiq * optimal_effects$surveillance * optimal_effects$extra_ttiq,
  )

# make a look up table of reference periods for PHSMs
phsm_periods <- bind_rows(
    # VIC vs NSW at the peak of the Vic stage 4 lockdown are high and low
    tibble::tribble(
      ~phsm_scenario, ~state, ~date,
      "high", "VIC", as_date("2020-08-23"),
      "medium", "NSW", as_date("2021-07-01"),
      "low", "NSW", as_date("2020-08-23")
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
    tp_minimal_ttiq = mean(tp_minimal_ttiq),
    tp_partial_ttiq = mean(tp_partial_ttiq),
    tp_optimal_ttiq = mean(tp_optimal_ttiq)
  ) %>%
  # reshape to have TTIQ/baseline scenarios as rows
  pivot_longer(
    cols = -phsm_scenario,
    names_to = "ttiq",
    values_to = "tp"
  ) %>%
  mutate(
    ttiq = case_when(
      ttiq == "tp_no_ttiq" ~ "none",
      ttiq == "tp_minimal_ttiq" ~ "minimal",
      ttiq == "tp_partial_ttiq" ~ "partial",
      ttiq == "tp_optimal_ttiq" ~ "optimal"
    )
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
      baseline_type == "baseline_low" ~ "low (WA)"
    )
  ) %>%
  rename_at(
    vars(baseline, low, medium, high),
    ~paste0("tp_", .)
  ) %>%
  relocate(
    ttiq,
    baseline_type,
    tp_baseline,
    tp_low,
    tp_medium,
    tp_high
  )
    
# tp_delta %>%
#   ggplot(
#     aes(
#       y = tp_partial_ttiq,
#       x = date
#     )
#   ) +
#   geom_vline(
#     aes(
#       xintercept = date
#     ),
#     data = phsm_periods,
#     color = grey(0.7)
#   ) +
#   geom_hline(
#     yintercept = 1,
#     linetype = 2
#   ) +
#   geom_line() +
#   geom_line(
#     aes(
#       y = tp_optimal_ttiq,
#     ),
#     color = grey(0.4)
#   ) +
#   geom_line(
#     aes(
#       y = tp_no_ttiq,
#     ),
#     color = grey(0.4)
#   ) +
#   facet_wrap(
#     ~state,
#     ncol = 2
#   ) +
#   scale_y_continuous(
#     breaks = 1:8,
#     trans = "log"
#   ) +
#   ylab("baseline TP (alternative TTIQ effectiveness)") +
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
age_pops_eligible_10y <- pop_data %>%
  mutate(
    eligible = as.integer(age_lower >= 16)
  ) %>%
  left_join(
    age_lookup,
    by = c("age_lower", "age_upper")
  ) %>%
  group_by(
    age_band_quantium, age_band_id
  ) %>%
  summarise(
    eligible_population = sum(population * eligible),
    total_population = sum(population),
    .groups = "drop"
  )

age_pops_eligible_5y <- pop_data %>%
  mutate(
    eligible = as.integer(age_lower >= 16)
  ) %>%
  left_join(
    age_lookup,
    by = c("age_lower", "age_upper")
  ) %>%
  group_by(
    age_band_5y,
  ) %>%
  summarise(
    eligible_population = sum(population * eligible),
    total_population = sum(population),
    .groups = "drop"
  )

# compute total and eligible (16+) populations for sanity check
total_pops <- age_pops_eligible_10y %>%
  summarise(
    across(
      ends_with("population"),
      ~sum(.)
    )
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
    age_pops_eligible_10y,
    by = "age_band_id"
  ) %>%
  mutate(
    any_dose_2 = dose_2_AZ + dose_2_mRNA,
    any_dose_1 = dose_1_AZ + dose_1_mRNA,
    any_only_dose_1 = only_dose_1_AZ + only_dose_1_mRNA
  ) %>%
  mutate(
    coverage_any_dose_2 = any_dose_2 / total_population,
    coverage_any_dose_1 = any_dose_1 / total_population,
    coverage_any_only_dose_1 = any_only_dose_1 / total_population,
    across(
      c(only_dose_1_AZ, dose_2_AZ, only_dose_1_mRNA, dose_2_mRNA),
      .fns = list(fraction = ~ . / any_dose_1),
      .names = "{.fn}_{.col}"
    )
  )

# check the fractions for each age group sum to one
vacc_coverage %>%
  mutate(
    fraction_sum = fraction_only_dose_1_AZ +
      fraction_dose_2_AZ +
      fraction_only_dose_1_mRNA +
      fraction_dose_2_mRNA,
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
  mutate(
    coverage_any_dose_2_eligible = any_dose_2 / eligible_population,
  ) %>%
  group_by(
    scenario,
    coverage
  ) %>%
  summarise(
    coverage_any_dose_2_eligible = weighted_mean(x = coverage_any_dose_2_eligible, w = eligible_population),
    across(c(contains("dose"), eligible_population), sum),
    .groups = "drop"
  ) %>%
  mutate(
    observed_coverage_group_count = any_dose_2 / eligible_population,
    observed_coverage_total = any_dose_2 / total_pops$eligible_population
  ) %>%
  mutate(
    target_met_groups = coverage_any_dose_2_eligible >= coverage,
    target_met_group_count = observed_coverage_group_count >= coverage,
    target_met_total = observed_coverage_total >= coverage
  ) %>%
  mutate(
    identical_total = observed_coverage_total == coverage_any_dose_2_eligible,
    identical_group_count = observed_coverage_group_count == coverage_any_dose_2_eligible
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

# compute the proportion of each 10y population falling into each corresponding
# 5y group - use this to compute a coverage correction for the 15-19 age group
proportion_of_10y <- age_lookup_quantium_5y %>%
  left_join(
    age_pops_eligible_5y,
    by = "age_band_5y"
  ) %>%
  rename(
    total_population_5y = total_population
  ) %>%
  left_join(
    age_pops_eligible_10y,
    by = "age_band_quantium"
  ) %>%
  rename(
    total_population_10y = total_population
  ) %>%
  mutate(
    proportion_of_10y = total_population_5y / total_population_10y
  ) %>%
  select(
    age_band_5y,
    age_band_quantium,
    proportion_of_10y
  )

# compute the fraction of the (10-14 and 15-19) 5y populations that are in the
# currently-ineligible school ages of 12-15.
fraction_schoolkid <- pop_data %>%
  group_by(
    age_lower,
    age_upper
  ) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  ) %>%
  left_join(
    age_lookup,
    by = c("age_lower", "age_upper")
  ) %>%
  mutate(
    school_age = age_lower >= 12 &
      age_upper <= 15
  ) %>%
  group_by(
    age_band_5y
  ) %>%
  summarise(
    age_band_pop = sum(population),
    schoolkid_pop = sum(population * school_age),
    .groups = "drop"
  ) %>%
  mutate(
    fraction_schoolkid = schoolkid_pop / age_band_pop
  ) %>%
  select(
    age_band_5y,
    fraction_schoolkid
  )
  
# compute age-structured vaccination coverages, fractions of each type/dose,
# and disaggregate to 5-year age bins for estimating the vaccination effect
vacc_coverage_5y <- vacc_coverage %>%
  right_join(
    age_combinations,
    by = c("scenario", "coverage", "age_band_quantium")
  ) %>%
  # add on indicators to expand out to schoolkid vaccination
  mutate(
    vacc_schoolkids_true = TRUE,
    vacc_schoolkids_false = FALSE
  ) %>%
  pivot_longer(
    cols = starts_with("vacc_schoolkids_"),
    names_to = "vacc_schoolkids_names",
    values_to = "vacc_schoolkids"
  ) %>%
  select(
    scenario,
    target_coverage = coverage,
    vacc_schoolkids,
    age_band_5y,
    proportion_vaccinated = coverage_any_dose_1,
    proportion_fully_vaccinated = coverage_any_dose_2,
    starts_with("fraction_")
  ) %>%
  # the above assume coverage at 10y bins applies to all component 5y bins. This
  # is fine, except for the 10-19 10y bracket, where the coverage should be
  # higher in 15-19, and 0% in 10-14. so manual fix this, based on the population split for these
  left_join(
    proportion_of_10y,
    by = "age_band_5y"
  ) %>%
  # redenominate the 15-19 group multiply by 10-19 population to get number of
  # vaccinees, and then divide by 15-19 population to get new coverage ie.
  # equivalently multiply by 1 over the fraction of the 10y bin that is in the
  # 5y bin
  mutate(
    coverage_correction = case_when(
      age_band_5y == "10-14" ~ 0,
      age_band_5y == "15-19" ~ 1 / proportion_of_10y,
      TRUE ~ 1
    ),
    across(
      c(
        proportion_vaccinated,
        proportion_fully_vaccinated
      ),
      ~ . * coverage_correction
    )
  ) %>%
  # add additional vaccination coverage for schoolkids IFF we are doing
  # schoolkids vaccinations
  left_join(
    fraction_schoolkid,
    by = "age_band_5y"
  ) %>%
  mutate(
    fraction_schoolkid = fraction_schoolkid * vacc_schoolkids
  ) %>%
  mutate(
    # multiply the proportion vaccinated by the population to get the number vaccinated in non-schoolkid cohort
    # multiply the fraction schoolkids by pop to get the population of schoolkids in this band
    # multiply the population of schoolkids by the coverage fraction to get the number of vaccinated schoolkids
    # add this to the existing number vaccinated, and divide by pop to get the new age group coverage.
    # ((proportion_vaccinated * pop) + (target_coverage * fraction_schoolkid * pop)) / pop
    # pop factorises and cancels:
    # = pop * (proportion_vaccinated + target_coverage * fraction_schoolkid) / pop
    # = proportion_vaccinated + target_coverage * fraction_schoolkid
    across(
      c(
        proportion_vaccinated,
        proportion_fully_vaccinated
      ),
      ~ . + target_coverage * fraction_schoolkid
    )
  ) %>%
  mutate(
    proportion_vaccinated = replace_na(proportion_vaccinated, 0),
    proportion_fully_vaccinated = replace_na(proportion_fully_vaccinated, 0),
    across(
      starts_with("fraction_"),
      ~ replace_na(., 0.25)
    )
  ) %>%
  select(
    -age_band_quantium,
    -proportion_of_10y,
    -coverage_correction,
  ) %>%
  arrange(
    scenario,
    target_coverage,
    age_band_5y
  )

# lookup for vaccination scenarios
vacc_scenario_lookup <- read_csv(
  "data/vaccinatinon/quantium_simulations/dim_scenario.csv",
  col_types = cols(
    scenario = col_double(),
    priority_order = col_character(),
    az_dose_gap = col_character(),
    az_age_cutoff = col_double()
  )
)

# compute the reduction in transmission from different age-structured 
# vaccine coverage scenarios - accounting for reduced efficacy in AZ
# 2-doses with shorter intervals, and adding in a relative vaccine efficacy
# multiplier for each of protection from infection and protection from
# breakthrough transmission in immune escape variants
vacc_effect_by_age <- vacc_coverage_5y %>%
  left_join(
    vacc_scenario_lookup,
    by = "scenario"
  ) %>%
  mutate(
    relative_efficacy_baseline = 1,
    relative_efficacy_escape = 0.5
  ) %>%
  pivot_longer(
    cols = starts_with("relative_efficacy_"),
    values_to = "relative_efficacy",
    names_to = "relative_efficacy_names"
  ) %>%
  select(
    -relative_efficacy_names
  ) %>%
  mutate(
    az_2_dose_multiplier = case_when(
      az_dose_gap == "12 weeks" ~ 1,
      az_dose_gap == "8 weeks" ~ 0.85,
      az_dose_gap == "4 weeks" ~ 0.75
    ),
    efficacy_az_2_dose = combine_efficacy(
      0.60 * az_2_dose_multiplier * relative_efficacy,
      0.65 * relative_efficacy
    ),
    efficacy_pf_2_dose = combine_efficacy(
      0.79 * relative_efficacy,
      0.65 * relative_efficacy
    ),
    efficacy_pf_1_dose = combine_efficacy(
      0.30 * relative_efficacy,
      0.46 * relative_efficacy
    ),
    efficacy_az_1_dose = combine_efficacy(
      0.18 * relative_efficacy,
      0.48 * relative_efficacy
    ),    
    average_efficacy_transmission = average_efficacy(
      efficacy_pf_2_dose = efficacy_pf_2_dose,
      efficacy_az_2_dose = efficacy_az_2_dose,
      efficacy_pf_1_dose = efficacy_pf_1_dose,
      efficacy_az_1_dose = efficacy_az_1_dose,
      proportion_pf_2_dose = fraction_dose_2_mRNA,
      proportion_az_2_dose = fraction_dose_2_AZ,
      proportion_pf_1_dose = fraction_only_dose_1_mRNA,
      proportion_az_1_dose = fraction_only_dose_1_AZ
    )
  ) %>%
  select(
    scenario,
    target_coverage,
    vacc_schoolkids,
    relative_efficacy,
    age_band_5y,
    proportion_vaccinated,
    average_efficacy_transmission
  ) %>%
  group_by(
    scenario,
    target_coverage,
    vacc_schoolkids,
    relative_efficacy
  ) %>%
  mutate(
    vacc_effect = vaccination_transmission_effect(
      age_coverage = proportion_vaccinated,
      efficacy_mean = average_efficacy_transmission,
      next_generation_matrix = baseline_matrix()
    )$by_age    
  ) %>%
  rename(
    vacc_scenario = scenario,
    vacc_coverage = target_coverage,
    vacc_relative_efficacy = relative_efficacy
  )

vaccination_scenarios <- vacc_effect_by_age %>%
  group_by(
    vacc_scenario,
    vacc_coverage,
    vacc_schoolkids,
    vacc_relative_efficacy
  ) %>%
  summarise(
    vacc_effect = vaccination_transmission_effect(
      age_coverage = proportion_vaccinated,
      efficacy_mean = average_efficacy_transmission,
      next_generation_matrix = baseline_matrix()
    )$overall,
    .groups = "drop"
  )

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
    ttiq = unique(phsm_scenarios$ttiq),
    baseline_type = unique(phsm_scenarios$baseline_type),
    vacc_scenario = unique(vaccination_scenarios$vacc_scenario),
    vacc_coverage = unique(vaccination_scenarios$vacc_coverage),
    vacc_schoolkids = unique(vaccination_scenarios$vacc_schoolkids),
    vacc_relative_efficacy = unique(vaccination_scenarios$vacc_relative_efficacy)
  ) %>%
  left_join(
    vacc_scenario_lookup,
    by = c("vacc_scenario" = "scenario")
  ) %>%
  left_join(
    phsm_scenarios,
    by = c("ttiq", "baseline_type")
  ) %>%
  left_join(
    vaccination_scenarios,
    by = c(
      "vacc_scenario",
      "vacc_coverage",
      "vacc_schoolkids",
      "vacc_relative_efficacy"
    )
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

# output all TP scenarios
scenarios %>%
  write.csv(
    file = paste0(
      "~/Desktop/tp_scenarios_draft_",
      Sys.Date(),
      ".csv"
    ),
    row.names = FALSE
  )

# output subset of scenarios for treasury
scenarios %>%
  filter(
    ttiq %in% c("partial", "optimal"),
    baseline_type == "standard",
    vacc_scenario %in% 1:3,
    vacc_schoolkids == FALSE,
    vacc_relative_efficacy == 1
  ) %>%
  write.csv(
    file = paste0(
      "~/Desktop/tp_scenarios_draft_for_treasury_",
      Sys.Date(),
      ".csv"
    ),
    row.names = FALSE
  )

# output subset of scenarios for Jodie
scenarios %>%
  filter(
    ttiq == "partial",
    baseline_type == "standard",
    vacc_scenario %in% 1:3
  ) %>%
  write.csv(
    file = paste0(
      "~/Desktop/tp_scenarios_draft_for_jodie_",
      Sys.Date(),
      ".csv"
    ),
    row.names = FALSE
  )

# compute TP for Eamon
scenarios %>%
  filter(
    ttiq == "none",
    baseline_type == "standard"
  ) %>%
  summarise(
    tp_baseline = first(tp_baseline),
    all_identical = all(tp_baseline == first(tp_baseline))
  )

table_2_x <- scenarios %>%
  filter(
    ttiq == "partial",
    baseline_type == "standard",
    vacc_schoolkids == FALSE,
    vacc_relative_efficacy == 1
  ) %>%
  select(
    priority_order,
    az_age_cutoff,
    az_dose_gap,
    vacc_coverage,
    tp_baseline_vacc
  ) %>%
  mutate(
    priority_order = factor(
      priority_order,
      levels = c(
        "Oldest to youngest",
        "Youngest to oldest (40+ first then 16+)",
        "Random"
      )
    ),
    tp_baseline_vacc = round(tp_baseline_vacc, 1)
  ) %>%
  pivot_wider(
    names_from = vacc_coverage,
    values_from = tp_baseline_vacc
  ) %>% 
  arrange(
    priority_order,
    desc(az_age_cutoff),
    az_dose_gap
  )

table_2_x
write_csv(table_2_x, "~/Desktop/table_2_x.csv")

table_3_1 <- scenarios %>%
  filter(
    ttiq == "partial",
    baseline_type == "standard",
    vacc_schoolkids == FALSE,
    vacc_relative_efficacy == 0.5,
    vacc_scenario %in% 1:3
  ) %>%
  select(
    priority_order,
    vacc_coverage,
    tp_baseline_vacc
  ) %>%
  mutate(
    priority_order = factor(
      priority_order,
      levels = c(
        "Oldest to youngest",
        "Youngest to oldest (40+ first then 16+)",
        "Random"
      )
    ),
    tp_baseline_vacc = round(tp_baseline_vacc, 1)
  ) %>%
  pivot_wider(
    names_from = vacc_coverage,
    values_from = tp_baseline_vacc
  ) %>% 
  arrange(
    priority_order
  )

table_3_1
write_csv(table_3_1, "~/Desktop/table_3_1.csv")


table_schoolkids <- scenarios %>%
  filter(
    ttiq == "partial",
    baseline_type == "standard",
    vacc_schoolkids == TRUE,
    vacc_relative_efficacy == 1,
    vacc_scenario %in% 1:3
  ) %>%
  select(
    priority_order,
    vacc_coverage,
    tp_baseline_vacc
  ) %>%
  mutate(
    priority_order = factor(
      priority_order,
      levels = c(
        "Oldest to youngest",
        "Youngest to oldest (40+ first then 16+)",
        "Random"
      )
    ),
    tp_baseline_vacc = round(tp_baseline_vacc, 1)
  ) %>%
  pivot_wider(
    names_from = vacc_coverage,
    values_from = tp_baseline_vacc
  ) %>% 
  arrange(
    priority_order
  )

table_schoolkids
write_csv(table_schoolkids, "~/Desktop/table_schoolkids.csv")
