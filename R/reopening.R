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

# join to dates of targets being achieved, filter, and sum to get 1 dose and 2 dose counts
vacc_cohorts %>%
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
  )

# need to join populations and compute coverages
# collapse vaccine types to make moderna the same as pfizer







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
  
  effect <- vaccination_transmission_effect(
    age_coverage = rep(total_coverage, 17),
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



