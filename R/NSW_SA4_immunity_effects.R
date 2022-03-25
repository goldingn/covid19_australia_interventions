source("R/functions.R")

## Vaccination effect ---------
# Vaccination effect calculations  --------

# find most recent data or specify date, check dir printed is sensible
get_quantium_data_dates()

dir <- get_quantium_data_dir()
dir

# check dir date is sensible and get date of data set
data_date <- sub(
  pattern = ".*\\/",
  replacement = "",
  x = dir
) %>%
  as.Date

data_date
data_date_save <- format(data_date, "%Y%m%d")
data_date_save



# reaad-in lookups
# object
lookups <- get_quantium_lookups(dir = dir)

# read in and label data
vaccine_raw <- read_quantium_vaccination_data()

# check scenarios and assign appropriate one for use
# currently only difference is % booster uptake (100, 80, 75)
# choose 75
unique(vaccine_raw$scenario)

scenario_to_use <- lookups$scenario$scenario[which(lookups$scenario$booster_uptake == 75)]

# this may fail if scenario lookup table is not up to date so check this is TRUE or will cause failure later
# otherwise may need to check email for appropriate scenario number and assign manually
scenario_to_use %in% unique(vaccine_raw$scenario)
#scenario_to_use <- 141

scenario_to_use

# aggregate to SA4 just for NSW
vaccine_sa4_nsw <- vaccine_raw %>%
  left_join(
    state_short_long_table,
    by = c("STE_NAME16" = "state_long")
  ) %>%
  filter(state_short == "NSW") %>%
  select(
    scenario,
    state = SA4_NAME16,
    age_band,
    vaccine,
    vaccine_booster,
    date_dose_1,
    date_dose_2,
    date_booster,
    num_people
  )


# library(furrr)
# 
# plan(multisession, workers = 4)
# plan(sequential)

ve_tables_nsw_sa4 <- tibble(
  date = seq.Date(
    from = as.Date("2021-02-22"),
    to = Sys.Date() + weeks(16),
    by = "1 week"
  )
) %>%
  mutate(
    cohorts = map(
      .x = date,
      .f = get_vaccine_cohorts_at_date,
      vaccine_scenarios = vaccine_sa4_nsw
    ),
    coverage = map(
      .x = cohorts,
      .f = get_coverage
    ),
    ves = map(
      .x = cohorts,
      .f = get_vaccine_efficacies
    ),
    vaccine_transmission_effects = map2(
      .x = ves,
      .y = coverage,
      .f = get_vaccine_transmission_effects
    )
  )


date_state_variant_table_nsw <- expand_grid(
  date = seq.Date(
    from = min(ve_tables_nsw_sa4$date),
    to = max(ve_tables_nsw_sa4$date),
    by = 1
  ),
  state = unique(ve_tables_nsw_sa4$ves[[1]]$state),
  variant = unique(ve_tables_nsw_sa4$ves[[1]]$variant)
)

vaccination_effect_timeseries_nsw_sa4 <- ve_tables_nsw_sa4 %>%
  select(date, vaccine_transmission_effects) %>%
  unnest(vaccine_transmission_effects) %>%
  filter(omicron_scenario == "estimate", scenario == scenario_to_use) %>%
  select(date, state, variant, vaccination_effect) %>%
  mutate(
    effect_multiplier = 1 - vaccination_effect
  ) %>%
  full_join(
    y = date_state_variant_table_nsw,
    by = c("state", "date", "variant")
  ) %>%
  arrange(state, variant, date) %>%
  group_by(state, variant) %>%
  rename(effect = effect_multiplier) %>%
  mutate(
    effect = ifelse(
      is.na(effect),
      approx(date, effect, date)$y,
      effect
    ),
    percent_reduction = 100 * (1 - effect)
  ) %>%
  ungroup %>%
  select(-vaccination_effect)

write_csv(
  vaccination_effect_timeseries_nsw_sa4,
  file = sprintf(
    "outputs/vaccination_effect_nswsa4_%s.csv",
    data_date_save
  )
)



# population-wide VE by SA4

coverage_fraction <- ve_tables_nsw_sa4 %>%
  select(date, coverage) %>%
  unnest(coverage) %>%
  group_by(date, scenario, state) %>%
  mutate(
    total_coverage = sum(coverage)
  ) %>%
  ungroup %>%
  mutate(
    fraction_coverage = coverage / total_coverage
  ) %>%
  #select(-coverage, -total_coverage) %>%
  arrange(scenario, state, date, age_band)

ve_tables_expanded <- ve_tables_nsw_sa4 %>%
  select(date, cohorts) %>%
  mutate(
    cohorts_all_pop = map(
      .x = cohorts,
      .f = function(vaccine_cohorts){
        dir <- get_quantium_data_dir(date = NULL)
        lookups <- get_quantium_lookups(dir = dir)
        
        state_age_fractions <- lookups$age %>%
          select(age_band) %>%
          mutate(
            lower = sub(
              pattern = "\\-.*",
              replacement = "",
              x = age_band
            ) %>%
              sub(
                pattern = "\\+",
                replacement = "",
                x = .
              ) %>%
              as.numeric,
            upper = sub(
              pattern = ".*\\-",
              replacement = "",
              x = age_band
            ) %>%
              sub(
                pattern = ".*\\+",
                replacement = "Inf",
                x = .
              ) %>%
              as.numeric
          ) %>%
          rename(classes = age_band) %>%
          get_age_distribution_by_state(ages = .) %>%
          ungroup %>%
          filter(state == "NSW") %>%
          select(-state)
        
        # add in 0-4 yo cohort which is missing
        age_class_state_scenario <- expand_grid(
          state_age_fractions %>%
            select(age_class),
          state = unique(vaccine_cohorts$state),
          scenario = unique(vaccine_cohorts$scenario)
        )
        
        vaccine_cohorts_all <- age_class_state_scenario %>%
          full_join(
            y = vaccine_cohorts,
            by = c("scenario", "state", "age_class" = "age_band")
          ) %>%
          left_join(
            state_age_fractions,
            by = c("age_class")
          ) %>%
          group_by(
            state, scenario
          ) %>%
          mutate(
            qspop = sum(num_people, na.rm = TRUE),
            qpop = qspop / (1 - fraction),
            num_people = if_else(
              age_class == "0-4",
              fraction * qpop,
              num_people
            )
          ) %>%
          rename(age_band = age_class) %>%
          select(
            scenario, state, age_band, fraction
          )
        
        return(vaccine_cohorts_all)
      }
    )
  )


population_fraction <- ve_tables_expanded  %>%
  select(date, cohorts_all_pop) %>%
  unnest(cohorts_all_pop) %>%
  rename(age_fraction = fraction) %>%
  distinct %>%
  arrange(scenario, state, date, age_band) 


  

population_mean_ve <- ve_tables_nsw_sa4 %>%
  select(date, ves) %>%
  unnest(ves) %>%
  complete(
    date,
    scenario,
    state,
    omicron_scenario,
    age_band = unique(lookups$age$age_band),
    variant,
    outcome,
    fill = list(
      ve = 0
    )
  ) %>%
  left_join(
    coverage_fraction,
    by = c("date", "scenario", "state", "age_band")
  ) %>%
  left_join(
    population_fraction,
    by = c("date", "scenario", "state", "age_band")
  ) %>%
  mutate(
    weighted_ve = ve * coverage * age_fraction
  ) %>%
  group_by(date, scenario, state, omicron_scenario, variant, outcome) %>%
  summarise(population_mean_ve = sum(weighted_ve)) %>%
  ungroup


age_class_mean_ve <- ve_tables_nsw_sa4 %>%
  select(date, ves) %>%
  unnest(ves) %>%
  complete(
    date,
    scenario,
    state,
    omicron_scenario,
    age_band = unique(lookups$age$age_band),
    variant,
    outcome,
    fill = list(
      ve = 0
    )
  ) %>%
  left_join(
    coverage_fraction,
    by = c("date", "scenario", "state", "age_band")
  ) %>%
  left_join(
    population_fraction,
    by = c("date", "scenario", "state", "age_band")
  ) %>%
  mutate(
    weighted_ve = ve * coverage
  )

daily_population_mean_ve <- population_mean_ve %>%
  ungroup %>%
  filter(
    scenario == scenario_to_use,
    omicron_scenario == "estimate"#,
    #outcome %in% c("acquisition", "transmission", "symptoms")
  ) %>%
  select(-scenario, -omicron_scenario) %>%
  full_join(
    y = date_state_variant_table_nsw %>%
      expand_grid(outcome = unique(population_mean_ve$outcome)),
    by = c("state", "date", "variant", "outcome")
  ) %>%
  arrange(state, variant, outcome, date) %>%
  group_by(state, variant, outcome) %>%
  mutate(
    population_mean_ve = ifelse(
      is.na(population_mean_ve),
      approx(date, population_mean_ve, date)$y,
      population_mean_ve
    )
  ) %>%
  ungroup

write_csv(
  x = daily_population_mean_ve,
  file = "outputs/daily_population_mean_ves_nsw_sa4.csv"
)


write_csv(
  x = vaccination_effect_timeseries_nsw_sa4,
  file = "outputs/nsw_sa4_tp_ve_multiplier.csv"
)

dpi <- 150

for (i in 1:3){
  
  if (i == 1){
    x <- 1:10
  } else if (i == 2) {
    x <- 11:20
  } else if (i == 3) {
    x <- 21:28
  }
  
  ggplot(
    daily_population_mean_ve %>%
      filter(state == unique(.$state)[x])
  ) +
    geom_line(
      aes(
        x = date,
        y = population_mean_ve,
        col = outcome,
        linetype = variant
      )
    ) +
    geom_vline(
      aes(xintercept = Sys.Date())
    ) +
    facet_wrap(~state, ncol = 2) +
    ylim(0, 1) +
    scale_x_date(minor_breaks = "month") +
    ylab("Population mean vaccine efficacy")
  
  ggsave(
    filename = sprintf(
      "outputs/figures/nsw_sa4_ve/nsw_sa4_ve_%s_%s.png",
      i,
      data_date_save
    ),
    dpi = dpi,
    width = 1500 / dpi,
    height = 1250 / dpi,
    scale = 1.2,
    bg = "white"
  )
}

for (i in 1:3){
  
  if (i == 1){
    x <- 1:10
  } else if (i == 2) {
    x <- 11:20
  } else if (i == 3) {
    x <- 21:28
  }
  
  ggplot(
    daily_population_mean_ve %>%
      filter(
        state == unique(.$state)[x],
        variant == "Omicron",
        outcome == "transmission"
      )
  ) +
    geom_line(
      aes(
        x = date,
        y = population_mean_ve,
        col = outcome,
        linetype = variant
      )
    ) +
    geom_vline(
      aes(xintercept = Sys.Date())
    ) +
    facet_wrap(~state, ncol = 2) +
    scale_x_date(minor_breaks = "month") +
    ylab("Population mean vaccine efficacy")
  
  ggsave(
    filename = sprintf(
      "outputs/figures/nsw_sa4_ve/nsw_sa4_ve_omicron_transmission_%s_%s.png",
      i,
      data_date_save
    ),
    dpi = dpi,
    width = 1500 / dpi,
    height = 1250 / dpi,
    scale = 1.2,
    bg = "white"
  )
}

for (i in 1:3){
  
  if (i == 1){
    x <- 1:10
  } else if (i == 2) {
    x <- 11:20
  } else if (i == 3) {
    x <- 21:28
  }
  
  ggplot(
    vaccination_effect_timeseries_nsw_sa4 %>%
      filter(
        state == unique(.$state)[x]
    )
  ) +
    geom_line(
      aes(
        x = date,
        y = effect,
        linetype = variant
      )
    ) +
    geom_vline(
      aes(xintercept = Sys.Date())
    ) +
    facet_wrap(~state, ncol = 2) +
    ylim(0, 1) +
    scale_x_date(minor_breaks = "month") +
    ylab("Proportion reduction on transmission potential")
  
  ggsave(
    filename = sprintf(
      "outputs/figures/nsw_sa4_ve/nsw_sa4_ve_tp_%s_%s.png",
      i,
      data_date_save
    ),
    dpi = dpi,
    width = 1500 / dpi,
    height = 1250 / dpi,
    scale = 1.2,
    bg = "white"
  )
}
