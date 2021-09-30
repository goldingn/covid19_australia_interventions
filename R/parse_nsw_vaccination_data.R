# compute NSW vaccination effects on transmission, by LGA.
source("R/functions.R")
  
lgas_of_concern <- c(
  "Bayside (A)",
  "Blacktown (C)",
  "Burwood (A)",
  "Campbelltown (C) (NSW)",
  "Canterbury-Bankstown (A)",
  "Cumberland (A)",
  "Fairfield (C)",
  "Georges River (A)",
  "Liverpool (C)",
  "Parramatta (C)",
  "Strathfield (A)",
  "Penrith (C)"
)

extra_lgas <- c(
  "Hawkesbury (C)",
  "Camden (A)",
  "Sutherland Shire (A)",
  "Ryde (C)",
  "Inner West (A)",
  "Blue Mountains (C)",
  "The Hills Shire (A)",
  "Sydney (C)"
)

# lookup to rename the LGAs
lga_rename <- read_csv(
  "data/vaccinatinon/map_lga_name_20210916.csv",
  col_types = cols(
    LGA_CODE19 = col_character(),
    LGA_NAME19_OLD = col_character(),
    LGA_NAME19_new = col_character()
  )
)

# load population data from Cth Health (at SA2 level, not LGA) to compute
# NSW-wide fractions of the 0-14 that are 12-14, and fractions of the 15-29 that
# are 16-29 (for AIR age distributions), and fractions of the 10-14 that are
# 12-14, and fractions of the 15-19 that are 16-19 (for 5y age distributions),
# and any other interesting fractions
pop_detailed <- read_csv(
  file = "data/vaccinatinon/2021-07-13-census-populations.csv",
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

# get a lookup between the detailed age fractions, air fractions (including
# 12-15), and the age groups for modelling
air_12_age_lookup <- tibble::tribble(
   ~age_lower, ~age_upper, ~age_air, ~age_air_80,       ~age_model,
            0,          4,  "0-11",         "0-11",          "0-4",
            5,          9,  "0-11",         "0-11",          "5-9", 
           10,         10,  "0-11",         "0-11",        "10-11",
           11,         11,  "0-11",         "0-11",        "10-11",
           12,         12, "12-15",        "12-15",        "12-15",
           13,         13, "12-15",        "12-15",        "12-15",
           14,         14, "12-15",        "12-15",        "12-15",
           15,         15, "12-15",        "12-15",        "12-15",
           16,         16, "16-29",        "16-29",        "16-19",
           17,         17, "16-29",        "16-29",        "16-19",
           18,         19, "16-29",        "16-29",        "16-19",
           20,         24, "16-29",        "16-29",        "20-24",
           25,         29, "16-29",        "16-29",        "25-29",
           30,         34, "30-39",        "30-39",        "30-34",
           35,         39, "30-39",        "30-39",        "35-39",
           40,         44, "40-49",        "40-49",        "40-44",
           45,         49, "40-49",        "40-49",        "45-49",
           50,         54, "50-59",        "50-59",        "50-54",
           55,         59, "50-59",        "50-59",        "55-59",
           60,         64, "60-69",        "60-69",        "60-64",
           65,         69, "60-69",        "60-69",        "65-69",
           70,         74, "70-79",        "70-79",        "70-79",
           75,         79, "70-79",        "70-79",        "75-79",
           80,         84, "80-84",          "80+",          "80+",
           85,         89,   "85+",          "80+",          "80+",
           90,         94,   "85+",          "80+",          "80+",
           95,         99,   "85+",          "80+",          "80+",
          100,        999,   "85+",          "80+",          "80+"
)

lga18_lga19_lookup <- read_csv(
  "data/spatial/abs/CG_LGA_2018_LGA_2019 - All.csv",
  col_types = cols(
    LGA_CODE_2018 = col_double(),
    LGA_NAME_2018 = col_character(),
    LGA_CODE_2019 = col_double(),
    LGA_NAME_2019 = col_character(),
    RATIO_FROM_TO = col_double(),
    INDIV_TO_REGION_QLTY_INDICATOR = col_character(),
    OVERALL_QUALITY_INDICATOR = col_character(),
    BMOS_NULL_FLAG = col_double()
  )
) %>%
  select(LGA_NAME_2018, LGA_NAME_2019)

# all 100% correspondences
lga17_lga18_lookup <- read_excel(
  "data/spatial/abs/CG_LGA_2017_LGA_2018_Update.xls",
  sheet = "Table 3",
  skip = 4
) %>%
  filter(
    row_number() > 1
  ) %>%
  select(LGA_NAME_2017, LGA_NAME_2018)

sa3_lga17_lookup <- read_excel(
  "data/spatial/abs/CG_SA3_2016_LGA_2017.xls",
  sheet = "Table 3",
  skip = 4
) %>%
  filter(
    !(row_number() == 1)
  ) %>%
  select(
    SA3_NAME_2016,
    LGA_NAME_2017,
    RATIO
  )

sa3_lga19_lookup <- sa3_lga17_lookup %>%
  left_join(
    lga17_lga18_lookup,
    by = "LGA_NAME_2017"
  ) %>%
  left_join(
    lga18_lga19_lookup,
    by = "LGA_NAME_2018"
  ) %>%
  select(
    SA3_NAME_2016,
    LGA_NAME_2019,
    RATIO
  )


# get detailed populations for NSW LGAs with old names and new age groups
pop_detailed_nsw <- pop_detailed %>%
  filter(
    ste_name16 == "New South Wales"
  ) %>%
  # convert 2016 sa3s to 2016 LGAs
  # join on the LGA names to get the old ones
  left_join(
    sa3_lga19_lookup,
    by = c("sa3_name16" = "SA3_NAME_2016")
  ) %>%
  group_by(
    LGA_NAME_2019, age_lower, age_upper
  ) %>%
  summarise(
    population = sum(population * RATIO),
    .groups = "drop"
  ) %>%
  rename(
    lga = LGA_NAME_2019
  ) %>%
  left_join(
    air_12_age_lookup,
    by = c("age_lower", "age_upper")
  ) %>%
  group_by(
    across(c(-population))
  ) %>%
  summarise(
    across(
      population,
      sum
    ),
    .groups = "drop"
  )

# get populations by lga (with old lga names) in each of the air age groups
pop_air <- pop_detailed_nsw %>%
  group_by(lga, age_air) %>%
  summarise(
    across(
      population,
      sum
    ),
    .groups = "drop"
  )

# get populations by lga in the model age groups
pop_model <- pop_detailed_nsw %>%
  group_by(lga, age_model) %>%
  summarise(
    across(
      population,
      sum
    ),
    .groups = "drop"
  )

# load ABS postcode to LGA concordance
postcode_lga_lookup <- get_poa_lga_correspondence()

# load air data, remove provider type, add populations, and tidy
air_raw_postcode <- list.files(
  "~/not_synced/vaccination/nsw/",
  pattern = "^AIR_.*_UNSW.csv$",
  full.names = TRUE
) %>%
  as_tibble() %>%
  mutate(
    file = basename(value),
    date = str_remove(file, "^AIR_"),
    date = str_remove(date, "_UNSW.csv$"),
    date = as_date(date)
  ) %>%
  filter(
    date == max(date)
  ) %>%
  pull(value) %>%
  read_csv(
    col_types = cols(
      LGA_CODE19 = col_double(),
      LGA_NAME19 = col_character(),
      POSTCODE = col_character(),
      DOSE_NUMBER = col_double(),
      VACCINE_TYPE = col_character(),
      AGE_GROUP = col_character(),
      PROVIDER_TYPE = col_character(),
      ENCOUNTER_DATE = col_date(format = ""),
      DAILY_COUNT = col_double(),
      CUMULATIVE_DAILY_COUNT = col_double(),
      SNAPSHOT_DATE = col_date(format = "")
    )
  ) %>%
  select(
    -LGA_CODE19,
    -SNAPSHOT_DATE,
    -DAILY_COUNT
  ) %>%
  filter(
    CUMULATIVE_DAILY_COUNT > 0
  )

# aggregate to LGA level and collapse moderna/pfizer
air_raw <- air_raw_postcode %>%
  mutate(
    VACCINE_TYPE = case_when(
      VACCINE_TYPE == "Moderna" ~ "Pfizer",
      TRUE ~ VACCINE_TYPE
    )
  ) %>%
  # rename the LGAs to the old version
  left_join(
    lga_rename,
    by = c(LGA_NAME19 = "LGA_NAME19_new")
  ) %>%
  select(
    -LGA_NAME19,
    -LGA_CODE19
  ) %>%
  rename(
    LGA_NAME19 = LGA_NAME19_OLD
  ) %>%
  relocate(
    LGA_NAME19,
    .after = POSTCODE
  ) %>%
  left_join(
    postcode_lga_lookup,
    by = "POSTCODE"
  ) %>%
  # where the postcode is not in the lookup, assign all weight to the NSW Health allocated LGA
  mutate(
    LGA_NAME19 = coalesce(LGA_NAME19, LGA_NAME_2019),
    weight = replace_na(weight, 1)
  ) %>%
  group_by(
    LGA_NAME19,
    DOSE_NUMBER,
    VACCINE_TYPE,
    AGE_GROUP,
    PROVIDER_TYPE,
    ENCOUNTER_DATE
  ) %>%
  summarise(
    CUMULATIVE_DAILY_COUNT = sum(CUMULATIVE_DAILY_COUNT * weight),
    .groups = "drop"
  )

# check sums
air_raw_postcode %>%
  filter(ENCOUNTER_DATE == max(ENCOUNTER_DATE)) %>%
  summarise(
    sum(CUMULATIVE_DAILY_COUNT)
  )
air_raw %>%
  filter(ENCOUNTER_DATE == max(ENCOUNTER_DATE)) %>%
  summarise(
    sum(CUMULATIVE_DAILY_COUNT)
  )

latest_data_date <- max(air_raw$ENCOUNTER_DATE)

# add on 0 coverage for younger age groups and missing LGAs and dates, and collapse over 80 group
air_current <- air_raw %>%
  complete(
    ENCOUNTER_DATE = unique(air_raw$ENCOUNTER_DATE),
    LGA_NAME19 = unique(air_raw$LGA_NAME19),
    AGE_GROUP = unique(air_12_age_lookup$age_air),
    DOSE_NUMBER = unique(air_raw$DOSE_NUMBER),
    VACCINE_TYPE = unique(air_raw$VACCINE_TYPE),
    PROVIDER_TYPE = unique(air_raw$PROVIDER_TYPE),
    fill = list(CUMULATIVE_DAILY_COUNT = 0)
  ) %>%
  # collapse provider type
  group_by(
    LGA_NAME19,
    DOSE_NUMBER,
    VACCINE_TYPE,
    AGE_GROUP,
    ENCOUNTER_DATE
  ) %>%
  summarise(
    CUMULATIVE_DAILY_COUNT = sum(CUMULATIVE_DAILY_COUNT),
    .groups = "drop"
  ) %>%
  rename_all(
    tolower
  ) %>%
  filter(
    lga_name19 != "Other/Not stated"
  ) %>%
  select(
    date = encounter_date,
    lga = lga_name19,
    age_air = age_group,
    dose_number,
    vaccine = vaccine_type,
    cumulative = cumulative_daily_count
  ) %>%
  # widen out doses and vaccine types, so each row is a unique date/lga/age
  pivot_wider(
    names_from = dose_number,
    values_from = cumulative,
    names_prefix = "dose_"
  ) %>%
  mutate(
    # some bins have more people with dose 2 than (any) dose 1, due to people
    # moving bin. So remove round these down (undercount very slightly, rather
    # than overcounting).
    dose_2 = pmin(dose_2, dose_1),
  ) %>%
  # compute fractional coverage by dose and vaccine type
  pivot_wider(
    names_from = vaccine,
    values_from = c(
      # only_dose_1,
      dose_1,
      dose_2
    )
  ) %>%
  # join on populations
  left_join(
    pop_air,
    by = c(
      "lga",
      "age_air"
    )
  ) %>%
  # collapse the over 80 population and numbers of vaccinations of each type
  left_join(
    air_12_age_lookup %>%
      select(
        age_air,
        age_air_80
      ) %>%
      distinct(),
    by = "age_air"
  ) %>%
  select(
    -age_air
  ) %>%
  group_by(
    date,
    lga,
    age_air_80
  ) %>%
  summarise(
    across(
      starts_with("dose_"),
      ~sum(.x)
    ),
    population = sum(population),
    .groups = "drop"
  )


# write out an age group lookup for Nic R
expand_grid(
  age = 0:100,
  age_lower = unique(air_12_age_lookup$age_lower)
) %>%
  left_join(
    air_12_age_lookup,
    by = "age_lower"
  ) %>%
  filter(
    age >= age_lower & age <= age_upper
  ) %>%
  select(
    integer_age = age,
    age =  age_model,
    age_air_80
  ) %>%
  write_csv(
    "outputs/nsw/age_group_lookup.csv"
  )

# to do:
stop("compare multiple population sources against the rate of over-vaccination, and the LGA population totals for each LGA")
stop("plumb in age-specific transmission parammeters to match these age groups")
stop("use new contact matrix with age breaks matching these")

# assumed maximum coverages
max_coverage <- air_12_age_lookup %>%
  select(
    age_air_80
  ) %>%
  distinct() %>%
  mutate(
    hypothetical_max_coverage = case_when(
      age_air_80 %in% c("40-49", "50-59", "60-69", "70-79", "80+") ~ 0.95,
      TRUE ~ 0.85
    )
  )

air <- bind_rows(
  # forecast doses under current vaccination rate, assuming no under 16s are
  # being vaccinated (we cannot determine the rate of vaccination of 15 year olds from the data)
  forecast_vaccination(
    air_current,
    previous_days_average = 0:6,
    max_date = Sys.Date() + 7 * 8,
    az_interval_weeks = 6,
    pfizer_interval_weeks = 3,
    max_coverages = max_coverage
  )
) %>%
  # tidy up labels for printing
  mutate(
    coverage_scenario = "95% / 85%"
  )

# plot coverage stats
air %>%
  filter(
    lga %in% lgas_of_concern
  ) %>%
  mutate(
    any_vaccinated = dose_1_AstraZeneca + dose_1_Pfizer,
    fully_vaccinated = dose_2_AstraZeneca + dose_2_Pfizer
  ) %>%
  group_by(lga, date, forecast, scenario) %>%
  summarise(
    across(
      ends_with("vaccinated"),
      sum
    ),
    population = sum(population),
    eligible_population = sum(eligible_population),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = ends_with("vaccinated"),
    names_to = "doses",
    values_to = "vaccinated" 
  ) %>%
  mutate(
    coverage = vaccinated / eligible_population,
    doses = case_when(
      doses == "any_vaccinated" ~ "either dose",
      doses == "fully_vaccinated" ~ "both doses"
    )
  ) %>%
  ggplot(
    aes(
      x = date,
      y = coverage,
      color = lga,
      linetype = forecast,
    ),
  ) +
  facet_grid(
    scenario ~ doses
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(
      accuracy = 1
    )
  ) +
  geom_hline(
    yintercept = seq(0, 1, by = 0.25),
    color = grey(0.7),
    size = 0.2
  ) +
  geom_line() +
  ylab("Vaccination coverage (eligible population)") +
  xlab("") +
  ggtitle(
    "Scenario forecast vaccination coverage in LGAs of concern",
    "assuming 95% / 85% coverage in the over/under 40s"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "none",
    strip.background = element_blank()
  )

ggsave(
  "outputs/nsw/scenario_95_85.png",
  bg = "white",
  width = 9,
  height = 6
)
  
# compute efficacies against transmission, based on type and number of doses
efficacy_az_1_dose <- combine_efficacy(0.18, 0.48)
efficacy_pf_1_dose <- combine_efficacy(0.30, 0.46)
efficacy_az_2_dose <- combine_efficacy(0.60, 0.65)
efficacy_pf_2_dose <- combine_efficacy(0.79, 0.65)

# compute the *additional* effect of the second dose
efficacy_az_2_dose_extra <- efficacy_az_2_dose - efficacy_az_1_dose
efficacy_pf_2_dose_extra <- efficacy_pf_2_dose - efficacy_pf_1_dose

# compute fractional coverage by type and number of doses, then compute average efficacy against delta
coverage_air_80 <- air %>%
  arrange(
    scenario, coverage_scenario, lga, age_air_80, date
  ) %>%
  group_by(
    scenario, coverage_scenario, lga, age_air_80
  ) %>%
  # compute corrections for lag in acquired immunity for 1 dose and for 2 doses
  mutate(
    across(
      c(dose_1_AstraZeneca, dose_1_Pfizer),
      .fns = list(
        correction = ~slider::slide2_dbl(
          date, .x,
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
          date, .x,
          .f = immunity_lag_correction,
          weeks_increase = 2,
          weeks_wait = 0,
          .before = Inf
        )
      )
    )
  ) %>%
  # compute coverage with any vaccine (without correction)
  mutate(
    any_vaccine = dose_1_Pfizer + dose_1_AstraZeneca,
    coverage_any_vaccine = any_vaccine / population,
    .after = population
  ) %>%
  mutate(
    across(
      c(
        dose_1_AstraZeneca,
        dose_2_AstraZeneca,
        dose_1_Pfizer,
        dose_2_Pfizer
      ),
      .fns = c(
        fraction = ~ replace_na(.x / any_vaccine, 0.25)
      ),
      .names = "{.fn}_{.col}"
    )
  ) %>%
  # Use efficacies by dose and vaccine type, corrections for lag to acquired
  # immunity, and fractions of numbers of doses to compute the average efficacy
  # of all vaccinated people. Note that here we are deliberately using
  # proportions that do not sum to 1. We use the proportion of all vaccinated
  # people who have one (or two) doses of each type (so proportion_pf_1_dose +
  # proportion_az_1_dose = 1), and the proportion of all people with two doses
  # of each vaccine type, but for the latter we only consider the *additional*
  # efficacy of the second dose. That way, coverage can't slip back as
  # individuals move from dose 1 to dose 2.
  mutate(
    average_efficacy_transmission = average_efficacy(
      efficacy_az_1_dose = efficacy_az_1_dose * dose_1_AstraZeneca_correction,    
      efficacy_pf_1_dose = efficacy_pf_1_dose * dose_1_Pfizer_correction,
      # add additional efficacies for dose 2s
      efficacy_az_2_dose = efficacy_az_2_dose_extra * dose_2_AstraZeneca_correction,
      efficacy_pf_2_dose = efficacy_pf_2_dose_extra * dose_2_Pfizer_correction,
      proportion_pf_2_dose = fraction_dose_2_Pfizer,
      proportion_az_2_dose = fraction_dose_2_AstraZeneca,
      # and use full complement of dose 1s (even those that went on the have
      # their second doses)
      proportion_pf_1_dose = fraction_dose_1_Pfizer,
      proportion_az_1_dose = fraction_dose_1_AstraZeneca
    ),
    average_efficacy_transmission = replace_na(average_efficacy_transmission, 0),
    .after = coverage_any_vaccine
  ) %>%
  ungroup()

# tidy up unnecessary columns and disaggregate from AIR (capped at 80) age bins
# into 5y age bins, assuming the same coverage within the eligible population
coverage <- coverage_air_80 %>%
  # keep only coverages and fractions, which we can disaggregate
  select(
    scenario, coverage_scenario,
    date, lga, age_air_80, forecast,
    coverage_any_vaccine,
    average_efficacy_transmission,
    starts_with("fraction")
  ) %>%
  # join on the ages (duplicating some rows)
  left_join(
    air_12_age_lookup %>%
      select(
        age_model,
        age_air_80
      ) %>%
      distinct(),
    by = "age_air_80"
  ) %>%
  relocate(
    age_model, .after = age_air_80
  ) %>%
  # join on model populations for later use
  left_join(
    pop_model,
    by = c("lga", "age_model")
  ) %>%
  rename(
    age = age_model
  ) %>%
  mutate(
    age = factor(
      age,
      levels = str_sort(
        unique(age),
        numeric = TRUE
      )
    )
  ) %>%
  arrange(
    scenario, coverage_scenario, date, lga, age
  ) %>%
  # remove any data within 3 weeks of the start, since the time to acquired
  # immunity isn't correctly accounted for
  filter(
    date > (min(date) + 21)
  )

# check the number of doses by lga/date is the same after disaggregation and
# correction (to within a small numerical tolerance)
1e-9 > max(
  abs(
    coverage_air_80 %>%
      filter(
        date > (min(date) + 21)
      ) %>%
      group_by(
        scenario,
        coverage_scenario,
        lga,
        date
      ) %>%
      summarise(
        doses = sum(coverage_any_vaccine * population),
        .groups = "drop"
      ) %>%
      pull(doses) -
      coverage %>%
      group_by(
        scenario,
        coverage_scenario,
        lga,
        date
      ) %>%
      summarise(
        doses = sum(coverage_any_vaccine * population),
        .groups = "drop"
      ) %>%
      pull(doses)
  )
)

write.csv(coverage, file = "outputs/nsw/nsw_lgas_vaccination_coverage.csv")

# collapse back to air age bin for plotting
coverage_age_air_80_plot <- coverage %>%
  group_by(
    scenario,
    coverage_scenario,
    date,
    lga,
    age_air_80,
    forecast
  ) %>%
  summarise(
    coverage_any_vaccine = sum(coverage_any_vaccine * population) / sum(population),
    average_efficacy_transmission = first(average_efficacy_transmission),
    .groups = "drop"
  ) %>%
  mutate(
    age_air_80 = factor(
      age_air_80,
      levels = rev(unique(age_air_80))
    ),
    coverage_scenario = factor(
      coverage_scenario,
      levels = unique(coverage_scenario)
    )
  )

for (this_lga in c(lgas_of_concern, extra_lgas)) {
  
  coverage_age_air_80_plot %>%
    filter(
      lga == this_lga
    ) %>%
    mutate(
      vaccination_effect = coverage_any_vaccine * average_efficacy_transmission,
      coverage_scenario = str_remove(coverage_scenario, " coverage"),
      coverage_scenario = factor(
        coverage_scenario,
        levels = rev(unique(coverage_scenario))
      )
    ) %>%
    rename(
      age = age_air_80
    ) %>%
    ggplot(
      aes(
        x = date,
        y = vaccination_effect,
        col = age,
        linetype = forecast
      )
    ) +
    geom_line() +
    ggtitle(this_lga) +
    ylab("Transmission reduction") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    facet_grid(coverage_scenario ~ scenario) +
    theme_cowplot() +
    theme(
      strip.background = element_blank()
    )
  
  ggsave(
    filename = sprintf("outputs/nsw/NSW_%s_age_effect.png", this_lga),
    bg = "white",
    width = 6,
    height = 8
  )
  
}

baseline_ngm <- baseline_matrix(1)

# estimate effects of vaccination on transmission
vaccination_effect <- coverage %>%
  arrange(scenario, coverage_scenario, lga, date, age) %>%
  group_by(
    lga, date, forecast, scenario, coverage_scenario
  ) %>%
  summarise(
    vaccination_transmission_multiplier = vaccination_transmission_effect(
      age_coverage = coverage_any_vaccine,
      efficacy_mean = average_efficacy_transmission,
      next_generation_matrix = baseline_ngm,
      R0 = 1
    )$overall,
    .groups = "drop"
  ) %>%
  mutate(
    vaccination_transmission_reduction_percent =
      100 * (1 - vaccination_transmission_multiplier)
  )

write_csv(vaccination_effect, "outputs/nsw/nsw_lgas_vaccination_effect.csv")  


vaccination_effect_plot <- vaccination_effect %>%
  filter(
    lga != "Unincorporated NSW"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = rev(
        unique(
          vaccination_effect$scenario
        )
      )
    ),
    coverage_scenario = str_remove(
      coverage_scenario,
      " coverage"
    ),
    coverage_scenario = factor(
      coverage_scenario,
      levels = unique(coverage_scenario)
    )
  )

vaccination_effect_plot %>%
  arrange(scenario, coverage_scenario, date, lga) %>%
  ggplot(
    aes(
      x = date,
      y = vaccination_transmission_reduction_percent / 100,
      col = lga,
      linetype = forecast
    )
  ) +
  facet_grid(
    coverage_scenario ~ scenario
  ) +
  geom_line(
    alpha = 0.2
  ) +
  geom_line(
    data = vaccination_effect_plot %>%
      filter(
        lga %in% lgas_of_concern
      ),
    size = 1
  ) +
  ylab("") +
  xlab("") +
  ggtitle(
    "Reduction in transmission potential for LGAs",
    "LGAs of concern in bold"
  ) +
  theme_cowplot() +
  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    )
  ) +
  theme(
    strip.background = element_blank(),
    legend.position = "none"
  )

ggsave(
  "outputs/nsw/nsw_lga_tp_reduction.png",
  bg = "white",
  width = 6,
  height = 9
)

      