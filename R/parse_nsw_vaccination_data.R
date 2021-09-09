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

pop_disagg <- pop_detailed %>%
  filter(
    ste_name16 == "New South Wales"
  ) %>%
  select(
    -starts_with("is")
  ) %>%
  # indicators for belonging to population groups 
  mutate(
    # AIR age bins
    is_0_14 = age_lower >= 0 & age_upper <= 14,
    is_15_29 = age_lower >= 15 & age_upper <= 29,
    # 5y age bins
    is_10_14 = age_lower >= 10 & age_upper <= 14,
    is_15_19 = age_lower >= 15 & age_upper <= 19,
    # age bins needed to disaggregating these around the 16+ adult eligibility
    # and 12-15 child eligibility
    # children
    is_12_14 = age_lower >= 12 & age_upper <= 14,
    is_15 = age_lower >= 15 & age_upper <= 15,
    # adults
    is_16_19 = age_lower >= 16 & age_upper <= 19,
    is_16_29 = age_lower >= 16 & age_upper <= 29,
    is_any = TRUE
  ) %>%
  # populations in these groups in NSW
  summarise(
    across(
      starts_with("is_"),
      .fns = list(pop = ~ sum(population * .x))
    )
  ) %>%
  mutate(
    # AIR
    fraction_0_14_eligible_child = is_12_14_pop / is_0_14_pop,
    fraction_15_29_eligible_child = is_15_pop / is_15_29_pop,
    fraction_15_29_eligible_adult = is_16_29_pop / is_15_29_pop,
    # 5y
    fraction_10_14_eligible_child = is_12_14_pop / is_10_14_pop,
    fraction_15_19_eligible_adult = is_16_19_pop / is_15_19_pop,
    fraction_15_19_eligible_child = is_15_pop / is_15_19_pop,
    # AIR to 5y
    fraction_0_14_are_10_14 = is_10_14_pop / is_0_14_pop
  ) %>%
  select(starts_with("fraction"))
   
# # get populations with all the age aggregations
# pop <- lga_age_population() %>%
#   filter(
#     state == "New South Wales"
#   ) %>%
#   select(
#     -state
#   ) %>%
#   left_join(
#     age_lookup(),
#     by = c("age" = "age_abs"),
#   ) %>%
#   # for some reason this name is borked
#   mutate(
#     LGA_NAME19 = case_when(
#       LGA_NAME19 == "Nambucca Valley (A)" ~ "Nambucca (A)",
#       TRUE ~ LGA_NAME19
#     )
#   )

pop <- lga_age_population_nsw() %>%
  left_join(
    age_lookup(),
    by = c("age" = "age_abs"),
  )

pop %>%
  group_by(age_air_80) %>%
  summarise(
    across(
      population,
      sum
    )
  )

# add on other age aggregations and join together

# collapse populations down to AIR age bins
pop_air <- pop %>%
  group_by(lga, age_air) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  )

# and to 5y age bins
pop_5y <- pop %>%
  select(
    lga,
    age_5y,
    population
  ) %>%
  group_by(
    lga, age_5y
  ) %>%
  summarise(
    across(population, sum),
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
  )

# aggregate to LGA level
air_raw <- air_raw_postcode %>%
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

# add on 0 coverage for younger age groups, and collapse over 80 group
air_current <- expand_grid(
  ENCOUNTER_DATE = unique(air_raw$ENCOUNTER_DATE),
  LGA_NAME19 = unique(air_raw$LGA_NAME19),
  AGE_GROUP = unique(age_lookup()$age_air),
  DOSE_NUMBER = unique(air_raw$DOSE_NUMBER),
  VACCINE_TYPE = unique(air_raw$VACCINE_TYPE),
  PROVIDER_TYPE = unique(air_raw$PROVIDER_TYPE),
) %>%
  filter(
    !(AGE_GROUP %in% air_raw$AGE_GROUP)
  ) %>%
  mutate(
    CUMULATIVE_DAILY_COUNT = 0
  ) %>%
  bind_rows(
    air_raw,
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
    age_lookup() %>%
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

air <- bind_rows(
  # forecast doses under current vaccination rate, assuming no under 16s are
  # being vaccinated (we cannot determine the rate of vaccination of 15 year olds from the data)
  forecast_vaccination(
    air_current,
    previous_days_average = 0:6,
    max_date = Sys.Date() + 7 * 8,
    vaccinating_12_15 = FALSE
  )
) %>%
  # tidy up labels for printing
  mutate(
    coverage_scenario = paste0(
      "max ",
      round(100 * hypothetical_max_coverage),
      "% coverage"
    ),
    coverage_scenario = factor(
      coverage_scenario,
      levels = unique(coverage_scenario)
    )
  )

for (coverage in unique(air$coverage_scenario)) {
  
  # plot coverage stats
  air %>%
    filter(
      lga %in% lgas_of_concern,
      coverage_scenario == coverage,
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
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = ends_with("vaccinated"),
      names_to = "doses",
      values_to = "vaccinated" 
    ) %>%
    mutate(
      coverage = vaccinated / population,
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
    ylab("Vaccination coverage (whole population)") +
    xlab("") +
    ggtitle(
      "Scenario forecast vaccination coverage in LGAs of concern",
      paste0("assuming ", coverage, " in each age group")
    ) +
    theme_cowplot() +
    theme(
      legend.position = "none",
      strip.background = element_blank()
    )
  
  ggsave(
    paste0(
      "outputs/nsw/scenario_",
      gsub(" ", "_", gsub("%", "", coverage)),
      ".png"
    ),
    bg = "white",
    width = 9,
    height = 6
  )
  
}

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

correction_10_14 <- pop %>%
  select(
    lga = LGA_NAME19,
    age_5y,
    age_air_80,
    population
  ) %>%
  filter(
    age_air_80 == "0-14"
  ) %>%
  group_by(lga) %>%
  summarise(
    fraction_0_14_are_10_14 = sum(population * (age_5y == "10-14")) / sum(population)
  )

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
    age_lookup() %>%
      select(
        age_5y,
        age_air_80
      ) %>%
      distinct(),
    by = "age_air_80"
  ) %>%
  relocate(
    age_5y, .after = age_air_80
  ) %>%
  # Correct the coverages for three 5y age brackets falling in the 0-14 AIR age
  # bracket, since the first two (0-4 and 5-9) should have 0 coverage, and
  # coverage for 10-14 should be higher, by the ratio of 0-14 population to
  # 10-14 population. I.e.:
  #   n_vaccinated = coverage * pop0_14
  #   new_coverage = n_vaccinated / pop10_14
  # so:
  #   new_coverage = coverage * pop0_14 / pop10_14
  #                = coverage * 1 / (pop10_14 / pop0_14)
  left_join(
    correction_10_14,
    by = "lga"
  ) %>%
  mutate(
    coverage_any_vaccine = case_when(
      age_5y %in% c("0-4", "5-9") ~ 0,
      age_5y == "10-14" ~ coverage_any_vaccine / fraction_0_14_are_10_14,
      TRUE ~ coverage_any_vaccine
    )
  ) %>%
  select(
    -fraction_0_14_are_10_14
  ) %>%
  # join on 5y populations for later use
  left_join(
    pop_5y,
    by = c("lga", "age_5y")
  ) %>%
  rename(
    age = age_5y
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
  # subset to recent weeks to speed up computation
  filter(
    date > as.Date("2021-06-16")
  ) %>%
  arrange(scenario, coverage_scenario, lga) %>%
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

# # percentage reduction in transmission due to vaccination
# vaccination_effect %>%
#   filter(date == latest_data_date) %>%
#   select(
#     vaccination_transmission_reduction_percent,
#     lga
#   ) %>%
#   pull(vaccination_transmission_reduction_percent) %>%
#   hist(breaks = 100)
# 
# vaccination_effect %>%
#   filter(date == latest_data_date - 14) %>%
#   select(
#     vaccination_transmission_reduction_percent,
#     lga
#   )

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
    "Reduction in transmission potential for each NSW LGA",
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

      