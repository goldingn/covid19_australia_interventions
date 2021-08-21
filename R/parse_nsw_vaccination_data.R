# compute NSW vaccination effects on transmission, by LGA.
source("R/functions.R")

# Account for lag in vaccination effect.

# For those with only 1 dose, no increase for 1 week, then linear increase from
# 0% to 100% over the next two weeks. For those with 2 doses, linear increase
# from 0% to 100% over the next two weeks.

# Compute a correction factor to downgrade the coverage (by number of doses)
# based on the fraction of all people with that number of doses that received it
# at each point in time the last 2-3 weeks.

# note that for first doses, this needs to be *all* first doses, since otherwise
# people leave and screw up the calculation
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

average_daily_doses <- function (
  air_current,
  latest_data_date = max(air_current$date),
  weeks_average = 4
) {

  air_current %>%
    filter(
      date > (latest_data_date - weeks_average * 7)
    ) %>%
    group_by(
      lga, age_air_80, population,
    ) %>%
    mutate(
      across(
        starts_with("dose"),
        ~diff(c(0, .))
      )
    ) %>%
    summarise(
      across(
        starts_with("dose"),
        mean
      ),
      .groups = "drop"
    )
  
}

# turn the forecast into a function, and add on a method to add extra vaccinations in some LGAs/weeks
forecast_vaccination <- function(
  air_current,
  latest_data_date = max(air_current$date),
  weeks_average = 4,
  max_date = as.Date("2021-09-30"),
  max_coverages = c(0.7, 0.8, 0.9, 1),
  extra_doses = NULL,
  scenario_name = "baseline"
) {
  
  # compute daily average numbers of doses in each age group and lga over the past weeks
  # start with an average, then try a random effects model (shrinkage & extrapolation will help for small populations)
  dailies <- average_daily_doses(
    air_current = air_current,
    latest_data_date = latest_data_date,
    weeks_average = weeks_average
  )
  
  # cumulative number of doses as a the most recent time point
  starting <- air_current %>%
    filter(
      date == latest_data_date
    ) %>%
    rename_with(
      .fn = function(x) paste0("starting_", x),
      .cols = starts_with("dose")
    ) %>%
    select(
      -date
    )
  
  # compute maximum coverages by age and LGA (allowing them to exceed that in
  # the observed data)
  max_coverage <- air_current %>%
    filter(
      date == max(date)
    ) %>%
    # compute coverage as at the latest date for each age and LGA (capped at 100%)
    mutate(
      any_doses = dose_1_Pfizer + dose_1_AstraZeneca,
      observed_max_coverage = pmin(1, any_doses / population)
    ) %>%
    select(
      lga, age_air_80, observed_max_coverage
    ) %>%
    # add on the assumed maximum coverage (later, for one of a number of scenarios)
    full_join(
      expand_grid(
        lga = unique(air_current$lga),
        age_air_80 = unique(air_current$age_air_80),
        hypothetical_max_coverage = max_coverages
      ),
      by = c("lga", "age_air_80")
    ) %>%
    # compute the maximum of the two for capping vaccination
    mutate(
      max_coverage = pmax(observed_max_coverage, hypothetical_max_coverage)
    ) %>%
    select(
      -observed_max_coverage
    )
  
  # extrapolate the number of cumulative doses into the future, without capping coverage
  
  # future dates for each LGA and age
  future_doses <- expand_grid(
    lga = unique(air_current$lga),
    age_air_80 = unique(air_current$age_air_80),
    date = seq(latest_data_date + 1, max_date, by = 1)
  ) %>%
    # add on daily vaccination rates (previous average)
    left_join(
      dailies,
      by = c("lga", "age_air_80")
    )
  
  # optionally add on some extra doses to represent an alternate vaccination
  # scenario
  if (!is.null(extra_doses)) {
    
    future_doses <- future_doses %>%
      bind_rows(
        extra_doses
      ) %>%
      group_by(
        lga, age_air_80, date, population
      ) %>%
      summarise(
        across(
          starts_with("dose"),
          sum
        ),
        .groups = "drop"
      ) %>%
      filter(
        date <= max_date
      )
    
  }
  
  # compute the cumulative sum to get total future doses by each day
  air_forecast <- future_doses %>%
    arrange(lga, age_air_80, date) %>%
    group_by(lga, age_air_80, population) %>%
    mutate(
      across(
        starts_with("dose"),
        cumsum
      ),
      across(
        starts_with("dose"),
        round
      )
    ) %>%
    # add on current observed total number of doses
    left_join(
      starting,
      by = c("lga", "age_air_80", "population")
    ) %>%
    mutate(
      dose_1_AstraZeneca = dose_1_AstraZeneca + starting_dose_1_AstraZeneca,
      dose_1_Pfizer = dose_1_Pfizer + starting_dose_1_Pfizer,
      dose_2_AstraZeneca = dose_2_AstraZeneca + starting_dose_2_AstraZeneca,
      dose_2_Pfizer = dose_2_Pfizer + starting_dose_2_Pfizer
    ) %>%
    select(
      -starts_with("starting")
    ) %>%
    # add on a forecast flag
    mutate(
      forecast = TRUE
    )
  
  # add the forecast to the current air data to get the full time series
  air_saturated <- air_current %>%
    mutate(
      forecast = FALSE
    ) %>%
    bind_rows(
      air_forecast
    ) %>%
    # add on the max coverages (with different thresholds)
    full_join(
      max_coverage,
      by = c("lga", "age_air_80")
    ) %>%
    arrange(date, lga, age_air_80) %>%
    # where the number of vaccinations exceeds the maximum population coverage, cap it
    mutate(
      # compute extra doses
      dose_1_extra = pmax(0, (dose_1_AstraZeneca + dose_1_Pfizer) - population * max_coverage),
      dose_2_extra = pmax(0, (dose_2_AstraZeneca + dose_2_Pfizer) - population * max_coverage)
    )
  
  # compute fraction of doses that are Pfizer - *as at the date the
  # population was saturated* otherwise it will keep adjusting
  # retrospectively
  air_final_fraction_dose_1 <- air_saturated %>%
    filter(
      dose_1_extra == 0
    ) %>%
    group_by(lga, age_air_80, hypothetical_max_coverage) %>%
    filter(
      date == max(date)
    ) %>%
    ungroup() %>%
    mutate(
      dose_1_Pfizer_fraction = dose_1_Pfizer / (dose_1_AstraZeneca + dose_1_Pfizer),
    ) %>%
    select(
      lga, age_air_80, hypothetical_max_coverage, dose_1_Pfizer_fraction 
    )
  
  air_final_fraction_dose_2 <- air_saturated %>%
    filter(
      dose_2_extra == 0
    ) %>%
    group_by(lga, age_air_80, hypothetical_max_coverage) %>%
    filter(
      date == max(date)
    ) %>%
    ungroup() %>%
    mutate(
      dose_2_Pfizer_fraction = dose_2_Pfizer / (dose_2_AstraZeneca + dose_2_Pfizer)
    ) %>%
    select(
      lga, age_air_80, hypothetical_max_coverage, dose_2_Pfizer_fraction 
    )

  # cap these extra doses, keeping the allocation between AZ and Pfizer constant after saturation   
  air <- air_saturated %>%
    left_join(
      air_final_fraction_dose_1,
      by = c("lga", "age_air_80", "hypothetical_max_coverage")
    ) %>%
    left_join(
      air_final_fraction_dose_2,
      by = c("lga", "age_air_80", "hypothetical_max_coverage")
    ) %>%
    mutate(
      # fill in any missing fractions (those that have not yet reached saturation)
      dose_1_Pfizer_fraction = replace_na(dose_1_Pfizer_fraction, 0),
      dose_2_Pfizer_fraction = replace_na(dose_2_Pfizer_fraction, 0),
      
      # compute the number of Pfizer doses at saturation
      dose_1_Pfizer_maximum = (population * max_coverage * dose_1_Pfizer_fraction),
      dose_1_AstraZeneca_maximum = (population * max_coverage * (1 - dose_1_Pfizer_fraction)),
      dose_2_Pfizer_maximum = (population * max_coverage * dose_2_Pfizer_fraction),
      dose_2_AstraZeneca_maximum = (population * max_coverage * (1 - dose_2_Pfizer_fraction)),
      
      # compute the numbers of excess doses
      dose_1_Pfizer_extra = pmax(0, dose_1_Pfizer - dose_1_Pfizer_maximum),
      dose_1_AstraZeneca_extra = pmax(0, dose_1_AstraZeneca - dose_1_AstraZeneca_maximum),
      dose_2_Pfizer_extra = pmax(0, dose_2_Pfizer - dose_2_Pfizer_maximum),
      dose_2_AstraZeneca_extra = pmax(0, dose_2_AstraZeneca - dose_2_AstraZeneca_maximum),

      # remove the excess doses
      dose_1_Pfizer = dose_1_Pfizer - dose_1_Pfizer_extra,
      dose_1_AstraZeneca = dose_1_AstraZeneca - dose_1_AstraZeneca_extra,
      dose_2_Pfizer = dose_2_Pfizer - dose_2_Pfizer_extra,
      dose_2_AstraZeneca = dose_2_AstraZeneca - dose_2_AstraZeneca_extra,
    )
  
  air %>%
    mutate(scenario = scenario_name)
  
}


# lookup to disaggregate coverages to 5y age groups
age_lookup <- tibble::tribble(
  ~age_5y, ~age_air, ~age_abs, ~age_air_80,
  "0-4", "0-14", "0-4", "0-14",
  "5-9", "0-14", "5-9", "0-14",
  "10-14", "0-14", "10-14", "0-14",
  "15-19", "15-29", "15-19", "15-29",
  "20-24", "15-29", "20-24", "15-29",
  "25-29", "15-29", "25-29", "15-29",
  "30-34", "30-39", "30-34", "30-39",
  "35-39", "30-39", "35-39", "30-39",
  "40-44", "40-49", "40-44", "40-49",
  "45-49", "40-49", "45-49", "40-49",
  "50-54", "50-59", "50-54", "50-59",
  "55-59", "50-59", "55-59", "50-59",
  "60-64", "60-69", "60-64", "60-69",
  "65-69", "60-69", "65-69", "60-69",
  "70-74", "70-79", "70-74", "70-79",
  "75-79", "70-79", "75-79", "70-79",
  "80+", "80-84", "80-84", "80+",
  "80+", "85+", "85+", "80+"
)

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

# load population data from Cth Health (at SA2 level, not LGA) to compute
# NSW-wide fractions of the 0-14 that are 12-14, and fractions of the 15-29 that
# are 16-29 (for AIR age distributions), and fractions of the 10-14 that are
# 12-14, and fractions of the 15-19 that are 16-19 (for 5y age distributions),
# and any other interesting fractions
pop_disagg <- read_csv(
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
) %>%
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
      .fns = list(pop = ~ sum(population * .))
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
  
# get populations with all the age aggregations
pop <- lga_age_population() %>%
  filter(
    state == "New South Wales"
  ) %>%
  select(
    -state
  ) %>%
  left_join(
    age_lookup,
    by = c("age" = "age_abs"),
  )

# collapse populations down to AIR age bins
pop_air <- pop %>%
  group_by(LGA_CODE19, LGA_NAME19, age_air) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  )

# load air data, remove provider type, add populations, and tidy
air_raw <- list.files(
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

latest_data_date <- max(air_raw$ENCOUNTER_DATE)

# add on 0 coverage for younger age groups, and collapse over 80 group
air_current <- expand_grid(
  ENCOUNTER_DATE = unique(air_raw$ENCOUNTER_DATE),
  LGA_NAME19 = unique(air_raw$LGA_NAME19),
  AGE_GROUP = age_lookup$age_air,
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
      "lga" = "LGA_NAME19",
      "age_air"
    )
  ) %>%
  # collapse the over 80 population and numbers of vaccinations of each type
  left_join(
    age_lookup %>%
      select(
        age_air,
        age_air_80
      ) %>%
      distinct(),
    by = "age_air"
  ) %>%
  select(
    -age_air,
    -LGA_CODE19
  ) %>%
  group_by(
    date,
    lga,
    age_air_80
  ) %>%
  summarise(
    across(
      starts_with("dose_"),
      ~sum(.)
    ),
    population = sum(population),
    .groups = "drop"
  )

# scenario for 670K more dose 1s delivered over the next three weeks
extra_670K_dose_1 <- air_current %>%
  # get average daily doses
  average_daily_doses() %>%
  # filter to only LGAs of concern, and age groups of concern
  filter(
    lga %in% lgas_of_concern,
    age_air_80 %in% c("15-29", "30-39")
  ) %>%
  # mask out types of dose not required
  mutate(
    across(
      c(starts_with("dose_2"), ends_with("AstraZeneca")),
      ~ . * 0
    )
  ) %>%
  # add on range of dates on which to overload doses (three weeks, starting the daty after the data was provided)
  full_join(
    expand_grid(
      lga = lgas_of_concern,
      date = max(air_current$date) + seq_len(21)
    ),
    by = "lga"
  ) %>%
  # normalise and multiply by number of doses
  mutate(
    dose_1_Pfizer = 670000 * dose_1_Pfizer / sum(dose_1_Pfizer)
  )

# and add 670K more dose 2s delivered 8 weeks after that to get the full scenario
extra_670K <- bind_rows(
  # half as many dose 1s on the same days
  extra_670K_dose_1,
  # half as many dose 2s 4 weeks later
  extra_670K_dose_1 %>%
    mutate(
      dose_2_Pfizer = dose_1_Pfizer,
      dose_1_Pfizer = 0,
      date = date + 7 * 8
    )
)

# check totals
extra_670K %>%
  summarise(
    across(
      starts_with("dose"),
      sum
    )
  )

air <- bind_rows(
  # forecast doses under current vaccination rate
  forecast_vaccination(
    air_current
  ),
  # forecast with additional dose 1s to 16-39s in LGAs of concern over 4 weeks
  forecast_vaccination(
    air_current,
    extra_doses = extra_670K,
    scenario_name = "670K extra doses"
  )
) %>%
  mutate(
    coverage_scenario = paste0(
      "max ",
      round(100 * hypothetical_max_coverage),
      "% coverage"
    )
  )

extra_670K_dose_1_cumulative <- extra_670K_dose_1 %>%
  filter(dose_1_Pfizer > 0) %>%
  arrange(lga, age_air_80, date) %>%
  group_by(lga, age_air_80) %>%
  mutate(
    across(
      starts_with("dose"),
      cumsum
    )
  ) %>%
  ungroup()

extra_670K_dose_1_excess <- extra_670K_dose_1_cumulative %>%
  full_join(
    air %>%
      filter(
        scenario != "baseline",
        lga %in% lgas_of_concern,
        age_air_80 %in% c("15-29", "30-39"),
        date >= min(extra_670K_dose_1_cumulative$date),
        date <= max(extra_670K_dose_1_cumulative$date)
      ) %>%
      select(
        c(date, lga, age_air_80, coverage_scenario, ends_with("extra"))
      ),
    by = c("date", "lga", "age_air_80")
  ) %>%
  mutate(
    dose_1_Pfizer_scenario_extra = pmin(dose_1_Pfizer_extra, dose_1_Pfizer)
  ) %>%
  select(
    lga, age_air_80, coverage_scenario, date, dose_1_Pfizer_scenario_extra
  ) %>%
  # collapse across age groups and LGAs
  group_by(date, coverage_scenario) %>%
  summarise(
    dose_1_Pfizer_scenario_extra = sum(dose_1_Pfizer_scenario_extra),
    .groups = "drop"
  )

# compute the number of AZ dose 1s given out to target group during this period
# under 670K rollout
air %>%
  filter(
    scenario != "baseline",
    lga %in% lgas_of_concern,
    age_air_80 %in% c("15-29", "30-39"),
    date >= min(extra_670K_dose_1_cumulative$date),
    date <= max(extra_670K_dose_1_cumulative$date)
  ) %>%
  arrange(lga, age_air_80, coverage_scenario, date) %>%
  group_by(lga, age_air_80, coverage_scenario) %>%
  mutate(
    across(
      starts_with("dose"),
      ~ . - min(.)
    )
  ) %>%
  group_by(coverage_scenario, date) %>%
  summarise(
    across(
      starts_with("dose_1"),
      sum
    )
  ) %>%
  ggplot(
    aes(
      x = date,
      y = dose_1_AstraZeneca,
      color = coverage_scenario
    )
  ) +
  scale_y_continuous(
    labels = scales::label_number()
  ) +
  geom_line() +
  ylab("") +
  xlab("") +
  ggtitle(
    "Cumulative number of AZ dose 1s used during 670K roll-out",
    "in 16-39 year olds in the LGAs of concern"
  ) +
  theme_cowplot()

ggsave(
  "outputs/nsw/AZ_doses_670K_target_pop.png",
  bg = "white",
  width = 9,
  height = 5
)

extra_670K_dose_1_excess %>%
  ggplot(
    aes(
      x = date,
      y = dose_1_Pfizer_scenario_extra,
      color = coverage_scenario
    )
  ) +
  scale_y_continuous(
    labels = scales::label_number()
  ) +
  geom_line() +
  ylab("") +
  xlab("") +
  ggtitle(
    "Cumulative number of Pfizer dose 1s unused during 670K roll-out"
  ) +
  theme_cowplot()

ggsave(
  "outputs/nsw/unused_doses_670K.png",
  bg = "white",
  width = 9,
  height = 5
)

# print the number of excess Pfizer doses by coverage scenario
extra_670K_dose_1_excess %>% filter(
  date == max(date)
) %>%
  select(-date)

# identify LGAs with greater than 90% coverage in the data
air %>%
  filter(
    date == max(air_current$date),
    coverage_scenario == "max 90% coverage",
    max_coverage > hypothetical_max_coverage,
    scenario == "baseline"
  ) %>%
  select(lga, age_air_80) %>%
  table()


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
    facet_grid(rows = vars(doses), cols = vars(scenario)) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(
        accuracy = 1
      )
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
    width = 8,
    height = 5
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
coverage <- air %>%
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
        fraction = ~ . / any_vaccine
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
  ungroup() %>%
  # break into the 5y age bins, assuming the same coverage
  left_join(
    age_lookup %>%
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
  select(
    # -age_air_80,
    -population,
    -ends_with("correction"),
    -starts_with("fraction")
  ) %>%
  rename(
    age = age_5y
  ) %>%
  arrange(
    lga, date, age
  ) %>%
  # remove any data within 3 weeks of the start, since the time to acquired
  # immunity isn't correctly accounted for
  filter(
    date > (min(date) + 21)
  )

# correct the coverages within the 5y age bins for the fraction of those age
# bins that are within the broader AIR age bins.    
coverage_corrected <- coverage %>%
  # remove the maxima - it doesn't make sense to correct these
  select(
    -ends_with("maximum")
  ) %>%
  # For 0-14 AIR (0-4, 5-9, 10-14 5y), set 0-4 and 5-9 to 0, and set 10-14 to
  # mop up the coverages they lose by dividing by the fraction of 0-14 year
  # olds that are 10-14
  mutate(
    # compute correction factors
    age_correction = case_when(
      age %in% c("0-4", "5-9") ~ 0,
      age == "10-14" ~ 1 / pop_disagg$fraction_0_14_are_10_14,
      TRUE ~ 1
    ),
    # apply correction factors
    across(
      c(any_vaccine, coverage_any_vaccine, starts_with("dose")),
      ~ . * age_correction
    )
  ) %>%
  select(-age_correction)

# check the number of doses is the same after correction
identical(
  coverage %>%
    select(
      -ends_with("maximum")
    ) %>%
    group_by(age) %>%
    summarise(
      across(
        starts_with("dose"),
      sum
      )
    ),
  coverage_corrected %>%
    group_by(age) %>%
    summarise(
      across(
        starts_with("dose"),
        sum
      )
    )
)

write.csv(coverage_corrected, file = "outputs/nsw/nsw_lgas_vaccination_coverage.csv")

for (this_lga in lgas_of_concern) {
  
  coverage_corrected %>%
    filter(
      lga == this_lga
    ) %>%
    mutate(
      vaccination_effect = coverage_any_vaccine * average_efficacy_transmission,
    ) %>%
    rename(
      age_5y = age,
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
    facet_grid(~scenario + coverage_scenario) +
    theme_cowplot() +
    theme(
      strip.background = element_blank()
    )
  
  ggsave(
    filename = sprintf("outputs/nsw/NSW_%s_age_effect.png", this_lga),
    bg = "white",
    width = 9,
    height = 4
  )
  
}


# estimate effects of vaccination on transmission
vaccination_effect <- coverage_corrected %>%
  # subset to recent weeks to speed up computation
  filter(
    date > as.Date("2021-06-16")
  ) %>%
  group_by(
    lga, date, forecast, scenario, coverage_scenario
  ) %>%
  summarise(
    vaccination_transmission_multiplier = vaccination_transmission_effect(
      age_coverage = coverage_any_vaccine,
      efficacy_mean = average_efficacy_transmission,
      next_generation_matrix = baseline_matrix(1),
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
      levels = c(
        "baseline",
        "670K extra doses"
      )
    ),
    coverage_scenario = factor(
      coverage_scenario,
      levels = c(
        "max 70% coverage",
        "max 80% coverage", 
        "max 90% coverage",
        "max 100% coverage"
      )
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
    scenario ~ coverage_scenario
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
  width = 7,
  height = 5
)


# for each lga of concern, plot the two scenarios, for each vaccination coverage
# assumption
for (this_lga in lgas_of_concern) {
  
  vaccination_effect_plot %>%
    filter(lga == this_lga) %>%
    mutate(
      scenario = factor(
        scenario,
        levels = rev(levels(scenario))
      )
    ) %>%
    ggplot(
      aes(
        x = date,
        y = vaccination_transmission_reduction_percent / 100,
        col = scenario,
        linetype = forecast
      )
    ) +
    geom_line(
      size = 1
    ) +
    facet_wrap(
      ~coverage_scenario,
      ncol = 4
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    ylab("") +
    xlab("") +
    ggtitle(
      paste0(this_lga, " - reduction in transmission potential"),
      "baseline (green) versus 670K scenario (red)"
    ) +
    theme_cowplot() +
    theme(
      legend.position = "none",
      strip.background = element_blank()
    )

  ggsave(
    paste0(
      "outputs/nsw/scenario_",
      gsub("_", "", this_lga),
      ".png"
      ),
    bg = "white",
    width = 10,
    height = 4
  )
    
}

