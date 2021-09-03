# summarise postcode level vaccination effect in LGAs of concern
source("R/functions.R")

# load sf object for Australian postcodes
get_poa <- function () {
  library(sf)
  
  st_read(
    "data/spatial/abs/POA_2016_AUST.shp",
    quiet = TRUE
  ) %>%
    select(
      postcode = POA_NAME16
    )
  
}

# maps of things
plot_map <- function(tibble, value) {
  
  value <- enquo(value)
  
  poa <- get_poa()
  
  data <- poa %>%
    right_join(
      tibble,
      by = "postcode"
    )
  
  poa_bg <- poa %>%
    st_intersection(
      st_as_sfc(
        st_bbox(
          data
        )
      )
    )

  ggplot() +
    geom_sf(
      data = poa_bg
    ) + 
    geom_sf(
      aes(
        fill = !!value
      ),
      data = data
    ) + 
    theme_minimal()
  
}

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
  "Sydney (C)",
  "Randwick (C)"
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

# get postcode age populations
pop <- postcode_age_pop()

# collapse populations down to AIR age bins
pop_air <- pop %>%
  group_by(postcode, age_air) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  )

# and to 5y age bins
pop_5y <- pop %>%
  select(
    postcode,
    age_5y,
    population
  ) %>%
  group_by(
    postcode, age_5y
  ) %>%
  summarise(
    across(population, sum),
    .groups = "drop"
  )

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

# add on 0 coverage for younger age groups, and collapse over 80 group
air_current <- expand_grid(
  ENCOUNTER_DATE = unique(air_raw_postcode$ENCOUNTER_DATE),
  POSTCODE = unique(air_raw_postcode$POSTCODE),
  AGE_GROUP = unique(age_lookup()$age_air),
  DOSE_NUMBER = unique(air_raw_postcode$DOSE_NUMBER),
  VACCINE_TYPE = unique(air_raw_postcode$VACCINE_TYPE),
  PROVIDER_TYPE = unique(air_raw_postcode$PROVIDER_TYPE),
) %>%
  filter(
    !(AGE_GROUP %in% air_raw_postcode$AGE_GROUP)
  ) %>%
  mutate(
    CUMULATIVE_DAILY_COUNT = 0
  ) %>%
  left_join(
    air_raw_postcode %>%
      select(
        POSTCODE,
        LGA_NAME19
      ) %>%
      distinct(),
    by = "POSTCODE"
  ) %>%
  bind_rows(
    air_raw_postcode,
  ) %>%
  # collapse provider type
  group_by(
    POSTCODE,
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
    postcode,
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
      "postcode",
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
    postcode,
    age_air_80
  ) %>%
  summarise(
    across(
      starts_with("dose_"),
      ~sum(.x)
    ),
    population = sum(population),
    .groups = "drop"
  ) %>%
  # remove any postcodes with no population (all post office boxes) - noting
  # that we will be undercounting coverage somewhat
  filter(
    !is.na(population),
    population > 0
  )

# cap the maximum doses
air_current_capped <- air_current %>%
  mutate(
    
    dose_1 = dose_1_AstraZeneca + dose_1_Pfizer,
    dose_2 = dose_2_AstraZeneca + dose_2_Pfizer,
    
    dose_1_extra = pmax(0, dose_1 - population),
    dose_2_extra = pmax(0, dose_2 - population),
    
    dose_1_Pfizer_fraction = dose_1_Pfizer / dose_1,
    dose_2_Pfizer_fraction = dose_2_Pfizer / dose_2,
    
    dose_1_Pfizer_fraction = replace_na(dose_1_Pfizer_fraction, 0),
    dose_2_Pfizer_fraction = replace_na(dose_2_Pfizer_fraction, 0),
    
    # compute the number of Pfizer doses at saturation
    dose_1_Pfizer_maximum = (population * dose_1_Pfizer_fraction),
    dose_1_AstraZeneca_maximum = (population * (1 - dose_1_Pfizer_fraction)),
    dose_2_Pfizer_maximum = (population * dose_2_Pfizer_fraction),
    dose_2_AstraZeneca_maximum = (population * (1 - dose_2_Pfizer_fraction)),
    
    # compute the numbers of excess doses
    dose_1_Pfizer_extra = pmax(0, dose_1_Pfizer - dose_1_Pfizer_maximum),
    dose_1_AstraZeneca_extra = pmax(0, dose_1_AstraZeneca - dose_1_AstraZeneca_maximum),
    dose_2_Pfizer_extra = pmax(0, dose_2_Pfizer - dose_2_Pfizer_maximum),
    dose_2_AstraZeneca_extra = pmax(0, dose_2_AstraZeneca - dose_2_AstraZeneca_maximum),
    
    # remove the excess doses
    dose_1_Pfizer = dose_1_Pfizer - dose_1_Pfizer_extra,
    dose_1_AstraZeneca = dose_1_AstraZeneca - dose_1_AstraZeneca_extra,
    dose_2_Pfizer = dose_2_Pfizer - dose_2_Pfizer_extra,
    dose_2_AstraZeneca = dose_2_AstraZeneca - dose_2_AstraZeneca_extra
  )


# filter to LGAs of concern
air_current_concern <- air_current_capped %>%
  filter(
    lga %in% c(lgas_of_concern, extra_lgas)
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
coverage_concern_air_80 <- air_current_concern %>%
  arrange(
    postcode, age_air_80, date
  ) %>%
  group_by(
    postcode, lga, age_air_80
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
coverage_concern <- coverage_concern_air_80 %>%
  # keep only coverages and fractions, which we can disaggregate
  select(
    date, postcode, lga, age_air_80,
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
  # join on 5y populations for later use
  left_join(
    pop_5y,
    by = c("postcode", "age_5y")
  ) %>%
  rename(
    age = age_5y
  ) %>%
  arrange(
    date, lga, postcode, age
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
    coverage_concern_air_80 %>%
      filter(
        date > (min(date) + 21)
      ) %>%
      group_by(
        lga,
        postcode,
        date
      ) %>%
      summarise(
        doses = sum(coverage_any_vaccine * population),
        .groups = "drop"
      ) %>%
      pull(doses) -
      coverage_concern %>%
      group_by(
        lga,
        postcode,
        date
      ) %>%
      summarise(
        doses = sum(coverage_any_vaccine * population),
        .groups = "drop"
      ) %>%
      pull(doses)
  )
)

write_csv(
  coverage_concern,
  file = "outputs/nsw/nsw_postode_lgas_concern_vaccination_coverage.csv"
)

baseline_ngm <- baseline_matrix(1)

# estimate effects of vaccination on transmission
vaccination_effect <- coverage_concern %>%
  # subset to recent weeks to speed up computation
  filter(
    date > as.Date("2021-06-16")
  ) %>%
  arrange(lga, postcode, date) %>%
  group_by(
    lga, postcode, date
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

write_csv(
  vaccination_effect,
  "outputs/nsw/nsw_postcode_lgas_concern_vaccination_effect.csv"
)  

# compute a case-weighted vaccination effect on transmission

# this is an enormous file padded with 0s, remove them first
cases <- read_csv(
  "~/not_synced/vaccination/nsw/CASES_2021-09-01_UNSW.csv",
  col_types = cols(
    POSTCODE = col_character(),
    LGA_CODE19 = col_character(),
    LGA_NAME19 = col_character(),
    NOTIFICATION_DATE = col_date(format = ""),
    CALCULATED_ONSET_DATE = col_date(format = ""),
    INFECTIOUS_STATUS = col_character(),
    LIKELY_SOURCE_OF_INFECTION_LOCAL = col_character(),
    number_cases = col_double(),
    snapshot_date = col_date(format = "")
  )) %>%
  filter(
    number_cases > 0
  )

# summarise case counts by postcode (total in previous week)
postcode_case_weights <- cases %>%
  filter(
    # filter to last week
    NOTIFICATION_DATE > (Sys.Date() - 7),
    # remove po-box postcodes
    LGA_NAME19 != "Not Mapped"
  ) %>%
  select(
    postcode = POSTCODE,
    lga = LGA_NAME19,
    cases = number_cases
  ) %>%
  group_by(
    postcode,
    lga
  ) %>%
  summarise(
    across(
      cases,
      sum
    ),
    .groups = "drop"
  ) %>%
  mutate(
    case_weight_nsw = cases / sum(cases)
  ) %>%
  group_by(
    lga
  ) %>%
  mutate(
    case_weight_lga = cases / sum(cases)
  ) %>%
  ungroup()
  
postcode_populations_concern <- coverage_concern %>%
  filter(
    date == max(date)
  ) %>%
  group_by(
    postcode
  ) %>%
  summarise(
    across(
      population,
      sum
    )
  )

# join them to the vaccination effect estimates and populations
vaccination_effect_with_weights <- vaccination_effect %>%
  filter(
    date == max(date)
  ) %>%
  left_join(
    postcode_populations_concern,
    by = "postcode"
  ) %>%
  mutate(
    population_weight_nsw = population / sum(population)
  ) %>%
  group_by(
    lga
  ) %>%
  mutate(
    population_weight_lga = population / sum(population)
  ) %>%
  ungroup() %>%
  left_join(
    postcode_case_weights,
    by = c("postcode", "lga")
  ) %>%
  mutate(
    across(
      c("cases", starts_with("case_weight")),
      ~ replace_na(.x, replace = 0)
    )
  )

# LGAS of concern population-weighted and case-weighted vaccination effect
vaccination_effect_with_weights %>%
  filter(
    lga %in% lgas_of_concern
  ) %>%
  summarise(
    population_weighted_reduction = weighted.mean(
      vaccination_transmission_reduction_percent,
      population_weight_nsw
    ),
    case_weighted_reduction = weighted.mean(
      vaccination_transmission_reduction_percent,
      case_weight_nsw
    )
  ) %>%
  mutate(
    ratio = case_weighted_reduction / population_weighted_reduction
  )


vaccination_effect_with_weights %>%
  filter(
    lga %in% c(lgas_of_concern, "Camden (A)", "Randwick (C)")
  ) %>%
  group_by(
    lga
  ) %>%
  summarise(
    population_weighted_reduction = weighted.mean(
      vaccination_transmission_reduction_percent,
      population_weight_nsw
    ),
    case_weighted_reduction = weighted.mean(
      vaccination_transmission_reduction_percent,
      case_weight_nsw
    )
  ) %>%
  mutate(
    ratio = case_weighted_reduction / population_weighted_reduction
  )


# compute a postcode-case-weighted average vaccination effect in each LGA
vaccination_effect_with_weights %>%
  filter(lga %in% lgas_with_cases) %>%
  summarise(
    overall_reduction = mean(vaccination_transmission_reduction_percent),
    case_weighted_reduction = weighted.mean(
      vaccination_transmission_reduction_percent,
      weight_nsw
    )
  )







# plot the latest vaccination effects

map_cols <- RColorBrewer::brewer.pal(9, "Set1")

vaccination_effect %>%
  filter(
    date == max(date)
  ) %>%
  mutate(
    `Reduction in\ntransmission potential` = vaccination_transmission_reduction_percent / 100
  ) %>%
  plot_map(
    value = `Reduction in\ntransmission potential`
  ) +
  scale_fill_steps(
    low = "pink",
    high = "blue",
    n.breaks = 10,
    show.limits = TRUE,
    labels = scales::percent_format(accuracy = 1)
  )

ggsave("outputs/nsw/postcode_vaccination_effect.png",
       bg = "white",
       width = 10,
       height = 10)

seifa <- read_csv(
  "data/spatial/seifa/SEIFA_POA_18082020161751202.csv",
  col_types = cols(
    POA = col_character(),
    `Postal Area Code` = col_double(),
    INDEX_TYPE = col_character(),
    `Index type` = col_character(),
    MEASURE = col_character(),
    Measure = col_character(),
    TIME = col_double(),
    Time = col_double(),
    Value = col_double(),
    `Flag Codes` = col_character(),
    Flags = col_character()
  )) %>%
  pivot_wider(
    names_from = INDEX_TYPE,
    values_from = Value
  ) %>%
  select(
    postcode = POA,
    IRSAD
  ) %>%
  filter(
    !is.na(IRSAD),
    postcode %in% vaccination_effect$postcode
  )


seifa %>% plot_map(
  value = IRSAD
) +
  scale_fill_steps(
    low = grey(0.9),
    high = map_cols[1],
    n.breaks = 10,
    show.limits = TRUE
  )

vaccination_reduction <- vaccination_effect %>%
  filter(
    date == max(date)
  ) %>%
  mutate(
    tp_reduction = vaccination_transmission_reduction_percent / 100
  ) %>%
  select(
    postcode,
    tp_reduction
  )
  
# plot vaccination TP reduction by SEIFA status
vaccination_reduction %>%
  left_join(
    seifa,
    by = "postcode"
  ) %>%
  mutate(
    IRSAD_quint = factor(ntile(IRSAD, 5))
  ) %>%
  filter(
    !is.na(IRSAD)
  ) %>%
  ggplot(
    aes(
      x = IRSAD_quint,
      y = tp_reduction
    )
  ) +
  geom_boxplot() +
  theme_cowplot()

# library(leaflet)
# get_poa() %>%
#   # st_simplify(dTolerance = 0.001) %>%
#   right_join(
#     seifa,
#     by = "postcode"
#   ) %>%
#   right_join(
#     vaccination_reduction,
#     by = "postcode"
#   ) %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolygons(
#     fillOpacity = 0.5
#   )


# for all postcodes, pull out:
# % reduction in transmission
# SEIFA score
# case counts
# week-on-week case growth rates

# cases <- read_csv(
#   "~/not_synced/vaccination/nsw/CASES_2021-08-29_UNSW.csv",
#   col_types = cols(
#     POSTCODE = col_double(),
#     LGA_CODE19 = col_logical(),
#     LGA_NAME19 = col_character(),
#     NOTIFICATION_DATE = col_date(format = ""),
#     CALCULATED_ONSET_DATE = col_date(format = ""),
#     INFECTIOUS_STATUS = col_character(),
#     LIKELY_SOURCE_OF_INFECTION_LOCAL = col_character(),
#     number_cases = col_double(),
#     snapshot_date = col_date(format = "")
#   )
# )



