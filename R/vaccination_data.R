library(readxl)

vax_files_dates <- function(
  dir = "~/not_synced/vaccination/vaccination_data/"
) {
  
  files <- list.files(
    path = dir,
    pattern = ".xl",
    full.names = TRUE
  )
  
  filenames <- list.files(
    path = dir,
    pattern = ".xl",
    full.names = FALSE
  )
  
  dates <- sapply(
    X = filenames,
    FUN = function(x){
      # this deals with the variable naming and format of files
      # but may need to be tweaked further if file names change again
      if (is.na(anytime::anydate(x))) {
        xsplit <- strsplit(x, split = "_")[[1]] %>%
          anytime::anydate(.)
        
        xsplit[!is.na(xsplit)]
      } else {
        anytime::anydate(x)
      }

    },
    USE.NAMES = FALSE
  ) %>% as.Date(origin = as.Date("1970-01-01"))
  
  tibble::tibble(
    file = files,
    date = dates
  )
  
}




read_vax_data <- function(
  file,
  date
){
  
 header <- readxl::read_excel(
    path = file,
    n_max = 2,
    col_names = FALSE,
    sheet = "Vaccine Brand Split"
  ) %>%
    t %>%
    as_tibble %>%
    tidyr::fill(V1, .direction = "down") %>%
    mutate(name = paste(V1, V2, sep = "_")) %>%
    pull(name)
  
  over_80 <- c(
    "80+",
    "80-84",
    "85+",
    "85-89",
    "90+",
    "90-94",
    "95+",
    "95-99",
    "100+"
  )
  
  n_max <- ifelse(date < "2021-08-17", 38, 40)
  
  readxl::read_excel(
    path = file,
    skip = 2,
    col_names = header,
    sheet = "Vaccine Brand Split",
    n_max = n_max,
    col_types = "text"
  ) %>%
    tidyr::fill(
      `NA_Vaccine Name`,
      .direction = "down"
    ) %>%
    dplyr::rename(
      vaccine = `NA_Vaccine Name`,
      age_class = 2 # need to fix this so works with either naming convention
    ) %>%
    pivot_longer(
      cols = -vaccine:-age_class,
      names_to = "name",
      values_to = "doses"
    ) %>%
    mutate(
      state = sub(
        pattern = "_.*",
        replacement = "",
        x = name
      ),
      dose_number = sub(
        pattern = ".* ", # splits in trailing space - could get caught if format changes
        replacement = "",
        x = name
      ) %>%
        as.integer
    ) %>%
    dplyr::select(state, age_class, vaccine, dose_number, doses) %>%
      filter(age_class != "Totals", state != "Totals") %>%
    mutate(
      vaccine = case_when(
        vaccine == "COVID-19 Vaccine AstraZeneca" ~ "az",
        TRUE ~ "pf"
      )
    ) %>%
    mutate(
      doses = ifelse(
        doses == "-",
        0,
        doses
      ) %>%
        as.integer
    ) %>%
    pivot_wider(
      names_from = dose_number,
      values_from = doses
    ) %>%
    mutate(
      `1` = `1` - `2`
    ) %>%
    pivot_longer(
      cols = `1`:`2`,
      names_to = "dose_number",
      values_to = "doses"
    ) %>%
    rowwise %>%
    mutate(
      age_class = case_when(
        any(age_class == over_80) ~ "80+",
        TRUE ~ age_class
      )
    ) %>%
    group_by(state, age_class, vaccine, dose_number) %>%
    summarise(
      doses = sum(doses)
    ) %>%
    ungroup %>%
    mutate(
      dose_number = as.integer(dose_number),
      date = date,
    )

}

load_vax_data <- function(){
 filesdates  <- vaccine_files_dates()
 
 mapply(
   FUN = read_vax_data,
   file = filesdates$file,
   date = filesdates$date,
   SIMPLIFY = FALSE,
   USE.NAMES = FALSE
 ) %>%
   bind_rows
 
}

vdat <- load_vax_data()



immunity_lag_correction <- function(
  date,
  doses,
  dose_number
) {
  
  if (dose_number[1] == 1) {
    weeks_increase <- 2
    weeks_wait <- 1
  } else if (dose_number[1] == 2) {
    weeks_increase <- 2
    weeks_wait <- 0
  }
  
  # compute the current doses (by dose/vaccine)
  max_date <- which.max(date)
  latest_doses <- doses[max_date]
  
  # compute the diff of dosess for all dates to get the proportion of the
  # population added on/by that date
  new_doses <- diff(c(0, doses))
  
  # compute the ratio of the proportion added on each previous date to the
  # current doses (should sum to 1)
  date_weights <- new_doses / latest_doses
  
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

# lookup to disaggregate coverages to 5y age groups
age_lookup <- tibble::tribble(
  ~age_5y, ~age, ~proportion_of_group,
  "0-4", "0-14", 5/15,
  "5-9", "0-14", 5/15,   
  "10-14", "0-14", 5/15,
  "15-19", "15-29", 5/15,
  "20-24", "15-29", 5/15,
  "25-29", "15-29", 5/15,
  "30-34", "30-39", 5/10,
  "35-39", "30-39", 5/10,
  "40-44", "40-49", 5/10,
  "45-49", "40-49", 5/10,
  "50-54", "50-59", 5/10,
  "55-59", "50-59", 5/10,
  "60-64", "60-69", 5/10,
  "65-69", "60-69", 5/10,
  "70-74", "70-79", 5/10,
  "75-79", "70-79", 5/10,
  "80+", "80+", 1/1
)

# check these proportions all sum to 1
age_lookup %>% group_by(age) %>%
  summarise(
    sum(proportion_of_group)
  )

age_distribution <- get_age_distribution(final_age_bin = 80)

get_age_distribution_by_state <- function(
  final_age_bin = 80,
  by = 5,
  population_total = 25693000
) {
  
  # check the final age bin in sensible
  if (final_age_bin > 85) {
    stop(
      "No age-specific population data for ages greater than 85",
      call. = TRUE
    )
  }
  
  ages <- age_classes(
    final_age_bin = final_age_bin,
    by = by
  )
  
  # Age structure of the Australian population by year of age, up to 100+
  # This is a "standard" distribution data frame but old population size data
  # from 2001 hence is adjusted later
  # aust_population_standard <- readxl::read_xls(
  #   path = "data/vaccinatinon/abs_standard_age_31010DO003_200106.xls",
  #   sheet = "Table_1",
  #   skip = 6,
  #   col_names = c(
  #     "age",
  #     "pop"
  #   )
  # ) %>%
  #   filter(age != "Total", !is.na(age), age != "© Commonwealth of Australia 2013") %>%
  #   mutate(
  #     age = case_when(
  #       age == "100 and over" ~ "100",
  #       TRUE ~ age
  #     ) %>%
  #       as.integer,
  #   )
  
  # use 2020 population, as this better matches proportion 80+
  aust_population_2020 <- readxl::read_xls(
    path = "data/vaccinatinon/abs_population_2020.xls",
    sheet = "Table_8",
    range = cell_rows(c(223:328)),
    col_names = c(
      "age",
      "NSW",
      "VIC",
      "QLD",
      "SA",
      "WA",
      "TAS",
      "NT",
      "ACT",
      "Aus"
    )
  ) %>%
    dplyr::select(-Aus) %>%
    mutate(
      age = case_when(
        age == "85-89" ~ "85",
        age == "90-94" ~ "85",
        age == "95-99" ~ "85",
        age == "100 and over" ~ "85",
        TRUE ~ age
      )
    ) %>%
    filter(
      !grepl("-", age)
    ) %>%
    pivot_longer(
      cols = -age,
      names_to = "state",
      values_to = "pop"
    ) %>%
    group_by(
      state,
      age
    ) %>%
    summarise(
      pop = sum(pop),
      .groups = "drop"
    ) %>%
    mutate(
      age = as.integer(age)
    ) %>%
    arrange(age)
  
  
  state_pops <- aust_population_2020 %>%
    group_by(state) %>%
    summarise(pop = sum(pop)) %>%
    mutate(state_pop = pop*population_total/sum(pop)) %>%
    dplyr::select(-pop)
  
  # aggregate into age classes and return
  age_class_fractions <- aust_population_2020 %>%
    mutate(
      age_class = cut(
        age,
        breaks = c(ages$lower - 1, Inf),
        labels = ages$classes
      ),
      age_class = as.character(age_class),
      age_class = factor(age_class, levels= unique(age_class))
    ) %>%
    group_by(
      state,
      age_class
    ) %>%
    summarise(
      pop = sum(pop)
    ) %>%
    left_join(
      y = state_pops,
      by = "state"
    ) %>%
    group_by(state) %>%
    mutate(
      fraction = pop / sum(pop),
      pop = fraction * state_pop
    ) %>%
    dplyr::select(-state_pop)
  
  return(age_class_fractions)
  
}





# over_80 <- c(
#   "80+",
#   "80-84",
#   "85+",
#   "85-89",
#   "90+",
#   "90-94",
#   "95+",
#   "95-99",
#   "100+"
# )
# 
# read_csv("~/not_synced/vaccination/2021-08-16-1559-tidy-not-clean-vaccine-rollout-ts.csv") %>%
#   rename(
#     age_class = age_group,
#     dose_number = dose
#   ) %>% 
#   mutate(
#     vaccine = case_when(
#       vaccine_type == "astra_zeneca" ~ "az",
#       vaccine_type == "pfizer_comirnaty" ~ "pf"
#     ),
#     state = case_when(
#       state == "act" ~ "ACT",
#       state == "nt"  ~ "NT",
#       state == "nsw" ~ "NSW",
#       state == "qld" ~ "QLD",
#       state == "sa"  ~ "SA",
#       state == "tas" ~ "TAS",
#       state == "vic" ~ "VIC",
#       state == "wa"  ~ "WA",
#       state == "unknown" ~ "unk"
#     ),
#     date_doses = ifelse(is.na(count), 0, count)
#   ) %>%
#   dplyr::select(state, date, age_class, vaccine, dose_number, date_doses) %>%
#   arrange(state, date, age_class, vaccine, dose_number) %>%
#   rowwise %>%
#   mutate(
#     age_class = case_when(
#       any(age_class == c(
#         "80+",
#         "80-84",
#         "85+",
#         "85-89",
#         "90+",
#         "90-94",
#         "95+",
#         "95-99",
#         "100+"
#       )) ~ "80+",
#       TRUE ~ age_class
#     )
#   ) %>%
#   group_by(state, date, age_class, vaccine, dose_number) %>%
#   summarise(date_doses = sum(date_doses)) %>%
#   arrange(date, age_class, vaccine, dose_number) %>%
#   group_by(date, age_class, vaccine, dose_number) %>%
#   mutate(
#    unknown = date_doses[7], # this is the unknown dose row,
#    aus_doses = sum(date_doses) - unknown,
#    date_doses_adjusted = date_doses + date_doses/aus_doses * unknown,
#    date_doses_adjusted = ifelse(is.nan(date_doses_adjusted), 0, date_doses_adjusted)
#   ) %>%
#   group_by(
#     state, date, age_class
#   )

age_distribution_state <- get_age_distribution_by_state()

vax_data <- load_vax_data()

dose_dates <- unique(vax_data$date)




dose_data <- vax_data %>%
  full_join(
    y = expand_grid(
      date = dose_dates,
      vaccine = c("az", "pf"),
      dose_number = 1:2,
      age_distribution_state
    )
  ) %>%
  mutate(doses = ifelse(is.na(doses), 0, doses)) %>%
  dplyr::select(-fraction) %>%
  arrange(state, age_class, vaccine, dose_number, date) %>%
  group_by(state, age_class, vaccine, dose_number) %>% 
  mutate(
    correction = slider::pslide_dbl(
      .l = list(
        date,
        doses,
        dose_number
      ),
      .f = immunity_lag_correction,
      .before = Inf
    ) %>%
      unlist,
    effective_doses = correction * doses
  ) %>%
  ungroup %>%
  group_by(state, age_class, date) %>%
  mutate(
    any_vaccine = sum(doses),
    effective_any_vaccine = sum(effective_doses),
  ) %>%
  ungroup %>%
  mutate(
    fraction = doses / any_vaccine,
    effective_fraction = effective_doses / effective_any_vaccine,
    coverage_any_vaccine = any_vaccine / pop,
    effective_coverage_any_vaccine = effective_any_vaccine / pop,
  ) %>%
  arrange(state, age_class, date) 

efficacy_data <- dose_data %>%
  pivot_wider(
    names_from = c(vaccine, dose_number),
    values_from = c(doses, correction, effective_doses, fraction, effective_fraction)
  ) %>%
  mutate(
    average_efficacy_transmission = average_efficacy(
      efficacy_az_2_dose = combine_efficacy(0.60, 0.65),
      efficacy_pf_2_dose = combine_efficacy(0.79, 0.65),
      efficacy_pf_1_dose = combine_efficacy(0.30, 0.46),
      efficacy_az_1_dose = combine_efficacy(0.18, 0.48),    
      proportion_pf_2_dose = fraction_pf_2,
      proportion_az_2_dose = fraction_az_2,
      proportion_pf_1_dose = fraction_pf_1,
      proportion_az_1_dose = fraction_az_1
    ),
    effective_average_efficacy_transmission = average_efficacy(
      efficacy_az_2_dose = combine_efficacy(0.60, 0.65),
      efficacy_pf_2_dose = combine_efficacy(0.79, 0.65),
      efficacy_pf_1_dose = combine_efficacy(0.30, 0.46),
      efficacy_az_1_dose = combine_efficacy(0.18, 0.48),    
      proportion_pf_2_dose = effective_fraction_pf_2,
      proportion_az_2_dose = effective_fraction_az_2,
      proportion_pf_1_dose = effective_fraction_pf_1,
      proportion_az_1_dose = effective_fraction_az_1
    ),
    average_efficacy_transmission = replace_na(average_efficacy_transmission, 0),
    effective_average_efficacy_transmission = replace_na(effective_average_efficacy_transmission, 0)
  ) %>%
  left_join(
    age_lookup,
    by = c("age_class" = "age_5y")
  )  %>%
  dplyr::select(
    -age
  ) %>%
  rename(
    age = age_class
  )

efficacy_data
efficacy_data %>% glimpse

vaccination_effect <- efficacy_data %>%
  group_by(
    state, date
  ) %>%
  summarise(
    vaccination_transmission_multiplier = vaccination_transmission_effect(
      age_coverage = coverage_any_vaccine,
      efficacy_mean = average_efficacy_transmission,
      next_generation_matrix = baseline_matrix()
    )$overall,
    effective_vaccination_transmission_multiplier = vaccination_transmission_effect(
      age_coverage = effective_coverage_any_vaccine,
      efficacy_mean = effective_average_efficacy_transmission,
      next_generation_matrix = baseline_matrix()
    )$overall,
    .groups = "drop"
  ) %>%
  mutate(
    vaccination_transmission_reduction_percent =
      100 * (1 - vaccination_transmission_multiplier),
    effective_vaccination_transmission_reduction_percent =
      100 * (1 - effective_vaccination_transmission_multiplier)
  ) %>%
  mutate(
    dubious = (date - min(date)) < 21,
    across(
      starts_with("effective_"),
      ~ ifelse(dubious, NA, .)
    )
  ) %>%
  select(
    -dubious
  )


earliest_effect_date <- min(vaccination_effect$date[which(!is.na(vaccination_effect$effective_vaccination_transmission_multiplier))])
last_effect_date    <- max(vaccination_effect$date[which(!is.na(vaccination_effect$effective_vaccination_transmission_multiplier))])

earliest_vaccination_effect <- vaccination_effect %>%
  filter(date == earliest_effect_date) %>% 
  dplyr::select(state, date, effective_vaccination_transmission_multiplier)

state_effect_ratios <- timeseries %>% 
  dplyr::select(date, overall_transmission_effect) %>% 
  inner_join(earliest_vaccination_effect) %>%
  mutate(
    multiplier = (1-effective_vaccination_transmission_multiplier)/(1-overall_transmission_effect)
  ) %>%
  dplyr::select(state, multiplier)

scaled_timeseries <- timeseries %>%
  dplyr::select(date, overall_transmission_effect) %>% 
  expand_grid(state_effect_ratios) %>%
  filter(date <= earliest_effect_date) %>%
  group_by(state) %>%
  mutate(
    datenum = as.numeric(date - min(date)),
    #scaled_effect = overall_transmission_effect - (overall_transmission_effect * (1-ratio)* datenum/  max(datenum))
    scaled_effect = 1 - (1 - overall_transmission_effect)*datenum/max(datenum)*multiplier
  ) %>% 
  dplyr::select(state, date, effect = scaled_effect)




interpolated_effect <- vaccination_effect %>%
  rename(effect = effective_vaccination_transmission_multiplier) %>%
  dplyr::select(state, date, effect) %>%
  filter(!is.na(effect)) %>%
  full_join(
    y = expand_grid(
      date = seq.Date(
          from = earliest_effect_date,
          to = last_effect_date,
          by = 1
        ),
      state = states
      ),
    by = c("state", "date")
  ) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(
    effect = ifelse(
      is.na(effect),
      approx(date, effect, date)$y,
      effect
    )
  )

vaccine_effect_timeseries <- bind_rows(
  scaled_timeseries %>%
    filter(date < earliest_effect_date),
  interpolated_effect
)


ggplot(vaccine_effect_timeseries) +
  geom_line(
    aes(
      x = date,
      y = effect,
      colour = state
    )
  )

saveRDS(
  vaccine_effect_timeseries,
  file = "output/vaccine_effect_timeseries.RDS"
)

  