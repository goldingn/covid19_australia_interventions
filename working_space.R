load_air_data <- function(
  data_dir = "~/not_synced/vaccination/vaccination_data_with_booster/"
){
  
  do_dir <- file.path(data_dir, "dose_ordering") 
  un_dir <- file.path(data_dir, "unknowns")
  
  do_dates <- list.files(
    path = do_dir
  ) %>%
    sub(
      pattern = "DohertyTimeseriesExport_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "DoseOrdering_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "_.*",
      replacement = "",
      x = .
    ) %>%
    as.Date
  
  un_dates <- list.files(
    path = un_dir
  ) %>%
    sub(
      pattern = "DohertyTimeseriesExport_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "Unknowns_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "_.*",
      replacement = "",
      x = .
    ) %>%
    as.Date
  
  if(max(do_dates) != max(un_dates)){
    stop("Most recent dose ordering and unknowns files have different dates")
  }
  
  do_index <- which.max(do_dates)
  un_index <- which.max(un_dates)
  
  extraction_date <- do_dates[do_index]
  
  do_path <- list.files(
    path = do_dir,
    full.names = TRUE
  )[do_index]
  
  un_path <-list.files(
    path = un_dir,
    full.names = TRUE
  )[un_index]
  
  do_raw <- read_csv(
    file = do_path
  ) %>%
    rename(
      "state" = PATIENT_MEDICARE_STATE
    )
  
  un_raw <- read_csv(
    file = un_path
  ) %>%
    rename(
      "state" = PROVIDER_STATE
    )
  
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
  
  df <- bind_rows(
    do_raw %>%
      filter(
        state != "Unknown",
        state != "UNK"
      ),
    un_raw
  ) %>%
    rename(
      "age_class" = CURRENT_AGE_GROUP,
      "week" = AS_OF_ENCOUNTER_WEEK,
      "date" = AS_OF_WEEK_COMMENCING,
      "dose1" = FIRST_DOSE,
      "dose2" = SECOND_DOSE,
      "dose3" = THIRD_DOSE,
      "count" = CUMULATIVE_UNIQUE_INDIVIDUALS_VACCINATED
    ) %>%
    select(-week) %>%
    mutate(
      date = as.Date(
        date,
        format = "%d/%m/%Y"
      ),
      age_class = case_when(
        age_class %in% over_80 ~ "80+",
        TRUE ~ age_class
      )
    )
  
  df_1014 <- df %>%
    filter(age_class == "12-15") %>%
    mutate(
      count = 0.75 * count,
      age_class = "10-14"
    )
  
  df_1519 <- df %>%
    filter(age_class == "12-15" | age_class == "16-19") %>%
    mutate(
      count = case_when(
        age_class == "12-15" ~ 0.25 * count,
        TRUE ~ count
      ),
      age_class = "15-19"
    ) 
  
  df2 <- bind_rows(
    df %>%
      filter(age_class != "12-15", age_class != "16-19"),
    df_1014,
    df_1519
  ) %>%
    group_by(
      state,
      age_class,
      date,
      dose1,
      dose2,
      dose3
    ) %>%
    summarise(
      count = sum(count),
      .groups = "drop"
    )
  
  
  if(
    any(
      any(!is.na(df2$dose3) & is.na(df2$dose2)),
      any(!is.na(df2$dose2) & is.na(df2$dose1))
    )
  ){
    stop("Dodgy schedules: at least one entry has had a subsequent dose without previous dose, e.g. dose 2 without dose 1")
  }
  
  age_distribution_state <- get_age_distribution_by_state()
  
  dose_dates <- unique(df2$date)
  
  df2 %$%
    expand_grid(
      age_distribution_state %>%
        dplyr::select(state, age_class),
      date = dose_dates,
      dose1 = unique(dose1),
      dose2 = unique(dose2),
      dose3 = unique(dose3)
    ) %>%
    mutate(
      legitimate_schedule = case_when(
        !is.na(dose3) & is.na(dose2) ~ FALSE,
        !is.na(dose2) & is.na(dose1) ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    filter(legitimate_schedule) %>%
    dplyr::select(-legitimate_schedule) %>%
    full_join(
      df2,
      by = c("state", "age_class", "date", "dose1", "dose2", "dose3")
    ) %>%
    mutate(
      count = ifelse(
        is.na(count),
        yes = 0,
        no = count
      )
    ) %>%
    arrange(
      state,
      age_class,
      date,
      dose1,
      dose2,
      dose3
    )
  
}

air_data <- load_air_data()


air_collapsed <- air_data %>%
  mutate(
    # Here assuming Moderna Spikevax equivalent to Pfizer Comirnaty so collapsing
    across(
      .cols = starts_with("dose"),
      ~ case_when(
        is.na(.) ~ NA_character_,
        . == "COVAST" ~ "az",
        TRUE ~ "pf"
      )
    ),
    # here assuming that in a mixed schedule you continue to get the level of efficacy
    # offered by the more effective vaccine from the point of receiving the more effective one
    dose2 = case_when(
      dose1 == "pf" & dose2 == "az" ~ "pf",
      TRUE ~ dose2
    ),
    dose3 = case_when(
      dose2 == "pf" & dose3 == "az" ~ "pf",
      TRUE ~ dose3
    )
  ) %>%
  group_by(
    state, age_class, date, dose1, dose2, dose3
  ) %>%
  summarise(
    count = sum(count),
    .groups = "drop"
  )


efficacy_az_1_dose <- combine_efficacy(0.46, 0.02)
efficacy_az_2_dose <- combine_efficacy(0.67, 0.36)
efficacy_pf_1_dose <- combine_efficacy(0.57, 0.13)
efficacy_pf_2_dose <- combine_efficacy(0.80, 0.65)

marginal_az_az <- efficacy_az_2_dose - efficacy_az_1_dose
marginal_pf_pf <- efficacy_pf_2_dose - efficacy_pf_1_dose
marginal_az_pf <- efficacy_pf_2_dose - efficacy_pf_2_dose

marginal_az_az_az <- 0
marginal_az_az_pf <- efficacy_pf_2_dose - efficacy_az_2_dose
marginal_pf_pf_pf <- 0

cumulative_first_dosed_individuals <- air_collapsed %>%
  group_by(
    state, age_class, date, dose1
  ) %>%
  summarise(
    count = sum(count),
    .groups = "drop"
  ) %>% 
  group_by(state, age_class, dose1) %>%
  mutate(
    correction = slider::pslide_dbl(
      .l = list(
        date = date,
        doses = count,
        dose_number = 1
      ),
      .f = immunity_lag_correction,
      .before = Inf
    ) %>%
      unlist,
    effective_count = correction * count,
    new = slider::slide_dbl(
      .x = count,
      .f = function(.x){
        if(length(.x) == 1){
          .x
        } else {
          .x[2]-.x[1]
        }
      },
      .before = 1
    )
  ) %>%
  arrange(state, age_class, dose1, date) %>%
  ungroup %>%
  mutate(
    marginal_effect = case_when(
      dose1 == "az" ~ efficacy_az_1_dose,
      dose1 == "pf" ~ efficacy_pf_1_dose
    ),
    effective_marginal_efficacy = effective_count * marginal_effect
  )

cumulative_second_dosed_individuals <- air_collapsed %>%
  filter(!is.na(dose2)) %>%
  group_by(
    state, age_class, date, dose1, dose2
  ) %>%
  summarise(
    count = sum(count),
    .groups = "drop"
  ) %>% 
  group_by(state, age_class, dose1, dose2) %>%
  mutate(
    correction = slider::pslide_dbl(
      .l = list(
        date,
        count,
        2
      ),
      .f = immunity_lag_correction,
      .before = Inf
    ) %>%
      unlist,
    effective_count= correction * count,
    new = slider::slide_dbl(
      .x = count,
      .f = function(.x){
        if(length(.x) == 1){
          .x
        } else {
          .x[2]-.x[1]
        }
      },
      .before = 1
    )
  ) %>%
  arrange(state, age_class, dose1, dose2, date) %>%
  ungroup %>%
  mutate(
    marginal_effect = case_when(
      dose1 == "az" & dose2 == "az" ~ marginal_az_az,
      dose1 == "az" & dose2 == "pf" ~ marginal_az_pf,
      dose1 == "pf" & dose2 == "pf" ~ marginal_pf_pf,
    ),
    effective_marginal_efficacy = effective_count * marginal_effect
  )

cumulative_third_dosed_individuals <- air_collapsed %>%
  filter(!is.na(dose3)) %>%
  group_by(
    state, age_class, date, dose1, dose2, dose3
  ) %>%
  summarise(
    count = sum(count),
    .groups = "drop"
  ) %>% 
  group_by(state, age_class, dose1, dose2, dose3) %>%
  mutate(
    correction = slider::pslide_dbl(
      .l = list(
        date,
        count,
        2
      ),
      .f = immunity_lag_correction,
      .before = Inf
    ) %>%
      unlist,
    effective_count = correction * count,
    new = slider::slide_dbl(
      .x = count,
      .f = function(.x){
        if(length(.x) == 1){
          .x
        } else {
          .x[2]-.x[1]
        }
      },
      .before = 1
    )
  ) %>%
  arrange(state, age_class, dose1, dose2, dose3, date) %>%
  ungroup %>%
  mutate(
    marginal_effect = case_when(
      dose2 == "az" & dose3 == "az" ~ marginal_az_az_az,
      dose2 == "az" & dose3 == "pf" ~ marginal_az_az_pf,
      dose2 == "pf" & dose3 == "pf" ~ marginal_pf_pf_pf,
    ),
    effective_marginal_efficacy = effective_count * marginal_effect
  )

age_distribution_state <- get_age_distribution_by_state()

cumulative_individuals_any_vaccine <- cumulative_first_dosed_individuals %>%
  group_by(state, age_class, date) %>%
  summarise(
    count = sum(count),
    effective_count = sum(effective_count),
    .groups = "drop"
  )

effective_dose_data <- bind_rows(
  first_dosed_individuals %>%
    select(state, age_class, date, effective_marginal_efficacy),
  second_dosed_individuals %>%
    select(state, age_class, date, effective_marginal_efficacy),
  third_dosed_individuals %>%
    select(state, age_class, date, effective_marginal_efficacy)
) %>%
  group_by(state, age_class, date) %>%
  summarise(
    effective_total_efficacy = sum(effective_marginal_efficacy),
    .groups = "drop"
  ) %>%
  full_join(
    age_distribution_state,
    by = c("state", "age_class")
  ) %>%
  dplyr::select(-fraction) %>%
  mutate(
    effective_average_efficacy_transmission = effective_total_efficacy/pop
  ) %>%
  left_join(
    cumulative_individuals_any_vaccine,
    by = c("state", "age_class", "date")
  )


vaccination_effect <- effective_dose_data %>%
  mutate(
    age_group = age_class %>%
      factor(
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80+"
        )
      ),
    .after = age_class
  ) %>%
  dplyr::select(-age_class) %>%
  rename(
    "effective_any_vaccine" = effective_count
  ) %>%
  mutate(
    effective_coverage_any_vaccine = effective_any_vaccine / pop
  ) %>%
  arrange(
    state, date, age_group
  ) %>%
  group_by(
    state, date
  ) %>%
  summarise(
    effective_vaccination_transmission_multiplier = vaccination_transmission_effect(
      age_coverage = effective_coverage_any_vaccine,
      efficacy_mean = effective_average_efficacy_transmission,
      next_generation_matrix = baseline_matrix()
    )$overall,
    .groups = "drop"
  ) %>%
  mutate(
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
  ) %>% 
  mutate(
    effective_vaccination_transmission_multiplier = ifelse(
      is.na(effective_vaccination_transmission_multiplier),
      1,
      effective_vaccination_transmission_multiplier
    ),
    effective_vaccination_transmission_reduction_percent = ifelse(
      is.na(effective_vaccination_transmission_reduction_percent),
      0,
      effective_vaccination_transmission_reduction_percent
    )
  )


vaccine_effect_timeseries <- bind_rows(
  vaccination_effect[1,],
  vaccination_effect %>%
    mutate(
      date = date + 6
    )
) %>%
  dplyr::select(
    state,
    date,
    effect = effective_vaccination_transmission_multiplier,
    percent_reduction = effective_vaccination_transmission_reduction_percent
  ) %>%
  full_join(
    y = expand_grid(
      date = seq.Date(
        from = min(dose_dates),
        to = max(dose_dates) + 6,
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
    ),
    percent_reduction = ifelse(
      is.na(percent_reduction),
      approx(date, percent_reduction, date)$y,
      percent_reduction
    )
  ) %>%
  ungroup

#### issue with existing method
tibble(
  date = rep(
    seq.Date(
      from = as.Date("2021-02-22"),
      to = as.Date("2021-03-29"),
      by = 7
    ),
    times = 2
  ),
  doses = c(10, 20, 20, 20, 10,  0,
            0,  0, 10, 20, 30, 40),
  dose_number = c(
    rep(1, 6),
    rep(2, 6)
  )
) %>%
  group_by(dose_number) %>%
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
  )
