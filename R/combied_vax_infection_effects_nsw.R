# combined vaccination and omicron resistance effect

source("R/functions.R")

vaccine_state <- readRDS("outputs/vaccine_state.RDS")
fitted_model <- readRDS("outputs/fitted_reff_model_20220215.RDS")
#ve_tables <- readRDS("outputs/ve_tables.RDS")

state_population <- vaccine_state %>%
  filter(scenario == max(scenario)) %>%
  group_by(state) %>%
  summarise(
    population = sum(num_people, na.rm = TRUE),
    .groups = "drop"
  )



infection_dates <- fitted_model$data$dates$infection

omicron_infections_only <- fitted_model$data$local$cases %>%
  as_tibble %>%
  mutate(
    date = infection_dates
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "state",
    values_to = "cases"
  ) %>%
  mutate(
    year = year(date) %>% as.integer,
    week = isoweek(date) %>% as.integer,
    month = month(date) %>% as.integer,
    yearweek = if_else(
      week == 52 & month == 1,
      sprintf("%s%02d", year - 1, week),
      sprintf("%s%02d", year, week)
    )
  ) %>%
  group_by(state, yearweek) %>%
  mutate(
    num_people = sum(cases)
  ) %>%
  filter(date == min(date)) %>%
  ungroup %>%
  mutate(
    num_people = if_else(
      date < "2021-12-01",
      0,
      num_people
    )
  ) %>%
  select(date, state, num_people) %>%
  expand_grid(
    ascertainment = c(0.25, 0.5, 0.75, 1)
  ) %>%
  mutate(
    num_people = num_people/ascertainment
  )


not_infected <- omicron_infections_only %>% 
  group_by(state, ascertainment) %>%
  summarise(
    num_people = sum(num_people)
  ) %>%
  left_join(
    state_population,
    by = "state"
  ) %>%
  mutate(
    num_people = population - num_people
  ) %>%
  mutate(
    date = NA_Date_
  ) %>%
  select(date, state, num_people, ascertainment)


omicron_infections <- bind_rows(
  omicron_infections_only,
  not_infected
) %>%
  nest(
    "omicron_infections" = -ascertainment
  )



#target_date <- as.Date("2022-01-17")

get_infection_cohorts_at_date <- function(infection_series, target_date) {
  
  # set future times to NA and collapse to get contemporary data
  infection_series %>%
    mutate(
      across(
        starts_with("date"),
        ~if_else(.x > target_date, as.Date(NA), .x)
      )
    ) %>%
    group_by(
      across(
        -num_people
      )
    ) %>%
    summarise(
      num_people = sum(num_people),
      .groups = "drop"
    ) %>%
    mutate(
      immunity = case_when(
        !is.na(date) ~ "omicron_infection",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      days_ago = as.numeric(target_date - date)
    )
  
}

# infection_cohort <- get_infection_cohorts_at_date(
#   omicron_infections,
#   target_date
# )
# 
# infection_cohort

get_coverage_infection <- function(infection_cohort) {
  
  # get current coverage with any dose in each age band, for each scenario
  coverage <- infection_cohort %>%
    mutate(
      immune = !is.na(immunity)
    ) %>%
    group_by(
      state
    ) %>%
    summarise(
      coverage = weighted.mean(immune, num_people),
      .groups = "drop"
    )
  
}

# coverage <- get_coverage_infection(infection_cohort)
# 
# coverage


get_infection_efficacies_vax <- function(vaccine_cohorts, param_file = "infection") {
  
  # load omicron parameters in wide format and subset to different parameter sets
  params_wide <- get_omicron_params_wide(param_file)
  
  neut_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      log10_mean_neut_AZ_dose_1,
      log10_mean_neut_AZ_dose_2,
      log10_mean_neut_Pfizer_dose_1,
      log10_mean_neut_Pfizer_dose_2,
      log10_mean_neut_mRNA_booster,
      log10_mean_neut_omicron_infection,
      additional_log10_mean_neut_omicron_infection,
      neut_decay
    )
  
  ve_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      starts_with("c50"),
      log_k,
      sd_log10_neut_titres,
      omicron_log10_neut_fold
    )
  
  # compute the average neutralisation level (mean log10 neut fold of WT
  # convalescent) in each age group, scenario, and omicron scenario
  mean_neuts <- vaccine_cohorts %>%
    filter(
      !is.na(immunity)
    ) %>%
    full_join(
      tibble(
        omicron_scenario = c(
          # "intermediate",
          # "optimistic",
          "estimate",
          "pessimistic"
        )
      ),
      by = character()
    ) %>%
    left_join(
      neut_params_wide,
      by = "omicron_scenario"
    ) %>%
    # compute the peak and waned log10 mean neuts for each cohort
    mutate(
      peak_neuts = case_when(
        immunity == "AZ_dose_1" ~ log10_mean_neut_AZ_dose_1,
        immunity == "AZ_dose_2" ~ log10_mean_neut_AZ_dose_2,
        immunity == "Pf_dose_1" ~ log10_mean_neut_Pfizer_dose_1,
        immunity == "Pf_dose_2" ~ log10_mean_neut_Pfizer_dose_2,
        immunity == "mRNA_booster" ~ log10_mean_neut_mRNA_booster,
        immunity == "omicron_infection" ~ log10_mean_neut_Pfizer_dose_2 + additional_log10_mean_neut_omicron_infection
      )
    ) %>%
    mutate(
      neuts = log10_neut_over_time(
        time = days_ago,
        maximum_log10_neut = peak_neuts,
        decay = neut_decay
      )
    ) %>%
    select(
      -starts_with("log10_mean_neut"),
      -peak_neuts,
      -neut_decay
    ) %>%
    # average the mean neuts over cohorts and scenarios
    group_by(
      state, omicron_scenario
    ) %>%
    summarise(
      neuts = weighted.mean(neuts, num_people),
      .groups = "drop"
    )
  
  # now compute VEs against each outcome, for Omicron and Delta
  ves <- mean_neuts %>%
    left_join(
      ve_params_wide,
      by = "omicron_scenario"
    ) %>%
    # for omicron, adjust down the neuts
    full_join(
      tibble(
        variant = c("Delta", "Omicron")
      ),
      by = character()
    ) %>%
    mutate(
      neuts = case_when(
        variant == "Omicron" ~ neuts + omicron_log10_neut_fold,
        TRUE ~ neuts
      )
    ) %>%
    # compute all the VEs in one shot with Gaussian integration
    pivot_longer(
      cols = starts_with("c50"),
      names_to = "outcome",
      values_to = "c50",
      names_prefix = "c50_"
    ) %>%
    mutate(
      ve = ve_from_mean_log10_neut(
        mean_log10_neut_vec = neuts,
        sd_log10_neut = sd_log10_neut_titres,
        log_k = log_k,
        c50_vec = c50,
        method = "gaussian"
      )
    ) %>%
    select(
      -neuts,
      -log_k,
      -sd_log10_neut_titres,
      -omicron_log10_neut_fold,
      -c50
    )
  
  ves
  
}


get_infection_efficacies_novax <- function(vaccine_cohorts, param_file = "infection") {
  
  # load omicron parameters in wide format and subset to different parameter sets
  params_wide <- get_omicron_params_wide(param_file)
  
  neut_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      log10_mean_neut_AZ_dose_1,
      log10_mean_neut_AZ_dose_2,
      log10_mean_neut_Pfizer_dose_1,
      log10_mean_neut_Pfizer_dose_2,
      log10_mean_neut_mRNA_booster,
      log10_mean_neut_omicron_infection,
      additional_log10_mean_neut_omicron_infection,
      neut_decay
    )
  
  ve_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      starts_with("c50"),
      log_k,
      sd_log10_neut_titres,
      omicron_log10_neut_fold
    )
  
  # compute the average neutralisation level (mean log10 neut fold of WT
  # convalescent) in each age group, scenario, and omicron scenario
  mean_neuts <- vaccine_cohorts %>%
    filter(
      !is.na(immunity)
    ) %>%
    full_join(
      tibble(
        omicron_scenario = c(
          # "intermediate",
          # "optimistic",
          "estimate",
          "pessimistic"
        )
      ),
      by = character()
    ) %>%
    left_join(
      neut_params_wide,
      by = "omicron_scenario"
    ) %>%
    # compute the peak and waned log10 mean neuts for each cohort
    mutate(
      peak_neuts = case_when(
        immunity == "AZ_dose_1" ~ log10_mean_neut_AZ_dose_1,
        immunity == "AZ_dose_2" ~ log10_mean_neut_AZ_dose_2,
        immunity == "Pf_dose_1" ~ log10_mean_neut_Pfizer_dose_1,
        immunity == "Pf_dose_2" ~ log10_mean_neut_Pfizer_dose_2,
        immunity == "mRNA_booster" ~ log10_mean_neut_mRNA_booster,
        immunity == "omicron_infection" ~ log10_mean_neut_omicron_infection
      )
    ) %>%
    mutate(
      neuts = log10_neut_over_time(
        time = days_ago,
        maximum_log10_neut = peak_neuts,
        decay = neut_decay
      )
    ) %>%
    select(
      -starts_with("log10_mean_neut"),
      -peak_neuts,
      -neut_decay
    ) %>%
    # average the mean neuts over cohorts and scenarios
    group_by(
      state, omicron_scenario
    ) %>%
    summarise(
      neuts = weighted.mean(neuts, num_people),
      .groups = "drop"
    )
  
  # now compute VEs against each outcome, for Omicron and Delta
  ves <- mean_neuts %>%
    left_join(
      ve_params_wide,
      by = "omicron_scenario"
    ) %>%
    # for omicron, adjust down the neuts
    full_join(
      tibble(
        variant = c("Delta", "Omicron")
      ),
      by = character()
    ) %>%
    mutate(
      neuts = case_when(
        variant == "Omicron" ~ neuts + omicron_log10_neut_fold,
        TRUE ~ neuts
      )
    ) %>%
    # compute all the VEs in one shot with Gaussian integration
    pivot_longer(
      cols = starts_with("c50"),
      names_to = "outcome",
      values_to = "c50",
      names_prefix = "c50_"
    ) %>%
    mutate(
      ve = ve_from_mean_log10_neut(
        mean_log10_neut_vec = neuts,
        sd_log10_neut = sd_log10_neut_titres,
        log_k = log_k,
        c50_vec = c50,
        method = "gaussian"
      )
    ) %>%
    select(
      -neuts,
      -log_k,
      -sd_log10_neut_titres,
      -omicron_log10_neut_fold,
      -c50
    )
  
  ves
  
}

# ves <- get_infection_efficacies_infection(infection_cohort)
# 
# 
# ves

get_infection_transmission_effects <- function(ves, coverage) {
  
  
  # load quantium lookup tables
  dir <- get_quantium_data_dir()
  lookups <- get_quantium_lookups(dir = dir)
  
  age_breaks_quantium <-lookups$age %>%
    mutate(
      brk = sub(
        pattern = "\\-.*",
        replacement = "",
        x = age_band
      ) %>%
        sub(
          pattern = "\\+",
          replacement = "",
          x = .
        ) %>%
        as.numeric
    ) %>%
    pull(brk)
  
  labq <- length(age_breaks_quantium)
  
  age_breaks_quantium[labq + 1] <- Inf
  
  # get a conmat NGM for Australia
  australia_ngm <- baseline_matrix(age_breaks = age_breaks_quantium)
  
  # combine coverage and VEs to get transmission reduction for each rollout
  # scenario, and omicron scenario
  ves %>%
    # add back in the younger age_groups
    expand_grid(
      age_band = lookups$age$age_band
    ) %>%
    # get the two transmission VEs as columns
    filter(
      outcome %in% c("acquisition", "transmission")
    ) %>%
    pivot_wider(
      names_from = outcome,
      values_from = ve
    ) %>%
    group_by(
      state,
      omicron_scenario,
      variant
    ) %>%
    arrange(
      age_band,
      .by_group = TRUE
    ) %>%
    # join on coverages
    left_join(
      coverage %>%
        expand_grid(
          age_band = lookups$age$age_band
        ),
      by = c("age_band", "state")
    ) %>%
    # compute percentage reduction in acquisition and transmission in each age group
    mutate(
      acquisition_multiplier = 1 - acquisition * coverage,
      transmission_multiplier = 1 - transmission * coverage,
    ) %>%
    select(
      -acquisition,
      -transmission,
      -coverage
    ) %>%
    # transform these into matrices of reduction in transmission, matching the NGM
    summarise(
      transmission_reduction_matrix =
        list(
          outer(
            # 'to' groups are on the rows in conmat, and first element in outer is rows,
            # so acquisition first
            acquisition_multiplier,
            transmission_multiplier,
            FUN = "*"
          )
        ),
      .groups = "drop"
    ) %>%
    group_by(
      state,
      omicron_scenario,
      variant
    ) %>%
    mutate(
      ngm_unvaccinated = list(australia_ngm),
      ngm_vaccinated = list(ngm_unvaccinated[[1]] * transmission_reduction_matrix[[1]]),
      infection_effect = 1 - get_R(ngm_vaccinated[[1]]) / get_R(ngm_unvaccinated[[1]])
    ) %>%
    select(
      -transmission_reduction_matrix,
      -ngm_unvaccinated,
      -ngm_vaccinated
    ) %>%
    ungroup
  
}


combine_transmission_effects <- function(
  ves,
  coverage_vaccination,
  ies,
  coverage_infection,
  vies
) {
  
  # load quantium lookup tables
  dir <- get_quantium_data_dir()
  lookups <- get_quantium_lookups(dir = dir)
  
  age_breaks_quantium <-lookups$age %>%
    mutate(
      brk = sub(
        pattern = "\\-.*",
        replacement = "",
        x = age_band
      ) %>%
        sub(
          pattern = "\\+",
          replacement = "",
          x = .
        ) %>%
        as.numeric
    ) %>%
    pull(brk)
  
  labq <- length(age_breaks_quantium)
  
  age_breaks_quantium[labq + 1] <- Inf
  
  # get a conmat NGM for Australia
  australia_ngm <- baseline_matrix(age_breaks = age_breaks_quantium)
  
  # combine coverage and VEs to get transmission reduction for each rollout
  # scenario, and omicron scenario
  ves %>%
    # add back in the younger age_groups
    complete(
      scenario,
      state,
      omicron_scenario,
      variant,
      outcome,
      age_band = unique(lookups$age$age_band),
      fill = list(
        ve = 0
      )
    ) %>%
    rename(effect_vaccination = ve) %>%
    left_join(
      ies %>% rename(effect_infection = ve)
    ) %>%
    left_join(
      vies %>% rename(effect_infandvax = ve)
    ) %>%
    # get the two transmission VEs as columns
    filter(
      outcome %in% c("acquisition", "transmission")
    ) %>%
    pivot_longer(
      cols = starts_with("effect_"),
      names_prefix = "effect_",
      names_to = "effect_type",
      values_to = "ve"
    ) %>%
    pivot_wider(
      names_from = c(outcome, effect_type),
      values_from = ve
    ) %>%
    group_by(
      scenario,
      state,
      omicron_scenario,
      variant
    ) %>%
    arrange(
      age_band,
      .by_group = TRUE
    ) %>%
    # join on coverages
    left_join(
      coverage_vaccination %>%
        rename(coverage_vaccination = coverage),
      by = c("age_band", "state", "scenario")
    ) %>%
    left_join(
      coverage_infection %>%
        rename(coverage_infection = coverage) %>%
        expand_grid(
          age_band = lookups$age$age_band
        ),
      by = c("age_band", "state")
    ) %>%
    # compute percentage reduction in acquisition and transmission in each age group
    mutate(
      p_infected_only = coverage_infection * (1 - coverage_vaccination),
      p_vaccinated_only = coverage_vaccination * (1 - coverage_infection),
      p_infandvax = coverage_vaccination * coverage_infection,
      weighted_acquisition = p_infected_only * acquisition_infection + p_vaccinated_only * acquisition_vaccination + p_infandvax * acquisition_infandvax,
      weighted_transmission = p_infected_only * transmission_infection + p_vaccinated_only * transmission_vaccination + p_infandvax * transmission_infandvax,
      acquisition_multiplier = 1 - weighted_acquisition,
      transmission_multiplier = 1 - weighted_transmission
    ) %>%
    # select(
    #   -acquisition,
    #   -transmission,
    #   -coverage
    # ) %>%
    # transform these into matrices of reduction in transmission, matching the NGM
    summarise(
      transmission_reduction_matrix =
        list(
          outer(
            # 'to' groups are on the rows in conmat, and first element in outer is rows,
            # so acquisition first
            acquisition_multiplier,
            transmission_multiplier,
            FUN = "*"
          )
        ),
      .groups = "drop"
    ) %>%
    group_by(
      state,
      omicron_scenario,
      variant
    ) %>%
    mutate(
      ngm_unvaccinated = list(australia_ngm),
      ngm_vaccinated = list(ngm_unvaccinated[[1]] * transmission_reduction_matrix[[1]]),
      combined_effect = 1 - get_R(ngm_vaccinated[[1]]) / get_R(ngm_unvaccinated[[1]])
    ) %>%
    select(
      -transmission_reduction_matrix,
      -ngm_unvaccinated,
      -ngm_vaccinated
    ) %>%
    ungroup
  
}
#vaccine_effect <- get_infection_transmission_effects(ves, coverage)

ie_tables <- tibble(
  date = seq.Date(
    from = as.Date("2021-12-07"),
    to = data_date + weeks(16),
    by = "1 week"
  )
) %>%
  expand_grid(omicron_infections) %>%
  mutate(
    cohorts_infection = map2(
      .x = omicron_infections,
      .y = date,
      .f = get_infection_cohorts_at_date
    ),
    coverage_infection = map(
      .x = cohorts_infection,
      .f = get_coverage_infection
    ),
    ies = map(
      .x = cohorts_infection,
      .f = get_infection_efficacies_novax
    ),
    infection_transmission_effects = map2(
      .x = ies,
      .y = coverage_infection,
      .f = get_infection_transmission_effects
    ),
    vies = map(
      .x = cohorts_infection,
      .f = get_infection_efficacies_vax
    ),
    infection_vaccination_transmission_effects = map2(
      .x = vies,
      .y = coverage_infection,
      .f = get_infection_transmission_effects
    )
  )



combined_effect_tables <- left_join(
  ie_tables,
  ve_tables %>%
    rename(
      cohorts_vaccination = cohorts,
      coverage_vaccination = coverage
    ) %>%
    mutate(date = date + 1),
  by = "date"
) %>%
  mutate(
    combined_transmission_effects = pmap(
      .l = list(
        ves = ves,
        coverage_vaccination = coverage_vaccination,
        ies = ies,
        coverage_infection = coverage_infection,
        vies = vies
      ),
      .f = combine_transmission_effects
    )
  )

# ves <- combined_effect_tables$ves[[1]]
# ies <- combined_effect_tables$ies[[1]]
# coverage_infection <- combined_effect_tables$coverage_infection[[1]]
# coverage_vaccination <- combined_effect_tables$coverage_vaccination[[1]]
# vies <- combined_effect_tables$vies[[1]]

date_state_variant_table_infection <- expand_grid(
  date = seq.Date(
    from = min(ie_tables$date),
    to = max(ie_tables$date),
    by = 1
  ),
  state = unique(ie_tables$infection_transmission_effects[[1]]$state),
  variant = unique(ie_tables$infection_transmission_effects[[1]]$variant),
  ascertainment = unique(ie_tables$ascertainment)
)


combined_effect_timeseries <- combined_effect_tables %>%
  select(date, ascertainment, combined_transmission_effects) %>%
  unnest(combined_transmission_effects) %>%
  filter(omicron_scenario == "estimate", scenario == 129) %>%
  select(date, ascertainment, state, variant, combined_effect) %>%
  mutate(
    effect_multiplier = 1 - combined_effect
  ) %>%
  full_join(
    y = date_state_variant_table_infection,
    by = c("state", "date", "variant", "ascertainment")
  ) %>%
  arrange(state, variant, ascertainment, date) %>%
  group_by(state, variant, ascertainment) %>%
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
  select(-combined_effect)

data_date <- fitted_model$data$dates$linelist

write_csv(
  combined_effect_timeseries,
  file = sprintf(
    "outputs/combined_effect_%s.csv",
    data_date
  )
)

saveRDS(
  combined_effect_timeseries,
  file = sprintf(
    "outputs/combined_effect_%s.RDS",
    data_date
  )
)

## plot vaccine_effect_timeseries


dpi <- 150
font_size <- 12
ggplot(combined_effect_timeseries %>% filter(variant == "Omicron") %>% mutate(ascertainment = as.character(ascertainment))) +
  geom_line(
    aes(
      x = date,
      y = effect,
      colour = state,
      #linetype = variant,
      alpha = ascertainment
    ),
    size = 1
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in transmission potential",
    col = "State",
    alpha = "Variant"
  ) +
  scale_x_date(
    breaks = "1 week",
    date_labels = "%d/%m"
  ) +
  ggtitle(
    label = "Vaccination effect",
    subtitle = "Change in transmission potential due to vaccination"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90, size = font_size),
    legend.position = c(0.02, 0.135),
    #legend.position = c(0.02, 0.18),
    legend.text = element_text(size = font_size),
    axis.text = element_text(size = font_size),
    plot.title = element_text(size = font_size + 8),
    plot.subtitle = element_text(size = font_size)
  ) +
  scale_colour_manual(
    values = c(
      "darkgray",
      "cornflowerblue",
      "chocolate1",
      "violetred4",
      "red1",
      "darkgreen",
      "darkblue",
      "gold1"
    )
  ) +
  scale_alpha_manual(values = c(0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  geom_vline(
    aes(
      xintercept = data_date
    )
  )

ggsave(
  filename = sprintf(
    "outputs/figures/combined_effect_%s.png",
    data_date
  ),
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2,
  bg = "white"
)

combined_effect_timeseries %>%
  filter(variant == "Omicron", state == "NSW", date <= data_date) %>%
  mutate(
    ascertainment = ascertainment * 100,
    ascertainment = factor(ascertainment, levels = c("100", "75", "50", "25"))
  ) %>%
ggplot() +
  geom_line(
    aes(
      x = date,
      y = effect,
      #colour = state,
      #linetype = variant,
      alpha = ascertainment
    ),
    colour = "cornflowerblue",
    size = 1.5
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in transmission potential",
    #col = "State",
    alpha = "Case\nascertainment\npercentage"
  ) +
  scale_x_date(
    breaks = "1 week",
    date_labels = "%d/%m"
  ) +
  ggtitle(
    label = "Combined vaccination and infection effect in NSW",
    subtitle = "Change in transmission potential of Omicron variant due to vaccination and infection from the Omicron variant"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90, size = font_size),
    legend.position = c(0.02, 0.135),
    #legend.position = c(0.02, 0.18),
    legend.text = element_text(size = font_size),
    axis.text = element_text(size = font_size),
    plot.title = element_text(size = font_size + 8),
    plot.subtitle = element_text(size = font_size)
  ) +
  # scale_colour_manual(
  #   values = c(
  #     "darkgray",
  #     "cornflowerblue",
  #     "chocolate1",
  #     "violetred4",
  #     "red1",
  #     "darkgreen",
  #     "darkblue",
  #     "gold1"
  #   )
  # ) +
  scale_alpha_manual(values = c(1, 0.75, 0.5, 0.25)) +
  scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  geom_vline(
    aes(
      xintercept = data_date
    )
  )

ggsave(
  filename = sprintf(
    "outputs/figures/combined_effect_nsw_actual_%s.png",
    data_date
  ),
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2,
  bg = "white"
)

combined_effect_timeseries %>%
  filter(variant == "Omicron", state == "NSW") %>%
  mutate(
    ascertainment = ascertainment * 100,
    ascertainment = factor(ascertainment, levels = c("100", "75", "50", "25")),
    effect_type = if_else(date > data_date, "Forecast vaccination only", "Actual vaccination & Omicron infection")
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = effect,
      #colour = state,
      linetype = effect_type,
      alpha = ascertainment
    ),
    colour = "cornflowerblue",
    size = 1.5
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in transmission potential",
    #col = "State",
    alpha = "Case ascertainment percentage",
    linetype = "Effect from"
  ) +
  scale_x_date(
    breaks = "1 month",
    date_labels = "%b %y"
  ) +
  ggtitle(
    label = "Combined vaccination and infection effect in NSW",
    subtitle = "Change in transmission potential of Omicron variant due to vaccination and infection from the Omicron variant"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90, size = font_size),
    #legend.position = c(0.02, 0.135),
    legend.position = c(0.02, 0.18),
    legend.text = element_text(size = font_size),
    axis.text = element_text(size = font_size),
    plot.title = element_text(size = font_size + 8),
    plot.subtitle = element_text(size = font_size)
  ) +
scale_alpha_manual(values = c(1, 0.75, 0.5, 0.25)) +
  scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  scale_linetype_manual(values = c(1,3)) +
  geom_vline(
    aes(
      xintercept = data_date
    )
  )

ggsave(
  filename = sprintf(
    "outputs/figures/combined_effect_nsw_future_%s.png",
    data_date
  ),
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2,
  bg = "white"
)


combined_effect_timeseries %>%
  filter(variant == "Omicron", state == "NSW") %>%
  mutate(
    ascertainment = ascertainment * 100,
    ascertainment = factor(ascertainment, levels = c("100", "75", "50", "25")),
    effect_type = if_else(date > data_date, "Forecast vaccination only", "Actual vaccination & Omicron infection")
  ) %>%
  write_csv(file = "outputs/nsw_omicron_combined_effect_20220215.csv")
    