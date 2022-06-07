# Immunity effect:
## combined effect of immunity from vaccination and infection from omicron variant

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

scenario_to_use <- lookups$scenario$scenario[grep("Realistic", lookups$scenario$booster_uptake)]

# this may fail if scenario lookup table is not up to date so check this is TRUE or will cause failure later
# otherwise may need to check email for appropriate scenario number and assign manually
scenario_to_use %in% unique(vaccine_raw$scenario)
#scenario_to_use <- 141

scenario_to_use

# aggregate to state
vaccine_state <- aggregate_quantium_vaccination_data_to_state(vaccine_raw) %>%
  filter(scenario == scenario_to_use)

vaccine_state

saveRDS(
  object = vaccine_state,
  file = sprintf(
    "outputs/vaccine_state_%s.RDS",
    data_date_save
  )
)

#vaccine_state <- readRDS("outputs/vaccine_state_20220419.RDS")

date_sequence <- seq.Date(
  from = as.Date("2021-02-22"),
  to = data_date + weeks(16),
  by = "1 week"
)#[63:66]

# calculate vaccine effects
ve_tables <- tibble(
  date = date_sequence
) %>%
  mutate(
    cohorts = map(
      .x = date,
      .f = get_vaccine_cohorts_at_date,
      vaccine_scenarios = vaccine_state
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


# expand timeseries to daily and calculate

# blank table of dates, states, variant
date_state_variant_table <- expand_grid(
  date = seq.Date(
    from = min(ve_tables$date),
    to = max(ve_tables$date),
    by = 1
  ),
  state = unique(ve_tables$vaccine_transmission_effects[[1]]$state),
  variant = unique(ve_tables$vaccine_transmission_effects[[1]]$variant)
)



vaccination_effect_timeseries <- ve_tables %>%
  select(date, vaccine_transmission_effects) %>%
  unnest(vaccine_transmission_effects) %>%
  filter(omicron_scenario == "estimate", scenario == scenario_to_use) %>%
  select(date, state, variant, vaccination_effect) %>%
  mutate(
    effect_multiplier = 1 - vaccination_effect
  ) %>%
  full_join(
    y = date_state_variant_table,
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
  vaccination_effect_timeseries,
  file = sprintf(
    "outputs/vaccination_effect_%s.csv",
    data_date_save
  )
)

write_csv(
  vaccination_effect_timeseries,
  file = "outputs/vaccination_effect.csv"
)

saveRDS(
  vaccination_effect_timeseries,
  file = sprintf(
    "outputs/vaccination_effect_%s.RDS",
    data_date_save
  )
)

saveRDS(
  vaccination_effect_timeseries,
  file = "outputs/vaccination_effect.RDS"
)

# vaccination effect plots --------
dpi <- 150
font_size <- 12


ve_ticks_labels <- split_ticks_and_labels(
  data = vaccination_effect_timeseries,
  tick_freq = "1 month",
  label_freq = "2 months",
  label_format = "%b %y"
)

vaccination_effect_timeseries %>%
  mutate(
    data_type = if_else(
      date <= data_date,
      "Actual",
      "Forecast"
    )
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = effect,
      colour = state,
      linetype = data_type,
      alpha = variant
    ),
    size = 1
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in transmission potential",
    col = "State",
    alpha = "Variant",
    linetype = "Data type"
  ) +
  scale_x_date(
    breaks = ve_ticks_labels$ticks,
    labels = ve_ticks_labels$labels
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
    legend.position = c(0.02, 0.25),
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
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_linetype_manual(values = c(1,3)) +
  scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  )

ggsave(
  filename = sprintf(
    "outputs/figures/vaccination_effect_%s.png",
    data_date_save
  ),
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2,
  bg = "white"
)



vaccination_effect_timeseries %>%
  mutate(
    data_type = if_else(
      date <= data_date,
      "Actual",
      "Forecast"
    )
  ) %>%
  group_by(state, variant) %>%
  mutate(
    delta_week = slider::slide(
      .x = -percent_reduction,
      .f = function(x){
        x[1] - x[7]
      },
      .before = 7
    ) %>%
      unlist
  ) %>%
  ggplot() +
  geom_line(
    aes(
      x = date,
      y = delta_week,
      col = state,
      linetype = data_type
    ),
    size = 1
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in percentage reduction of transmission potential",
    col = "State",
    linetype = "Data type"
  ) +
  scale_x_date(
    breaks = ve_ticks_labels$ticks,
    labels = ve_ticks_labels$labels
  ) +
  ggtitle(
    label = "Chanve in vaccination effect",
    subtitle = "Change in weekly average percentage reduction in transmission potential due to vaccination"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90),
    panel.spacing = unit(1.2, "lines")
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
  facet_wrap(~variant, ncol = 1) +
  geom_hline(
    aes(
      yintercept = 0
    ),
    linetype = "dotted"
  ) +
  scale_linetype_manual(values = c(1,3))


ggsave(
  filename = sprintf(
    "outputs/figures/vaccination_weekly_percent_change_in_tp_%s.png",
    data_date_save
  ),
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2,
  bg = "white"
)

# population-wide VE of the vaccinated population for Peter / Adeshina --------
coverage_fraction <- ve_tables %>%
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
  select(-coverage, -total_coverage) %>%
  arrange(scenario, state, date, age_band)

vaccinated_population_mean_ve <- ve_tables %>%
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
  mutate(
    weighted_ve = ve * fraction_coverage
  ) %>%
  group_by(date, scenario, state, omicron_scenario, variant, outcome) %>%
  summarise(population_mean_ve = sum(weighted_ve)) %>%
  ungroup %>%
  filter(
    scenario == scenario_to_use,
    omicron_scenario == "estimate",
    outcome %in% c("acquisition", "transmission", "symptoms")
  ) %>%
  select(-scenario, -omicron_scenario) %>%
  full_join(
    y = date_state_variant_table %>%
      expand_grid(outcome = c("acquisition", "transmission", "symptoms")),
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
  x = vaccinated_population_mean_ve,
  fil = sprintf(
    "outputs/vaccinated_population_mean_ve_%s.csv",
    data_date_save
  )
)

ggplot(vaccinated_population_mean_ve) +
  geom_line(
    aes(
      x = date,
      y = population_mean_ve,
      col = outcome,
      linetype = variant
    )
  ) +
  facet_wrap(~state)

## Immunity effect -------------




state_population <- vaccine_state %>%
  filter(scenario == max(scenario)) %>%
  group_by(state) %>%
  summarise(
    population = sum(num_people, na.rm = TRUE),
    .groups = "drop"
  )


local_cases <- read_csv("outputs/local_cases_input.csv") %>%
  select(
    date = date_onset,
    state,
    cases = count
  ) %>%
  filter(date <= data_date)

ascertainment_rates <- c(
  1,
  0.9,
  0.8,
  0.7,
  0.6,
  0.5,
  0.4,
  0.3#,
  #0.2
)

omicron_infections <- get_omicron_infections(
  local_cases,
  ascertainment_rates,
  state_population
)



ie_tables <- tibble(
  date = seq.Date(
    from = as.Date("2021-12-07"),
    to = data_date + weeks(16),
    by = "1 week"
  #)[22:25] - 1
  ) - 1
) %>%
  expand_grid(omicron_infections) %>%
  left_join(
    y = ve_tables,
    by = "date"
  ) %>%
  rename(cohorts_vaccination = cohorts) %>%
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
      .f = get_infection_efficacies_infection_only
    ),
    infection_transmission_effects = map2(
      .x = ies,
      .y = coverage_infection,
      .f = get_infection_transmission_effects
    ),
    vies = map2(
      .x = cohorts_vaccination,
      .y = cohorts_infection,
      .f = get_infection_efficacies_vax
    ),
    infection_vaccination_transmission_effects = map2(
      .x = vies,
      .y = coverage_infection,
      .f = get_infection_transmission_effects
    )
  )

combined_effect_tables <- ie_tables %>%
  rename(coverage_vaccination = coverage) %>%
  #filter(date < "2022-04-01" | ascertainment > 0.2) %>%
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
  filter(omicron_scenario == "estimate", scenario == scenario_to_use) %>%
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


combined_effect_timeseries_full <- vaccination_effect_timeseries %>%
  expand_grid(ascertainment = unique(combined_effect_timeseries$ascertainment)) %>%
  filter(date < min(combined_effect_timeseries$date)) %>%
  bind_rows(combined_effect_timeseries)


write_csv(
  combined_effect_timeseries,
  file = sprintf(
    "outputs/combined_effect_%s.csv",
    data_date_save
  )
)

write_csv(
  combined_effect_timeseries_full,
  file = sprintf(
    "outputs/combined_effect_full_%s.csv",
    data_date_save
  )
)

saveRDS(
  combined_effect_timeseries,
  file = sprintf(
    "outputs/combined_effect_%s.RDS",
    data_date_save
  )
)

saveRDS(
  combined_effect_timeseries_full,
  file = sprintf(
    "outputs/combined_effect_full_%s.RDS",
    data_date_save
  )
)

saveRDS(
  combined_effect_timeseries_full,
  file = "outputs/combined_effect_full.RDS"
)

combined_and_vax_timeseries <- bind_rows(
  combined_effect_timeseries %>%
    mutate(effect_type = "Combined immunity"),
  vaccination_effect_timeseries %>%
    mutate(
      ascertainment = NA,
      effect_type = "Vaccination immunity"
    )
)

# immunity effect plots ------

combined_effect_timeseries %>%
  filter(variant == "Omicron", date <= data_date) %>%
  mutate(ascertainment = as.character(ascertainment)) %>%
  ggplot() +
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
    #col = "State",
    colour = NULL,
    alpha = "Case\nascertainment\nproportion"
  ) +
  scale_x_date(
    breaks = ve_ticks_labels$ticks,
    labels = ve_ticks_labels$labels
  ) +
  ggtitle(
    label = "Immunity effect",
    subtitle = "Change in transmission potential of Omicron variant due to immunity from vaccination and infection with Omicron variant"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90, size = font_size),
    legend.position = c(0.0, 0.18),
    #legend.position = c(0.02, 0.18),
    legend.text = element_text(size = font_size-2),
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
    ),
  ) +
  guides(colour = "none") +
  scale_alpha_manual(values = unique(combined_effect_timeseries$ascertainment)) +
  scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  geom_vline(
    aes(
      xintercept = data_date
    )
  ) +
  facet_wrap(~state, ncol = 2) +
  geom_line(
    data = vaccination_effect_timeseries %>%
      filter(date <= data_date, variant == "Omicron"),
    aes(x = date, y = effect),
    linetype = "dashed"
  )


ggsave(
  filename = sprintf(
    "outputs/figures/combined_effect_long_%s.png",
    data_date_save
  ),
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2,
  bg = "white"
)


ie_short_labels <- split_ticks_and_labels(
  data = combined_effect_timeseries %>%
    filter(variant == "Omicron", date <= data_date) %>%
    mutate(ascertainment = as.character(ascertainment)),
  tick_freq = "1 week",
  label_freq = "2 week",
  label_format = "%d/%m"
)

combined_effect_timeseries %>%
  filter(variant == "Omicron", date <= data_date) %>%
  mutate(ascertainment = as.character(ascertainment)) %>%
  ggplot() +
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
    col = NULL,
    alpha = "Ascertainment\nproportion"
  ) +
  scale_x_date(
    breaks = ie_short_labels$ticks,
    labels = ie_short_labels$labels
  ) +
  ggtitle(
    label = "Immunity effect",
    subtitle = "Change in transmission potential of Omicron variant due to immunity from vaccination and infection with Omicron variant"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90, size = font_size),
    legend.position = c(0.0, 0.18),
    #legend.position = c(0.02, 0.18),
    legend.text = element_text(size = font_size-2),
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
  guides(colour = "none") +
  scale_alpha_manual(values = unique(combined_effect_timeseries$ascertainment)) +
  scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  geom_vline(
    aes(
      xintercept = data_date
    )
  ) +
  facet_wrap(~state, ncol = 2) +
  geom_line(
    data = vaccination_effect_timeseries %>%
      filter(date <= data_date, date >= "2021-12-07",  variant == "Omicron"),
    aes(x = date, y = effect),
    linetype = "dashed"
  )


ggsave(
  filename = sprintf(
    "outputs/figures/combined_effect_short_%s.png",
    data_date_save
  ),
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2,
  bg = "white"
)


## aggregated data
# 
# lapply(
#   X = date_sequence[1:2],
#   FUN = function(date, data){
#     state_pop <- data %>%
#       group_by(state, age_band) %>%
#       summarise(
#         pop = sum(num_people),
#         .groups = "drop"
#       )
#     
#     data %>%
#       group_by(
#         age_band,
#         state
#       ) %>%
#       mutate(
#         
#       )
#     
#   },
#   data = vaccine_state %>%
#     filter(scenario == scenario_to_use) %>%
#     select(-scenario)
# )
# 
# 


