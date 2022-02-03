source("R/functions.R")


dir <- get_quantium_data_dir()
lookups <- get_quantium_lookups(dir = dir)

vaccine_raw <- read_quantium_vaccination_data()

vaccine_state <- aggregate_quantium_vaccination_data_to_state(vaccine_raw)

vaccine_state


# target_date <- as.Date("2022-01-01")
# vaccine_cohorts_now <- get_vaccine_cohorts_at_date(
#   vaccine_scenarios = vaccine_state,
#   target_date = target_date
# )

#vaccine_cohorts_now_all <- add_missing_age_cohorts(vaccine_cohorts_now)

#coverage_now_all <- get_coverage(vaccine_cohorts_now_all)



#ves_now_all <- get_vaccine_efficacies(vaccine_cohorts_now_all)


# vaccine_transmission_effects_now <- get_vaccine_transmission_effects(
#   ves = ves_now_all,
#   coverage = coverage_now_all
# )

ve_tables <- tibble(
  date = seq.Date(
    from = as.Date("2021-02-22"),
    to = Sys.Date() + weeks(7),
    by = "1 week"
  )
) %>%
  #filter(date < "2021-03-02") %>%
  mutate(
    cohorts = map(
      .x = date,
      .f = get_vaccine_cohorts_at_date,
      vaccine_scenarios = vaccine_state
    ),
    # cohorts_all = map(
    #   .x = cohorts,
    #   .f = add_missing_age_cohorts
    # ),
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


date_state_variant_table <- expand_grid(
  date = seq.Date(
    from = min(ve_tables$date),
    to = max(ve_tables$date),
    by = 1
  ),
  state = unique(ve_tables$vaccine_transmission_effects[[1]]$state),
  variant = unique(ve_tables$vaccine_transmission_effects[[1]]$variant)
)

ve_waning <- ve_tables %>%
  select(date, vaccine_transmission_effects) %>%
  unnest(vaccine_transmission_effects) %>%
  #filter(omicron_scenario == "intermediate", scenario == "81") %>%
  filter(omicron_scenario == "estimate", scenario == "109") %>%
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
  ve_waning,
  file = "outputs/vaccine_effect_timeseries_2022-01-28.csv"
)

saveRDS(
  ve_waning,
  file = "outputs/vaccine_effect_timeseries.RDS"
)

ve_old <- read_csv("outputs/vaccine_effect_timeseries_2022-01-23.csv")

ve_compare <- bind_rows(
  ve_old %>%
    select(-percent_reduction) %>%
    mutate(
      variant = "Delta",
      estimate = "Old"
    ),
  ve_waning %>%
    select(state, date, effect = effect_multiplier, variant) %>%
    mutate(estimate = "New")
)

ve_waning_old <- read_csv("outputs/vaccine_effect_timeseries_2022-01-28.csv")

vew <- bind_rows(
  ve_waning %>%
    mutate(estimate = "new"),
  ve_waning_old %>%
    mutate(estimate = "old")
)

dpi <- 150
font_size <- 12
ggplot(ve_waning) +
 geom_line(
  aes(
    x = date,
    y = effect,
    colour = state,
    linetype = variant#,
    #alpha = estimate
  ),
  size = 1.5
) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in transmission potential",
    col = "State",
    lty = "Variant"
  ) +
  scale_x_date(
    breaks = "1 month",
    date_labels = "%b %Y"
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
    #legend.position = c(0.02, 0.135),
    legend.position = c(0.02, 0.18),
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
  scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  scale_alpha_manual(values = c(1, 0.6)) +
  geom_vline(
    aes(
      xintercept = Sys.Date()
    )
  ) 

ggsave(
  filename = "outputs/figures/vaccination_effect.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2,
  bg = "white"
)


write_csv(
  ve_waning,
  file = "outputs/vaccine_effect_timeseries_2022-02-02.csv"
)


saveRDS(
  ve_waning,
  file = "outputs/vaccine_effect_timeseries_2022-02-02.RDS"
)


ve_waning %>%
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
    ),
    size = 1
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in percentage reduction of transmission potential",
    col = "State"
  ) +
  scale_x_date(
    breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  ggtitle(
    label = "Vaccination effect",
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
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = Sys.Date()))


ggsave(
  filename = "outputs/figures/vaccination_weekly_percent_change_in_tp.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)



# population-wide VE for Peter / Adeshina

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

population_mean_ve <- ve_tables %>%
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
  summarise(population_mean_ve = sum(weighted_ve))

daily_population_mean_ve <- population_mean_ve %>%
  ungroup %>%
  filter(
    scenario == 109,
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
  x = daily_population_mean_ve,
  fil = "outputs/daily_population_mean_transmission_acquisition.csv"
)

ggplot(daily_population_mean_ve) +
  geom_line(
    aes(
      x = date,
      y = population_mean_ve,
      col = outcome,
      linetype = variant
    )
  ) +
  facet_wrap(~state)
