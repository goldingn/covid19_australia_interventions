# Immunity effect

## combined effect of immunity from vaccination and infection from omicron variant

source("R/functions.R")

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

# reaad-in lookups
# object
lookups <- get_quantium_lookups(dir = dir)

# read in and label data
vaccine_raw <- read_quantium_vaccination_data()

# check scenarios and assign appropriate one for use
# currently only difference is 100% booster and 80% booster uptake in vaccinated
unique(vaccine_raw$scenario)

scenario_to_use <- max(vaccine_raw$scenario)
#scenario_to_use <- 141

# aggregate to state
vaccine_state <- aggregate_quantium_vaccination_data_to_state(vaccine_raw)

vaccine_state

saveRDS(
  object = vaccine_state,
  file = sprintf(
    "outputs/vaccine_state_%s.RDS",
    data_date
  )
)

# calculate vaccine effects
ve_tables <- tibble(
  date = seq.Date(
    from = as.Date("2021-02-22"),
    to = data_date + weeks(16),
    by = "1 week"
  )
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
    data_date
  )
)

saveRDS(
  vaccination_effect_timeseries,
  file = sprintf(
    "outputs/vaccination_effect_%s.RDS",
    data_date
  )
)

## vaccination effect plots --------
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
    #legend.position = c(0.02, 0.135),
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
  ) #+
  # geom_vline(
  #   aes(
  #     xintercept = data_date
  #   )
  # )

ggsave(
  filename = sprintf(
    "outputs/figures/vaccination_effect_%s.png",
    data_date
  ),
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2,
  bg = "white"
)

