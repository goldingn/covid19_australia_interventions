source("R/functions.R")
library(gaussquad)


dir <- get_quantium_data_dir()
lookups <- get_quantium_lookups(dir = dir)

vaccine_raw <- read_quantium_vaccination_data()
# 
# vaccine_state <- aggregate_quantium_vaccination_data_to_state(vaccine_raw)
# 
# vaccine_state
# 
vaccine_sa4_nsw <- vaccine_raw %>%
  left_join(
    state_short_long_table,
    by = c("STE_NAME16" = "state_long")
  ) %>%
  filter(state_short == "NSW") %>%
  select(
    scenario,
    state = SA4_NAME16,
    age_band,
    vaccine,
    vaccine_booster,
    date_dose_1,
    date_dose_2,
    date_booster,
    num_people
  )
# 
# target_date <- as.Date("2022-01-01")
# vaccine_cohorts_now <- get_vaccine_cohorts_at_date(
#   vaccine_scenarios = vaccine_sa4_nsw,
#   target_date = target_date
# )
# 
# vaccine_cohorts_now_all <- add_missing_age_cohorts_nsw_sa4(vaccine_cohorts_now)
# 
# coverage_now_all <- get_coverage(vaccine_cohorts_now_all)
# 
# 
# 
# ves_now_all <- get_vaccine_efficacies(vaccine_cohorts_now_all)


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
      vaccine_scenarios = vaccine_sa4_nsw
    ),
    cohorts_all = map(
      .x = cohorts,
      .f = add_missing_age_cohorts_nsw_sa4
    ),
    coverage = map(
      .x = cohorts_all,
      .f = get_coverage
    ),
    ves = map(
      .x = cohorts_all,
      .f = get_vaccine_efficacies
    ),
    vaccine_transmission_effects = map2(
      .x = ves,
      .y = coverage,
      .f = get_vaccine_transmission_effects
    )
  )

ve_tables_plus_transmission <- ve_tables %>%
  mutate(
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
  state = unique(ve_tables$ves[[1]]$state),
  variant = unique(ve_tables$ves[[1]]$variant)
)

ve_waning <- ve_tables_plus_transmission %>%
  select(date, vaccine_transmission_effects) %>%
  unnest(vaccine_transmission_effects) %>%
  #filter(omicron_scenario == "intermediate", scenario == "81") %>%
  filter(omicron_scenario == "estimate", scenario == "82") %>%
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

# write_csv(
#   ve_waning,
#   file = "outputs/vaccine_effect_timeseries.csv"
# )

saveRDS(
  ve_tables,
  file = "outputs/ve_tables_nsw_sa4.RDS"
)


# 
# dpi <- 150
# font_size <- 12
# ggplot(ve_compare) +
#  geom_line(
#   aes(
#     x = date,
#     y = effect,
#     colour = state,
#     linetype = variant,
#     alpha = estimate
#   ),
#   size = 1.5
# ) +
#   theme_classic() +
#   labs(
#     x = NULL,
#     y = "Change in transmission potential",
#     col = "State"
#   ) +
#   scale_x_date(
#     breaks = "1 month",
#     date_labels = "%b %Y"
#   ) +
#   ggtitle(
#     label = "Vaccination effect",
#     subtitle = "Change in transmission potential due to vaccination"
#   ) +
#   cowplot::theme_cowplot() +
#   cowplot::panel_border(remove = TRUE) +
#   theme(
#     strip.background = element_blank(),
#     axis.title.y.right = element_text(vjust = 0.5, angle = 90, size = font_size),
#     #legend.position = c(0.02, 0.135),
#     legend.position = c(0.02, 0.25),
#     legend.text = element_text(size = font_size),
#     axis.text = element_text(size = font_size),
#     plot.title = element_text(size = font_size + 8),
#     plot.subtitle = element_text(size = font_size)
#   ) +
#   scale_colour_manual(
#     values = c(
#       "darkgray",
#       "cornflowerblue",
#       "chocolate1",
#       "violetred4",
#       "red1",
#       "darkgreen",
#       "darkblue",
#       "gold1"
#     )
#   ) +
#   scale_y_continuous(
#     position = "right",
#     limits = c(0, 1),
#     breaks = seq(0, 1, by = 0.1)
#   ) +
#   scale_alpha_manual(values = c(1, 0.6)) +
#   geom_vline(
#     aes(
#       xintercept = Sys.Date()
#     )
#   )
# 
# ggsave(
#   filename = "outputs/figures/vaccination_effect_comparison.png",
#   dpi = dpi,
#   width = 1500 / dpi,
#   height = 1250 / dpi,
#   scale = 1.2,
#   bg = "white"
# )
# 
# 
# write_csv(
#   ve_compare,
#   file = "outputs/ve_timeseries_comparison.csv"
# )


# population-wide VE by SA4

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
  #select(-coverage, -total_coverage) %>%
  arrange(scenario, state, date, age_band)

ve_tables_expanded <- ve_tables %>%
  mutate(
    cohorts_all_pop = map(
      .x = cohorts,
      .f = function(vaccine_cohorts){
        dir <- get_quantium_data_dir(date = NULL)
        lookups <- get_quantium_lookups(dir = dir)
        
        state_age_fractions <- lookups$age %>%
          select(age_band) %>%
          mutate(
            lower = sub(
              pattern = "\\-.*",
              replacement = "",
              x = age_band
            ) %>%
              sub(
                pattern = "\\+",
                replacement = "",
                x = .
              ) %>%
              as.numeric,
            upper = sub(
              pattern = ".*\\-",
              replacement = "",
              x = age_band
            ) %>%
              sub(
                pattern = ".*\\+",
                replacement = "Inf",
                x = .
              ) %>%
              as.numeric
          ) %>%
          rename(classes = age_band) %>%
          get_age_distribution_by_state(ages = .) %>%
          ungroup %>%
          filter(state == "NSW") %>%
          select(-state)
        
        # add in 0-4 yo cohort which is missing
        age_class_state_scenario <- expand_grid(
          state_age_fractions %>%
            select(age_class),
          state = unique(vaccine_cohorts$state),
          scenario = unique(vaccine_cohorts$scenario)
        )
        
        vaccine_cohorts_all <- age_class_state_scenario %>%
          full_join(
            y = vaccine_cohorts,
            by = c("scenario", "state", "age_class" = "age_band")
          ) %>%
          left_join(
            state_age_fractions,
            by = c("age_class")
          ) %>%
          group_by(
            state, scenario
          ) %>%
          mutate(
            qspop = sum(num_people, na.rm = TRUE),
            qpop = qspop / (1 - fraction),
            num_people = if_else(
              age_class == "0-4",
              fraction * qpop,
              num_people
            )
          ) %>%
          rename(age_band = age_class) %>%
          select(
            scenario, state, age_band, fraction
          )
        
        return(vaccine_cohorts_all)
      }
    )
  )


population_fraction <- ve_tables_expanded  %>%
  select(date, cohorts_all_pop) %>%
  unnest(cohorts_all_pop) %>%
  rename(age_fraction = fraction) %>%
  distinct %>%
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
  left_join(
    population_fraction,
    by = c("date", "scenario", "state", "age_band")
  ) %>%
  mutate(
    weighted_ve = ve * coverage * age_fraction
  ) %>%
  group_by(date, scenario, state, omicron_scenario, variant, outcome) %>%
  summarise(population_mean_ve = sum(weighted_ve)) %>%
  ungroup


age_class_mean_ve <- ve_tables %>%
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
  left_join(
    population_fraction,
    by = c("date", "scenario", "state", "age_band")
  ) %>%
  mutate(
    weighted_ve = ve * coverage
  )

daily_population_mean_ve <- population_mean_ve %>%
  ungroup %>%
  filter(
    scenario == 82,
    omicron_scenario == "estimate"#,
    #outcome %in% c("acquisition", "transmission", "symptoms")
  ) %>%
  select(-scenario, -omicron_scenario) %>%
  full_join(
    y = date_state_variant_table %>%
      expand_grid(outcome = unique(population_mean_ve$outcome)),
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
  file = "outputs/daily_population_mean_ves_nsw_sa4.csv"
)


write_csv(
  x = ve_waning,
  file = "outputs/nsw_sa4_tp_ve_multiplier.csv"
)

dpi <- 150

for (i in 1:3){
  
  if (i == 1){
    x <- 1:10
  } else if (i == 2) {
    x <- 11:20
  } else if (i == 3) {
    x <- 21:28
  }
  
  ggplot(
    daily_population_mean_ve %>%
      filter(state == unique(.$state)[x])
  ) +
    geom_line(
      aes(
        x = date,
        y = population_mean_ve,
        col = outcome,
        linetype = variant
      )
    ) +
    geom_vline(
      aes(xintercept = Sys.Date())
    ) +
    facet_wrap(~state, ncol = 2) +
    ylim(0, 1) +
    scale_x_date(minor_breaks = "month") +
    ylab("Population mean vaccine efficacy")
  
  ggsave(
    filename = sprintf(
      "outputs/figures/nsw_sa4_ve/nsw_sa4_ve_%s.png",
      i
    ),
    dpi = dpi,
    width = 1500 / dpi,
    height = 1250 / dpi,
    scale = 1.2,
    bg = "white"
  )
}

for (i in 1:3){
  
  if (i == 1){
    x <- 1:10
  } else if (i == 2) {
    x <- 11:20
  } else if (i == 3) {
    x <- 21:28
  }
  
  ggplot(
    daily_population_mean_ve %>%
      filter(
        state == unique(.$state)[x],
        variant == "Omicron",
        outcome == "transmission"
      )
  ) +
    geom_line(
      aes(
        x = date,
        y = population_mean_ve,
        col = outcome,
        linetype = variant
      )
    ) +
    geom_vline(
      aes(xintercept = Sys.Date())
    ) +
    facet_wrap(~state, ncol = 2) +
    scale_x_date(minor_breaks = "month") +
    ylab("Population mean vaccine efficacy")
  
  ggsave(
    filename = sprintf(
      "outputs/figures/nsw_sa4_ve/nsw_sa4_ve_%s_omicron_transmission.png",
      i
    ),
    dpi = dpi,
    width = 1500 / dpi,
    height = 1250 / dpi,
    scale = 1.2,
    bg = "white"
  )
}

for (i in 1:3){
  
  if (i == 1){
    x <- 1:10
  } else if (i == 2) {
    x <- 11:20
  } else if (i == 3) {
    x <- 21:28
  }
  
  ggplot(
    ve_waning %>%
      filter(
        state == unique(.$state)[x]
    )
  ) +
    geom_line(
      aes(
        x = date,
        y = effect,
        linetype = variant
      )
    ) +
    geom_vline(
      aes(xintercept = Sys.Date())
    ) +
    facet_wrap(~state, ncol = 2) +
    ylim(0, 1) +
    scale_x_date(minor_breaks = "month") +
    ylab("Proportion reduction on transmission potential")
  
  ggsave(
    filename = sprintf(
      "outputs/figures/nsw_sa4_ve/nsw_sa4_ve_%s_ve_tp.png",
      i
    ),
    dpi = dpi,
    width = 1500 / dpi,
    height = 1250 / dpi,
    scale = 1.2,
    bg = "white"
  )
}
