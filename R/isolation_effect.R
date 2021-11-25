source("R/lib.R")

source("R/functions.R")

nindss <- get_nndss_linelist()

isolation_data <- nindss %>%
  filter(
    import_status == "local",
    !is.na(date_onset),
    !is.na(date_quarantine),
  ) %>%
  mutate(
    date_infection = date_onset - 5,
    time_to_isolation = as.numeric(date_quarantine - date_onset),
    time_to_detection = as.numeric(date_detection - date_onset), # date detection vs date confirmation in rolling delays?
    .after = date_confirmation
  ) %>%
  filter(
    time_to_isolation < 50,
    time_to_isolation > -50
  )

isolation_data %>%
  mutate(
    ym = format(date_confirmation, "%Y-%m")
  ) %>%
  ggplot() +
  geom_boxplot(
    aes(
      ym,
      time_to_isolation
    )
  ) +
  facet_wrap(
    ~ state,
    ncol = 2
  )


isolation_delays_from_onset <- isolation_data %$%
  estimate_delays(
    state = state,
    date = date_onset,
    delay = time_to_isolation,
    direction = "forward",
    national_exclusions = NULL
  )

detection_delays_from_onset <- isolation_data %$%
  estimate_delays(
    state = state,
    date = date_onset,
    delay = time_to_detection,
    direction = "forward"
  )

p_isolation <- isolation_delays_from_onset %>%
  plot_delays(
    date = isolation_data$date_onset,
    state = isolation_data$state,
    delay = isolation_data$time_to_isolation,
    ylim = c(-20, 20),
    base_colour = "purple"
  ) +
  ggtitle(
    label = "Isolation trend",
    subtitle = "Time from modelled symptom onset to isolation for locally-acquired cases"
  ) +
  xlab("Symptom onset date") +
  ylab("Days to case isolation")

p_isolation

ggsave(
  filename = "time_to_isolation.png",
  plot = p_isolation,
  path = "outputs/figures",
  width = 6, height = 5,
  bg = "white"
)



p_detection <- detection_delays_from_onset %>%
  plot_delays(
    date = isolation_data$date_onset,
    state = isolation_data$state,
    delay = isolation_data$time_to_detection,
    ylim = c(-5, 20)
  ) +
  ggtitle(
    label = "Surveillance trend",
    subtitle = "Time from modelled symptom onset to detection for locally-acquired cases"
  ) +
  xlab("Symptom onset date") +
  ylab("Days to case detection")

ggsave(
  filename = "time_to_detection.png",
  plot = p_detection,
  path = "outputs/figures",
  width = 6, height = 5,
  bg = "white"
)


# compute overall ecdf
optimal_isolation_ecdf <- ecdf(isolation_data$time_to_isolation)

surveillance_cdfs <- readRDS("outputs/delay_from_onset_cdfs.RDS")
head(surveillance_cdfs)
surveillance <- surveillance_effect(
  dates = seq(
    min(surveillance_cdfs$date),
    max(surveillance_cdfs$date),
    by = 1
  ),
  states = unique(surveillance_cdfs$state),
  cdf = gi_cdf
)


# convert surveillance effect to weights (to represent how the effectiveness
# of the contact tracing system changed over time) and compute a weighted
# time-to-isolation cdf for each date and state
isolation_cdfs <- surveillance_cdfs %>%
  ungroup() %>%
  rename(
    surveillance_cdf = ecdf
  ) %>%
  mutate(
    surveillance_effect = c(t(surveillance)),
    ideal_isolation_ecdf = list(optimal_isolation_ecdf),
    isolation_weight = 1 - surveillance_effect,
    isolation_weight = isolation_weight / max(isolation_weight),
    isolation_ecdf = mapply(
      FUN = weight_ecdf,
      surveillance_cdf,
      ideal_isolation_ecdf,
      1 - isolation_weight,
      SIMPLIFY = FALSE
    )
  ) %>%
  select(
    date,
    state,
    ecdf = isolation_ecdf
  )

# save this
saveRDS(isolation_cdfs, "outputs/isolation_cdfs.RDS")

# compute the additional isolation effect as a check
extra_isolation <- extra_isolation_effect(
  dates = seq(
    min(surveillance_cdfs$date),
    max(surveillance_cdfs$date),
    by = 1
  ),
  states = unique(surveillance_cdfs$state),
  cdf = gi_cdf
)

# range of percentage reductions:
# effect of detection and isolation of cases:
range(100 * (1 - surveillance))
# additional effect of isolating case contacts before they test positive
range(100 * (1 - extra_isolation))
