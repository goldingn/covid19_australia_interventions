# NSW TTIQ effectiveness over time
source("R/functions.R")

nsw_ll <- read_csv(
  "~/not_synced/vaccination/nsw/cases_29.csv",
  col_types = cols(
    .default = col_character(),
    AGE_AT_EVENT_YEARS = col_double(),
    SA2_2016_CODE = col_double(),
    EARLIEST_CONFIRMED_OR_PROBABLE = col_date(format = ""),
    CALCULATED_ONSET_DATE = col_date(format = ""),
    SYMPTOM_ONSET_DATE = col_date(format = ""),
    LAST_CONTACT_WITH_CASE_DATE = col_date(format = ""),
    INITIAL_CLOSE_CONTACT_INTERVIEW_DATE = col_date(format = ""),
    INTERVIEWED_DATE = col_date(format = ""),
    DATE_ISOLATION_BEGAN = col_date(format = ""),
    SETTING_OF_TRANSMISSION_DATE = col_date(format = ""),
    earliest_detected = col_date(format = ""),
    link_CALCULATED_ONSET_DATE = col_date(format = ""),
    link_EARLIEST_CONFIRMED_OR_PROBABLE = col_date(format = ""),
    link_AGE_AT_EVENT_YEARS = col_double(),
    link_SA2_2016_CODE = col_double(),
    link_earliest_detected = col_date(format = "")
  )
) %>%
  select(
    date_infection = SETTING_OF_TRANSMISSION_DATE,
    date_onset = SYMPTOM_ONSET_DATE,
    date_isolation = DATE_ISOLATION_BEGAN,
    date_detection = EARLIEST_CONFIRMED_OR_PROBABLE
  ) %>%
  mutate_all(
    as_date
  ) %>%
  # if any infection dates are after onset/isolation/detection,
  # set the infection date to NA (and remove)
  mutate(
    date_infection = case_when(
      date_infection > date_onset ~ as_date(NA),
      date_infection > date_isolation ~ as_date(NA),
      date_infection > date_detection ~ as_date(NA),
      date_infection < as.Date("2020-01-01") ~ as_date(NA),
      TRUE ~ date_infection
    )
  ) %>%
  # use modelled date of onset for period where dates of infection are reliable
  mutate(
    modelled_onset = date_detection < as_date("2021-02-28"),
    date_onset = if_else(
      modelled_onset,
      date_infection + 5,
      date_onset
    )
  ) %>%
  # compute time from (possibly modelled) onset to isolation
  mutate(
    time_to_isolation = pmax(-5, as.numeric(date_isolation - date_onset)),
    time_to_detection = pmax(-5, as.numeric(date_detection - date_onset)),
    state = "NSW"
  ) %>%
  filter(
    !is.na(time_to_isolation)
  ) %>%
  # account for right-truncation
  filter(
    date_detection <= (max(date_detection) - 21)
  )

# compute and plot delays over time in NSW
isolation_delays_from_onset <- estimate_delays(
  state = nsw_ll$state,
  date = nsw_ll$date_onset,
  delay = nsw_ll$time_to_isolation,
  direction = "forward",
  min_records = 300,
  absolute_min_records = 50,
  revert_to_national = FALSE
)

detection_delays_from_onset <- estimate_delays(
  state = nsw_ll$state,
  date = nsw_ll$date_onset,
  delay = nsw_ll$time_to_detection,
  direction = "forward",
  min_records = 300,
  absolute_min_records = 50,
  revert_to_national = FALSE
)

null_function <- function(...) {NA}
p_isolation <- isolation_delays_from_onset %>%
  filter(
    !use_national
  ) %>%
  plot_delays(
    date = nsw_ll$date_onset,
    state = nsw_ll$state,
    delay = nsw_ll$time_to_isolation,
    ylim = c(-5, 20),
    base_colour = "purple"
  ) +
  geom_ribbon(
    aes(x = date, y = 1, ymin = -10, ymax = use_national * 100 - 10),
    fill = grey(1),
    alpha = 0.5,
    colour = grey(0.9),
    linetype = 3,
    data = isolation_delays_from_onset
  ) +
  ggtitle(
    label = "Isolation trend",
    subtitle = "Time from modelled symptom onset to isolation for locally-acquired cases"
  ) +
  xlab("Symptom onset date") +
  ylab("Days to case isolation")

p_isolation

ggsave(
  filename = "nsw_detailed_time_to_isolation.png",
  plot = p_isolation,
  path = "~/Desktop",
  width = 6, height = 5,
  bg = "white"
)


p_detection <- detection_delays_from_onset %>%
  filter(
    !use_national
  ) %>%
  plot_delays(
    date = nsw_ll$date_onset,
    state = nsw_ll$state,
    delay = nsw_ll$time_to_detection,
    ylim = c(-5, 20)
  ) +
  ggtitle(
    label = "Surveillance trend",
    subtitle = "Time from modelled symptom onset to detection for locally-acquired cases"
  ) +
  xlab("Symptom onset date") +
  ylab("Days to case detection")

ggsave(
  filename = "nsw_detailed_time_to_detection.png",
  plot = p_detection,
  path = "~/Desktop",
  width = 6, height = 5,
  bg = "white"
)



# 
# 
# 
# 
# 
# nsw_ecdfs <- nsw_ll %>%
#   group_by(date_infection) %>%
#   summarise(
#     ecdf = list(ecdf(time_to_isolation)),
#     cases = n()
#   ) %>%
#   arrange(
#     desc(date_infection)
#   ) %>%
#   mutate(
#     state = "NSW"
#   )
# 
# # do surveillance_effect using these CDFs, and do it using the statewide ones,
# # and calculate the ratio
# 
# effects <- nsw_ecdfs %>%
#   mutate(
#     statewide_effect = surveillance_effect(
#       dates = date_infection,
#       cdf = gi_cdf,
#       states = "NSW"
#     ),
#     response_effect = surveillance_effect(
#       dates = date_infection,
#       cdf = gi_cdf,
#       states = "NSW",
#       ttd_cdfs = mutate(., date = date_infection)
#     )
#   ) %>%
#   select(-ecdf) %>%
#   mutate(
#     response_reduction = response_effect / statewide_effect
#   )
#   
# 
# # compute average effects to plot
# 
# # ignore cases infected on or before Avalon super-spreader events (11th and 13th)
# cutoff_date <- as.Date("2020-12-13")
# 
# # compute average reduciton in transmission sue to isolation
# average_response_effect <- nsw_ll %>%
#   filter(
#     date_infection > cutoff_date
#   ) %>%
#   summarise(
#     ecdf = list(ecdf(time_to_isolation)),
#     state = "NSW",
#     date = min(date_infection)
#   ) %>%
#   surveillance_effect(
#     dates = .$date,
#     cdf = gi_cdf,
#     states = "NSW",
#     ttd_cdfs = .
#   ) %>%
#   c()
# 
# # compute average expected surveillance effect (barely changes over this period)
# average_statewide_effect <- mean(effects$statewide_effect)
# 
# # average extra effect of contact tracing from these two
# average_response_reduction <- average_response_effect / average_statewide_effect
# 
# 
# # extra effect on top of surveillance
# plot(
#   response_reduction ~ date_infection,
#   data = effects,
#   col = "purple",
#   pch = 16,
#   cex = log1p(cases),
#   ylab = "multiplier on Reff",
#   xlab = "infection date"
# )
# 
# abline(
#   h = 1,
#   lty = 2,
#   lwd = 2
# )
# 
# abline(
#   h = average_statewide_effect,
#   lwd = 2,
#   col = "yellow"
# )
# 
# abline(
#   h = average_response_effect,
#   lwd = 2,
#   col = grey(0.4)
# )
# 
# abline(
#   h = average_response_reduction,
#   lwd = 2,
#   col = "purple"
# )
# 
# abline(
#   v = cutoff_date + 0.5,
#   col = grey(0.7),
#   lty = 3,
#   lwd = 2
# )
# 
# title(
#   main = sprintf(
#     "Average %i%s reduction in transmission\non top of surveillance effect",
#     round(100 * (1 - average_response_reduction)),
#     "%"
#   )
# )


# compute overall ecdf from after cutoff
ideal_isolation_ecdf <- nsw_ll %>%
  # filter(date_infection > cutoff_date) %>%
  pull(time_to_isolation) %>%
  ecdf

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




p_isolation_all <- isolation_cdfs %>%
  mutate(
    use_national = FALSE
  ) %>%
  plot_delays(
    date = min(isolation_cdfs$date),
    state = isolation_cdfs$state[1],
    delay = 0,
    base_colour = "purple",
    ylim = c(-5, 18)
  ) +
  ggtitle(
    label = "Notification delay",
    subtitle = "Time from symptom onset to notification for locally-acquired cases"
  ) +
  xlab("Notification date") +
  ylab("Days since symptom onset")
p_isolation_all

save_ggplot("time_to_isolation.png", multi = TRUE)

