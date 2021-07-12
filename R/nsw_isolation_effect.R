# estimate NSW isolation effect
source("R/functions.R")

nsw_ll <- read_csv(
  "~/not_synced/nsw/20210123 - Case list - James McCaw.csv",
  col_types = cols(
    .default = col_character(),
    CASE_ID = col_double(),
    EARLIEST_CONFIRMED_OR_PROBABLE = col_date(),
    SYMPTOM_ONSET_DATE = col_date(),
    CALCULATED_ONSET_DATE = col_date(),
    AGE_AT_EVENT_YEARS = col_double(),
    DATE_ISOLATION_BEGAN = col_date(),
    SETTING_OF_TRANSMISSION_DATE = col_nsw_date("long"),
      INTERVIEWED_DATE = col_date()
  )
) %>%
  # if any infection dates are after onset, set the infection date to NA
  mutate(
    SETTING_OF_TRANSMISSION_DATE = case_when(
      SETTING_OF_TRANSMISSION_DATE > SYMPTOM_ONSET_DATE ~ as.Date(NA),
      SETTING_OF_TRANSMISSION_DATE < as.Date("2020-01-01") ~ as.Date(NA),
      TRUE ~ SETTING_OF_TRANSMISSION_DATE
    )
  ) %>%
  mutate(
    date_onset = SYMPTOM_ONSET_DATE,
    date_infection = SETTING_OF_TRANSMISSION_DATE,
    time_to_isolation = as.numeric(DATE_ISOLATION_BEGAN - SETTING_OF_TRANSMISSION_DATE) - 5
  ) %>%
  filter(
    !is.na(time_to_isolation) & !is.na(date_onset) & !is.na(date_infection)
  )


nsw_ll$time_to_isolation




nsw_ecdfs <- nsw_ll %>%
  group_by(date_infection) %>%
  summarise(
    ecdf = list(ecdf(time_to_isolation)),
    cases = n()
  ) %>%
  arrange(
    desc(date_infection)
  ) %>%
  mutate(
    state = "NSW"
  )

# do surveillance_effect using these CDFs, and do it using the statewide ones,
# and calculate the ratio

effects <- nsw_ecdfs %>%
  mutate(
    statewide_effect = surveillance_effect(
      dates = date_infection,
      cdf = gi_cdf,
      states = "NSW"
    ),
    response_effect = surveillance_effect(
      dates = date_infection,
      cdf = gi_cdf,
      states = "NSW",
      ttd_cdfs = mutate(., date = date_infection)
    )
  ) %>%
  select(-ecdf) %>%
  mutate(
    response_reduction = response_effect / statewide_effect
  )
  

# compute average effects to plot

# ignore cases infected on or before Avalon super-spreader events (11th and 13th)
cutoff_date <- as.Date("2020-12-13")

# compute average reduciton in transmission sue to isolation
average_response_effect <- nsw_ll %>%
  filter(
    date_infection > cutoff_date
  ) %>%
  summarise(
    ecdf = list(ecdf(time_to_isolation)),
    state = "NSW",
    date = min(date_infection)
  ) %>%
  surveillance_effect(
    dates = .$date,
    cdf = gi_cdf,
    states = "NSW",
    ttd_cdfs = .
  ) %>%
  c()

# compute average expected surveillance effect (barely changes over this period)
average_statewide_effect <- mean(effects$statewide_effect)

# average extra effect of contact tracing from these two
average_response_reduction <- average_response_effect / average_statewide_effect


# extra effect on top of surveillance
plot(
  response_reduction ~ date_infection,
  data = effects,
  col = "purple",
  pch = 16,
  cex = log1p(cases),
  ylab = "multiplier on Reff",
  xlab = "infection date"
)

abline(
  h = 1,
  lty = 2,
  lwd = 2
)

abline(
  h = average_statewide_effect,
  lwd = 2,
  col = "yellow"
)

abline(
  h = average_response_effect,
  lwd = 2,
  col = grey(0.4)
)

abline(
  h = average_response_reduction,
  lwd = 2,
  col = "purple"
)

abline(
  v = cutoff_date + 0.5,
  col = grey(0.7),
  lty = 3,
  lwd = 2
)

title(
  main = sprintf(
    "Average %i%s reduction in transmission\non top of surveillance effect",
    round(100 * (1 - average_response_reduction)),
    "%"
  )
)
