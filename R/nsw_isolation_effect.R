# estimate NSW isolation effect
source("R/functions.R")

nsw_ll <- read_csv(
  "~/not_synced/nsw/20201222 - Case list - James McCaw.csv",
  col_types = cols(
    .default = col_character(),
    CASE_ID = col_double(),
    EARLIEST_CONFIRMED_OR_PROBABLE = col_nsw_date(),
    SYMPTOM_ONSET_DATE = col_nsw_date(),
    CALCULATED_ONSET_DATE = col_nsw_date(),
    AGE_AT_EVENT_YEARS = col_double(),
    DATE_ISOLATION_BEGAN = col_nsw_date(),
    SETTING_OF_TRANSMISSION_DATE = col_nsw_date(),
    INTERVIEWED_DATE = col_nsw_date()
  )
) %>%
  mutate(
    date_onset = SYMPTOM_ONSET_DATE,
    time_to_isolation = as.numeric(DATE_ISOLATION_BEGAN - SETTING_OF_TRANSMISSION_DATE) - 5
  ) %>%
  filter(
    !is.na(time_to_isolation) & !is.na(date_onset)
  )


nsw_ll$time_to_isolation




nsw_ecdfs <- nsw_ll %>%
  group_by(date_onset) %>%
  summarise(
    ecdf = list(ecdf(time_to_isolation))
  ) %>%
  arrange(
    desc(date_onset)
  ) %>%
  mutate(
    state = "NSW"
  )

# convert to a reduction in Reff
nsw_ecdfs$ecdf[[1]](-5:21)


# do surveillance_effect using these CDFs, and do it using the statewide ones,
# and calculate the ratio

nsw_ecdfs %>%
  mutate(
    statewide_effect = surveillance_effect(
      dates = date_onset,
      cdf = gi_cdf,
      states = "NSW"
    ),
    response_effect = surveillance_effect(
      dates = date_onset,
      cdf = gi_cdf,
      states = "NSW",
      ttd_cdfs = mutate(., date = date_onset)
    )
  ) %>%
  select(-ecdf) %>%
  mutate(
    response_reduction = response_effect / statewide_effect
  )
  