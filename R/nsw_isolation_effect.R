# estimate NSW isolation effect
source("R/functions.R")

# load and format data on time to isolation in NSW outbreak
nsw_isolation_data <- read_csv(
  "data/miscs/nsw_isolation_data.csv",
  col_types = cols(
    notification_date = col_date(format = "%d/%m/%y"),
    cases = col_double(),
    symptomatic_at_test = col_double(),
    isol_by_m2d = col_double(),
    isol_by_1d = col_double(),
    isol_by_2d = col_double(),
    isol_by_3d = col_double(),
    isol_after_3d = col_double()
  )
) %>%
  # replace the symptomatic at test column due to a data mismatch
  mutate(
    symptomatic_at_test = isol_by_m2d + isol_by_3d + isol_after_3d
  )

# cumulative fractions in isolation by time since symptom onset
cumulative_isolation_reporting <- nsw_isolation_data %>%
  mutate(
    prop_m2d = isol_by_m2d / symptomatic_at_test,
    prop_1d = isol_by_1d / symptomatic_at_test,
    prop_2d = isol_by_2d / symptomatic_at_test,
    prop_3d = isol_by_3d / symptomatic_at_test
  ) %>%
  select(
    date_confirmation = notification_date,
    prop_m2d,
    prop_1d,
    prop_2d,
    prop_3d
  )

# reweight these by dates of symptom onset
cumulative_isolation_onset <- load_linelist() %>%
  filter(
    state == "NSW" &
      date_confirmation > as.Date("2020-12-10") &
      import_status == "local"
  ) %>%
  select(
    date_onset,
    date_confirmation
  ) %>%
  right_join(
    cumulative_isolation_reporting,
    by = "date_confirmation"
  ) %>%
  group_by(date_onset) %>%
  summarise_at(
    vars(starts_with("prop")),
    mean
  )

nsw_ecdfs <- cumulative_isolation_onset %>%
  pivot_longer(
    cols = starts_with("prop"),
    names_to = "days",
    values_to = "prop"
  ) %>%
  mutate(
    days = case_when(
      days == "prop_m2d" ~ -2,
      days == "prop_1d" ~ 1,
      days == "prop_2d" ~ 2,
      days == "prop_3d" ~ 3
    )
  ) %>%
  group_by(date_onset) %>%
  summarise(
    ecdf = list(make_ecdf(prop, days))
  ) %>%
  arrange(
    desc(date_onset)
  ) %>%
  mutate(
    state = "NSW"
  )

# convert to a reduction in Reff
nsw_ecdfs$ecdf[[10]](-5:21)


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
  