# read in and parse the BETA barometer data dump
parse_barometer <- function(filename = "data/barometer_all/Barometer full contact_dates.csv") {
  df <- read_csv(
    filename,
    col_types = cols(
      .default = col_double(),
      postcode = col_character(),
      state = col_character(),
      gender = col_character(),
      vulnerable = col_character(),
      age_groups_pd = col_character(),
      city = col_character(),
      location = col_character(),
      household = col_character(),
      income = col_character(),
      parent = col_character(),
      employment = col_character(),
      phys_contact = col_character(),
      phys_distance = col_character(),
      wash_hands = col_character(),
      cough = col_character(),
      wave_date = col_date(format = ""),
      date = col_date(format = "%Y%m%d")
    )
  ) %>%
    # filter out leading numbers in responses
    mutate_at(
      vars(income, phys_distance, wash_hands, cough),
      ~substr(., 3, 100)
    ) %>%
    mutate_at(
      vars(vulnerable, city, phys_contact, parent),
      ~yesno_to_logical(.)
    ) %>%
    mutate_at(
      vars(starts_with("contacts_")),
      ~ifelse(. == -66, NA, .)
    ) %>%
    mutate(
      survey = "barometer",
      state = abbreviate_states(state),
      cough_any = NA,
      mask = NA,
      face_covering = NA,
      distancing_any = NA
    ) %>%
    rename(
      wave = barometer_week
    ) %>%
    select(-age_groups_pd)
  
  # fill in missing dates withthe wave date - the only information we have
  missing_date <- is.na(df$date)
  df$date[missing_date] <- df$wave_date[missing_date]
  df %>%
    select(-wave_date)
  
}
