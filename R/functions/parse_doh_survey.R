# read in and parse raw DoH survey files in a similar way to BETA
parse_doh_survey <- function(filename) {
  full <- filename %>%
    read_csv(
      col_types = cols(
        .default = col_character(),
        S1 = col_double(),
        Q138 = col_double(),
        Q166_1 = col_double(),
        Q166_4 = col_double(),
        Q167_1 = col_double(),
        Q168_2 = col_double(),
        Q169_1 = col_double()
      )
    ) %>%
    mutate(
      wave = wave_from_file(filename),
      state = abbreviate_states(S3),
      city = Location == "Major city",
      parent = Q39 %in% parenty_households,
      distancing_any = NA,
      cough_any = NA,
      mask = NA,
      survey = "doh",
      date = as.Date(StartDate, format = "%Y%m%d"),
    )
  
  # add face covering data if it is there
  if ("Q222" %in% names(full)) {
    full <- full %>%
      mutate(face_covering = Q222)
  } else {
    full <- full %>%
      mutate(face_covering = NA)
  }
  
  full %>%
    select(
      wave,
      state,
      gender = S2,
      age = S1,
      vulnerable = Q75,
      age_groups = AgeBracket,
      city,
      location = Location,
      postcode = Q37,
      household = Q39,
      income = Q42,
      parent,
      employment = Q38,
      phys_contact = Q109,
      phys_distance = Q65,
      wash_hands = Q110,
      cough_any,
      cough = Q111,
      mask,
      face_covering,
      contact_num = Q138,
      contacts_under18 = Q166_1,
      contacts_18to39 = Q166_2,
      contacts_40to59 = Q166_3,
      contacts_60plus = Q166_4,
      contacts_ageunsure = Q166_5,
      contacts_phys = Q167_1,
      contacts_close = Q167_2,
      contacts_notclose = Q167_3,
      contacts_physdontknow = Q167_4,
      contacts_home = Q168_1,
      contacts_work = Q168_2,
      contacts_worship = Q168_3,
      contacts_school = Q168_4,
      contacts_shop = Q168_5,
      contacts_cafe = Q168_6,
      contacts_sport = Q168_7,
      contacts_public = Q168_8,
      contacts_other = Q168_9,
      contacts_cantremember = Q168_10,
      contacts_5min = Q169_1,
      contacts_5to30min = Q169_2,
      contacts_30to90min = Q169_3,
      contacts_90minto3hrs = Q169_4,
      contacts_3hrsplus = Q169_5,
      contacts_timedontknow = Q169_6,
      date,
      survey
    ) %>%
    mutate_at(
      vars(vulnerable, phys_contact),
      ~yesno_to_logical(.)
    ) %>%
    mutate_at(
      vars(starts_with("contacts_")),
      ~as.numeric(.)
    )
  
}
