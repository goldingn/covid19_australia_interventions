# load the data needed for the macrodistancing model
macrodistancing_data <- function(dates = NULL, breaks = c(0:10, 20, 50, Inf)) {
  
  # modelled change (after/before ratio) in time at types of locations from Google
  location_change_trends <- location_change(dates) %>%
    mutate_at(
      vars(public, home, retail, transit, work),
      ~replace_na(., 1)
    ) %>%
    mutate(state = abbreviate_states(state))
  
  # state-level numbers of non-household contacts by state and date from Freya's
  # survey and the BETA barometer. Remove implausible responses, from the
  # reporting clump at 999 and above (short conversation with 999 or more people
  # in a day is implausible, and probably an entry/reporting/understanding error)
  contact_data <- parse_all_surveys() %>%
    filter(
      !is.na(contact_num),
      !is.na(state)
    ) %>%
    select(
      state,
      date,
      wave,
      wave_date,
      wave_duration,
      weekend_fraction,
      age_groups,
      city,
      employment,
      contact_num,
      starts_with("contacts_")
    ) %>%
    filter(
      contact_num <= 999,
      date %in% location_change_trends$date
    )
  
  list(
    breaks = breaks,
    contacts = contact_data,
    location_change_trends = location_change_trends
  )
  
}
