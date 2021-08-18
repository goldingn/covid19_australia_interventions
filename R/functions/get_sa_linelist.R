get_sa_linelist <- function(file = "~/not_synced/sa/SA_linelist_25July2021.csv") {
  
  # We need to time travel as SA health has given us an outdated linelist (by 1 day)
  # We'll undo this on the EpiFX end.
  t_offset <- days(1)
  
  file %>%
    read_csv() %>%
    mutate(
      symptom_onset = dmy(symptom_onset) + t_offset,
      notification_date = dmy(notification_date) + t_offset,
      likely_infection_date = dmy(likely_infection_date) + t_offset) %>%
  
  mutate(
      date_onset = case_when(!is.na(likely_infection_date) ~ likely_infection_date + 5,
                             T                             ~ symptom_onset),
      date_detection = NA,
      date_confirmation = notification_date,
      state = "SA",
      postcode_of_acquisition = "8888",
      postcode_of_residence = NA,
      state_of_acquisition = ifelse(
        import_status == "local",
        "SA",
        NA
      ),
      state_of_residence = NA,
      report_delay = NA,
      date_linelist = as.Date("2021-07-25"),
      interstate_import = FALSE
    ) %>%
    select(
      date_onset,
      date_detection,
      date_confirmation,
      state,
      import_status,
      postcode_of_acquisition,
      postcode_of_residence,
      state_of_acquisition,
      state_of_residence,
      report_delay,
      date_linelist,
      interstate_import
    ) %>%
    filter(
      import_status == "local"
    )
}
