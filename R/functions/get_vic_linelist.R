# replace VIC elements with VIC linelist
get_vic_linelist <- function(file) {
  
  linelist_date <- file %>%
    basename() %>%
    substr(1, 8) %>%
    as.Date(format = "%Y%m%d")
  

  file %>%
      read_csv(
        col_types = cols(
          CaseNumber = col_double(),
          DiagnosisDate = col_date(format = ""),
          SymptomsOnsetDate = col_date(format = ""),
          LGA = col_character(),
          Acquired = col_character(),
          FirstSpecimenPositiveDate = col_date(format = "")
        ),
        na = "NA"
      ) %>%
    # read_excel(
    #   col_types = c("numeric", "date", "date", "text", "text", "date"),
    #   na = "NA"
    # ) %>%
    mutate(
      date_onset = as.Date(SymptomsOnsetDate),
      date_confirmation = as.Date(DiagnosisDate),
      date_detection = clean_date(as.Date(FirstSpecimenPositiveDate)),
      state = "VIC",
      import_status = case_when(
        Acquired == "Travel overseas" ~ "imported",
        TRUE ~ "local"
      ),
      postcode_of_acquisition = "8888",
      postcode_of_residence = "8888",
      state_of_acquisition = NA,
      state_of_residence = NA,
      report_delay = NA,
      date_linelist = linelist_date
    ) %>%
    mutate(
      date_detection = case_when(
        is.na(date_detection) ~ date_confirmation - 1,
        TRUE ~ date_detection
      )
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
      date_linelist
    )
  
  
  # 
  # file %>%
  #   read_csv(
  #     col_types = cols(
  #       PHESSID = col_double(),
  #       diagnosis_date = col_datetime(format = ""),
  #       ss_onset = col_datetime(format = ""),
  #       Localgovernmentarea = col_character(),
  #       acquired = col_character(),
  #       SPECIMEN_DATE = col_date(format = "%d/%m/%Y")
  #     ),
  #     na = "NULL"
  #   ) %>%
  #   mutate(
  #     date_onset = as.Date(ss_onset),
  #     date_confirmation = as.Date(diagnosis_date),
  #     date_detection = clean_date(SPECIMEN_DATE),
  #     state = "VIC",
  #     import_status = case_when(
  #       acquired == "Travel overseas" ~ "imported",
  #       TRUE ~ "local"
  #     ),
  #     postcode_of_acquisition = "8888",
  #     postcode_of_residence = "8888",
  #     state_of_acquisition = NA,
  #     state_of_residence = NA,
  #     report_delay = NA,
  #     date_linelist = linelist_date
  #   ) %>%
  #   # the mode and mean of the delay from testing to confirmation in VIC is around 3 days at the moment
  #   mutate(
  #     date_detection = case_when(
  #       is.na(date_detection) ~ date_confirmation - 3,
  #       TRUE ~ date_detection
  #     )
  #   ) %>%
  #   select(
  #     date_onset,
  #     date_detection,
  #     date_confirmation,
  #     state,
  #     import_status,
  #     postcode_of_acquisition,
  #     postcode_of_residence,
  #     state_of_acquisition,
  #     state_of_residence,
  #     report_delay,
  #     date_linelist
  #   )
  
  
}
