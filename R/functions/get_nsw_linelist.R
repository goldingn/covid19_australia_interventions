# get latest NSW linelist
get_nsw_linelist <- function () {
  
  files <- list.files(
    "~/not_synced/nsw",
    pattern = ".csv",
    full.names = TRUE
  )

  dates <- files %>%
    basename() %>%
    substr(1, 8) %>%
    as.Date(format = "%Y%m%d")

  latest <- which.max(dates)
  file <- files[latest]
  date <- dates[latest]


  file %>%
    read_csv(
      col_types = cols(
        .default = col_character(),
        CASE_ID = col_double(),
        EARLIEST_CONFIRMED_OR_PROBABLE = col_nsw_date(),
        SYMPTOM_ONSET_DATE = col_nsw_date(),
        CALCULATED_ONSET_DATE = col_nsw_date(),
        AGE_AT_EVENT_YEARS = col_double(),
        DATE_ISOLATION_BEGAN = col_nsw_date(),
        SETTING_OF_TRANSMISSION_DATE = col_nsw_date("long"),
        INTERVIEWED_DATE = col_nsw_date()
      )
    ) %>%
    # remove some bogus dates
    mutate(across(
      all_of(c(
        "EARLIEST_CONFIRMED_OR_PROBABLE",
        "SYMPTOM_ONSET_DATE",
        "SETTING_OF_TRANSMISSION_DATE",
        "CALCULATED_ONSET_DATE",
        "DATE_ISOLATION_BEGAN",
        "SETTING_OF_TRANSMISSION_DATE",
        "INTERVIEWED_DATE"
      )),
      clean_date
    )
    ) %>%
    # if any infection dates are after onset, or on/after confirmation, set the infection date to NA
    mutate(
      SETTING_OF_TRANSMISSION_DATE = case_when(
        SETTING_OF_TRANSMISSION_DATE > SYMPTOM_ONSET_DATE ~ as.Date(NA),
        SETTING_OF_TRANSMISSION_DATE >= EARLIEST_CONFIRMED_OR_PROBABLE ~ as.Date(NA),
        TRUE ~ SETTING_OF_TRANSMISSION_DATE
      )
    ) %>%
    mutate(
      date_onset = case_when(
        !is.na(SETTING_OF_TRANSMISSION_DATE) ~ SETTING_OF_TRANSMISSION_DATE + 5,
        TRUE ~ CALCULATED_ONSET_DATE
      ),
      date_detection = NA,
      date_confirmation = EARLIEST_CONFIRMED_OR_PROBABLE,
      state = "NSW",
      import_status = ifelse(
        PLACE_ACQUISITION == "Acquired in NSW",
        "local",
        "imported"
      ),
      postcode_of_acquisition = NA,
      postcode_of_residence = NA,
      state_of_acquisition = "NSW",
      state_of_residence = NA,
      report_delay = NA,
      date_linelist = date,
      interstate_import = FALSE
    ) %>%
    select(
      date_onset,
      date_detection,
      date_confirmation = EARLIEST_CONFIRMED_OR_PROBABLE,
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
    arrange(
      desc(date_onset)
    )
  
  # edit for excel file  
  # file <- "~/not_synced/nsw/cases_freya.xlsx"
  # date <- as.Date("2021-07-21")
  # 
  # file %>%
  #   read_xlsx(
  #     col_types = c(
  #       "text",
  #       "date",
  #       "date",
  #       "text",
  #       "text",
  #       "text",
  #       "text",
  #       "text",
  #       "date",
  #       "text"
  #     )
  #   ) %>%
  #   distinct(.keep_all = TRUE) %>% # some duplicated rows can be id'd by project_recid
  #   mutate(
  #     EARLIEST_CONFIRMED_OR_PROBABLE = as.Date(EARLIEST_CONFIRMED_OR_PROBABLE),
  #     CALCULATED_ONSET_DATE = as.Date(CALCULATED_ONSET_DATE),
  #     SETTING_OF_TRANSMISSION_DATE = as.Date(SETTING_OF_TRANSMISSION_DATE)
  #   ) %>%
  # # if any infection dates are after onset, or on/after confirmation, set the infection date to NA
  # mutate(
  #   SETTING_OF_TRANSMISSION_DATE = case_when(
  #     SETTING_OF_TRANSMISSION_DATE >= EARLIEST_CONFIRMED_OR_PROBABLE ~ as.Date(NA),
  #     TRUE ~ SETTING_OF_TRANSMISSION_DATE
  #   )
  # ) %>%
  #   mutate(
  #     date_onset = case_when(
  #       !is.na(SETTING_OF_TRANSMISSION_DATE) ~ SETTING_OF_TRANSMISSION_DATE + 5,
  #       TRUE ~ CALCULATED_ONSET_DATE
  #     ),
  #     date_detection = NA,
  #     date_confirmation = EARLIEST_CONFIRMED_OR_PROBABLE,
  #     state = "NSW",
  #     import_status = ifelse(
  #       PLACE_ACQUISITION == "Acquired in NSW",# rethink this if goes to prime time
  #       "local",
  #       "imported"
  #     ),
  #     postcode_of_acquisition = NA,
  #     postcode_of_residence = NA,
  #     state_of_acquisition = "NSW",
  #     state_of_residence = NA,
  #     report_delay = NA,
  #     date_linelist = date#,
  #     #interstate_import = FALSE # rethink this if goes to prime time
  #   ) %>%
  #   select(
  #     date_onset,
  #     date_detection,
  #     date_confirmation = EARLIEST_CONFIRMED_OR_PROBABLE,
  #     state,
  #     import_status,
  #     postcode_of_acquisition,
  #     postcode_of_residence,
  #     state_of_acquisition,
  #     state_of_residence,
  #     report_delay,
  #     date_linelist#,
  #     #interstate_import
  #   ) %>%
  #   arrange(
  #     desc(date_onset)
  #   )
  
}
