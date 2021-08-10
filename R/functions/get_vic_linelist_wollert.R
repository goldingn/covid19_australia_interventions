get_vic_linelist_wollert <- function(file = "~/not_synced/vic/Whittlesea outbreak dates 30 May 2021.xlsx"){

  file %>%
    read_excel(
      col_names = c(
        "ID",
        "CRM",
        "infectious_date",
        "inferred_onset",
        "positive"
      ),
      col_types = c(
        "text",
        "numeric",
        "date",
        "date",
        "date"
      ),
      skip = 1
    ) %>%
    mutate(
      date_onset = as.Date(inferred_onset),
      date_confirmation = as.Date(positive),
      state = "VIC",
      import_status = "local",
      postcode_of_acquisition = "8888",
      postcode_of_residence = "8888",
      state_of_acquisition = NA,
      state_of_residence = NA,
      report_delay = NA,
      date_linelist = NA,
      date_detection = date_confirmation - 1
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

}
