# load in data from the contact surveys
contact_survey_data <- function() {
  
  # load the first set of awkward datasets and their dates
  contact_data_list <- list(
    
    # Freya's survey waves
    load_contacts_by_state(
      "data/contacts/freya_survey/contact_counts_NG_wave1.csv",
      as.Date("2020-04-04")
    ),
    
    load_contacts_by_state(
      "data/contacts/freya_survey/contact_counts_NG_wave2.csv",
      as.Date("2020-05-02")
    ),
    
    # first barometer wave
    load_contacts_by_state(
      "data/contacts/barometer/contacts_by_state.csv",
      as.Date("2020-05-27")
    )
    
  )
  
  # add on barometer survey data that comes in waves
  contact_wave_files <- list.files("data/contacts/barometer",
                                   pattern = "\\d.csv",
                                   full.names = TRUE)
  
  # find survey dates from corresponding microdistancing data
  wave_dates <- contact_wave_files %>%
    # pull out wave number
    basename() %>%
    strsplit(" ") %>%
    vapply(`[`, 4, FUN.VALUE = character(1)) %>%
    gsub(".csv", "", x = .) %>%
    # pull in file
    paste("Barometer wave", ., "compliance.csv") %>%
    file.path("data/microdistancing/", .) %>%
    lapply(read_csv) %>%
    lapply(pull, "date") %>%
    lapply(first) %>%
    do.call(c, .)
  
  barometer_data_list <- mapply(
    load_contacts_by_state,
    csv = contact_wave_files,
    date = wave_dates,
    SIMPLIFY = FALSE
  )
  
  # combine these
  do.call(
    bind_rows,
    c(contact_data_list, barometer_data_list)
  )
  
}
