# read in the latest linelist and format for analysis
get_nndss_linelist <- function(
  date = NULL,
  dir = "~/not_synced/nndss",
  strict = TRUE,
  #missing_location_assumption = "imported"
  missing_location_assumption = "local"
  #missing_location_assumption = "missing"
) {
  
  data <- linelist_date_times(dir)
  
  # subset to this date
  if (!is.null(date)) {
    data <- data %>%
      filter(as.Date(date_time) == date)
  }
  
  # get the latest linelist
  data <- data %>%
    filter(date_time == max(date_time, na.rm = TRUE))
  
  col_types <- NULL
  if (strict) {
    col_types_1 <- c(
      STATE = "text",
      POSTCODE = "numeric",
      CONFIRMATION_STATUS = "numeric",
      TRUE_ONSET_DATE = "date",
      SPECIMEN_DATE = "date",
      NOTIFICATION_DATE = "date",
      NOTIFICATION_RECEIVE_DATE = "date",
      Diagnosis_Date = "date",
      AGE_AT_ONSET = "numeric",
      SEX = "numeric",
      DIED = "numeric",
      PLACE_OF_ACQUISITION = "text",
      HOSPITALISED = "numeric",
      CV_ICU = "numeric",
      CV_VENTILATED = "numeric",
      OUTBREAK_REF = "text",
      CASE_FOUND_BY = "numeric",
      CV_SYMPTOMS = "text",
      CV_OTHER_SYMPTOMS = "text",
      CV_COMORBIDITIES = "text",
      CV_OTHER_COMORBIDITIES = "text",
      CV_GESTATION = "numeric",
      CV_CLOSE_CONTACT = "numeric"
      #CV_EXPOSURE_SETTING = "numeric",
      #CV_SOURCE_INFECTION = "numeric"
    )
    
    
    col_types_2 <- c(
      STATE = "text",
      POSTCODE = "numeric",
      CONFIRMATION_STATUS = "numeric",
      TRUE_ONSET_DATE = "date",
      SPECIMEN_DATE = "date",
      NOTIFICATION_DATE = "date",
      NOTIFICATION_RECEIVE_DATE = "date",
      Diagnosis_Date = "date",
      AGE_AT_ONSET = "numeric",
      SEX = "numeric",
      DIED = "numeric",
      PLACE_OF_ACQUISITION = "text",
      HOSPITALISED = "numeric",
      CV_ICU = "numeric",
      CV_VENTILATED = "numeric",
      OUTBREAK_REF = "text",
      CASE_FOUND_BY = "numeric",
      CV_SYMPTOMS = "text",
      CV_OTHER_SYMPTOMS = "text",
      CV_COMORBIDITIES = "text",
      CV_OTHER_COMORBIDITIES = "text",
      CV_GESTATION = "numeric",
      #CV_CLOSE_CONTACT = "numeric"
      CV_EXPOSURE_SETTING = "numeric",
      CV_SOURCE_INFECTION = "numeric"
    )
  }
  
  
  ll_date <- data$date_time[[1]]
  
  
  if(ll_date < "2021-03-08"){
    col_types <- col_types_1
  } else {
    col_types <- col_types_2
  }
  
  dat <- read_xlsx(
    data$file,
    col_types = col_types
  )
  
  if(ll_date < "2021-03-08"){
    dat <- dat %>%
      mutate(
        CV_SOURCE_INFECTION = NA_real_
      )
  }
  
  
  if (is.numeric(dat$POSTCODE)) {
    dat <- dat %>%
      mutate(
        POSTCODE = sprintf("%04d", dat$POSTCODE),
        POSTCODE = ifelse(POSTCODE == "00NA", NA, POSTCODE) 
      )
  } else {
    dat <- dat %>%
      mutate(POSTCODE = NA)
  }
  
  # Remove cases without a state
  dat <- dat %>%
    filter(!is.na(STATE))
  
  # tidy up dates and parse place of acquisition to local (Australia) vs. overseas
  dat <- dat %>%
    mutate(
      TRUE_ONSET_DATE = clean_date(TRUE_ONSET_DATE),
      NOTIFICATION_RECEIVE_DATE = clean_date(NOTIFICATION_RECEIVE_DATE),
      SPECIMEN_DATE = clean_date(SPECIMEN_DATE)
    ) %>%
    mutate(
      import_status = case_when(
        # return "ERROR" if place of acquisition and cv_source_infection
        # indicate opposite import statuses
        grepl("^1101", PLACE_OF_ACQUISITION) & CV_SOURCE_INFECTION == 1 ~ "ERROR",
        !is.na(PLACE_OF_ACQUISITION) & 
          !grepl("^1101|^00038888", PLACE_OF_ACQUISITION) &
          CV_SOURCE_INFECTION == 2 ~ "ERROR",
        !is.na(PLACE_OF_ACQUISITION) & 
          !grepl("^1101|^00038888", PLACE_OF_ACQUISITION) &
          CV_SOURCE_INFECTION == 3 ~ "ERROR",
        !is.na(PLACE_OF_ACQUISITION) & 
          !grepl("^1101|^00038888", PLACE_OF_ACQUISITION) &
          CV_SOURCE_INFECTION == 4 ~ "ERROR",
        # where source known us that
        grepl("^1101", PLACE_OF_ACQUISITION) ~ "local",
        CV_SOURCE_INFECTION == 1 ~ "imported",
        CV_SOURCE_INFECTION == 2 ~ "local",
        CV_SOURCE_INFECTION == 3 ~ "local",
        CV_SOURCE_INFECTION == 4 ~ "local",
        CV_SOURCE_INFECTION == 6 ~ "local",
        CV_SOURCE_INFECTION == 7 ~ "local",
        # otherwise impute it
        CV_SOURCE_INFECTION == 5 ~ missing_location_assumption,
        grepl("^00038888", PLACE_OF_ACQUISITION) ~ missing_location_assumption,
        !is.na(PLACE_OF_ACQUISITION) ~ "imported",
        is.na(PLACE_OF_ACQUISITION) ~ missing_location_assumption,
        is.na(CV_SOURCE_INFECTION) ~ missing_location_assumption
      )#,
      # import_status = case_when(
      #   import_status == "missing" & STATE == "WA" ~ "local",
      #   import_status == "missing" & STATE != "WA" ~ "imported",
      #   TRUE ~ import_status
      # )
    ) #%>%
    # mutate(
    #   import_status = ifelse(import_status == "ERROR", "imported", import_status)
    # )
  
  # record state of acquisition, and residence
  dat <- dat %>%
    # fill in missing places of acquisition with correct code
    mutate(
      PLACE_OF_ACQUISITION = ifelse(
        is.na(PLACE_OF_ACQUISITION),
        "00038888",
        PLACE_OF_ACQUISITION)
    ) %>%
    mutate(
      postcode_of_acquisition = substr(PLACE_OF_ACQUISITION, 5, 8),
      postcode_of_residence = replace_na(POSTCODE, "8888"),
      state_of_acquisition = postcode_to_state(postcode_of_acquisition),
      state_of_residence = postcode_to_state(postcode_of_residence)
    )
  

  
  # Generate linelist data
  linelist <- dat %>%
    # notification receive date seems buggy, and is sometimes before the
    # notification date and specimen collection date
    mutate(
      date_confirmation = pmax(NOTIFICATION_RECEIVE_DATE,
                               NOTIFICATION_DATE,
                               na.rm = TRUE),
    ) %>%
    select(
      date_onset = TRUE_ONSET_DATE,
      date_detection = SPECIMEN_DATE,
      date_confirmation,
      state = STATE,
      import_status,
      postcode_of_acquisition,
      postcode_of_residence,
      state_of_acquisition,
      state_of_residence
    ) %>%
    mutate(
      report_delay = as.numeric(date_confirmation - date_onset),
      date_linelist = as.Date(data$date_time, tz = "Australia/Canberra"),
      state = as.factor(state)
    ) %>%
    
    # Remove those with onset date after confirmation date
    # only keep individuals with date of confirmation after onset date if less than 2 days (inclusive) because
    # we assume some individuals tested via contact tracing will test positive before symptom onset and therefore plausible
    # (noting that reporting delay distribution only calculated from positive differences)
    # also remove any individuals with NA for both notification and symptom onset dates
    # filter(
    #   date_confirmation >= (date_onset - 2) | is.na(date_confirmation) | is.na(date_onset)
    # ) %>%
    filter(
      !(is.na(date_confirmation) & is.na(date_onset))
    ) %>%
    mutate_at(
      vars(starts_with("date_")),
      ~as.Date(.)
    ) %>%
    # for cases missing a date of detection, assume it's the day before the date
    # of confirmation (1 days is the median and mode of this delay distribution)
    mutate(
      date_detection = case_when(
        is.na(date_detection) ~ date_confirmation - 1,
        TRUE ~ date_detection
      )
    )
  
  linelist
  
}
