# check NNDSS linelist for missing entries and local cases in last 3 weeks.

dir <- "~/not_synced/nndss"
  
data <- linelist_date_times(dir) %>%
    filter(date_time == max(date_time, na.rm = TRUE))
  
strict <- TRUE

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
  
  dat <- readxl::read_xlsx(
    data$file,
    col_types = col_types
  )
  
filter_date <- ll_date - lubridate::days(21)


df <- dat %>%
  filter(Diagnosis_Date >= filter_date) %>%
  mutate(
    check = case_when(
      is.na(PLACE_OF_ACQUISITION) ~ TRUE,
      is.na(CV_SOURCE_INFECTION) ~ TRUE,
      grepl("^1101|^00038888", PLACE_OF_ACQUISITION) ~ TRUE,
      CV_SOURCE_INFECTION != 1 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(check) %>%
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
      CV_SOURCE_INFECTION == 5 ~ "local",
      grepl("^00038888", PLACE_OF_ACQUISITION) ~ "imported",
      !is.na(PLACE_OF_ACQUISITION) ~ "imported",
      is.na(PLACE_OF_ACQUISITION) ~ "local",
      is.na(CV_SOURCE_INFECTION) ~ "local"
    )
  )


df %>%
  dplyr::select(
    STATE,
    Diagnosis_Date,
    PLACE_OF_ACQUISITION,
    CV_SOURCE_INFECTION,
    import_status
  ) %>% 
  print

