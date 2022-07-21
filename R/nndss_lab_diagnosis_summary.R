# read in the latest linelist and format for analysis
library(tidyverse)
library(tidytext)
source("R/functions.R")
date = NULL
dir = "~/not_synced/nndss"
strict = TRUE
#missing_location_assumption = "imported"
missing_location_assumption = "local"
#missing_location_assumption = "missing",
location_conflict_assumption = "local"


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
    CONFIRMATION_STATUS = "text",
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
    CONFIRMATION_STATUS = "text",
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
  
  col_types_3 <- c(
    STATE = "text",
    POSTCODE = "numeric",
    CONFIRMATION_STATUS = "text",
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
    CV_SOURCE_INFECTION = "numeric",
    CV_SYMPTOMS_REPORTED = "numeric",
    CV_QUARANTINE_STATUS = "numeric",
    CV_DATE_ENTERED_QUARANTINE = "date"
  )
  
  col_types_4 <- c(
    STATE = "text",
    POSTCODE = "numeric",
    CONFIRMATION_STATUS = "text",
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
    CV_SOURCE_INFECTION = "numeric",
    CV_SYMPTOMS_REPORTED = "numeric",
    CV_QUARANTINE_STATUS = "numeric",
    CV_DATE_ENTERED_QUARANTINE = "date",
    "numeric",
    "text",
    "date",
    "numeric",
    "text",
    "date",
    "numeric",
    "text",
    "date",
    "numeric",
    "text",
    "date",
    "numeric",
    "text",
    "date"
  )
  
  col_types_5 <- c(
    #same as 4 but postcode in lower case
    STATE = "text",
    CONFIRMATION_STATUS = "text",
    Postcode = "numeric",
    TRUE_ONSET_DATE = "date",
    SPECIMEN_DATE = "date",
    NOTIFICATION_DATE = "date",
    NOTIFICATION_RECEIVE_DATE = "date",
    `DIAGNOSIS DATE` = "date",
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
    CV_SOURCE_INFECTION = "numeric",
    CV_SYMPTOMS_REPORTED = "numeric",
    CV_QUARANTINE_STATUS = "numeric",
    CV_DATE_ENTERED_QUARANTINE = "date",
    "numeric",
    "text",
    "date",
    "numeric",
    "text",
    "date",
    "numeric",
    "text",
    "date",
    "numeric",
    "text",
    "date",
    "numeric",
    "text",
    "date"
  )
}


ll_date <- data$date_time[[1]]


if (ll_date < "2021-03-08") {
  col_types <- col_types_1
} else if (ll_date < "2021-11-08") {
  col_types <- col_types_2
} else if (ll_date < "2021-12-02") {
  col_types <- col_types_3
} else if (ll_date < "2022-01-06") {
  col_types <- col_types_4
} else {
  col_types <- col_types_5
}


if (ll_date >= "2022-01-25") {
  #try csv
  # sheets <- readxl::excel_sheets(data$file)
  # dat <- lapply(sheets,
  #               function(X) readxl::read_xlsx(data$file,
  #                                             col_types = col_types,
  #                                             #na = "NULL",
  #                                             sheet = X))
  # dat <- dat %>% reduce(full_join)
  
  dat <- readr::read_csv(
    data$file,
    col_types = cols_only(
      STATE = col_character(),
      POSTCODE = col_double(),
      LAB_DIAGNOSIS_METHOD = col_character(),
      CONFIRMATION_STATUS = col_character(),
      TRUE_ONSET_DATE = col_date(format = "%d/%m/%Y"),
      SPECIMEN_DATE = col_date(format = "%d/%m/%Y"),
      NOTIFICATION_DATE = col_date(format = "%d/%m/%Y"),
      NOTIFICATION_RECEIVE_DATE = col_date(format = "%d/%m/%Y"),
      'DIAGNOSIS DATE' = col_date(format = "%d/%m/%Y"),
      AGE_AT_ONSET = col_double(),
      SEX = col_double(),
      DIED = col_double(),
      PLACE_OF_ACQUISITION = col_character(),
      HOSPITALISED = col_double(),
      CV_ICU = col_double(),
      CV_VENTILATED = col_double(),
      OUTBREAK_REF = col_character(),
      CASE_FOUND_BY = col_double(),
      CV_SYMPTOMS = col_character(),
      CV_OTHER_SYMPTOMS = col_character(),
      CV_COMORBIDITIES = col_character(),
      CV_OTHER_COMORBIDITIES = col_character(),
      CV_GESTATION = col_double(),
      #CV_CLOSE_CONTACT = "numeric"
      CV_EXPOSURE_SETTING = col_double(),
      CV_SOURCE_INFECTION = col_double(),
      CV_SYMPTOMS_REPORTED = col_double(),
      CV_QUARANTINE_STATUS = col_double(),
      CV_DATE_ENTERED_QUARANTINE = col_date(format = "%d/%m/%Y"),
      LOADED_DATE = col_date(format = "%Y-%m-%d %H:%M:%S")
    ),
    na = c("", "NULL") # usually turn this off
  )
  #%>% rename(POSTCODE = Postcode)
  
  # dat <- dat %>%
  #   mutate(
  #     Postcode = as.numeric(Postcode),
  #     TRUE_ONSET_DATE = as.Date(TRUE_ONSET_DATE, format = "%d/%m/%Y"),
  #     SPECIMEN_DATE  = as.Date(SPECIMEN_DATE, format = "%d/%m/%Y"),
  #     NOTIFICATION_DATE  = as.Date(NOTIFICATION_DATE, format = "%d/%m/%Y"),
  #     NOTIFICATION_RECEIVE_DATE = as.Date(NOTIFICATION_DATE, format = "%d/%m/%Y"),
  #     CV_DATE_ENTERED_QUARANTINE  = as.Date(CV_DATE_ENTERED_QUARANTINE, format = "%d/%m/%Y"),
  #     CV_SOURCE_INFECTION = as.numeric(CV_SOURCE_INFECTION)
  #   )
  
} else if (ll_date < "2022-01-06" |
           ll_date > "2022-01-07" & ll_date < "2022-01-25") {
  if (length(readxl::excel_sheets(data$file)) == 1) {
    #handle multiple sheets
    dat <- readxl::read_xlsx(data$file,
                             col_types = col_types#,
                             #na = "NULL" # usually turn this off)
  } else {
    #deal with multiple sheets
    sheets <- readxl::excel_sheets(data$file)
    dat <- lapply(sheets,
                  function(X)
                    readxl::read_xlsx(data$file,
                                      col_types = col_types,
                                      #na = "NULL",
                                      sheet = X))
    dat <- dat %>% reduce(full_join)
  }
}
# } else { #read the xls format starting from 06-01-2022
#   dat <- readr::read_csv(
#     data$file,
#     col_types = cols_only(
#       STATE = col_character(),
#       Postcode = col_double(),
#       CONFIRMATION_STATUS = col_character(),
#       TRUE_ONSET_DATE = col_date(format = "%d/%m/%Y"),
#       SPECIMEN_DATE = col_date(format = "%d/%m/%Y"),
#       NOTIFICATION_DATE = col_date(format = "%d/%m/%Y"),
#       NOTIFICATION_RECEIVE_DATE = col_date(format = "%d/%m/%Y"),
#       'DIAGNOSIS DATE' = col_date(format = "%d/%m/%Y"),
#       AGE_AT_ONSET = col_double(),
#       SEX = col_double(),
#       DIED = col_double(),
#       PLACE_OF_ACQUISITION = col_character(),
#       HOSPITALISED = col_double(),
#       CV_ICU = col_double(),
#       CV_VENTILATED = col_double(),
#       OUTBREAK_REF = col_character(),
#       CASE_FOUND_BY = col_double(),
#       CV_SYMPTOMS = col_character(),
#       CV_OTHER_SYMPTOMS = col_character(),
#       CV_COMORBIDITIES = col_character(),
#       CV_OTHER_COMORBIDITIES = col_character(),
#       CV_GESTATION = col_double(),
#       #CV_CLOSE_CONTACT = "numeric"
#       CV_EXPOSURE_SETTING = col_double(),
#       CV_SOURCE_INFECTION = col_double(),
#       CV_SYMPTOMS_REPORTED = col_double(),
#       CV_QUARANTINE_STATUS = col_double(),
#       CV_DATE_ENTERED_QUARANTINE = col_date(format = "%d/%m/%Y")),
#     na = "NULL" # usually turn this off
#   ) %>% rename(POSTCODE = Postcode)
# }

if (ll_date < "2021-03-08") {
  dat <- dat %>%
    mutate(CV_SOURCE_INFECTION = NA_real_)
}

# if(ll_date > "2022-01-07"){ #fix changed postcode colname
#   dat <- dat %>%
#     rename(POSTCODE = Postcode) %>%
#     mutate(POSTCODE = as.numeric(POSTCODE)) #not sure why this breaks down
# }

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
    SPECIMEN_DATE = clean_date(SPECIMEN_DATE),
    CV_DATE_ENTERED_QUARANTINE = clean_date(CV_DATE_ENTERED_QUARANTINE)
  ) %>%
  mutate(#tidy up PoA codes - maybe fixed at some point
    PLACE_OF_ACQUISITION = ifelse(
      nchar(PLACE_OF_ACQUISITION) == 5,
      paste0("000", PLACE_OF_ACQUISITION),
      PLACE_OF_ACQUISITION
    )) %>%
  mutate(
    import_status = case_when(
      # return "ERROR" if place of acquisition and cv_source_infection
      # indicate opposite import statuses
      grepl("^1101", PLACE_OF_ACQUISITION) &
        CV_SOURCE_INFECTION == 1 ~ "ERROR",!is.na(PLACE_OF_ACQUISITION) &
        !grepl("^1101|^00038888", PLACE_OF_ACQUISITION) &
        CV_SOURCE_INFECTION == 2 ~ "ERROR",!is.na(PLACE_OF_ACQUISITION) &
        !grepl("^1101|^00038888", PLACE_OF_ACQUISITION) &
        CV_SOURCE_INFECTION == 3 ~ "ERROR",!is.na(PLACE_OF_ACQUISITION) &
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
      grepl("^00038888", PLACE_OF_ACQUISITION) ~ missing_location_assumption,!is.na(PLACE_OF_ACQUISITION) ~ "imported",
      is.na(PLACE_OF_ACQUISITION) ~ missing_location_assumption,
      is.na(CV_SOURCE_INFECTION) ~ missing_location_assumption
    )#,
    # import_status = case_when(
    #   import_status == "missing" & STATE == "WA" ~ "local",
    #   import_status == "missing" & STATE != "WA" ~ "imported",
    #   TRUE ~ import_status
    # )
  ) %>%
  mutate(
    import_status = ifelse(
      import_status == "ERROR",
      location_conflict_assumption,
      import_status
    )
  )

list(data = data,
     dat = dat) -> raw_nndss_list

raw_nndss <- raw_nndss_list$dat %>%
  # notification receive date seems buggy, and is sometimes before the
  # notification date and specimen collection date
  mutate(date_confirmation = pmax(NOTIFICATION_RECEIVE_DATE,
                                  NOTIFICATION_DATE,
                                  na.rm = TRUE),) %>%
  rename(
    date_onset = TRUE_ONSET_DATE,
    date_detection = SPECIMEN_DATE,
    date_quarantine = CV_DATE_ENTERED_QUARANTINE,
    state = STATE
  ) %>%
  mutate(state = as.factor(state)) %>%
  
  # Remove those with onset date after confirmation date
  # only keep individuals with date of confirmation after onset date if less than 2 days (inclusive) because
  # we assume some individuals tested via contact tracing will test positive before symptom onset and therefore plausible
  # (noting that reporting delay distribution only calculated from positive differences)
  # also remove any individuals with NA for both notification and symptom onset dates
  # filter(
  #   date_confirmation >= (date_onset - 2) | is.na(date_confirmation) | is.na(date_onset)
  # ) %>%
  filter(!(is.na(date_confirmation) & is.na(date_onset))) %>%
  mutate_at(vars(starts_with("date_")),
            ~ as.Date(.)) %>%
  # for cases missing a date of detection, assume it's the day before the date
  # of confirmation (1 days is the median and mode of this delay distribution)
  mutate(date_detection = case_when(
    is.na(date_detection) ~ date_confirmation - 1,
    TRUE ~ date_detection
  ))



# summary

sort(unique(raw_nndss$LAB_DIAGNOSIS_METHOD)) -> lab_methods

raw_nndss %>%
  select(state,
         CONFIRMATION_STATUS,
         date_confirmation,
         LAB_DIAGNOSIS_METHOD) %>%
  mutate(LAB_DIAGNOSIS_METHOD = replace_na(LAB_DIAGNOSIS_METHOD, "NULL")) %>%
  mutate(index = row_number()) -> assess_raw_nndss

glimpse(raw_nndss)




fix_lab_grouping_data <- assess_raw_nndss %>%
  mutate(lab_method_token = strsplit(LAB_DIAGNOSIS_METHOD, "\\|")) %>%
  unnest((lab_method_token)) %>%
  group_by(index) %>%
  arrange(lab_method_token) %>%
  mutate(type = paste(lab_method_token, collapse = " | "))

gc()


recode_lab_data <- fix_lab_grouping_data %>%
  mutate(
    lab_methods_type = case_when(
      type %in% c(
        "NUCLEIC ACID TESTING | NUCLEIC ACID TESTING",
        "NUCLEIC ACID TESTING | NUCLEIC ACID TESTING | NUCLEIC ACID TESTING"
      ) ~ "NUCLEIC ACID TESTING",
      type %in% c(
        "NUCLEIC ACID TESTING | SEROLOGY | SEROLOGY",
        "NUCLEIC ACID TESTING | NUCLEIC ACID TESTING | SEROLOGY"
      ) ~ "NUCLEIC ACID TESTING | SEROLOGY",
      TRUE ~ as.character(type)
    )
  )

recode_lab_data <- recode_lab_data %>%
  arrange(index) %>%
  distinct(
    state,
    CONFIRMATION_STATUS,
    date_confirmation,
    LAB_DIAGNOSIS_METHOD,
    lab_methods_type
  )

unique(recode_lab_data$lab_methods_type) -> lab_methods_new

sort(lab_methods_new)

saveRDS(recode_lab_data, file = "outputs/recode_lab_data.rds")
readRDS("outputs/recode_lab_data.rds") -> recode_lab_data

recode_lab_data %>%
  mutate(
    new_group = case_when(
      str_detect(lab_methods_type, "NUCLEIC ACID TESTING") &
        str_detect(lab_methods_type, "ANTIGEN DETECTION") ~ "has_antigen_and_nucleic",
      lab_methods_type == "NUCLEIC ACID TESTING" ~ "only_nucleic",
      lab_methods_type == "ANTIGEN DETECTION" ~ "only_antigen",
      str_detect(lab_methods_type, "ANTIGEN DETECTION") &
        !str_detect(lab_methods_type, "NUCLEIC ACID TESTING") ~ "has_antigen_others",
      str_detect(lab_methods_type, "NUCLEIC ACID TESTING") &
        !str_detect(lab_methods_type, "ANTIGEN DETECTION") ~ "has_nucleic_others",
      lab_methods_type %in% c("NULL", "UNKNOWN") ~ "UNKNOWN",
      TRUE ~ "Others"
    )
  ) -> test

lab_cat_data <- test

saveRDS(lab_cat_data, file = "outputs/lab_cat_data.RDS")

lab_cat_data %>%
  ungroup() %>%
  distinct(lab_methods_type, new_group) -> check_data
# Read the data

readRDS("outputs/lab_cat_data.RDS") -> lab_cat_data
unique(lab_cat_data$CONFIRMATION_STATUS)
probable_cases <-  lab_cat_data %>%
  filter(CONFIRMATION_STATUS == "PROBABLE")
lab_cat_data %>%
  group_by(state, CONFIRMATION_STATUS, new_group, date_confirmation) %>%
  summarise(n_cases = n()) -> state_lab_methods


levels <- c(
  "only_nucleic",
  "has_nucleic_others" ,
  "has_antigen_and_nucleic",
  "only_antigen",
  "has_antigen_others",
  "Others",
  "UNKNOWN"
)
state_lab_methods %>%
  group_by(state, CONFIRMATION_STATUS, date_confirmation) %>%
  summarise(total_cases = sum(n_cases)) -> state_total_cases

state_lab_methods %>%
  left_join(state_total_cases,
            by = c("state", "CONFIRMATION_STATUS", "date_confirmation")) %>%
  mutate(new_group = as.factor(new_group),
         lab_method_proportion = n_cases / total_cases) %>%
  dplyr::mutate(new_group = forcats::fct_relevel(new_group, levels)) ->
  daily_state_lab_proportion


# state_lab_proportion%>%
#   filter(state=="ACT")%>%
#   filter(lab_methods_type=="SEROLOGY")

# recode_lab_data%>%
#   filter(state=="NSW")%>%
#   ggplot(aes(x=date_confirmation,fill=lab_methods_type))+
#   geom_bar(stat="count")+
#   facet_wrap(~CONFIRMATION_STATUS,scales = "free")+
#   theme(legend.position="bottom")

daily_state_lab_proportion %>%
  filter(state == "NSW") %>%
  filter(date_confirmation >= "2021-12-01") %>%
  ggplot(aes(x = date_confirmation, y = lab_method_proportion, fill = new_group)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap( ~ CONFIRMATION_STATUS, scales = "fixed") +
  theme(legend.position = "bottom") + scale_fill_brewer(palette = "Dark2")

daily_state_lab_proportion %>%
  ggplot(aes(x = date_confirmation, y = lab_method_proportion, fill = new_group)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(state ~ CONFIRMATION_STATUS, scales = "fixed") +
  theme(legend.position = "bottom") + scale_fill_brewer(palette = "Dark2")

# lab_cat_data %>%
#   filter(date_confirmation>="2021-12-01")%>%
#   group_by(state,CONFIRMATION_STATUS,new_group,date_confirmation)%>%
#   summarise(n_cases=n())->Post_Dec
#
# lab_cat_data %>%
#   filter(date_confirmation<"2021-12-01")%>%
#   group_by(state,CONFIRMATION_STATUS,new_group,date_confirmation)%>%
#   summarise(n_cases=n())->Pre_Dec


cols <-
  c("#233d4d",
    "#fe7f2d",
    "#fcca46",
    "#a1c181",
    "#619b8a",
    "#6e0e0a",
    "#00509d")


daily_state_lab_proportion %>%
  filter(CONFIRMATION_STATUS == "PROBABLE") %>%
  ggplot(aes(
    x = date_confirmation,
    y = log(n_cases),
    color = new_group
  )) +
  #geom_bar(position="stack", stat="identity")+
  geom_point(size = 0.9) +
  facet_wrap( ~ state, scales = "free_y", nrow = 4) +
  theme(legend.position = "bottom") + scale_color_brewer(palette = "Dark2") +
  geom_vline(aes(xintercept = as.Date("2021-12-01"))) +
  ggtitle("Probable cases") + theme_bw() +
  ylab("Log(Cases)") +
  guides(col = guide_legend("Lab Methods")) -> state_probable

state_probable
ggsave(
  "outputs/figures/log_probable_cases_lab_methods.png",
  bg = 'white',
  height = 8,
  width = 20
)

daily_state_lab_proportion %>%
  filter(CONFIRMATION_STATUS == "CONFIRMED") %>%
  ggplot(aes(
    x = date_confirmation,
    y = log(n_cases),
    color = new_group
  ), alpha = 0.4) +
  #geom_bar(position="stack", stat="identity")+
  geom_point(size = 0.9) +
  facet_wrap( ~ state, scales = "free_y", nrow = 4) +
  theme(legend.position = "bottom") + scale_color_brewer(palette = "Dark2") +
  geom_vline(aes(xintercept = as.Date("2021-12-01"))) +
  ggtitle("Confirmed cases") + theme_bw() +
  ylab("Log(Cases)") +
  guides(col = guide_legend("Lab Methods")) -> state_confirmed

ggsave(
  "outputs/figures/log_confirmed_cases_lab_methods.png",
  bg = 'white',
  height = 8,
  width = 20
)

daily_state_lab_proportion %>%
  filter(CONFIRMATION_STATUS == "PROBABLE", state == "ACT") -> probable_act



state_probable

# Pre-Post


daily_state_lab_proportion %>%
  filter(CONFIRMATION_STATUS == "PROBABLE") %>%
  filter(date_confirmation < as.Date("2021-12-01")) %>%
  ggplot(aes(
    x = date_confirmation,
    y = log(n_cases),
    color = new_group
  ), alpha = 0.9) +
  #geom_bar(position = "stack", stat = "identity") +
  geom_point(size = 0.9) +
  facet_wrap(~ state, scales = "free", nrow = 4) +
  theme(legend.position = "bottom") + scale_color_brewer(palette = "Dark2") +
  ggtitle("Probable cases", subtitle = "Pre 01 Dec 2021") + theme_bw() +
  ylab("Log(Cases)") +
  guides(col = guide_legend("Lab Methods")) -> pre_dec_state_probable

pre_dec_state_probable

ggsave(
  "outputs/figures/pre_dec_state_probable.png",
  bg = 'white',
  height = 8,
  width = 20
)

daily_state_lab_proportion %>%
  filter(CONFIRMATION_STATUS == "PROBABLE") %>%
  filter(date_confirmation >= as.Date("2021-12-01")) %>%
  ggplot(aes(
    x = date_confirmation,
    y = log(n_cases),
    color = new_group
  ), alpha = 0.9) +
  geom_point(size = 0.9) +
  facet_wrap(~ state, scales = "free", nrow = 4) +
  theme(legend.position = "bottom") + scale_color_brewer(palette = "Dark2") +
  ggtitle("Probable cases", subtitle = "01 Dec 2021 -Present") + theme_bw() +
  ylab("Log(Cases)") +
  guides(col = guide_legend("Lab Methods")) -> post_dec_state_probable

post_dec_state_probable
ggsave(
  "outputs/figures/post_dec_state_probable.png",
  bg = 'white',
  height = 8,
  width = 20
)
daily_state_lab_proportion %>%
  filter(CONFIRMATION_STATUS == "CONFIRMED") %>%
  filter(date_confirmation >= as.Date("2021-12-01")) %>%
  ggplot(aes(
    x = date_confirmation,
    y = log(n_cases),
    color = new_group
  ), alpha = 0.9) +
  geom_point(size = 0.9) +
  facet_wrap(~ state, scales = "free", nrow = 4) +
  theme(legend.position = "bottom") + scale_color_brewer(palette = "Dark2") +
  ggtitle("Confirmed cases", subtitle = " 01 Dec 2021 -Present") + theme_bw() +
  ylab("Log(Cases)") +
  guides(col = guide_legend("Lab Methods")) -> post_dec_state_confirmed

ggsave(
  "outputs/figures/post_dec_state_confirmed.png",
  bg = 'white',
  height = 8,
  width = 20
)

daily_state_lab_proportion %>%
  filter(CONFIRMATION_STATUS == "CONFIRMED") %>%
  filter(date_confirmation < as.Date("2021-12-01")) %>%
  ggplot(aes(
    x = date_confirmation,
    y = log(n_cases),
    color = new_group
  ), alpha = 0.4) +
  geom_point(size = 0.9) +
  facet_wrap(~ state, scales = "free", nrow = 4) +
  theme(legend.position = "bottom") + scale_color_brewer(palette = "Dark2") +
  ggtitle("Confirmed cases", subtitle = "Pre 01 Dec 2021") + theme_bw() +
  ylab("Log(Cases)") +
  guides(col = guide_legend("Lab Methods")) -> pre_dec_state_confirmed

ggsave(
  "outputs/figures/pre_dec_state_confirmed.png",
  bg = 'white',
  height = 8,
  width = 20
)


lab_cat_data %>%
  group_by(state, CONFIRMATION_STATUS, new_group) %>%
  summarise(cases = n()) %>%
  pivot_wider(names_from = new_group,
              values_from = cases,
              values_fill = 0) %>%
  mutate(
    total_cases = only_nucleic + Others + only_antigen + has_antigen_and_nucleic +
      has_nucleic_others + UNKNOWN + has_antigen_others
  ) %>%
  rename(unknown = UNKNOWN,
         others = Others) %>%
  relocate(
    state,
    CONFIRMATION_STATUS,
    only_nucleic,
    only_antigen,
    has_antigen_and_nucleic,
    has_nucleic_others,
    has_antigen_others,
    everything()
  ) -> total_cases_lab

lab_cat_data %>%
  group_by(state, CONFIRMATION_STATUS) %>%
  summarise(cases = n()) -> total_cases_check

lab_cat_data <-  lab_cat_data %>%
  mutate(timeline = case_when(
    date_confirmation >= as.Date("2021-12-01") ~ "Post Dec 2021",
    TRUE ~ as.character("Pre Dec 2021")
  ))

lab_cat_data %>%
  group_by(state, CONFIRMATION_STATUS, new_group, timeline) %>%
  summarise(n_cases = n()) -> cases_lab_methods

cases_lab_methods %>%
  group_by(state, CONFIRMATION_STATUS, timeline) %>%
  summarise(total_cases = sum(n_cases)) -> total_cases_lab_methods

options(scipen = 999)
cases_lab_methods %>%
  left_join(total_cases_lab_methods,
            by = c("state", "CONFIRMATION_STATUS", "timeline")) %>%
  mutate(
    new_group = as.factor(new_group),
    lab_method_percentage = 100 * (n_cases / total_cases)
  ) %>%
  select(state,
         CONFIRMATION_STATUS,
         timeline,
         new_group,
         lab_method_percentage) %>%
  pivot_wider(names_from = new_group,
              values_from = lab_method_percentage,
              values_fill = 0) %>%
  rename(unknown = UNKNOWN,
         others = Others) %>%
  relocate(
    state,
    CONFIRMATION_STATUS,
    timeline,
    only_nucleic,
    only_antigen,
    has_antigen_and_nucleic,
    has_nucleic_others,
    has_antigen_others,
    everything()
  ) -> total_state_lab_percentage


daily_state_lab_proportion %>%
  filter(date_confirmation >= as.Date("2021-12-01")) %>%
  mutate(log_cases = log(n_cases)) -> nlog

lab_cat_data %>%
  group_by(state, CONFIRMATION_STATUS, new_group, timeline) %>%
  summarise(cases = n()) %>%
  pivot_wider(names_from = new_group,
              values_from = cases,
              values_fill = 0) %>%
  mutate(
    total_cases = only_nucleic + Others + only_antigen + has_antigen_and_nucleic +
      has_nucleic_others + UNKNOWN + has_antigen_others
  ) %>%
  rename(unknown = UNKNOWN,
         others = Others) %>%
  relocate(
    state,
    CONFIRMATION_STATUS,
    timeline,
    only_nucleic,
    only_antigen,
    has_antigen_and_nucleic,
    has_nucleic_others,
    has_antigen_others,
    everything()
  ) -> timeline_total_cases_lab


cases_lab_methods %>%
  left_join(total_cases_lab_methods,
            by = c("state", "CONFIRMATION_STATUS", "timeline")) %>%
  mutate(
    new_group = as.factor(new_group),
    lab_method_percentage = 100 * (n_cases / total_cases)
  ) %>%
  select(
    state,
    CONFIRMATION_STATUS,
    n_cases,
    total_cases,
    timeline,
    new_group,
    lab_method_percentage
  ) %>%
  dplyr::mutate(new_group = forcats::fct_relevel(new_group, levels)) -> plot_percentage




plot_percentage %>%
  ggplot(aes(
    x = timeline,
    y = n_cases / total_cases,
    fill = new_group
  )) +
  geom_bar(alpha = 0.8,
           position = "stack",
           stat = "identity") +
  scale_x_discrete(limits = c("Pre Dec 2021", "Post Dec 2021"))   +
  facet_grid(CONFIRMATION_STATUS ~ state, scales = "free") +
  ggtitle("Proportion of lab methods in case types",
          subtitle = "Pre & Post 01 Dec 2021") + theme_bw() + scale_fill_brewer(palette = "Dark2") +
  ylab("Proportion of Cases") +
  guides(fill = guide_legend("Lab Methods"))



# Tas -> no probable cases after december. ACT no probable cases pre december
