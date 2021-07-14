# estimate NSW isolation effect
source("R/functions.R")

nsw_ll <- readxl::read_xlsx(
  "~/not_synced/nsw/data.xlsx",
  col_types = c(
    project_recid = "text",
    link_original = "text",
    link_location_date = "text",
    network = "numeric",
    network_source = "logical",
    CLASSIFICATION = "text",
    AGE_AT_EVENT_YEARS = "numeric",
    GENDER = "text",
    LHD_2010_NAME = "text",
    SA2_2016_CODE = "text",
    SA2_2016_NAME = "text",
    EARLIEST_CONFIRMED_OR_PROBABLE = "date",
    CALCULATED_ONSET_DATE = "date",
    SYMPTOMS = "logical",
    SYMPTOM_ONSET_DATE = "date",
    LAST_CONTACT_WITH_CASE_DATE = "date",
    SETTING_OF_TRANSMISSION_DATE = "date",
    INITIAL_CLOSE_CONTACT_INTERVIEW_DATE = "date",
    INTERVIEWED_DATE = "date",
    DATE_ISOLATION_BEGAN = "date",
    PERSON_IS_HOUSEHOLD_CONTACT_OF_CONFIRMED_CASE = "logical",
    PERSON_IS_SOCIAL_CONTACT_OF_CONFIRMED_CASE = "logical",
    PERSON_IS_HIGHER_RISK_CLOSE_CONTACT_OF_SOURCE_CASE = "logical",
    PLACE_ACQUISITION = "text",
    LIKELY_SOURCE_OF_INFECTION_LOCAL = "text",
    SETTING_OF_TRANSMISSION = "text",
    SUB_SETTING_OF_TRANSMISSION = "text",
    SUB_SETTING_OF_TRANSMISSION_DESC = "text",
    SETTING_OF_TRANSMISSION_LOCATION = "text",
    SETTING_OF_TRANSMISSION_MULTIPLE_LOCATIONS = "text",
    SETTING_OF_TRANSMISSION_WORK_RELATED = "text",
    SETTING_OF_TRANSMISSION_WORK_RELATED_SPECIFY = "text",
    SETTING_OF_TRANSMISSION_MULTIPLE_DATES = "text",
    SETTING_OF_TRANSMISSION_LHD = "text",
    earliest_detected = "date",
    geocoded_setting_of_transmission = "text",
    geocoded_sub_setting_of_transmission = "text",
    geocoded_name = "text",
    geocoded_address = "text",
    geocoded_lng = "numeric",
    geocoded_lat = "numeric",
    link_date = "date",
    link_result = "text",
    ct_date = "date",
    ct_confirmed = "logical",
    ct_platform = "text",
    ct_e = "text",
    ct_n = "text",
    ct_rdrp = "text",
    ct_orf1 = "text",
    ct_s = "text",
    ct_orf8 = "text"
  )
) %>%
  filter(
    CLASSIFICATION == "Case",
    PLACE_ACQUISITION == "Acquired in NSW" 
  ) %>%
  select(
    date_infection = SETTING_OF_TRANSMISSION_DATE,
    date_onset = SYMPTOM_ONSET_DATE,
    date_isolation = DATE_ISOLATION_BEGAN,
    date_detection = EARLIEST_CONFIRMED_OR_PROBABLE
  ) %>%
  mutate_all(
    as_date
  ) %>%
  # if any infection dates are after onset/isolation/detection,
  # set the infection date to NA (and remove)
  mutate(
    date_infection = case_when(
      date_infection > date_onset ~ as_date(NA),
      date_infection > date_isolation ~ as_date(NA),
      date_infection > date_detection ~ as_date(NA),
      date_infection < as.Date("2020-01-01") ~ as_date(NA),
      TRUE ~ date_infection
    )
  ) %>%
  # compute time from modelled onset (5 days after infection) to isolation
  mutate(
    date_onset = date_infection + 5,
    time_to_isolation = as.numeric(date_isolation - date_onset),
    time_to_detection = as.numeric(date_detection - date_onset),
    state = "NSW"
  ) %>%
  filter(
    !is.na(time_to_isolation)
  ) 

# compute and plot delays over time in NSW
isolation_delays_from_onset <- estimate_delays(
  state = nsw_ll$state,
  date = nsw_ll$date_onset,
  delay = nsw_ll$time_to_isolation,
  direction = "forward",
  revert_to_national = FALSE
)

detection_delays_from_onset <- estimate_delays(
  state = nsw_ll$state,
  date = nsw_ll$date_onset,
  delay = nsw_ll$time_to_detection,
  direction = "forward",
  revert_to_national = FALSE
)

p_isolation <- isolation_delays_from_onset %>%
  filter(
    !use_national
  ) %>%
  plot_delays(
    date = nsw_ll$date_onset,
    state = nsw_ll$state,
    delay = nsw_ll$time_to_isolation,
    ylim = c(-5, 20),
    base_colour = "purple"
  ) +
  ggtitle(
    label = "Isolation trend",
    subtitle = "Time from modelled symptom onset to isolation for locally-acquired cases"
  ) +
  xlab("Symptom onset date") +
  ylab("Days to case isolation")

ggsave(
  filename = "nsw_detailed_time_to_isolation.png",
  plot = p_isolation,
  path = "outputs/figures",
  width = 6, height = 5,
  bg = "white"
)


p_detection <- detection_delays_from_onset %>%
  filter(
    !use_national
  ) %>%
  plot_delays(
    date = nsw_ll$date_onset,
    state = nsw_ll$state,
    delay = nsw_ll$time_to_detection,
    ylim = c(-5, 20)
  ) +
  ggtitle(
    label = "Surveillance trend",
    subtitle = "Time from modelled symptom onset to detection for locally-acquired cases"
  ) +
  xlab("Symptom onset date") +
  ylab("Days to case detection")

ggsave(
  filename = "nsw_detailed_time_to_detection.png",
  plot = p_detection,
  path = "outputs/figures",
  width = 6, height = 5,
  bg = "white"
)

# compute overall ecdf
optimal_isolation_ecdf <- ecdf(nsw_ll$time_to_isolation)

surveillance_cdfs <- readRDS("outputs/delay_from_onset_cdfs.RDS")
head(surveillance_cdfs)
surveillance <- surveillance_effect(
  dates = seq(
    min(surveillance_cdfs$date),
    max(surveillance_cdfs$date),
    by = 1
  ),
  states = unique(surveillance_cdfs$state),
  cdf = gi_cdf
)


# convert surveillance effect to weights (to represent how the effectiveness
# of the contact tracing system changed over time) and compute a weighted
# time-to-isolation cdf for each date and state
isolation_cdfs <- surveillance_cdfs %>%
  ungroup() %>%
  rename(
    surveillance_cdf = ecdf
  ) %>%
  mutate(
    surveillance_effect = c(t(surveillance)),
    ideal_isolation_ecdf = list(optimal_isolation_ecdf),
    isolation_weight = 1 - surveillance_effect,
    isolation_weight = isolation_weight / max(isolation_weight),
    isolation_ecdf = mapply(
      FUN = weight_ecdf,
      surveillance_cdf,
      ideal_isolation_ecdf,
      1 - isolation_weight,
      SIMPLIFY = FALSE
    )
  ) %>%
  select(
    date,
    state,
    ecdf = isolation_ecdf
  )

# save this
saveRDS(isolation_cdfs, "outputs/isolation_cdfs.RDS")

# compute the additional isolation effect as a check
extra_isolation <- extra_isolation_effect(
  dates = seq(
    min(surveillance_cdfs$date),
    max(surveillance_cdfs$date),
    by = 1
  ),
  states = unique(surveillance_cdfs$state),
  cdf = gi_cdf
)

# range of percentage reductions:
# effect of detection and isolation of cases:
range(100 * (1 - surveillance))
# additional effect of isolating case contacts before they test positive
range(100 * (1 - extra_isolation))
