
source("R/functions.R")

omicron_raw <- read_xlsx(
  path = "~/not_synced/omicron_linelist/Copy of Omicron Tracker 13 December 8am_update.xlsx",
  sheet = "Omicron tracker",
  skip = 1,
  col_names = c(
    #"id",
    #"category",
    "ncims",
    "test_date",
    "test_notification",
    "source",
    "onset_date",
    "adj_onset_date",
    "infection_date"
  ),
  col_types = c(
    #"numeric",
    #"text",
    "text",
    "date",
    "date",
    "text",
    "date",
    "date",
    "date"
  )
) %>%
  filter(!is.na(test_date)) %>%
  mutate(
    date_onset = as.Date(adj_onset_date),
    date_detection = as.Date(test_date),
    date_confirmation = if_else(!is.na(as.Date(test_notification)), as.Date(test_notification), as.Date(test_date)),
    state = "NSW",
    import_status = "local",
    report_delay = as.numeric(date_confirmation - date_onset),
    interstate_import = ifelse(source == "Overseas", TRUE, FALSE),
    interstate_import = ifelse(is.na(interstate_import), FALSE, interstate_import)
  ) %>%
  select(
    date_onset,
    date_detection,
    date_confirmation,
    state,
    import_status,
    report_delay,
    interstate_import
  )


omicron_raw$interstate_import[1] <- TRUE

earlierst_omicron <- min(omicron_raw$date_onset, omicron_raw$date_detection, na.rm = TRUE)

latest_delta <- earlierst_omicron - 28

nindss_linelist <- load_linelist(use_vic = FALSE, use_nsw = FALSE)

omicron_linelist <- nindss_linelist %>%
  filter(
    !(state == "NSW" & date_detection >= latest_delta),
    !(state == "NSW" & date_onset >= latest_delta),
    !(state == "NSW" & date_confirmation >= latest_delta)
  ) %>%
  bind_rows(
    omicron_raw
  ) %>%
  arrange(state, date_confirmation)

#bastartised reff_model_data

linelist_raw <- omicron_linelist
n_weeks_ahead <- 6
inducing_gap <- 3
  
  linelist_date <- max(linelist_raw$date_linelist, na.rm = TRUE)
  
  # load modelled google mobility data 
  mobility_data <- readRDS("outputs/google_change_trends.RDS")
  
  # compute delays from symptom onset to detection for each state over time
  notification_delay_cdf <- get_notification_delay_cdf(linelist_raw)
  
  # impute onset dates and infection dates using this
  linelist <- linelist_raw %>%
    impute_linelist(notification_delay_cdf = notification_delay_cdf)
  # truncate mobility data to no later than the day before the linelist (needed
  # for modelling on historic linelists) and then get the most recent date
  latest_mobility_date <- mobility_data %>%
    filter(date < linelist_date) %>%
    pull(date) %>%
    max()
  
  # get linelist date and state information
  earliest_date <- min(linelist$date)
  latest_date <- max(linelist$date)
  
  states <- sort(unique(linelist$state))
  dates <- seq(earliest_date, latest_date, by = 1)
  mobility_dates <- seq(earliest_date, latest_mobility_date, by = 1)
  
  n_states <- length(states)
  n_dates <- length(dates)
  n_extra <- as.numeric(Sys.Date() - max(dates)) + 7 * n_weeks_ahead
  date_nums <- seq_len(n_dates + n_extra)
  dates_project <- earliest_date + date_nums - 1
  n_dates_project <- n_date_nums <- length(date_nums)
  
  # build a vector of inducing points, regularly spaced over time but with one on
  # the most recent date
  inducing_date_nums <- rev(seq(n_date_nums, 1, by = -inducing_gap))
  n_inducing <- length(inducing_date_nums)
  
  # get detection probabilities for these dates and states
  detection_prob_mat <- detection_probability_matrix(
    latest_date = linelist_date - 1,
    infection_dates = dates,
    states = states,
    notification_delay_cdf = notification_delay_cdf
  )
  
  # subset to dates with reasonably high detection probabilities in some states
  detectable <- detection_prob_mat >= 0.5
  
  # the last date with infection data we include
  last_detectable_idx <- which(!apply(detectable, 1, any))[1]
  latest_infection_date <- dates[ifelse(is.na(last_detectable_idx), length(dates), last_detectable_idx)]
  
  # those infected in the state
  local_cases <- linelist %>%
    filter(!interstate_import) %>%
    infections_by_region(
      region_type = "state",
      case_type = "local"
    )
  
  # and those infected in any state, but infectious in this one
  local_cases_infectious <- linelist %>%
    infections_by_region(
      region_type = "state",
      case_type = "local"
    )
  
  # those imported (only considered infectious, but with a different Reff)
  imported_cases <- linelist %>%
    infections_by_region(
      region_type = "state",
      case_type = "imported"
    )
  
  # correct Reff denominator for right-truncation (infectors not yet detected) by
  # expectation (resolving divide-by-zero error)
  detection_prob_mat[] <- pmax(detection_prob_mat, 1e-6)
  local_cases_infectious_corrected <- local_cases_infectious /  detection_prob_mat
  imported_cases_corrected <- imported_cases / detection_prob_mat
  
  # disaggregate imported and local cases according to the generation interval
  # probabilities to get the expected number of infectious people in each state
  # and time
  #tti_cdfs <- readRDS("outputs/isolation_cdfs.RDS")
  
  local_infectiousness <- gi_convolution(
    local_cases_infectious_corrected,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf#,
    #ttd_cdfs = tti_cdfs
  )
  
  imported_infectiousness <- gi_convolution(
    imported_cases_corrected,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf#,
  )
  
  # elements to exclude due to a lack of infectiousness
  local_valid <- is.finite(local_infectiousness) & local_infectiousness > 0
  import_valid <- is.finite(imported_infectiousness) & imported_infectiousness > 0
  valid_mat <- (local_valid | import_valid) & detectable
  
  # data on quarantine spillovers (import-local infections) and imported cases
  # since mandatory hotel quarantine was implemented hotel quarantine
  n_hotel_spillovers <- nrow(hotel_quarantine_spillover_data())
  hotel_quarantine_start_date <- max(quarantine_dates()$date)
  n_hotel_cases <- sum(imported_cases[dates >= hotel_quarantine_start_date, ])
  
  vaccine_effect_timeseries <- readRDS("outputs/vaccine_effect_timeseries.RDS")
  
  vaccine_effect_matrix <- vaccine_effect_timeseries %>%
    pivot_wider(
      names_from = state,
      values_from = effect
    ) %>%
    full_join(
      y = tibble(date = dates_project)
    ) %>%
    arrange(date) %>%
    tidyr::fill(
      everything(),
      .direction = "updown"
    ) %>%
    dplyr::select(-date) %>%
    as.matrix
  
  vaccine_dates <- unique(vaccine_effect_timeseries$date)
  
  # return a named, nested list of these objects
  data_omicron <- list(
    local = list(
      cases = local_cases,
      cases_infectious = local_cases_infectious,
      infectiousness = local_infectiousness
    ),
    imported = list(
      cases = imported_cases,
      infectiousness = imported_infectiousness,
      total_hotel_cases = n_hotel_cases,
      total_hotel_spillovers = n_hotel_spillovers
    ),
    detection_prob_mat = detection_prob_mat,
    valid_mat = valid_mat,
    states = states,
    dates = list(
      infection = dates,
      infection_project = dates_project,
      onset = dates + 5,
      date_nums = date_nums,
      inducing_date_nums = inducing_date_nums,
      mobility = mobility_dates,
      earliest = earliest_date,
      latest = latest_date,
      latest_mobility = latest_mobility_date,
      latest_infection = latest_infection_date,
      latest_project = max(dates_project),
      linelist = linelist_date,
      vaccine_dates = vaccine_dates
    ),
    n_dates = n_dates,
    n_states = n_states,
    n_date_nums = n_date_nums,
    n_dates_project = n_dates_project,
    n_inducing =  n_inducing,
    vaccine_effect_matrix = vaccine_effect_matrix
  )
  




source("R/lib.R")

set.seed(2020-04-29)
source("R/functions.R")

data_omicron$dates$linelist

# save the key dates for Freya and David to read in, and tabulated local cases
# data for the Robs

#write_reff_key_dates(data_omicron)
write_local_cases(data_omicron)

# format and write out any new linelists to the past_cases folder for Rob H
#update_past_cases()

# define the model (and greta arrays) for Reff, and sample until convergence
fitted_model_omicron <- fit_reff_model(data_omicron, max_tries = 1)

# save the fitted model object
saveRDS(fitted_model_omicron, "outputs/fitted_reff_model_omicron.RDS")



# generatge sims for plotting
# (saves repeat generation of sims in each reff_plotting call and keeps them consistent)
sims <- reff_plotting_sims(fitted_model_omicron)

# # do plots for main period
# reff_plotting(
#   fitted_model_omicron,
#   dir = "outputs",
#   sims = sims
# )
# 
# # most recent six months
# reff_plotting(
#   fitted_model_omicron,
#   dir = "outputs",
#   subdir = "figures/six_month",
#   min_date = NA,
#   sims = sims
# )

#plot to 
reff_plotting(
  fitted_model_omicron,
  dir = "outputs",
  subdir = "figures/omicron_nsw",
  min_date = as.Date("2021-11-01"),
  max_date = fitted_model_omicron$data$dates$latest_infection,
  mobility_extrapolation_rectangle = FALSE,
  sims = sims
)

# last plotting date
fitted_model_omicron$data$dates$latest_infection

write_reff_sims(fitted_model_omicron, dir = "outputs/projection")

########

omicron_ncims <- read_xlsx(
  path = "~/not_synced/omicron_linelist/Copy of Omicron Tracker 13 December 8am_update.xlsx",
  sheet = "Omicron tracker",
  skip = 1,
  col_names = c(
    #"id",
    #"category",
    "ncims",
    "test_date",
    "test_notification",
    "source",
    "onset_date",
    "adj_onset_date",
    "infection_date"
  ),
  col_types = c(
    #"numeric",
    #"text",
    "text",
    "date",
    "date",
    "text",
    "date",
    "date",
    "date"
  )
) %>%
  filter(!is.na(test_date)) %>%
  mutate(
    date_onset = as.Date(adj_onset_date),
    date_detection = as.Date(test_date),
    date_confirmation = if_else(!is.na(as.Date(test_notification)), as.Date(test_notification), as.Date(test_date)),
    state = "NSW",
    import_status = "local",
    report_delay = as.numeric(date_confirmation - date_onset),
    interstate_import = ifelse(source == "Overseas", TRUE, FALSE),
    interstate_import = ifelse(is.na(interstate_import), FALSE, interstate_import)
  )

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


nsw_ncims <- file %>%
  read_csv(
    col_types = cols(
      .default = col_character(),
      CASE_ID = col_double(),
      EARLIEST_CONFIRMED_OR_PROBABLE = col_nsw_date(),
      SYMPTOM_ONSET_DATE = col_nsw_date(),
      CALCULATED_ONSET_DATE = col_nsw_date(),
      AGE_AT_EVENT_YEARS = col_double(),
      DATE_ISOLATION_BEGAN = col_nsw_date(),
      #SETTING_OF_TRANSMISSION_DATE = col_nsw_date("long"),
      SETTING_OF_TRANSMISSION_DATE = col_nsw_date(),
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
      #TRUE ~ CALCULATED_ONSET_DATE
      TRUE ~ SYMPTOM_ONSET_DATE
    ),
    ncims = CASE_ID,
    #date_onset = NA,
    date_detection = NA,
    date_confirmation = EARLIEST_CONFIRMED_OR_PROBABLE,
    date_quarantine = DATE_ISOLATION_BEGAN,
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
  )

nsw_corrected <- nsw_ncims %>%
  mutate(
    ncims = as.character(ncims)
  ) %>%
  left_join(
    omicron_ncims %>%
      select(ncims, date_onset),
    by = "ncims"
  ) %>%
  mutate(
    date_onset = if_else(
      !is.na(date_onset.y),
      date_onset.y,
      date_onset.x
    )
  ) %>%
  select(
    date_onset,
    date_detection,
    date_confirmation = EARLIEST_CONFIRMED_OR_PROBABLE,
    date_quarantine,
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

