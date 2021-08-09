# given a raw (unimputed) linelist, prepare all the data needed for modelling
reff_model_data <- function(
  linelist_raw = load_linelist(),
  n_weeks_ahead = 6,
  inducing_gap = 3
) {
  
  linelist_date <- max(linelist_raw$date_linelist)
  
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
  tti_cdfs <- readRDS("outputs/isolation_cdfs.RDS")
  
  local_infectiousness <- gi_convolution(
    local_cases_infectious_corrected,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    ttd_cdfs = tti_cdfs
  )
  
  imported_infectiousness <- gi_convolution(
    imported_cases_corrected,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    ttd_cdfs = tti_cdfs
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
  
  # return a named, nested list of these objects
  list(
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
      linelist = linelist_date
    ),
    n_dates = n_dates,
    n_states = n_states,
    n_date_nums = n_date_nums,
    n_dates_project = n_dates_project,
    n_inducing =  n_inducing
  )
  
}
