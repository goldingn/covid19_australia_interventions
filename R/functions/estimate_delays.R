# Calculate time-varying delay distributions for each state, smoothly reverting
# to the national average when and where there are insufficient records to
# compute state-level distributions. Return a tibble of empirical CDFs of the
# distribution by state and date.
# Parameters:
# state, date, delay: vectors of equal length giving the data on observed delays
#   by date and state
# all_dates, all_states: optional vectors of dates and states for which to compute
#   delays (taken from linelist if not specified)
# direction: whether to tabulate delays by date in a 'forward' ('date' is at the
#   start of the delay) or 'backward' ('date' is at the end of the delay) manner
# delay_plausible_bounds: a vector of length 2 of plausible delays. Records with
#   values outside these bounds are assumed to be erroneous and removed.
# min_records: the minimum number of records required to reliably estimate the delay
#   distribution within a window
# absolute_min_records: the absolute minimum number of records to estimate a
#   state-level delay distribution within a window. If the number of records is
#   below this (even with the maximum window size), the national estimate is used
#   instead. If it is between this and 'min_records', the distribution is
#   estimated as a weighted average of the state and national distributions.
# min_window: the minimum window size (number of days wide) in which to estimate
#   the delay distribution
# max_window: the maximum window size in which to estimate the delay
#   distribution for each date at the state level. At the national level there is
#   no maximum applied
# national_exclusions: a tibble of states, start dates, and end dates denoting
#   times and places that should not contribute to the national estimate. If
#   either of the dates are NA, the earliest (or latest) dates in the linelist
#   are used
estimate_delays <- function(
  state,
  date,
  delay,
  all_dates = NULL,
  all_states = NULL,
  direction = c("forward", "backward"),
  min_records = 500,
  absolute_min_records = 100,
  min_window = 7,
  max_window = 56,
  national_exclusions = tibble(state = "VIC", start = as.Date("2020-06-14"), end = NA)
) {
  
  direction <- match.arg(direction)
  
  # account for right-truncation when tabulating
  # which date to tabulate by
  if (direction == "forward") {
    date_from <- date
    date_to <- date + delay
    date_tabulation <- "date_from"
  } else {
    date_from <- date - delay
    date_to <- date
    date_tabulation <- "date_to"
  }
  
  delay_data <- tibble(
    state = state,
    date_from = date_from,
    date_to = date_to,
    delay = delay
  )
  
  if (is.null(all_dates)) {
    all_dates <- seq(
      min(delay_data$date_from),
      max(delay_data$date_to),
      by = 1
    )
  }
  
  if (is.null(all_states)) {
    all_states <- unique(delay_data$state)
  }
  
  date_state <- expand_grid(
    date = all_dates,
    state = all_states
  )
  
  # get the half-window size (number of days on either side of the target)
  absolute_max_window <- as.numeric(diff(range(all_dates)))
  min_window <- ceiling((min_window - 1) / 2)
  max_window <- floor((max_window - 1) / 2)
  
  # for each confirmation date, run the algorithm on each date
  statewide <- date_state %>%
    group_by(date, state) %>%
    mutate(
      window = get_window_size(
        date,
        state,
        delay_data = delay_data,
        date_tabulation = date_tabulation,
        n_min = min_records,
        window_min = min_window,
        window_max = max_window 
      ),
      count = count_in_window(
        date,
        state,
        delay_data = delay_data,
        date_tabulation = date_tabulation,
        window = window
      ),
      state_ecdf = delay_ecdf(
        date,
        state,
        window = window,
        delay_data = delay_data,
        date_tabulation = date_tabulation
      )
    )
  
  # fill in exclusion periods
  national_exclusions <- national_exclusions %>%
    mutate(
      start = as.Date(start),
      end = as.Date(end),
      start = replace_na(start, min(all_dates)),
      end = replace_na(end, max(all_dates))
    )
  
  # remove the specified data for estimating the national background distribution
  for (i in seq_len(nrow(national_exclusions))) {
    delay_data <- delay_data %>%
      filter(
        !(
          state == national_exclusions$state[i] &
            date_from >= national_exclusions$start[i] &
            date_to <= national_exclusions$end[i]
        )
      )
  }
  
  nationwide <- date_state %>%
    # arbitrarily pick one set of dates
    filter(state == "ACT") %>%
    select(-state) %>%
    group_by(date) %>%
    mutate(
      window = get_window_size(
        date,
        all_states,
        delay_data = delay_data,
        date_tabulation = date_tabulation,
        n_min = min_records,
        window_min = min_window,
        window_max = absolute_max_window
      ),
      national_ecdf = delay_ecdf(
        date,
        all_states,
        window = window,
        delay_data = delay_data,
        date_tabulation = date_tabulation
      )
    )
  
  # for statewide, replace any invalid ecdfs with the national one
  state_ecdfs <- statewide %>%
    right_join(
      nationwide %>%
        select(-window)
    ) %>%
    mutate(
      use_national = count < absolute_min_records,
      weight = pmin(1, count / min_records),
      weight = ifelse(use_national, 0, weight),
      ecdf = mapply(
        FUN = weight_ecdf,
        state_ecdf,
        national_ecdf,
        weight,
        SIMPLIFY = FALSE
      )
    ) %>%
    select(
      date, state, ecdf, weight, use_national
    )
  
  
  state_ecdfs
  
}
