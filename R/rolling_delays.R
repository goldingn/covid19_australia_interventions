# Define rolling delay distributions:

# For each state, for each date, build an ecdf from the dates x days either
# side, where x is the smallest positive integer such there are at least N
# recorded delays.
# Record x.
# Do the same for the national dataset (excluding VIC outbreak).
# In the state datasets, where x > xmax replace the ecdf with the national ecdf

mean.ecdf <- function(x, ...) {
  mean(evalq(rep.int(x, diff(c(0, round(nobs * y)))), environment(x)), ...)
}

count_in_window <- function(target_date, states, delay_data, window, date_tabulation) {
  dates <- delay_data %>%
    filter(state %in% states) %>%
    pull(!!date_tabulation)
  diff <- abs(dates - target_date)
  in_window <- diff <= window
  sum(in_window)
}

get_window_size <- function(
  target_date,
  states,
  delay_data,
  date_tabulation,
  n_min = 500,
  window_min = 7,
  window_max = 42
) {
  
  dates <- delay_data %>%
    filter(state %in% states) %>%
    pull(!!date_tabulation)
  
  # find the smallest window that yields the required number of counts
  for (window in window_min:window_max) {
    
    diff <- abs(dates - target_date)
    in_window <- diff <= window
    
    if (sum(in_window) >= n_min) {
      break()
    }
    
  }
  
  window
  
}

delay_ecdf <- function(target_date, states, window, delay_data, date_tabulation) {
  
  data <- delay_data %>%
    filter(state %in% states)
  dates <- pull(data, !!date_tabulation)
  delays <- pull(data, delay)
  
  diff <- abs(dates - target_date)
  in_window <- diff <= window
  
  valid_delays <- delays[in_window]
  
  if (length(valid_delays) > 0) {
    distribution <- ecdf(valid_delays)
  } else {
    distribution <- NULL
  }
  
  list(distribution)
  
}

ci_ribbon <- function(ci) {
  
  lo <- paste0("ci_", ci, "_lo")
  hi <- paste0("ci_", ci, "_hi")
  
  geom_ribbon(
    aes_string(ymin = lo,
               ymax = hi),
    alpha = 1/9
  )
}

# calculate a weighted average ecdf out of two (weight is the probability of the first)
weight_ecdf <- function(ecdf_1, ecdf_2, weight) {
  
  if (is.null(ecdf_1) | weight == 0) {
    return(ecdf_2)
  }
  if (is.null(ecdf_2) | weight == 1) {
    return(ecdf_1)
  }
  
  e1 <- environment(ecdf_1)
  e2 <- environment(ecdf_2)
  
  # reconcile the xs
  x_1 <- e1$x
  x_2 <- e2$x

  x <- sort(unique(c(x_1, x_2)))
  
  # get the two CDFs
  y_1 <- ecdf_1(x)
  y_2 <- ecdf_2(x)
  
  # get the two pdfs
  pdf_1 <- diff(c(0, y_1))
  pdf_2 <- diff(c(0, y_2))
  
  # get a weighted average of them
  pdf <- pdf_1 * weight + pdf_2 * (1 - weight)
  
  # convert back to a CDF
  y <- cumsum(pdf)
  
  # rebuild an ecdf object, the slow way
  method <- 2L
  yleft <- 0
  yright <- 1
  f <- e1$f
  n <- e1$nobs
  rval <- function (v) {
    stats:::.approxfun(x, y, v, method, yleft, yright, f)
  }
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- attr(ecdf_1, "call")
  rval

}

get_cis <- function(date, state, ecdf, weight, use_national) {
  cis <- quantile(ecdf, deciles)
  names(cis) <- decile_names
  cis
}

# Calculate time-varying delay distributions for each state, smoothly reverting
# to the national average when and where there are insufficient records to
# compute state-level distributions. Return a tibble of empirical CDFs of the
# distribution by state and date.
# Parameters:

# linelist: a linelist dataset as returned by load_linelist()
# dates, states: optional vectors of dates and states for which to compute
#   delays (taken from linelist if not specified)
# from, to: strings giving the names of the date columns in the linelist to use
#   as the start and end of the delays
# tabulate_by_to: whether to tabulate delays by the 'to' date, rather than by
#   the 'from' date (the default)
# right_truncation: the number of days of data to remove (before the linelist
#   date) to prevent bias due to right-truncation of delay data.
# import_statuses: which import statuses to use when computing the delays
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
  linelist,
  dates = NULL,
  states = NULL,
  from = "date_onset",
  to = "date_confirmation",
  tabulate_by_to = FALSE,
  right_truncation = ifelse(tabulate_by_to, 3, 15),
  import_statuses = "local",
  delay_plausible_bounds = c(-5, 42),
  min_records = 500,
  absolute_min_records = 100,
  min_window = 7,
  max_window = 56,
  national_exclusions = tibble(state = "VIC", start = as.Date("2020-06-14"), end = NA)
) {
  
  # fill in exclusion periods
  national_exclusions <- national_exclusions %>%
    mutate(
      start = as.Date(start),
      end = as.Date(end),
      start = replace_na(start, min(linelist$date_onset)),
      end = replace_na(end, max(linelist$date_confirmation))
    )
  
  # account for right-truncation when tabulating
  linelist_date <- linelist$date_linelist[1]
  truncation_date <- linelist_date - right_truncation

  # which date to tabulate by
  date_tabulation <- ifelse(tabulate_by_to, "date_to", "date_from")
  
  delay_data <- linelist %>%
    rename(
      date_from = !!from,
      date_to = !!to
    ) %>%
    filter(
      !is.na(date_from),
      !is.na(date_to),
      import_status %in% import_statuses,
      date_from <= truncation_date
    ) %>%
    select(
      date_from,
      date_to,
      state,
      import_status
    ) %>%
    mutate(
      delay = as.numeric(date_to - date_from)
    ) %>%
    filter(
      delay <= delay_plausible_bounds[2],
      delay >= delay_plausible_bounds[1]
    )
  
  if (is.null(dates)) {
    dates <- seq(
      min(delay_data$date_from),
      max(delay_data$date_to),
      by = 1
    )
  }
  
  if (is.null(states)) {
    states <- unique(delay_data$state)
  }
  
  date_state <- expand_grid(
    date = dates,
    state = states
  )
  
  # get the half-window size (number of days on either side of the target)
  absolute_max_window <- as.numeric(diff(range(dates)))
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
    filter(state == "ACT") %>%
    select(-state) %>%
    group_by(date) %>%
    mutate(
      window = get_window_size(
        date,
        states,
        delay_data = delay_data,
        date_tabulation = date_tabulation,
        n_min = min_records,
        window_min = min_window,
        window_max = absolute_max_window
      ),
      national_ecdf = delay_ecdf(
        date,
        states,
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



source("R/functions.R")

linelist <- load_linelist()

state_ecdfs <- estimate_delays(linelist)
  

# plot these changing distributions

# mutate to output quantiles and then plot them
deciles_lower <- seq(0.05, 0.45, by = 0.05)
deciles_upper <- 1 - deciles_lower
deciles <- c(deciles_lower, deciles_upper)
decile_names <- paste0("ci_", (1 - 2 * deciles_lower) * 100)
decile_names <- c(paste0(decile_names, "_lo"),
                  paste0(decile_names, "_hi"))

quantiles <- state_ecdfs %>%
  # plot it as the date of infection, not date of onset!
  mutate(date = date - 5) %>%
  pmap_dfr(get_cis) %>%
  bind_cols(state_ecdfs, .) %>%
  mutate(
    median = vapply(
      ecdf,
      quantile,
      0.5,
      FUN.VALUE = numeric(1)
    ),
    mean = vapply(
      ecdf,
      mean,
      FUN.VALUE = numeric(1)
    )
  )

# use this in the imputation code


library(ggplot2)
base_colour <- yellow

p <- quantiles %>%
  mutate(type = "Nowcast") %>%
  ggplot() + 
  
  aes(date, mean, fill = type) +
  
  facet_wrap(~state, ncol = 2) +
  
  xlab(element_blank()) +
  
  coord_cartesian(
    ylim = c(0, 20),
    xlim = c(as.Date("2020-03-01"), max(state_ecdfs$date))
  ) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  ci_ribbon("90") +
  ci_ribbon("80") +
  ci_ribbon("70") +
  ci_ribbon("60") +
  ci_ribbon("50") +
  ci_ribbon("40") +
  ci_ribbon("30") +
  ci_ribbon("20") +
  ci_ribbon("10") +
  
  geom_line(aes(y = ci_90_lo),
            colour = base_colour,
            alpha = 0.8) + 
  geom_line(aes(y = ci_90_hi),
            colour = base_colour,
            alpha = 0.8) + 
  
  geom_line(aes(y = mean),
            colour = grey(0.4),
            alpha = 1,
            size = 1) +

  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines")) +
  ggtitle(label = "Surveillance trend",
          subtitle = "Time from symptom onset to notification for locally-acquired cases") +
  ylab("Days")

# # add points for true delays
# df_obs <- delay_data %>%
#   mutate(type = "Nowcast")
# 
# p <- p + geom_point(
#   aes(date_onset, delay),
#   data = df_obs,
#   pch = 16,
#   size = 0.2,
#   alpha = 0.1
# )

# add shading for regions where the national distribution is used
p <- p +
  geom_ribbon(
    aes(ymin = -10, ymax = use_national * 100 - 10),
    fill = grey(1),
    alpha = 0.5,
    colour = grey(0.9)
  )

p

save_ggplot("surveillance_effect_state.png", multi = TRUE)

# to do:
# output dataset too
# wrap up plotting code in function
# run twice, once for imputation (aggregate_by_to = TRUE), and once for other uses
# compute reff adjustment by state
# use for GI distribution adjustment by state
