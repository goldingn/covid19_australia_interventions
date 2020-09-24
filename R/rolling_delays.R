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

count_in_window <- function(target_date, states, delay_data, window) {
  dates <- delay_data %>%
    filter(state %in% states) %>%
    pull(date_onset)
  diff <- abs(dates - target_date)
  in_window <- diff <= window
  sum(in_window)
}

get_window_size <- function(
  target_date,
  states,
  delay_data,
  n_min = 500,
  window_min = 7,
  window_max = 42
) {
  
  dates <- delay_data %>%
    filter(state %in% states) %>%
    pull(date_onset)
  
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

delay_ecdf <- function(target_date, states, window, delay_data) {
  
  data <- delay_data %>%
    filter(state %in% states)
  dates <- data$date_onset
  delays <- data$delay
  
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

get_cis <- function(date, state, ecdf, weight, too_low) {
  cis <- quantile(ecdf, deciles)
  names(cis) <- decile_names
  cis
}

source("R/functions.R")

linelist <- load_linelist()

delay_data <- linelist %>%
  filter(
    !is.na(date_onset),
    import_status == "local",
    # account for potential right-truncation
    date_onset <= (date_linelist - 20)
  ) %>%
  select(
    date_onset,
    date_confirmation,
    state,
    import_status
  ) %>%
  mutate(
    delay = as.numeric(date_confirmation - date_onset)
  ) %>%
  filter(
    delay <= 6 * 7,
    delay > -5
  )

dates <- seq(
  min(delay_data$date_onset),
  max(linelist$date_confirmation),
  by = 1
)
states <- unique(delay_data$state)

date_state <- expand_grid(
  date = dates,
  state = states
)

n_absolute_min <- 100

national_window_min <- 3
national_n_min <- 500
national_window_max <- as.numeric(diff(range(dates)))

state_window_min <- national_window_min
state_n_min <- national_n_min
state_window_max <- 28


# for each confirmation date, run the algorithm on each date
statewide <- date_state %>%
  group_by(date, state) %>%
  mutate(
    window = get_window_size(
      date,
      state,
      delay_data,
      n_min = state_n_min,
      window_min = state_window_min,
      window_max = state_window_max 
    ),
    count = count_in_window(
      date,
      state,
      delay_data,
      window = window
    ),
    state_ecdf = delay_ecdf(
      date,
      state,
      window,
      delay_data
    )
  )
  
# remove the victorian data for estimating the national background distribution
delay_sub <- delay_data %>%
  filter(
    !(state == "VIC" & date_onset >= as.Date("2020-06-14"))
  )

nationwide <- date_state %>%
  filter(state == "ACT") %>%
  select(-state) %>%
  group_by(date) %>%
  mutate(
    window = get_window_size(
      date,
      states,
      delay_sub,
      n_min = national_n_min,
      window_min = national_window_min,
      window_max = national_window_max
    ),
    national_ecdf = delay_ecdf(
      date,
      states,
      window,
      delay_sub
    )
  )

# for statewide, replace any invalid ecdfs with the national one
state_ecdfs <- statewide %>%
  right_join(
    nationwide %>%
      select(-window)
  ) %>%
  mutate(
    too_low = count < n_absolute_min,
    weight = pmin(1, count / state_n_min),
    weight = ifelse(too_low, 0, weight),
    ecdf = mapply(
      FUN = weight_ecdf,
      state_ecdf,
      national_ecdf,
      weight,
      SIMPLIFY = FALSE
    )
  ) %>%
  select(
    date, state, ecdf, weight, too_low
  )

# plot these changing distributions

# mutate to output quantiles and then plot them
deciles_lower <- seq(0.05, 0.45, by = 0.05)
deciles_upper <- 1 - deciles_lower
deciles <- c(deciles_lower, deciles_upper)
decile_names <- paste0("ci_", (1 - 2 * deciles_lower) * 100)
decile_names <- c(paste0(decile_names, "_lo"),
                  paste0(decile_names, "_hi"))

cis <- quantile(nationwide$national_ecdf[[200]], deciles)
names(cis) <- decile_names

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
    xlim = c(as.Date("2020-03-01"), max(dates))
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

# add points for true delays
df_obs <- delay_data %>%
  mutate(type = "Nowcast")

p <- p + geom_point(
  aes(date_onset, delay),
  data = df_obs,
  pch = 16,
  size = 0.2,
  alpha = 0.1
)

# add shading for regions where the national distribiution is used
p <- p +
  geom_ribbon(
    aes(ymin = -10, ymax = too_low * 100 - 10),
    fill = grey(1),
    alpha = 0.5,
    colour = grey(0.9)
  )

p

save_ggplot("surveillance_effect_state.png", multi = TRUE)

# to do:
# use for imputation code (make sure it's from onset date, not confirmation date)
# compute reff adjustment 
