# simulate the impact of strategies for local lockdowns in response to different
# case count triggers

source("R/functions.R")

library(sf)

# discrete probability distribution over days to case detection, for infections on this date
delay_prob <- function(date) {
  days <- seq_len(101) - 1 
  survival <- ttd_survival(days, rep(date, length(days)))
  diff(c(0, 1 - survival))
}

# simulate n values of days to detection
detection_delay <- function(n, prob) {
  if (n > 0) {
    days <- sample.int(
      length(prob),
      size = n,
      replace = TRUE,
      prob = prob
    )
    days - 1
  }
}

# combine columns of a two-column matrix into single unnique identifier
combine <- function(matrix) {
  if (is.null(dim(matrix))) {
    paste(matrix[1], matrix[2])
  } else {
    paste(matrix[, 1],
          matrix[, 2])
  }
}

# simulate new infections and update state object
update_infections <- function(idx, state, config) {
  
  active_cases <- config$gi_matrix %*% state$infections_matrix
  expected_infections <- active_cases[idx, ] * config$r_eff[idx, ]
  reallocated_infections <- expected_infections %*% config$import_rate
  n_suburbs <- ncol(active_cases)
  new_infections <- rnbinom(n_suburbs,
                            config$size,
                            mu = reallocated_infections)
  state$infections_matrix[idx, ] <- new_infections
  state
  
}

# simulate detection process for new infections and update state object
update_detections <- function(idx, state, config) {
  
  new_infections <- state$infections_matrix[idx, ]
  
  # get probabilities of detection on days following this infection date
  max_days <- nrow(state$infections_matrix)
  
  # for each new infection in each suburb, assign a random day of future
  # detection
  delay_prob <- config$delay_probs[[idx]]
  delays <- unlist(lapply(new_infections, detection_delay, delay_prob))
  suburbs <- rep(seq_len(n_suburbs), new_infections)
  elements <- cbind(idx + delays, suburbs)
  
  # remove those not inside the matrix
  valid <- elements[, 1] <= max_days
  elements <- elements[valid, ]  
  
  # count the number in each cell and add to detections matrix
  unique_elements <- unique(elements)
  counts <- table(combine(elements))
  index <- match(combine(unique_elements), names(counts))
  new_detections <- counts[index]
  state$detections_matrix[unique_elements] <- new_detections
  
  state
  
}

# one step of the simulation, simultaneously across multiple suburbs
update_state <- function(idx, state, config) {
  
  # simulate infection process  
  state <- update_infections(idx, state, config)
  
  # simulate detection process
  state <- update_detections(idx, state, config)
  
  state
  
}

# iterate the state for all dates
iterate_state <- function(state, config) {
  start <- config$start
  end <- nrow(state$infections_matrix)
  for (idx in seq(start, end)) {
    # if (idx == 28) browser()
    state <- update_state(idx, state, config)
  }
  state
}

set.seed(2020-07-04)

# load melbourne postcode geometries and populations
# prep_melbourne_postcodes()
melbourne <- readRDS("data/abs/melbourne_postal.rds") %>%
  filter(POP > 0)
n_suburbs <- nrow(melbourne)

# dates and suburbs
dates <- seq(as.Date("2020-07-01"), as.Date("2020-08-01"), by = 1)
n_dates <- length(dates)

# empty cases matrix with some initial infections in the CBD
infections <- matrix(0, n_dates, n_suburbs)  
infections[1, 1] <- 5

# empty matrix of dates of confirmation
detections <- matrix(0, n_dates, n_suburbs)  

# generation interval convolution matrix
gi_cdf <- nishiura_cdf()
gi_mat <- gi_matrix(gi_cdf, dates)

# R effective across suburbs and times
r_eff <- matrix(1.4, n_dates, n_suburbs)

# mobility between suburbs
melbourne_centroids <- st_centroid(melbourne)
distance <- st_distance(melbourne_centroids, melbourne_centroids)
distance_km <- units::drop_units(distance) / 1e3
# diag(distance_km) <- 1e-3
log_pop <- log(melbourne$POP)
log_pop_matrix <- matrix(rep(log_pop, n_suburbs),
                         n_suburbs,
                         n_suburbs)
log_mobility <- -10 +
  -1 * log(distance_km) +
  1 * log_pop_matrix +
  1 * t(log_pop_matrix)
mobility <- exp(log_mobility)
diag(mobility) <- 0
import_rate <- sweep(mobility, 1, rowSums(mobility), FUN = "/")
# probability that a contact is outside the postcode
leaving_probability <- 0.25
diag(import_rate) <- 1 / leaving_probability - 1
import_rate <- sweep(import_rate, 1, rowSums(import_rate), FUN = "/")

state <- list(
  infections_matrix = infections,
  detections_matrix = detections
)

config <- list(
  dates = dates,
  start = 2,
  size = 1 / sqrt(abs(rnorm(1, 0, 0.5))),
  gi_matrix = gi_mat,
  r_eff_matrix = r_eff,
  delay_probs = lapply(dates, delay_prob),
  import_rate = import_rate
)

system.time(
  state <- iterate_state(state, config)
)

image(state$infections_matrix)
tail(state$infections_matrix)
totals <- colSums(state$infections_matrix)
sum(totals)
melbourne$POA_CODE16[which.max(totals)]

# to do:
#  get posterior samples of Reff and negative binomial size
#  incorporate parameter draws in samples
#  get case counts by location from linelist
#  probabilistically assign case counts based on these and info on hotspots?
#  run simulation without lockdown policy reducing Reff and import rates
#  add different lockdown policies (no incidence limits)

