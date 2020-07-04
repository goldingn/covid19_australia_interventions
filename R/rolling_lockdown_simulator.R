# simulate the impact of strategies for local lockdowns in response to different
# case count triggers

source("R/functions.R")

# simulate new cases given the infectiousness of current cases and Reff
sim_new_infections <- function(infectiousness, r_eff, import_rate, size = 3) {
  n <- length(infectiousness)
  expected_infections <- infectiousness * r_eff
  reallocated_infections <- expected_infections %*% import_rate
  rnbinom(n, size, mu = reallocated_infections)
}

detection_delay <- function(n, prob) {
  if (n > 0) {
    days <- sample.int(
      length(prob),
      size = n,
      replace = TRUE,
      prob =  prob
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
update_infections <- function(idx, state) {
  
  active_cases <- gi_mat %*% state$infections_matrix
  new_infections <- sim_new_infections(
    active_cases[idx, ],
    state$r_eff_matrix[idx, ],
    state$import_rate
  )
  state$infections_matrix[idx, ] <- new_infections
  state
  
}

# simulate detection process for new infections and update state object
update_detections <- function(idx, state) {
  
  new_infections <- state$infections_matrix[idx, ]
  
  # get probabilities of detection on days following this infection date
  max_days <- nrow(state$infections_matrix)
  days <- seq_len(101) - 1 
  survival <- ttd_survival(days, rep(dates[idx], length(days)))
  prob <- diff(c(0, 1 - survival))
  
  # for each new infection in each suburb, assign a random day of future
  # detection
  delays <- unlist(lapply(new_infections, detection_delay, prob))
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

# one step of the simulation, simultaneously across multiple areas
update_state <- function(idx, state) {

  # simulate infection process  
  state <- update_infections(idx, state)
  
  # simulate detection process
  state <- update_detections(idx, state)
  
  state
  
}

iterate_state <- function(state, start = 2) {
  end <- nrow(state$infections_matrix)
  for (idx in seq(start, end)) {
    # if (idx == 28) browser()
    state <- update_state(idx, state)
  }
  state
}

# determine when cases would be detected
# random, from distribution? how to dot his without recomputing?
# at each step, transfer over to a new matrix of connfirmations

set.seed(2020-07-04)

# dates and suburbs
dates <- seq(as.Date("2020-07-01"), as.Date("2020-08-01"), by = 1)
n_dates <- length(dates)
n_suburbs <- 30

# empty cases matrix with some initial case counts
infections <- matrix(0, n_dates, n_suburbs)  
infections[1, ] <- rpois(n_suburbs, 1)

# empty matrix of dates of confirmation
detections <- matrix(0, n_dates, n_suburbs)  

# generation interval convolution matrix
gi_cdf <- nishiura_cdf()
gi_mat <- gi_matrix(gi_cdf, dates)

# R effective across suburbs and times
r_eff <- matrix(1.4, n_dates, n_suburbs)

# mobility between suburbs
coords <- matrix(rnorm(2 * n_suburbs, 0, 5), n_suburbs)
mobility <- exp(-0.5 * as.matrix(dist(coords)))
import_rate <- sweep(mobility, 1, rowSums(mobility), FUN = "/")

state <- list(
  infections_matrix = infections,
  detections_matrix = detections,
  r_eff_matrix = r_eff,
  import_rate = import_rate
)

# iterate through the dates
state <- iterate_state(state)


image(state$infections_matrix)
tail(state$infections_matrix)
colSums(state$infections_matrix)
sum(state$infections_matrix)
