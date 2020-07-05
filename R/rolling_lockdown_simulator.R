# simulate the impact of strategies for local lockdowns in response to different
# case count triggers

source("R/functions.R")

library(sf)

# gravity model of mobility between suburbs - geometry must be an sf object with
# polygons
gravity_model <- function(geometry, population,
                          leaving_probability = 0.25,
                          coef = c(-10, -3, 1, 1)) {
  # distance matrix in km
  n_suburbs <- nrow(geometry)
  centroids <- st_centroid(geometry)
  distance <- st_distance(centroids, centroids)
  distance_km <- units::drop_units(distance) / 1e3
  
  # population of suburbs in matrix form
  log_pop <- log(population)
  log_pop_matrix <- matrix(rep(log_pop, n_suburbs),
                           n_suburbs,
                           n_suburbs)
  
  # mobility (ignoring probability of staying)
  log_mobility <- coef[1] +
    coef[2] * log(distance_km) +
    coef[3] * log_pop_matrix +
    coef[4] * t(log_pop_matrix)
  mobility <- exp(log_mobility)
  diag(mobility) <- 0
  
  # convert to impotation rate, with probability of leaving
  import_rate <- sweep(mobility, 1, rowSums(mobility), FUN = "/")
  # probability that a contact is outside the postcode
  diag(import_rate) <- 1 / leaving_probability - 1
  import_rate <- sweep(import_rate, 1, rowSums(import_rate), FUN = "/")
  
  import_rate
  
}

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

  # get the number of new active cases, and add on the effect of the initial cases  
  new_active_cases <- config$gi_matrix %*% state$infections_matrix
  active_cases <- config$initial_active_cases + new_active_cases
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

# create an empty state object
empty_state <- function (config) {
  zeros <- matrix(0, config$n_dates, config$n_suburbs)  
  list(
    infections_matrix = zeros,
    detections_matrix = zeros
  )
}

# generate a random number of initial infections
random_initial_infections <- function(expected_infections, size) {
  # later change this to accept the detection probability, postcode fractions,
  # observed infections, and sample the number of undetected infections from the
  # inverse negative binomial
  n <- length(expected_infections)
  initial_infections <- expected_infections
  initial_infections[] <- rnbinom(n, size = size, mu = expected_infections)
  initial_infections
}

# simulate a number of active cases from prior to the dynamic simulation
sim_initial_active_cases <- function(initial_expected_infections,
                                     nbinom_size,
                                     dates,
                                     gi_cdf) {
  
  n_suburbs <- ncol(initial_expected_infections)
  n_dates_prior <- nrow(initial_expected_infections)
  n_dates <- length(dates)
  
  # simulate numbers of new infections in each suburb on each date  
  initial_infections <- random_initial_infections(
    expected_infections = initial_expected_infections,
    size = nbinom_size
  )
  empty_infections <- matrix(0, n_dates, n_suburbs)
  initial_infections_all <- rbind(initial_infections, empty_infections)
  
  all_dates <- min(dates) + seq(-n_dates_prior, n_dates - 1)
  initial_active_cases <- gi_matrix(gi_cdf, all_dates) %*% initial_infections_all
  initial_active_cases <- tail(initial_active_cases, n_dates)
  
  initial_active_cases
  
}

# create a config object for running a simulation
prepare_config <- function (idx,
                            nbinom_size_samples,
                            r_eff_samples,
                            initial_expected_infections,
                            import_rate,
                            dates,
                            gi_cdf = nishiura_cdf()) {
  
  # negative binomial sample size
  nbinom_size <- nbinom_size_samples[idx]
  
  # simulate infections by postcode and compute the number of active cases from
  # before the dynamics
  initial_active_cases <- sim_initial_active_cases(
    initial_expected_infections = initial_expected_infections,
    nbinom_size = nbinom_size,
    dates = dates,
    gi_cdf = gi_cdf
  )
  
  # generation interval convolution matrix
  gi_mat <- gi_matrix(gi_cdf, dates)
  
  # matrix of initial R effective in each suburb by date
  r_eff <- r_eff_samples[[idx]]
  
  # detection delay distribution probabilities for each date
  delay_probs <- lapply(dates, delay_prob)
  
  # return configuration for this simulation
  list(
    n_dates = length(dates),
    n_suburbs = ncol(initial_expected_infections),
    dates = dates,
    size = nbinom_size,
    gi_matrix = gi_mat,
    r_eff_matrix = r_eff,
    delay_probs = delay_probs,
    import_rate = import_rate,
    initial_active_cases = initial_active_cases
  )
  
}

# iterate the state for all dates
iterate_state <- function(state, config) {
  for (idx in seq_len(config$n_dates)) {
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

# read in recent counts of local cases by date of infection
local_cases <- read_csv(
  "outputs/local_cases_input.csv",
  col_types = cols(
    date_onset = col_date(format = ""),
    detection_probability = col_double(),
    state = col_character(),
    count = col_double()
  )
) %>%
  filter(state == "VIC") %>%
  mutate(
    date = date_onset - 5,
    expected_infections = count / detection_probability
  ) %>%
  filter(
    detection_probability > 0.5
  ) %>%
  tail(14)

# use (active?) case distribution between lockdown suburbs as reported by the
# Age on July 3rd to disagreggate expected number of infections by postcode
case_distrib <- tibble::tribble(
  ~postcode, ~cases,
  3064, 52,
  3047, 25,
  3060, 11,
  3046, 10,
  3038, 4,
  3021, 16,
  3042, 2,
  3055, 3,
  3032, 9,
  3012, 8,
) %>%
  mutate(
    fraction = cases / sum(cases),
    POA_CODE16 = as.character(postcode)
  ) %>%
  right_join(
    st_drop_geometry(melbourne)
  ) %>%
  mutate(
    fraction = replace_na(fraction, 0)
  ) %>%
  select(
    postcode = POA_CODE16,
    fraction
  )


# take the last 2 weeks of case counts, and the expected fractions of cases in
# each of these postcodes, and get expected numbers of infections by postcode
initial_expected_infections <- local_cases$expected_infections %*% t(case_distrib$fraction)
colnames(initial_expected_infections) <- case_distrib$postcode
rownames(initial_expected_infections) <- as.character(local_cases$date)

# dates and suburbs
dates <- max(local_cases$date) + seq_len(6 * 7)
n_dates <- length(dates)

import_rate <- gravity_model(
  geometry = melbourne,
  population = melbourne$POP,
  leaving_probability = 0.1,
  coef = c(-10, -10, 1, 1)
)

size_samples <- 1 / sqrt(abs(rnorm(100, 0, 0.5)))

# R effective across suburbs and times
r_eff_samples <- replicate(100,
                           matrix(1.4, n_dates, n_suburbs),
                           simplify = FALSE)

config <- prepare_config(
  idx = 1,
  nbinom_size_samples = size_samples,
  r_eff_samples = r_eff_samples,
  initial_expected_infections = initial_expected_infections,
  import_rate = import_rate,
  dates = dates
)

initial_state <- empty_state(config)

system.time(
  final_state <- iterate_state(initial_state, config)
)

image(final_state$infections_matrix)
tail(final_state$infections_matrix)
totals <- colSums(final_state$infections_matrix)
sum(totals)
melbourne$POA_CODE16[which.max(totals)]

# to do:
#  get posterior samples of Reff and negative binomial size
#  incorporate parameter draws in samples
#  run simulation without lockdown policy reducing Reff and import rates
#  add different lockdown policies (no incidence limits)

