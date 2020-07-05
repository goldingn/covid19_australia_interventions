# simulate the impact of strategies for local lockdowns in response to different
# case count triggers

source("R/functions.R")

library(sf)

# random truncated normal samples
rtnorm <- function(n, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
  truncdist::rtrunc(n = n,
                    spec = "norm",
                    a = lower,
                    b = upper,
                    mean = mean,
                    sd = sd)
}

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
  
  # convert to importation rate, with probability of leaving
  import_rate <- set_leaving_probability(mobility, leaving_probability)
  
  import_rate
  
}

# modify an import rate matrix to set the probability that a infectee is
# inside/outside the suburb
set_leaving_probability <- function(import_rate, leaving_probability) {
  # make off-diagnal elements on each row (probability of visiting each other
  # suburbs if leaving) sum to 1
  diag(import_rate) <- 0
  import_rate <- sweep(import_rate, 1, rowSums(import_rate), FUN = "/")
  
  # set the probability that a contact is outside the postcode
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

# modification of R_ef in the face of lockdown
lockdown_r_eff <- function(idx, state, config) {
  baseline_r_eff <- config$r_eff_matrix[idx, ]
  lockdown_status <- state$lockdown_matrix[idx, ]
  modification <- 1 - lockdown_status * config$r_eff_reduction
  baseline_r_eff * modification
}

# modification of the inter-postcode import rate in the face of lockdown
lockdown_import_rate <- function(idx, state, config) {
  
  import_rate <- config$import_rate
  lockdown_status <- state$lockdown_matrix[idx, ]
  
  # when in lockdown, the probability of an infectee being in another suburb is
  # reduced by some value
  old_leaving_probability <- 1 - diag(import_rate)
  reduction <- 1 - lockdown_status * config$leaving_reduction
  new_leaving_probability <- old_leaving_probability * reduction
  
  import_rate <- set_leaving_probability(
    import_rate = import_rate,
    leaving_probability = new_leaving_probability
  )
  
  import_rate
  
  
  
  
}

# simulate new infections and update state object
update_infections <- function(idx, state, config) {

  # get the number of new active cases, and add on the effect of the initial cases  
  new_active_cases <- config$gi_matrix %*% state$infections_matrix
  active_cases <- config$initial_active_cases + new_active_cases
  
  # get modified Reff and import rate
  r_eff <- lockdown_r_eff(idx, state, config)
  import_rate <- lockdown_import_rate(idx, state, config)
  
  expected_infections <- active_cases[idx, ] * r_eff
  reallocated_infections <- expected_infections %*% import_rate
  n_suburbs <- ncol(active_cases)
  new_infections <- rnbinom(n_suburbs,
                            config$size,
                            mu = reallocated_infections)
  state$infections_matrix[idx, ] <- new_infections
  
  state$infections_matrix
  
}

# for each new infection in each suburb, assign a random day of future
# detection
increment_detections <- function(idx,
                                 detections_matrix,
                                 infections_matrix,
                                 delay_probs) {
  
  new_infections <- infections_matrix[idx, ]
  delay_prob <- delay_probs[[idx]]
  n_suburbs <- ncol(infections_matrix)
  n_dates <- nrow(infections_matrix)
  
  delays <- unlist(lapply(new_infections, detection_delay, delay_prob))
  suburbs <- rep(seq_len(n_suburbs), new_infections)
  elements <- cbind(idx + delays, suburbs)
  
  # remove those not inside the matrix
  valid <- elements[, 1] <= n_dates
  elements <- elements[valid, ]  
  
  # count the number in each cell and add to detections matrix
  unique_elements <- unique(elements)
  counts <- table(combine(elements))
  index <- match(combine(unique_elements), names(counts))
  new_detections <- counts[index]
  
  old_detections <- detections_matrix[unique_elements]
  detections_matrix[unique_elements] <- old_detections + new_detections
  
  detections_matrix
  
}

# simulate detection process for new infections and update state object
update_detections <- function(idx, state, config) {
  
  increment_detections(idx = idx,
                       detections_matrix = state$detections_matrix,
                       infections_matrix = state$infections_matrix,
                       delay_probs = config$delay_probs) 
  
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
sim_initial_cases <- function(initial_expected_infections,
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
  
  dates_all <- min(dates) + seq(-n_dates_prior, n_dates - 1)
  initial_active_cases <- gi_matrix(gi_cdf, dates_all) %*% initial_infections_all
  
  
  # detection delay distribution probabilities for each date
  delay_probs_all <- lapply(dates_all, delay_prob)
  
  detections_all <- matrix(0, n_dates_prior + n_dates, n_suburbs)
  for (i in seq_len(n_dates_prior)) {
    detections_all <- increment_detections(idx = i,
                                           detections_matrix = detections_all,
                                           infections_matrix = initial_infections_all,
                                           delay_probs = delay_probs_all)
  }

  # return both active cases and detections for the dynamic simulation period
  list(
    active_cases = tail(initial_active_cases, n_dates),
    detections = tail(detections_all, n_dates)
  )
  
}

# create a config object for running a simulation
prepare_config <- function (idx,
                            nbinom_size_samples,
                            r_eff_samples,
                            r_eff_reduction_samples,
                            leaving_reduction_samples,
                            initial_expected_infections,
                            import_rate,
                            dates,
                            populations,
                            gi_cdf = nishiura_cdf(),
                            lockdown_policy = original_policy) {
  
  # pull relevant samples
  nbinom_size <- nbinom_size_samples[idx]
  r_eff_reduction <- r_eff_reduction_samples[idx]
  leaving_reduction <- leaving_reduction_samples[idx]
  r_eff <- r_eff_samples[[idx]]
    
  # simulate infections by postcode and compute the number of active cases from
  # before the dynamics
  initial_cases <- sim_initial_cases(
    initial_expected_infections = initial_expected_infections,
    nbinom_size = nbinom_size,
    dates = dates,
    gi_cdf = gi_cdf
  )
  
  initial_active_cases <- initial_cases$active_cases
  initial_detections <- initial_cases$detections

  # generation interval convolution matrix
  gi_mat <- gi_matrix(gi_cdf, dates)
  
  # detection delay distribution probabilities for each date
  delay_probs <- lapply(dates, delay_prob)
  
  # return configuration for this simulation
  list(
    n_dates = length(dates),
    n_suburbs = ncol(initial_expected_infections),
    dates = dates,
    size = nbinom_size,
    r_eff_matrix = r_eff,
    r_eff_reduction = r_eff_reduction,
    leaving_reduction = leaving_reduction,
    gi_matrix = gi_mat,
    delay_probs = delay_probs,
    populations = populations,
    import_rate = import_rate,
    initial_active_cases = initial_active_cases,
    initial_detections = initial_detections,
    lockdown_policy = lockdown_policy
  )
  
}

# iterate the state for all dates
iterate_state <- function(state, config) {
  for (idx in seq_len(config$n_dates)) {
    
    # infection process  
    state$infections_matrix <- update_infections(idx, state, config)
    
    # detection process
    state$detections_matrix <- update_detections(idx, state, config)
    
    # lockdown decision process
    state$lockdown_matrix <- config$lockdown_policy(idx, state, config)
    
  }
  
  state
  
}

run_simulation <- function(config) {
  
  # set up the initial state
  zeros <- matrix(0, config$n_dates, config$n_suburbs)  
  initial_state <- list(
    infections_matrix = zeros,
    detections_matrix = config$initial_detections,
    lockdown_matrix = zeros
  )
  
  # iterate the state dynamics
  final_state <- iterate_state(initial_state, config)
  
  final_state
  
}

# the original lockdown policy: if a suburb has 5+ cases and an incidence of >20
# per 100,000 over some time period (a week?), lockdown that suburb
original_policy <- function(idx, state, config, incidence_days = 7, count_threshold = 5, incidence_threshold = 20) {

  # count the number of cases in the last few days
  days <- idx - seq_len(incidence_days) + 1
  days <- unique(pmax(1, days))
  
  count <- colSums(state$detections_matrix[days, , drop = FALSE])
  incidence <- count * 1e5 / config$populations
  
  # from here on, lock down that suburb
  new_lockdown <- count > count_threshold & incidence > incidence_threshold
  old_idx <- pmax(idx - 1, 1)
  current_lockdown <- state$lockdown_matrix[old_idx, ]
  state$lockdown_matrix[idx, ] <- pmax(as.numeric(new_lockdown), current_lockdown)
  
  state$lockdown_matrix
      
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

n_samples <- 100
size_samples <- 1 / sqrt(abs(rnorm(n_samples, 0, 0.5)))

# R effective across suburbs and times
r_eff_samples <- replicate(n_samples,
                           matrix(1.1, n_dates, n_suburbs),
                           simplify = FALSE)

leaving_reduction_samples <- runif(n_samples, 0.2, 0.8)
r_eff_reduction_samples <- runif(n_samples, 0.2, 0.8)

config <- prepare_config(
  idx = 1,
  nbinom_size_samples = size_samples,
  r_eff_samples = r_eff_samples,
  r_eff_reduction_samples = r_eff_reduction_samples,
  leaving_reduction_samples = leaving_reduction_samples,
  initial_expected_infections = initial_expected_infections,
  import_rate = import_rate,
  dates = dates,
  populations <- melbourne$POP
)

results <- run_simulation(config)

sum(results$infections_matrix)
sum(results$detections_matrix)
image(results$infections_matrix)
image(results$detections_matrix)
tail(results$infections_matrix)
totals <- colSums(results$infections_matrix)
sum(totals)
plot(rowSums(results$detections_matrix), type = "l")
melbourne$POA_CODE16[which.max(totals)]

image(results$lockdown_matrix)

# to do:
#  run simulation without lockdown policy reducing Reff and import rates
#  add different lockdown policies (no incidence limits)
#  get posterior samples of Reff and negative binomial size
#  incorporate parameter draws in samples
#  incorporate FB OD matrix
#  make diagnostic plots
