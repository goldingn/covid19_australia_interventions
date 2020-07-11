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
detection_delay <- function(region, new_infections, probs) {
  n <- new_infections[region]
  result <- NULL
  if (n > 0) {
    count <- rmultinom(1, n, probs)
    pos <- which(count > 0)
    result <- cbind(delay = pos - 1,
                    region = region,
                    count = count[pos])
  }
  result
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
  modification <- 1 - lockdown_status * (1 - config$r_eff_reduction)
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
  regions <- seq_along(new_infections)
  n_dates <- nrow(infections_matrix) 
  
  delays_list <- lapply(regions,
                        detection_delay,
                        new_infections,
                        delay_prob)
  delays <- do.call(rbind, delays_list)

  if (!is.null(delays)) {
   
    # numbers of new detections  
    new_detections <- delays[, 3]
    
    # where to assign them  
    elements <- delays[, 1:2, drop = FALSE]
    elements[, 1] <- elements[, 1] + idx
    valid <- elements[, 1] <= n_dates
    elements <- elements[valid, , drop = FALSE] 
    
    # update counts
    old_detections <- detections_matrix[elements]
    detections_matrix[elements] <- old_detections + new_detections
    
  }
  

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
                              delay_probs_all,
                              gi_matrix) {
  
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
  initial_active_cases <- gi_matrix %*% initial_infections_all
  
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
                            delay_probs,
                            delay_probs_all,
                            populations,
                            gi_mat,
                            gi_mat_all,
                            lockdown_policy = original_policy()) {
  
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
    delay_probs_all = delay_probs_all,
    gi_matrix = gi_mat_all
  )
  
  initial_active_cases <- initial_cases$active_cases
  initial_detections <- initial_cases$detections

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
    
    # lockdown decision process
    state$lockdown_matrix <- config$lockdown_policy(idx, state, config)
    
    # infection process  
    state$infections_matrix <- update_infections(idx, state, config)
    
    # detection process
    state$detections_matrix <- update_detections(idx, state, config)
    
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
  results <- iterate_state(initial_state, config)
  
  # get summaries
  results$cases <- rowSums(results$detections_matrix)
  results$people_in_lockdown <- (results$lockdown_matrix %*% config$populations)[, 1]
  
  results
  
}

# the original lockdown policy: if a suburb has 5+ cases and an incidence of >20
# per 100,000 over some time period (a week?), lockdown that suburb
original_policy <- function(incidence_days = 7, count_threshold = 5, incidence_threshold = 20) {
  
  lockdown_policy <- function(idx, state, config) {
    
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
      
  lockdown_policy
  
}

set.seed(2020-07-04)

# load VIC LGA geometries and populations
# prep_vic_lgas()

vic_lga <- readRDS("data/spatial/vic_lga.RDS")
n_lga <- nrow(vic_lga)

# read in numbers of cases by date of infection in LGAs, nd pad with 0s for
# other LGAs
lga_infections <- readRDS("~/not_synced/lga_local_infections_2020-07-10.RDS") %>%
  filter(state == "VIC") %>%
  mutate(
    expected_infections = infections / detection_probability
  ) %>%
  filter(
    detection_probability > 0.5
  ) %>%
  filter(
    date > max(date) - 14
  ) %>%
  select(
    date,
    expected_infections,
    lga
  )

dates_prior <- seq(
  min(lga_infections$date),
  max(lga_infections$date),
  by = 1
)

# convert to a matrix
initial_expected_infections <- lga_infections %>%
  full_join(
    expand_grid(
      lga = setdiff(vic_lga$lga, lga_infections$lga),
      date = dates_prior,
      expected_infections = 0
    )
  ) %>%
  pivot_wider(
    names_from = lga,
    values_from = expected_infections,
    values_fill = list(expected_infections = 0)
  ) %>%
  select(-date) %>%
  as.matrix()

# take the last 2 weeks of case counts, and the expected fractions of cases in
# each of these postcodes, and get expected numbers of infections by postcode
rownames(initial_expected_infections) <- as.character(dates_prior)

# dates and suburbs
dates <- max(dates_prior) + seq_len(6 * 7)
n_dates <- length(dates)
n_dates_prior <- length(dates_prior)
dates_all <- min(dates) + seq(-n_dates_prior, n_dates - 1)

parameter_draws <- readRDS("outputs/projection/postcode_forecast_draws.RDS")

n_samples <- 1000
size_samples <- parameter_draws$vic_size[seq_len(n_samples)]
r_eff_reduction_samples <- parameter_draws$vic_r_eff_reduction_full[seq_len(n_samples)]
leaving_reduction_samples <- runif(n_samples, 0.2, 0.5)
non_household_fraction <- parameter_draws$vic_fraction_non_household
  
# get the baseline fraction of new infections that are in another LGA

# from contact questions, 33% of trips by cases in the time prior to symptom
# onset were not within the same LGA

# from Reff model the fraction of transmisions that are to non-household members
# is around 45%
outside_transmission_fraction <- mean(non_household_fraction) * 0.33

import_rate <- gravity_model(
  geometry = vic_lga,
  population = vic_lga$pop,
  leaving_probability = outside_transmission_fraction,
  coef = c(0, -2.5, 1, 1)
)

# R effective across suburbs and times
idx <- match(dates, parameter_draws$dates)
r_eff_samples_mat <- parameter_draws$vic_r_eff[seq_len(n_samples), idx]
r_eff_samples_list <- apply(r_eff_samples_mat, 1, list)
r_eff_samples <- lapply(r_eff_samples_list, function(x) {
  matrix(rep(x[[1]], n_lga), n_dates, n_lga)
})

# prepare and run simulations in parallel
library(future.apply)
plan(multisession)

# generation interval convolution matrix for dynamic part and for dynamic &
# pre-dynamic part
gi_cdf <- nishiura_cdf()
gi_mat <- gi_matrix(gi_cdf, dates)
gi_mat_all <- gi_matrix(gi_cdf, dates_all)

# detection delay distribution probabilities for each date
delay_probs <- lapply(dates, delay_prob)
delay_probs_all <- lapply(dates_all, delay_prob)

configs <- future_lapply(seq_len(n_samples),
                         prepare_config,
                         nbinom_size_samples = size_samples,
                         r_eff_samples = r_eff_samples,
                         r_eff_reduction_samples = r_eff_reduction_samples,
                         leaving_reduction_samples = leaving_reduction_samples,
                         initial_expected_infections = initial_expected_infections,
                         import_rate = import_rate,
                         dates = dates,
                         delay_probs = delay_probs,
                         delay_probs_all = delay_probs_all,
                         gi_mat = gi_mat,
                         gi_mat_all = gi_mat_all,
                         populations = vic_lga$pop,
                         lockdown_policy = original_policy())

results <- future_lapply(configs, run_simulation)

# combine results
lga_case_list <- lapply(results, `[[`, "detections_matrix")
lga_case_sims <- do.call(abind, c(lga_case_list, list(along = 0)))

mn <- apply(lga_case_sims, 2:3, mean)
quants <- apply(lga_case_sims, 2:3, quantile, c(0.05, 0.25, 0.5, 0.75, 0.95))

df <- tibble(
  lga = rep(vic_lga$lga, each = n_dates),
  date = rep(dates, n_lga),
  mean = c(mn),
  median = c(quants[3, , ]),
  ci_50_lo = c(quants[2, , ]),
  ci_50_hi = c(quants[4, , ]),
  ci_90_lo = c(quants[1, , ]),
  ci_90_hi = c(quants[5, , ])
) %>%
  group_by(lga) %>%
  mutate(max = max(ci_90_hi)) %>%
  arrange(max)

top_new_lgas <- df %>%
  filter(!lga %in% lockdown_lgas()) %>%
  summarise(peak = max(median)) %>%
  arrange(desc(peak)) %>%
  head(8) %>%
  pull(lga)

library(ggplot2)
base_colour <- blue

p <- df %>%
  ungroup() %>%
  filter(lga %in% top_new_lgas) %>%
  mutate(
    lga = factor(lga, levels = top_new_lgas),
    type = "Nowcast"
  ) %>%
  arrange(desc(max)) %>%
  ggplot() + 
  
  aes(date, mean, fill = type) +
  
  xlab(element_blank()) +
  
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "1 months", date_labels = "%b %d") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_ribbon(aes(ymin = ci_90_lo,
                  ymax = ci_90_hi),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = ci_50_lo,
                  ymax = ci_50_hi),
              alpha = 0.5) +
  geom_line(aes(y = ci_90_lo),
            colour = base_colour,
            alpha = 0.8) + 
  geom_line(aes(y = ci_90_hi),
            colour = base_colour,
            alpha = 0.8) + 
  
  facet_wrap(~lga, ncol = 2) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines")) +
  ylab("confirmed cases per day")

p

save_ggplot("lga_forecast.png", multi = TRUE)

png("~/Desktop/median_cases.png")
image(log1p(quants[3, , ]),
      main = "cases (median)")
dev.off()

png("~/Desktop/example1.png")
par(mfrow = c(2, 1), mar = c(2, 2, 2, 1))
i <- 1
image(log1p(results[[i]]$detections_matrix),
      main  = "cases")
image(results[[i]]$lockdown_matrix,
      main = "lockdown")
dev.off()

png("~/Desktop/example2.png")
par(mfrow = c(2, 1), mar = c(2, 2, 2, 1))
i <- 7
image(log1p(results[[i]]$detections_matrix),
      main  = "cases")
image(results[[i]]$lockdown_matrix,
      main = "lockdown")
dev.off()

# to do:
#  start with current lockdown
#  use Google data to construct an LGA movement matrix
#  use Google data to construct a gravity model (population, distance, both LGAs
#  in lockdown, one LGA in lockdown)
#  add more lockdown policies - including statewide lockdown trigger
#  define loss of control & compute probability of reaching it
#  make nicer plots