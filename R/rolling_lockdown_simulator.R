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
gravity_model <- function(geometry, population, coef = c(-10, -3, 1, 1)) {
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
  mobility
  
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
delay_prob <- function(date, state) {
  days <- seq_len(101) - 1 
  survival <- ttd_survival(
    days,
    rep(date, length(days)),
    target_state = state
  )
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
  reduction <- 1 - lockdown_status * (1 - config$leaving_reduction)
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
  new_infections <- rnbinom(n = n_suburbs,
                            size = config$size,
                            mu = reallocated_infections)
  # cap infections at a ludicrously high level to avoid numerical issues
  new_infections <- pmin(new_infections, 1e4)
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
   
    # where to assign detections
    elements <- delays[, 1:2, drop = FALSE]
    elements[, 1] <- elements[, 1] + idx
    valid <- elements[, 1] <= n_dates
    elements <- elements[valid, , drop = FALSE] 

    # numbers of new detections  
    new_detections <- delays[valid, 3]
    
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
                            lockdown_policy = no_policy,
                            lockdown_matrix = NULL) {
  
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

  n_dates <- length(dates)
  n_suburbs <- ncol(initial_expected_infections)
  
  if (is.null(lockdown_matrix)) {
    lockdown_matrix <- matrix(0, n_dates, n_suburbs)
  }
  
  # return configuration for this simulation
  list(
    n_dates = n_dates,
    n_suburbs = n_suburbs,
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
    lockdown_matrix = lockdown_matrix,
    lockdown_policy = lockdown_policy
  )
  
}

# change the policy in a config
set <- function(config, which, new_value) {
  config[[which]] <- new_value
  config
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
    lockdown_matrix = config$lockdown_matrix
  )
  
  # iterate the state dynamics
  results <- iterate_state(initial_state, config)
  
  # get summaries
  results$cases <- rowSums(results$detections_matrix)
  results$people_in_lockdown <- (results$lockdown_matrix %*% config$populations)[, 1]
  
  results
  
}

# reactive policy: if an LGA has a given number of new cases in a week implement
# an LGA-wide lockdown
reactive_policy <- function(count_threshold, count_days = 7) {
  
  lockdown_policy <- function(idx, state, config) {
    
    # count the number of cases in the last few days
    days <- idx - seq_len(count_days) + 1
    days <- unique(pmax(1, days))
    
    count <- colSums(state$detections_matrix[days, , drop = FALSE])
    
    # from here on, lock down that suburb
    new_lockdown <- count > count_threshold
    old_idx <- pmax(idx - 1, 1)
    current_lockdown <- state$lockdown_matrix[old_idx, ]
    state$lockdown_matrix[idx, ] <- pmax(as.numeric(new_lockdown), current_lockdown)
    
    state$lockdown_matrix
    
  }
  
  lockdown_policy
  
}

no_policy <- function(idx, state, config) {
  state$lockdown_matrix
}
  

# return a tibble summarisinng statewode case counts over time
summarise_statewide <- function(results, scenario = NULL) {
  lga_case_list <- lapply(results, `[[`, "detections_matrix")
  lga_case_sims <- do.call(abind, c(lga_case_list, list(along = 0)))
  vic_case_sims <- apply(lga_case_sims, 1:2, sum)
  
  quants <- apply(vic_case_sims, 2, quantile, c(0.05, 0.25, 0.5, 0.75, 0.95))
  
  tibble(
    scenario = scenario,
    date = dates,
    median = quants[3, ],
    ci_50_lo = quants[2, ],
    ci_50_hi = quants[4, ],
    ci_90_lo = quants[1, ],
    ci_90_hi = quants[5, ]
  )
  
}

# plot an LGA-level risk map, with overlaid lockdown border
lga_map <- function (object, fill,
                     source_geometry = NA,
                     source_lockdown = FALSE,
                     source_fill = NA,
                     risk_col = grey(0.4),
                     trans = "identity") {
  p <- object %>%
    select(!!fill) %>%
    ggplot() +
    aes(fill = !!as.name(fill)) +
    geom_sf(
      size = 0.1
    ) +
    scale_fill_gradient(
      low = grey(0.9),
      high = risk_col,
      trans = trans
    ) +
    theme_minimal()
  
  if (!is.na(source_geometry)) {
    
    p <- p +
      geom_sf(
        aes(fill = 1),
        data = source_geometry,
        col = "white",
        size = ifelse(source_lockdown, 1.5, 0.5),
        fill = source_fill
      )
    
    if (source_lockdown) {
      p <- p +
        geom_sf(
          aes(fill = 1),
          data = source_geometry,
          col = "black",
          size = 0.2,
          fill = NA
        )
    }

  }
  
  p
  
}

# load the expected number of infections by LGA
latest_lga_cases <- function(which = c("local", "imported")) {
  which <- match.arg(which)
  pattern <- paste0("lga_", which, "_infections_")
  files <- list.files("~/not_synced/", pattern = pattern, full.names = TRUE)
  lengths <- nchar(files)
  dates <- files %>%
    substr(nchar(.) - 13, nchar(.) - 4) %>%
    as.Date()
  keep <- which.max(dates)
  readRDS(files[keep])
}

compute_infectious <- function(state_cases, lga, import_rate,
                               sources = NULL) {
  
  # compute the remaining total future infectious potential in each LGA at the latest date
  gi_vec <- gi_vector(gi_cdf(), max(state_cases$date))
  potential_remaining <- 1 - cumsum(gi_vec)
  
  infectious <- state_cases %>%
    mutate(
      days_ago = as.numeric(max(date) - date)
    ) %>%
    left_join(
      tibble(
        days_ago = seq_along(potential_remaining),
        potential = potential_remaining
      )
    ) %>%
    mutate(
      potential = replace_na(potential, 0),
      lga_code = as.character(lga_code)
    ) %>%
    group_by(lga, lga_code) %>%
    summarise(
      infectious_potential = sum(potential * infections)
    ) %>%
    ungroup() %>%
    # add on missing lgas and infectious import potential
    old_right_join(
      tibble(
        lga = colnames(import_rate)
      )
    ) %>%
    mutate(
      infectious_potential = replace_na(infectious_potential, 0),
      import_potential = (infectious_potential %*% import_rate)[1, ]
    )
  
  # define the source lgas if not provided
  if (is.null(sources)) {
    sources <- infectious %>%
      filter(infectious_potential > 0) %>%
      pull(lga)
  }
  
  infectious <- infectious %>%
    # compute the risk of non-lockdown areas bing infected *by lockdown areas*
    mutate(
      is_source = lga %in% sources,
      lockdown_export_potential = ifelse(is_source, infectious_potential, 0),
      sink_import_potential = (lockdown_export_potential %*% import_rate)[1, ],
      sink_import_potential = ifelse(is_source, 0, sink_import_potential),
      sink_import_potential = sink_import_potential / max(sink_import_potential),
    )
  
  lga %>%
    st_simplify(dTolerance = 0.001) %>%
    left_join(infectious) %>%
    mutate_at(
      vars(
        infectious_potential,
        import_potential,
        sink_import_potential
      ),
      ~replace_na(., 0)
    ) %>%
    mutate(
      infectious_potential_area = infectious_potential / area,
    )
}

simple_border <- function(object, tol= 0.001) {
  object %>%
    st_geometry() %>%
    st_union() %>%
    st_simplify(dTolerance = tol)
}

border_line <- function(border) {
  geom_sf(
    aes(fill = 1),
    data = border,
    col = grey(0.4),
    linetype = "dashed",
    size = 0.4,
    fill = NA
  )
}

# strip the parenthetical nonsense from LGA names
lga_name <- function(lga) {
  strsplit(lga, " \\(") %>%
    lapply(`[`, 1) %>%
    unlist()
}

# which lga is highest risk?
max_risk_lga <- function(lga_infectious) {
  lga_infectious %>%
    arrange(desc(sink_import_potential)) %>%
    filter(row_number() == 1) %>%
    pull(lga) %>%
    lga_name()
}

set.seed(2020-07-04)

# locally-acquired cases by LGA
local_cases <- latest_lga_cases()

# Reff model parameter samples
parameter_draws <- readRDS("outputs/projection/postcode_forecast_draws.RDS")
n_samples <- 1000
size_samples <- parameter_draws$vic_size[seq_len(n_samples)]
r_eff_reduction_samples <- parameter_draws$vic_r_eff_reduction_full[seq_len(n_samples)]
leaving_reduction_samples <- runif(n_samples, 0.05, 0.3)
non_household_fraction <- parameter_draws$vic_fraction_non_household

# get the baseline fraction of new infections that are in another LGA
# from contact questions, 33% of trips by cases in the time prior to symptom
# onset were not within the same LGA
# from Reff model the fraction of transmisions that are to non-household members
# is around 45%
outside_transmission_fraction <- mean(non_household_fraction) * 0.33

# load LGA geometries and populations
# prep_lgas("Victoria")
# prep_state_lgas("New South Wales")
# prep_state_lgas("Australian Capital Territory")
vic_lga <- readRDS("data/spatial/vic_lga.RDS")
act_lga <- readRDS("data/spatial/act_lga.RDS")
nsw_lga <- readRDS("data/spatial/nsw_lga.RDS")

# crop out Norflok Island
nsw_lga <- st_crop(
  nsw_lga,
  c(
    xmin = 140,
    ymin = -38.5,
    xmax = 154,
    ymax = -27
  )
)

nsw_act_lga <- st_union(
  nsw_lga,
  act_lga
)

vic_border <- simple_border(vic_lga)
act_border <- simple_border(act_lga)
nsw_border <- simple_border(nsw_lga)

# get geometry for Melbourne & Mitchell lockdown region (for plotting)
vic_lockdown <- vic_lga %>%
  filter(lga %in% lockdown_lgas()) %>%
  simple_border()

# get list of NSW and ACT active case LGAs
nsw_act_source_lgas <- nsw_lga_active_cases() %>%
  select(
    lga_short = Local.Government.Area,
    cases = Cases
  ) %>%
  filter(
    cases > 0
  ) %>%
  left_join(
    tibble(lga = nsw_lga$lga) %>%
      mutate(
        lga_short = lga_name(lga)
      ) %>%
      # need to deal with the new Bayside LGA (Botany bay and Rockdale combined)
      mutate(
        lga_short = case_when(
          lga %in% c("Botany Bay (C)", "Rockdale (C)") ~ "Bayside",
          TRUE ~ lga_short
        )
      )
  ) %>%
  pull(lga) %>%
  # add on all ACT
  c(act_lga$lga)

# importation rates from gravity models fitted to facebook data
vic_import_rate <- "data/facebook/vic_baseline_gravity_movement.RDS" %>%
  readRDS() %>%
  set_leaving_probability(outside_transmission_fraction)

nsw_act_import_rate <- "data/facebook/nsw_act_baseline_gravity_movement.RDS" %>%
  readRDS() %>%
  set_leaving_probability(outside_transmission_fraction)

vic_lga_infectious <- local_cases %>%
  filter(
    state == "VIC",
  ) %>%
  compute_infectious(
    lga = vic_lga,
    import_rate = vic_import_rate,
    sources = lockdown_lgas()
  )

nsw_act_lga_infectious <- local_cases %>%
  filter(
    state %in% c("NSW", "ACT"),
  ) %>%
  compute_infectious(
    lga = nsw_act_lga,
    import_rate = nsw_act_import_rate,
    sources = nsw_act_source_lgas
  )


# plot the infectious potential and import potential by LGA
p_vic_inf_area <- vic_lga_infectious %>%
  lga_map("infectious_potential_area",
          source_geometry = vic_lockdown,
          source_lockdown = TRUE,
          trans = "sqrt",
          risk_col = "blue") +
  border_line(vic_border) +
  labs(
    fill = "Infectious\npotential\nper sq. km"
  ) +
  ggtitle(
    "Infectious potential across VIC",
    paste(
      "Locally-acquired cases weighted by remaining",
      "potential to infect, as at",
      format(
        max(local_cases$date),
        format = "%d %B"
      )
    )
  )

p_vic_nl_imp <- vic_lga_infectious %>%
  lga_map("sink_import_potential",
          source_geometry = vic_lockdown,
          source_lockdown = TRUE,
          source_fill = grey(0.8),
          risk_col = "red") +
  border_line(vic_border) +
  labs(
    fill = "Relative risk"
  ) +
  ggtitle(
    "Case importation risk from restricted to non-restricted VIC LGAs",
    paste0(
      "Relative to the most at-risk non-restricted LGA (",
      max_risk_lga(vic_lga_infectious),
      ")"
    )
  )

ggsave(
  plot = p_vic_inf_area,
  filename = "outputs/figures/vic_infectious_potential_area_map.png",
  width = 8, height = 6,
  dpi = 250
)

ggsave(
  plot = p_vic_nl_imp,
  filename = "outputs/figures/vic_importation_potential_map.png",
  width = 8, height = 6,
  dpi = 250
)

# and to plot state boundaries

# get source lga shapefile
nsw_act_sources <- nsw_act_lga_infectious %>%
  filter(lga %in% nsw_act_source_lgas) %>%
  st_geometry() %>%
  st_union() %>%
  st_simplify(dTolerance = 0.001)

p_nsw_inf_area <- nsw_act_lga_infectious %>%
  lga_map("infectious_potential_area",
          trans = "sqrt",
          risk_col = "blue") +
  border_line(nsw_border) +
  labs(
    fill = "Infectious\npotential\nper sq. km"
  ) +
  ggtitle(
    "Infectious potential across NSW and ACT",
    paste(
      "Locally-acquired cases weighted by remaining",
      "potential to infect, as at",
      format(
        max(local_cases$date),
        format = "%d %B"
      )
    )
  )

p_nsw_nl_imp <- nsw_act_lga_infectious %>%
  lga_map("sink_import_potential",
          source_geometry = nsw_act_sources,
          source_fill = grey(0.8),
          risk_col = "red") +
  border_line(nsw_border) +
  labs(
    fill = "Relative risk"
  ) +
  ggtitle(
    "Case importation risk to case-free NSW LGAs",
    paste0(
      "Relative to the most at-risk case-free LGA (",
      max_risk_lga(nsw_act_lga_infectious),
      ")"
    )
  )


ggsave(
  plot = p_nsw_inf_area,
  filename = "outputs/figures/nsw_infectious_potential_area_map.png",
  width = 8, height = 6,
  dpi = 250
)

ggsave(
  plot = p_nsw_nl_imp,
  filename = "outputs/figures/nsw_importation_potential_map.png",
  width = 8, height = 6,
  dpi = 250
)

# plot versions clipped to Sydney
sydney_coord <- coord_sf(
  xlim = c(149.7, 152.5),
  ylim = c(-34.5, -32.7)
)

p_sydney_inf_area <- p_nsw_inf_area + sydney_coord
p_sydney_nl_imp <- p_nsw_nl_imp + sydney_coord

ggsave(
  plot = p_sydney_inf_area,
  filename = "outputs/figures/sydney_infectious_potential_area_map.png",
  width = 6, height = 6,
  dpi = 250
)

ggsave(
  plot = p_sydney_nl_imp,
  filename = "outputs/figures/sydney_importation_potential_map.png",
  width = 6, height = 6,
  dpi = 250
)

p_sydney_pop <- nsw_act_lga %>%
  lga_map(fill = "pop",
          source_geometry = nsw_act_sources,
          source_fill = "light blue") +
  sydney_coord +
  labs(
    fill = "Population"
  ) +
  ggtitle(
    "Population size of case-free NSW LGAs",
    "Light blue region indicates LGAs with active cases"
  )

ggsave(
  plot = p_sydney_pop,
  filename = "outputs/figures/sydney_population_map.png",
  width = 6, height = 6,
  dpi = 250
)

# convert to importation rate, with probability of leaving
import_rate <- vic_import_rate

# read in numbers of cases by date of infection in LGAs, and pad with 0s for
# other LGAs
lga_infections <- local_cases %>%
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

# R effective across suburbs and times
n_lga <- nrow(vic_lga)
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
# gi_cdf <- nishiura_cdf()
gi_mat <- gi_matrix(gi_cdf, dates)
gi_mat_all <- gi_matrix(gi_cdf, dates_all)

# detection delay distribution probabilities for each date
delay_probs <- lapply(dates, delay_prob)
delay_probs_all <- lapply(dates_all, delay_prob)

# set up current lockdown situation
lockdown_matrix <- matrix(0, n_dates, n_lga)
lockdown_cols <- match(lockdown_lgas(), vic_lga$lga)
lockdown_rows <- dates >= as.Date("2020-07-02")
lockdown_matrix[lockdown_rows, lockdown_cols] <- 1

configs_base <- future_lapply(
  seq_len(n_samples),
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
  lockdown_matrix = lockdown_matrix
)

configs_no_response <- configs_base

configs_react_50 <- lapply(configs_base,
                          set,
                          "lockdown_policy",
                          reactive_policy(50))

configs_react_10 <- lapply(configs_base,
                           set,
                           "lockdown_policy",
                           reactive_policy(20))

statewide_lockdown_matrix <- lockdown_matrix
statewide_lockdown_matrix[dates > Sys.Date(), ] <- 1

configs_statewide <- lapply(configs_base,
                            set,
                            "lockdown_matrix",
                            statewide_lockdown_matrix)

results_no_response <- future_lapply(configs_no_response, run_simulation)
results_statewide <- lapply(configs_statewide, run_simulation)
results_react_50 <- future_lapply(configs_react_50, run_simulation)
results_react_10 <- future_lapply(configs_react_10, run_simulation)

df <- bind_rows(
  summarise_statewide(results_react_50, "New LGAs after 50 cases"),
  summarise_statewide(results_react_10, "New LGAs after 10 cases"),
  summarise_statewide(results_statewide, "All Victoria from July 14"),
  summarise_statewide(results_no_response, "Melbourne & Mitchell only")
)

base_colour <- blue

p <- df %>%
  group_by(scenario) %>%
  mutate(max = max(ci_90_hi)) %>%
  arrange(max) %>%
  ungroup() %>%
  mutate(
    scenario = factor(scenario),
    type = "Nowcast"
  ) %>%
  arrange(desc(max)) %>%
  ggplot() + 
  
  aes(date, median, fill = type) +
  
  xlab(element_blank()) +
  
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
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
  coord_cartesian(ylim = c(0, 600),
                  xlim = c(Sys.Date(), max(dates))) +
  
  facet_wrap(~scenario, ncol = 2) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines")) +
  ylab("confirmed cases per day") +
  ggtitle("Forecast under different restriction policies")

p

save_ggplot("scenario_forecast.png", multi = TRUE)

# to do:
#  start with current lockdown
#  add more lockdown policies - including statewide lockdown trigger
#  define loss of control & compute probability of reaching it
#  make nicer plots