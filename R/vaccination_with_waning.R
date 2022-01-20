source("R/functions.R")
library(gaussquad)

get_quantium_lookups <- function(dir) {
  
  lookups <- list(
    age = read_csv(
      sprintf(
        "%s/dim_age_band.csv",
        dir
      ),
      col_types = cols(
        age_band_id = col_double(),
        age_band = col_character()
      )
    ),
    product = read_csv(
      sprintf(
        "%s/dim_vaccine.csv",
        dir
      ),
      col_types = cols(
        vaccine = col_double(),
        name = col_character(),
        short_name = col_character()
      )
    ),
    date = read_csv(
      sprintf(
        "%s/dim_time.csv",
        dir
      ),
      col_types = cols(
        time = col_double(),
        week_starting = col_date("%d/%m/%Y")
      )
    ),
    scenario = read_csv(
      sprintf(
        "%s/dim_scenario.csv",
        dir
      ),
      col_types = cols(
        scenario = col_double(),
        `5-11 uptake curve` = col_character(),
        booster_shape = col_character(),
        booster_scale_by_age = col_character(),
        booster_uptake_terminal = col_double(),
        booster_uptake_months = col_double()
      )
    ),
    sa4 = read_csv(
      sprintf(
        "%s/dim_sa4.csv",
        dir
      )
    )
  )
  
  return(lookups)
  
}

sort_age_groups <- function(age_groups) {
  start <- str_split_fixed(age_groups, "-", 2)[, 1]
  order <- str_order(start, numeric = TRUE)
  age_groups[order]
}


read_quantium_vaccination_data <- function(
  date = NULL
){
  
  # get most recent forecast
  dir_dates <- list.dirs(
    path = "~/not_synced/vaccination/quantium_forecasts/",
    full.names = FALSE,
    recursive = FALSE
  ) %>%
    as.Date
  
  if (is.null(date)) {
    dir_index <- which.max(dir_dates)
  } else {
    dir_index <- which(dir.dates == date)
    if (length(dir_index) != 1){
      stop("Either no directory or too many directories match this date")
    }
  }
  
  dir <- list.dirs(
    path = "~/not_synced/vaccination/quantium_forecasts/",
    full.names = TRUE,
    recursive = FALSE
  )[dir_index]
  
  
  # load all vaccine data
  vaccines <- read_csv(
    file = sprintf(
      "%s/vaccines.csv",
      dir
    )
  )
  
  # load quantium lookup tables
  lookups <- get_quantium_lookups(dir = dir)

  # add on times, age bands, and products, and return
  vaccine_data <- vaccines %>%
    # join on the dates
    ungroup() %>%
    left_join(
      lookups$date,
      by = c(
        "time_dose_1" = "time"
      )
    ) %>%
    rename(
      date_dose_1 = week_starting
    ) %>%
    left_join(
      lookups$date,
      by = c(
        "time_dose_2" = "time"
      )
    ) %>%
    rename(
      date_dose_2 = week_starting
    ) %>%
    left_join(
      lookups$date,
      by = c(
        "time_booster" = "time"
      )
    ) %>%
    rename(
      date_booster = week_starting
    ) %>%
    select(
      -starts_with("time")
    ) %>%
    # join on the products
    left_join(
      lookups$product,
      by = "vaccine"
    ) %>%
    select(
      -vaccine,
      -short_name
    ) %>%
    rename(
      vaccine = name
    ) %>%
    left_join(
      lookups$product,
      by = c("vaccine_booster" = "vaccine")
    ) %>%
    select(
      -vaccine_booster,
      -short_name
    ) %>%
    rename(
      vaccine_booster = name
    ) %>%
    # join on the age bands and convert to a factor
    left_join(
      lookups$age,
      by = "age_band_id"
    ) %>%
    select(
      -age_band_id
    ) %>%
    mutate(
      age_band = factor(
        age_band,
        levels = sort_age_groups(unique(age_band))
      )
    ) %>%
    left_join(
      lookups$sa4,
      by = "sa4_code16"
    )
  
  return(vaccine_data)
  
}



vaccine_raw <- read_quantium_vaccination_data()

aggregate_quantium_vaccination_data_to_state <- function(data){
  
  data %>%
    left_join(
      state_short_long_table,
      by = c("STE_NAME16" = "state_long")
    ) %>%
    group_by(
      scenario,
      date_dose_1,
      date_dose_2,
      date_booster,
      vaccine,
      vaccine_booster,
      age_band,
      state_short
    ) %>%
    summarise(
      num_people = sum(num_people, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(state = state_short) %>%
    select(
      scenario,
      state,
      age_band,
      vaccine,
      vaccine_booster,
      date_dose_1,
      date_dose_2,
      date_booster,
      num_people
    ) %>%
    arrange(
      scenario,
      state,
      age_band,
      vaccine,
      vaccine_booster,
      date_dose_1,
      date_dose_2,
    ) %>%
    # excluding vaccinations outside of the 6 states or ACT and NT
    filter(!is.na(state))
  
}

vaccine_state <- aggregate_quantium_vaccination_data_to_state(vaccine_raw)

vaccine_state


get_vaccine_cohorts_at_date <- function(vaccine_scenarios, target_date) {
  
  # set future times to NA and collapse to get contemporary data
  vaccine_scenarios %>%
    mutate(
      across(
        starts_with("date"),
        ~if_else(.x > target_date, as.Date(NA), .x)
      )
    ) %>%
    group_by(
      across(
        -num_people
      )
    ) %>%
    summarise(
      num_people = sum(num_people),
      .groups = "drop"
    ) %>%
    # compute most recent vaccines and how long ago they were for each cohort
    mutate(
      most_recent_dose = pmax(date_dose_1, date_dose_2, date_booster, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = starts_with("date"),
      names_to = "dose",
      values_to = "date",
      names_prefix = "date_"
    ) %>%
    # keep only one of these entries
    mutate(
      keep = case_when(
        is.na(date) ~ dose == "dose_1",
        TRUE ~ date == most_recent_dose
      )
    ) %>%
    filter(
      keep
    ) %>%
    select(
      -most_recent_dose,
      -keep
    ) %>%
    mutate(
      dose = if_else(is.na(date), NA_character_, dose),
      # labelling all booster as boosters instead of by brand, thus assuming all boosters are mRNA
      vaccine_booster = if_else(
        !is.na(vaccine_booster),
        "Booster",
        NA_character_
      ),
      product = case_when(
        dose == "booster" ~ vaccine_booster,
        TRUE ~ vaccine
      ),
      # rename products to match VE model, recoding Moderna as Pfizer for now
      # since there is not enough evidence on efficacy to distringuish it from
      # Pfizer
      product = case_when(
        product == "Pfizer" ~ "Pf",
        product == "AstraZeneca" ~ "AZ",
        product == "Moderna" ~ "Pf",
        product == "Booster" ~ "mRNA",
        product == "Pfizer (5-11)" ~ "Pf"
      ),
      immunity = case_when(
        !is.na(date) ~ paste(product, dose, sep = "_"),
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      days_ago = as.numeric(target_date - date)
    ) %>%
    select(
      -starts_with("vaccine"),
      -product,
      -dose,
      -date
    )
  
}

target_date <- as.Date("2022-01-01")
vaccine_cohorts_now <- get_vaccine_cohorts_at_date(
  vaccine_scenarios = vaccine_state,
  target_date = target_date
)

get_coverage <- function(vaccine_cohorts) {
  
  # get current coverage with any dose in each age band, for each scenario
  coverage <- vaccine_cohorts %>%
    mutate(
      immune = !is.na(immunity)
    ) %>%
    group_by(
      scenario, age_band
    ) %>%
    summarise(
      coverage = weighted.mean(immune, num_people),
      .groups = "drop"
    )
  
}


coverage_now <- get_coverage(vaccine_cohorts_now)

get_omicron_params_wide <- function() {
  read_csv(
    "outputs/scenario_parameters_omicron.csv",
    col_types = cols(
      parameter = col_character(),
      intermediate = col_double(),
      optimistic = col_double(),
      pessimistic = col_double()
    )
  ) %>%
    pivot_longer(
      cols = c(-parameter),
      names_to = "omicron_scenario",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = parameter,
      values_from = value
    )
}

log10_neut_over_time <- function (time, maximum_log10_neut, decay){
  # equivalent to: log10(10 ^ maximum_log10_neut * exp(-decay * time))
  maximum_log10_neut - decay * time / log(10)
}

ve_from_mean_log10_neut <- function(
  mean_log10_neut_vec,
  sd_log10_neut,
  log_k,
  c50_vec,
  method = c("adaptive", "gaussian"),
  lower = -10,
  upper = 10
) {
  
  # choose the method and dispatch to the appropriate integration function
  method <- match.arg(method)
  
  integrator <- switch(
    method,
    adaptive = adaptive_ve_integrator,
    gaussian = gaussian_ve_integrator
  )
  
  integrals <- integrator(
    c50_vec = c50_vec,
    mean_log10_neut_vec = mean_log10_neut_vec,
    sd_log10_neut = sd_log10_neut,
    log_k = log_k,
    lower = lower,
    upper = upper
  )
  
  integrals
  
}

gaussian_ve_integrator <- function(
  c50_vec,
  mean_log10_neut_vec,
  sd_log10_neut,
  log_k,
  lower,
  upper
) {
  
  # dimensions and quadrature rules
  n_obs <- length(mean_log10_neut_vec)
  quads <- get_quad_rules(n_obs, lower = lower, upper = upper)
  n_quads <- length(quads$values)
  
  # expand out the vector parameters of the logit-normal density to matrices
  if (is.vector(c50_vec) & is.vector(mean_log10_neut_vec)) {
    c50_vec <- as.matrix(c50_vec)
    mean_log10_neut_vec <- as.matrix(mean_log10_neut_vec)
    vector_input <- TRUE
  } else {
    vector_input <- FALSE
  }
  
  repeater <- rep(1, n_quads)
  c50_mat <- c50_vec[, repeater]
  mean_log10_neut_mat <- mean_log10_neut_vec[, repeater]
  
  # and expand out the integration points to match
  values_matrix <- t(replicate(n_obs, quads$values))
  
  # get function values in matrix
  function_values <- logit_normal_density(
    x = values_matrix,
    c50 = c50_mat,
    mean_log10_neut = mean_log10_neut_mat,
    sd_log10_neut = sd_log10_neut,
    log_k = log_k
  )
  
  weights <- quads$weights
  
  # if we're doing this with greta arrays, we need to work around an issue with
  # greta checking matrix multiplly dimensions too early
  if (inherits(function_values, "greta_array")) {
    weights <- as_data(quads$weights)
  }
  
  ves <- function_values %*% weights
  
  if(vector_input) {
    dim(ves) <- NULL
  }
  
  ves
  
}

adaptive_ve_integrator <- function(
  c50_vec,
  mean_log10_neut_vec,
  sd_log10_neut,
  log_k,
  lower,
  upper
) {
  
  
  if (
    inherits(c50_vec, "greta_array") |
    inherits(mean_log10_neut_vec, "greta_array") |
    inherits(sd_log10_neut, "greta_array") |
    inherits(log_k, "greta_array")
  ) {
    stop ("adaptive integration can not be used with greta models")
  }
  
  integrate_once <- function(c50, mean_log10_neut) {
    integral <- integrate(
      f = logit_normal_density,
      c50 = c50,
      mean_log10_neut = mean_log10_neut,
      sd_log10_neut = sd_log10_neut,
      log_k = log_k,
      lower = lower,
      upper = upper
    )$value
  }
  
  logit_normal_density(0, c50_vec[1], mean_log10_neut = mean_log10_neut_vec[1], sd_log10_neut = sd_log10_neut, log_k = log_k)
  
  integrals <- mapply(
    integrate_once,
    c50_vec,
    mean_log10_neut_vec
  )
  
  integrals
  
}

get_quad_rules <- function(n_observations, lower = -3, upper = 3) {
  
  n_quads <- round(5 * (upper - lower))
  
  # get quadrature rules on (-1, 1)
  quads <- gaussquad::legendre.quadrature.rules(n_quads)[[n_quads]]
  
  # transform them to (lower, upper) and return
  lambda <- (upper - lower) / 2
  mu <- (lower + upper) / 2
  
  list(
    values = lambda * quads$x + mu,
    weights = lambda * quads$w
  )
  
}

logit_normal_density <- function(x, c50, mean_log10_neut, sd_log10_neut, log_k) {
  prob <- prob_avoid_outcome(log10_neut = x, log_k = log_k, c50 = c50)
  dens <- log10_neut_density(x, mean_log10_neut, sd_log10_neut)
  prob * dens
}

prob_avoid_outcome <- function(log10_neut, log_k, c50) {
  1 / (1 + exp(-exp(log_k) * (log10_neut - c50)))
}

log10_neut_density <- function(x, mean, sd) {
  
  with_greta <- inherits(x, "greta_array") |
    inherits(mean, "greta_array") |
    inherits(sd, "greta_array")
  
  if (with_greta) {
    normal_density(x, mean, sd)
  } else {
    dnorm(x, mean, sd)
  }
}


get_vaccine_efficacies <- function(vaccine_cohorts) {
  
  # load omicron parameters in wide format and subset to different parameter sets
  params_wide <- get_omicron_params_wide()
  
  neut_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      log10_mean_neut_AZ_dose_1,
      log10_mean_neut_AZ_dose_2,
      log10_mean_neut_Pfizer_dose_1,
      log10_mean_neut_Pfizer_dose_2,
      log10_mean_neut_mRNA_booster,
      neut_decay
    )
  
  ve_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      starts_with("c50"),
      log_k,
      sd_log10_neut_titres,
      omicron_log10_neut_fold
    )
  
  # compute the average neutralisation level (mean log10 neut fold of WT
  # convalescent) in each age group, scenario, and omicron scenario
  mean_neuts <- vaccine_cohorts %>%
    filter(
      !is.na(immunity)
    ) %>%
    full_join(
      tibble(
        omicron_scenario = c(
          "intermediate",
          "optimistic",
          "pessimistic"
        )
      ),
      by = character()
    ) %>%
    left_join(
      neut_params_wide,
      by = "omicron_scenario"
    ) %>%
    # compute the peak and waned log10 mean neuts for each cohort
    mutate(
      peak_neuts = case_when(
        immunity == "AZ_dose_1" ~ log10_mean_neut_AZ_dose_1,
        immunity == "AZ_dose_2" ~ log10_mean_neut_AZ_dose_2,
        immunity == "Pf_dose_1" ~ log10_mean_neut_Pfizer_dose_1,
        immunity == "Pf_dose_2" ~ log10_mean_neut_Pfizer_dose_2,
        immunity == "mRNA_booster" ~ log10_mean_neut_mRNA_booster
      )
    ) %>%
    mutate(
      neuts = log10_neut_over_time(
        time = days_ago,
        maximum_log10_neut = peak_neuts,
        decay = neut_decay
      )
    ) %>%
    select(
      -starts_with("log10_mean_neut"),
      -peak_neuts,
      -neut_decay
    ) %>%
    # average the mean neuts over cohorts and scenarios
    group_by(
      scenario, state, omicron_scenario, age_band
    ) %>%
    summarise(
      neuts = weighted.mean(neuts, num_people),
      .groups = "drop"
    )
  
  # now compute VEs against each outcome, for Omicron and Delta
  ves <- mean_neuts %>%
    left_join(
      ve_params_wide,
      by = "omicron_scenario"
    ) %>%
    # for omicron, adjust down the neuts
    full_join(
      tibble(
        variant = c("Delta", "Omicron")
      ),
      by = character()
    ) %>%
    mutate(
      neuts = case_when(
        variant == "Omicron" ~ neuts + omicron_log10_neut_fold,
        TRUE ~ neuts
      )
    ) %>%
    # compute all the VEs in one shot with Gaussian integration
    pivot_longer(
      cols = starts_with("c50"),
      names_to = "outcome",
      values_to = "c50",
      names_prefix = "c50_"
    ) %>%
    mutate(
      ve = ve_from_mean_log10_neut(
        mean_log10_neut_vec = neuts,
        sd_log10_neut = sd_log10_neut_titres,
        log_k = log_k,
        c50_vec = c50,
        method = "gaussian"
      )
    ) %>%
    select(
      -neuts,
      -log_k,
      -sd_log10_neut_titres,
      -omicron_log10_neut_fold,
      -c50
    )
  
  ves
  
}


ves_now <- get_vaccine_efficacies(vaccine_cohorts_now)

et_vaccine_transmission_effects <- function(ves, coverage) {
  
  lookups <- get_quantium_lookups()
  
  # get a conmat NGM for Australia
  australia_ngm <- get_ngm()
  
  # combine coverage and VEs to get transmission reduction for each rollout
  # scenario, and omicron scenario
  ves %>%
    # add back in the younger age_groups
    complete(
      scenario,
      omicron_scenario,
      variant,
      outcome,
      age_band = unique(lookups$age$age_band),
      fill = list(
        ve = 0
      )
    ) %>%
    # get the two transmission VEs as columns
    filter(
      outcome %in% c("acquisition", "transmission")
    ) %>%
    pivot_wider(
      names_from = outcome,
      values_from = ve
    ) %>%
    group_by(
      scenario,
      omicron_scenario,
      variant
    ) %>%
    arrange(
      age_band,
      .by_group = TRUE
    ) %>%
    # join on coverages
    left_join(
      coverage,
      by = c("scenario", "age_band")
    ) %>%
    # compute percentage reduction in acquisition and transmission in each age group
    mutate(
      acquisition_multiplier = 1 - acquisition * coverage,
      transmission_multiplier = 1 - transmission * coverage,
    ) %>%
    select(
      -acquisition,
      -transmission,
      -coverage
    ) %>%
    # transform these into matrices of reduction in transmission, matching the NGM
    summarise(
      transmission_reduction_matrix =
        list(
          outer(
            # 'to' groups are on the rows in conmat, and first element in outer is rows,
            # so acquisition first
            acquisition_multiplier,
            transmission_multiplier,
            FUN = "*"
          )
        ),
      .groups = "drop"
    ) %>%
    group_by(
      scenario,
      omicron_scenario,
      variant
    ) %>%
    mutate(
      ngm_unvaccinated = list(australia_ngm),
      ngm_vaccinated = list(ngm_unvaccinated[[1]] * transmission_reduction_matrix[[1]]),
      vaccination_effect = 1 - get_R(ngm_vaccinated[[1]]) / get_R(ngm_unvaccinated[[1]])
    ) %>%
    select(
      -transmission_reduction_matrix,
      -ngm_unvaccinated,
      -ngm_vaccinated
    )
  
}


vaccine_transmission_effects_now <- get_vaccine_transmission_effects(
  ves = ves_now,
  coverage = coverage_now
)

