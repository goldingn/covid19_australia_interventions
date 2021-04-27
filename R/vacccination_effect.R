library(dplyr)
library(readr)
library(purrr)
library(readxl)
library(tidyr)
library(rvest)


get_ifr <- function() {
  
  # Age-structured infection fatality ratio (%) estimates from O'Driscoll et al.
  # 2020 https://doi.org/10.1038/s41586-020-2918-0 (Table S3 in supplement) and
  # Brazeau et al. 2020 (Imperial report 34)
  # https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-34-ifr/
  # (Table 2 - estimate *with* seroreversion)
  
  # Brazeau et al has separate estimates for ages 80-84, 85-89, 90+, so we
  # recombine these based on ABS 2020 population age fractions into the 80+
  # category (50% ofover 80s are in 80-84, 30% are in 85-89, and 20% are 90 or
  # older)
  
  tibble::tribble(
    ~age,      ~odriscoll,     ~brazeau,
    "0-4",          0.003,         0.00,
    "5-9",          0.001,         0.01,
    "10-14",        0.001,         0.01,
    "15-19",        0.003,         0.02,
    "20-24",        0.006,         0.02,
    "25-29",        0.013,         0.04,
    "30-34",        0.024,         0.06,
    "35-39",        0.040,         0.09,
    "40-44",        0.075,         0.15,
    "45-49",        0.121,         0.23,
    "50-54",        0.207,         0.36,
    "55-59",        0.323,         0.57,
    "60-64",        0.456,         0.89,
    "65-69",        1.075,         1.39,
    "70-74",        1.674,         2.17,
    "75-79",        3.203,         3.39,
    "80+",          8.292,         5.3*0.5 + 8.28*0.3 + 16.19*0.2
  )
  
}

# scrape the cumulative number of vaccine doses delivered in Australia by date,
# from covidlive.com.au
scrape_doses_timeseries <- function() {
  
  url <- "https://covidlive.com.au/report/daily-vaccinations/aus"
  
  cumulative_doses <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table(fill = TRUE) %>%
    mutate(
      date = as.Date(DATE, format = "%d %B %y"),
      doses = DOSES,
      doses = gsub(",", "", doses),
      doses = as.numeric(doses)
    ) %>%
    select(date, doses)
    
  
  cumulative_doses
  
}

# compute R from a transition (next generation) matrix
get_R <- function (transition_matrix, stable_age = NULL, tolerance = 0.001, max_iter = 1000) {
  
  if (is.null(stable_age)) {
    stable_age <- rep(1, ncol(transition_matrix))
  }
  old_stages <- stable_age
  converged <- FALSE
  iter <- 0
  old_Rs <- rep(.Machine$double.eps, ncol(transition_matrix))
  
  while (!converged & iter < max_iter) {
    new_stages <- transition_matrix %*% old_stages
    Rs <- new_stages / old_stages
    errors <- abs(1 - (Rs / old_Rs))
    converged <- all(errors < tolerance)
    old_Rs <- Rs
    old_stages <- new_stages
    iter <- iter + 1
  }
  
  if (!converged) {
    warning(
      "estimation of growth rate did not converge in ",
      max_iter,
      " iterations"
    )
  }
  
  # return the intrinsic growth rate
  Rs[1]
  
}

# compute the m that calibrates a next generation matrix to R0
find_m <- function(R_target, transition_matrix, stable_age = NULL) {
  
  obj <- function (m, R_target, transition_matrix, stable_age = NULL) {
    new_transition_matrix <- m*transition_matrix
    R_current <- get_R(new_transition_matrix, stable_age = stable_age)
    (R_current - R_target) ^ 2
  } 
  
  out <- stats::optimise(f = obj,
                         interval = c(0, 1),
                         R_target,
                         transition_matrix,
                         stable_age)
  out$minimum
  
  return(out$minimum)
}

disaggregation_vec <- function(mask, distribution) {
  masked_distribution <- mask * distribution
  masked_distribution / sum(masked_distribution)
}

# dsagregate a population across age groups, based on the age groups in that
# population and the national age distribution
disaggregate <- function(population, mask) {
  masked_age_distribution <- age_distribution * mask
  disaggregation <- masked_age_distribution / sum(masked_age_distribution)
  population * disaggregation
}

# construct a next generation matrix for Australia from Prem matrix
baseline_matrix <- function(R0 = 2.5, final_age_bin = 80) {
  
  # Prem 2017 contact matrix
  contact_matrix_raw <- readxl::read_xlsx(
    path = "data/vaccinatinon/MUestimates_all_locations_1.xlsx",
    sheet = "Australia",
    col_types = rep("numeric", 16)
  ) %>%
    as.matrix
  
  # expand out to add an 80+ category the same as the 75-80 category
  contact_matrix <- matrix(NA, 17, 17)
  contact_matrix[17, 17] <- contact_matrix_raw[16, 16]
  contact_matrix[17, 1:16] <- contact_matrix_raw[16, ]
  contact_matrix[1:16, 17] <- contact_matrix_raw[, 16]
  contact_matrix[1:16, 1:16] <- contact_matrix_raw

  # set names
  bin_names <- age_classes(80)$classes 
  dimnames(contact_matrix) <- list(
    bin_names,
    bin_names
  )
  
  # relative infectiousness data from Trauer et al 2021
  age_susceptability <- readr::read_csv(
    file = "data/vaccinatinon/trauer_2021_supp_table5.csv",
    col_names = c(
      "age_group",
      "clinical_fraction",
      "relative_susceptability",
      "infection_fatality_rate",
      "proportion_symtomatic_hospitalised"
    ),
    col_types = cols(
      age_group = col_character(),
      clinical_fraction = col_double(),
      relative_susceptability = col_double(),
      infection_fatality_rate = col_double(),
      proportion_symtomatic_hospitalised = col_double()
    ),
    skip = 1
  ) %>%
    # duplicate the final row, and re-use for the 74-79 and 80+ classes
    add_row(
      .[16, ]
    ) %>%
    mutate(
      age_class = bin_names
    ) %>%
    dplyr::select(
      age_class,
      everything(),
      -age_group
    )
  
  # calculate relative infectiousness - assume asymptomatics are 50% less
  # infectious, and use age-stratified symptomaticity
  relative_infectiousness <- age_susceptability$clinical_fraction*1 + 0.5*(1 - age_susceptability$clinical_fraction)
  q <- relative_infectiousness
  q_scaled <- q/max(q)
  
  # apply the q scaling before computing m
  contact_matrix_scaled <- sweep(contact_matrix, 2, q_scaled, FUN = "*")
  
  # calculate m - number of onward infections per relative contact 
  m <- find_m(
    R_target = R0,
    transition_matrix = contact_matrix_scaled
  )
  
  contact_matrix_scaled * m
  
}

age_classes <- function(final_age_bin = 80, by = 5) {
  
  # compute age classes based on this spec
  ages_lower = seq(0, final_age_bin, by = by)
  n_ages <- length(ages_lower)
  ages_upper = ages_lower + by - 1
  ages_upper[n_ages] <- Inf
  
  age_classes <- c(
    paste(
      ages_lower[-n_ages],
      ages_upper[-n_ages],
      sep = "-"
    ),
    paste0(
      final_age_bin,
      "+"
    )
  )
  
  tibble::tibble(
    classes = age_classes,
    lower = ages_lower,
    upper = ages_upper
  )
  
}

get_age_distribution <- function(
  final_age_bin = 85,
  by = 5,
  population_total = 25693000
) {
  
  # check the final age bin in sensible
  if (final_age_bin > 85) {
    stop(
      "No age-specific population data for ages greater than 85",
      call. = TRUE
    )
  }

  ages <- age_classes(
    final_age_bin = final_age_bin,
    by = by
  )
  
  # Age structure of the Australian population by year of age, up to 100+
  # This is a "standard" distribution data frame but old population size data
  # from 2001 hence is adjusted later
  # aust_population_standard <- readxl::read_xls(
  #   path = "data/vaccinatinon/abs_standard_age_31010DO003_200106.xls",
  #   sheet = "Table_1",
  #   skip = 6,
  #   col_names = c(
  #     "age",
  #     "pop"
  #   )
  # ) %>%
  #   filter(age != "Total", !is.na(age), age != "© Commonwealth of Australia 2013") %>%
  #   mutate(
  #     age = case_when(
  #       age == "100 and over" ~ "100",
  #       TRUE ~ age
  #     ) %>%
  #       as.integer,
  #   )
  
  # use 2020 population, as this better matches proportion 80+
  aust_population_2020 <- readxl::read_xls(
    path = "data/vaccinatinon/abs_population_2020.xls",
    sheet = "Table_8",
    range = cell_rows(c(223:328)),
    col_names = as.character(1:10)
  ) %>%
    select(1, 10) %>%
    rename(
      age = "1",
      pop = "10"
    ) %>%
    select(
      age,
      pop
    ) %>%
    mutate(
      age = case_when(
        age == "85-89" ~ "85",
        age == "90-94" ~ "85",
        age == "95-99" ~ "85",
        age == "100 and over" ~ "85",
        TRUE ~ age
      )
    ) %>%
    filter(
      !grepl("-", age)
    ) %>%
    group_by(
      age
    ) %>%
    summarise(
      pop = sum(pop),
      .groups = "drop"
    ) %>%
    mutate(
      age = as.integer(age)
    )
  
  # aggregate into age classes and return
  age_class_fractions <- aust_population_2020 %>%
    mutate(
      age_class = cut(
        age,
        breaks = c(ages$lower - 1, Inf),
        labels = ages$classes
      ),
      age_class = as.character(age_class)
    ) %>%
    group_by(
      age_class
    ) %>%
    summarise(
      pop = sum(pop),
      .groups = "drop"
    ) %>%
    mutate(
      fraction = pop / sum(pop),
      pop = fraction * population_total
    )

  age_class_fractions
  
}

# allocation leads to over vaccination in some groups - try to fix this!

# also cap coverage (of single doses) at 2?

# disaggregation vectors for different vaccination groups
phase_age_populations <- function() {
  
  ages <- age_classes(final_age_bin = 80)
  distribution <- get_age_distribution(final_age_bin = 80)$fraction
  
  # disaggregation vectors (summing to 1) into age classes for different
  # population groups
  under_50 <- disaggregation_vec(ages$lower < 50, distribution)
  over_50 <- disaggregation_vec(ages$lower >= 50, distribution)
  working_under_50 <- disaggregation_vec(ages$lower >= 16 & ages$lower < 50,
                                         distribution)
  working_over_50 <- disaggregation_vec(ages$lower >= 50 & ages$lower < 65,
                                        distribution)
  over_50_under_65 <- disaggregation_vec(ages$lower >= 50 & ages$lower < 65, distribution)
  over_50_under_70 <- disaggregation_vec(ages$lower >= 50 & ages$lower < 70, distribution)
  over_65 <- disaggregation_vec(ages$lower >= 65, distribution)
  over_70_under_80 <- disaggregation_vec(ages$lower >= 70 & ages$lower < 80, distribution)
  over_80 <- disaggregation_vec(ages$lower >= 80, distribution)
  
  # population sizes in vaccination roll-out groups
  populations_1A <- list(
    aged_care_residents = 183000 * over_65,
    disability_residents_u50 = 5000 * under_50,
    disability_residents_o50 = 21000 * over_50_under_65,
    border_workers_u50 = 16000 * working_under_50,
    border_workers_o50 = 11000 * working_over_50,
    care_staff_u50 = 143000 * working_under_50,
    care_staff_o50 = 107000 * working_over_50,
    health_staff_priority_u50 = 222000 * working_under_50,
    health_staff_priority_o50 = 90000 * working_over_50
  )
  
  populations_1B <- list(
    elderly_o80 = 915000 * over_80,
    elderly_70_79 = 1857000 * over_70_under_80,
    health_staff_other_u50 = 267000 * working_under_50,
    health_staff_other_o50 = 126000 * working_over_50,
    atsi_o50 = 91000 * over_50_under_70,
    medical_condition_u50 = 896000 * under_50,
    medical_condition_o50 = 1167000 * over_50_under_70,
    priority_workers_u50 = 201000 * working_under_50,
    priority_workers_o50 = 67000 * working_over_50
  )
  
  # combine these for each phase
  list(
    phase_1A = Reduce(`+`, populations_1A),
    phase_1B = Reduce(`+`, populations_1B)
  )

}

doses_by_age <- function(n_doses, age_populations) {
  
  # assuming a single dose per person, preferential allocation to group 1A
  # and then subsequent partial coverage in 1B, guesstimate the number of doses
  # given out in each age group coverage in each age group
  population_1A <- sum(age_populations$phase_1A)
  population_1B <- sum(age_populations$phase_1B)
  n_doses_1A <- min(n_doses, population_1A)
  age_doses_1A <- n_doses_1A * age_populations$phase_1A / population_1A
  n_doses_1B <- n_doses - n_doses_1A
  age_doses_1B <- n_doses_1B * age_populations$phase_1B / population_1B
  
  # combine into total doses so far
  age_doses_1A + age_doses_1B
  
}

# compute the average efficacy of doses, given the proportion of vaccines from
# each provider, and the proportion people who are vaccined with 2 doses - these
# are estiamtes for b.1.1.7, from Jammes Wood's email
average_efficacy <- function(
  efficacy_pf_2_dose = 0.9,
  efficacy_az_2_dose = 0.7,
  efficacy_pf_1_dose = efficacy_pf_2_dose/2,
  efficacy_az_1_dose = efficacy_az_2_dose/2,
  proportion_pf = 0.5,
  proportion_2_dose = 0
) {
  
  # proportion of each vaccine
  proportion_az <- 1 - proportion_pf
  
  # proportion fully dosed
  proportion_1_dose <- 1 - proportion_2_dose
  
  efficacy_mean <- proportion_2_dose * proportion_pf * efficacy_pf_2_dose +
    proportion_1_dose * proportion_pf * efficacy_pf_1_dose +
    proportion_2_dose * proportion_az * efficacy_az_2_dose +
    proportion_1_dose * proportion_az * efficacy_az_1_dose 
  
  efficacy_mean 
  
}

average_ifr_efficacy <- function() {
  average_efficacy(
    efficacy_pf_2_dose = 0.95,
    efficacy_az_2_dose = 0.95,
    efficacy_pf_1_dose = 0.8,
    efficacy_az_1_dose = 0.8
  )
}

average_transmission_efficacy <- function() {
  average_efficacy(
    efficacy_pf_2_dose = 0.9,
    efficacy_az_2_dose = 0.7,
  )
}

# given vaccination coverage in each age group, the average vaccine efficacy (by
# age or overall), and the baseline next generation matrix, compute the
# reduction in transmission for each age and overall
vaccination_transmission_effect <- function(
  age_coverage,
  efficacy_mean,
  next_generation_matrix
) {
  
  age_transmission_reduction <- 1 - age_coverage * efficacy_mean
  vc_next_gen_matrix <- sweep(
    next_generation_matrix,
    2,
    age_transmission_reduction,
    FUN = "*"
  )
  
  overall <- get_R(vc_next_gen_matrix) / get_R(next_generation_matrix)
  
  list(
    by_age = age_transmission_reduction,
    overall = overall
  )
  
}

vaccination_ifr_effect <- function(
  age_coverage,
  efficacy_mean,
  ifr
) {
  
  age_structure <- get_age_distribution(80)
  
  # compute age-specific IFRs, post vaccination
  age_reduction <- 1 - (age_coverage * efficacy_mean)
  age_odriscoll <- ifr$odriscoll * age_reduction
  age_brazeau <- ifr$brazeau * age_reduction

  overall_odriscoll <- sum(age_structure$fraction * age_odriscoll)
  overall_brazeau <- sum(age_structure$fraction * age_brazeau)
    
  list(
    age_reduction = age_reduction,
    age = list(
      odriscoll = age_odriscoll,
      brazeau = age_brazeau
    ),
    overall = list(
      odriscoll = overall_odriscoll,
      brazeau = overall_brazeau
    )
  )
  
}

# Given a number of doses, compute vaccine coverage in each age group, the
# effect on reducing transmission among the whole population, and the efect on
# reducing transmission in the most-vaccinated age group
summarise_effect <- function(
  n_doses,
  age_populations,
  age_distribution,
  next_generation_matrix,
  ifr
) {
  
  # compute coverage
  age_doses <- doses_by_age(n_doses, age_populations)
  age_coverage <- age_doses / age_distribution$pop

  # compute effect on transmission  
  transmission_effect <- vaccination_transmission_effect(
    age_coverage = age_coverage,
    efficacy_mean = average_transmission_efficacy(),
    next_generation_matrix = next_generation_matrix
  )
  
  # compute effect on IFR
  ifr_effect <- vaccination_ifr_effect(
    age_coverage = age_coverage,
    efficacy_mean = average_ifr_efficacy(),
    ifr = ifr
  )
  
  list(
    coverage_by_age = age_coverage,
    transmission = transmission_effect,
    ifr = ifr_effect
  )
  
}

overall_transmission_effect <- function (
  n_doses,
  age_populations,
  age_distribution,
  next_generation_matrix,
  ifr
) {
  all_effects <- summarise_effect(
    n_doses,
    age_populations,
    age_distribution,
    next_generation_matrix,
    ifr
  )
  all_effects$transmission$overall
}

overall_ifr_effect <- function (
  n_doses,
  age_populations,
  age_distribution,
  next_generation_matrix,
  ifr
) {
  all_effects <- summarise_effect(
    n_doses,
    age_populations,
    age_distribution,
    next_generation_matrix,
    ifr
  )
  all_effects$overall_ifr_reduction
}

# extract results for the overall transmission effect from a list of outputs
# from summarise_effect
extract_overall_transmission_effect <- function(x) {
  x$transmission$overall
}

# extract results for the population-wide IFR from a list of outputs from
# summarise_effect
extract_overall_ifr_effect <- function(x, which = c("odriscoll", "brazeau")) {
  which <- match.arg(which)
  x$ifr$overall[[which]]
}

# extract age-specific IFRs after vaccination  from a list of outputs from
# summarise_effect, then population weight them to represent the IFR for the
# population over 70
extract_over_70_ifr_effect <- function(x, which = c("odriscoll", "brazeau")) {
  which <- match.arg(which)
  age_effects <- x$ifr$age[[which]]
  weighting <- get_age_distribution(80) %>%
    mutate(
      mask = case_when(
        age_class %in% c("70-74", "75-79", "80+") ~ 1,
        TRUE ~ 0
      ),
      weights = mask * fraction,
      weights = weights / sum(weights)
    ) %>%
    pull(weights)
  
  sum(age_effects * weighting)
}

# do analysis

# get the baseline next generation matrix
next_generation_matrix <- baseline_matrix(
  R0 = 3
)

# check R0
get_R(next_generation_matrix)

ages <- age_classes(80)
age_populations <- phase_age_populations()
age_distribution <- get_age_distribution(final_age_bin = 80)
timeseries <- scrape_doses_timeseries()
ifr <- get_ifr()

# get effects for all timepoints
all_effects <- lapply(
  timeseries$doses,
  summarise_effect,
  age_populations = age_populations,
  age_distribution = age_distribution,
  next_generation_matrix = next_generation_matrix,
  ifr = ifr
)

timeseries$overall_transmission_effect <- vapply(
  X = all_effects,
  extract_overall_transmission_effect,
  FUN.VALUE = numeric(1)
)

timeseries$overall_ifr_effect <- vapply(
  X = all_effects,
  extract_overall_ifr_effect,
  FUN.VALUE = numeric(1),
  which = "brazeau"
)

timeseries$over_70_ifr_effect <- vapply(
  X = all_effects,
  extract_over_70_ifr_effect,
  FUN.VALUE = numeric(1),
  which = "brazeau"
)

plot(
  I(100 * (1 - overall_transmission_effect)) ~ date,
  data = timeseries,
  type = "l",
  ylab = "Percentage reduction in Reff",
  xlab = "",
  lwd = 2,
  las = 1,
  main = "Vaccination effect on population-wide\nCOVID-19 transmission potential"
)

plot(
  overall_ifr_effect ~ date,
  data = timeseries,
  type = "l",
  ylab = "Population-wide IFR (%)",
  xlab = "",
  lwd = 2,
  col = "forestgreen",
  las = 1,
  ylim = c(0, 1),
  main = "Change in population-wide infection fatality risk"
)

range(timeseries$overall_ifr_effect)

plot(
  over_70_ifr_effect ~ date,
  data = timeseries,
  type = "l",
  ylab = "IFR (%)",
  xlab = "",
  lwd = 2,
  col = "deepskyblue",
  las = 1,
  ylim = c(0, 6),
  main = "Change in infection fatality risk among over-70s"
)
range(timeseries$over_70_ifr_effect)

# plot current vaccination coverage by age
current_effect <- summarise_effect(
  n_doses = max(timeseries$doses),
  age_populations = age_populations,
  age_distribution = age_distribution,
  next_generation_matrix = next_generation_matrix
)

barplot(100 * current_effect$coverage_by_age,
        names.arg = age_distribution$age_class,
        axes = FALSE,
        xlab = "age group",
        main = "Assumed current vaccine coverage (single dose)",
        ylim = c(0, 100))
y_axis_ticks <- seq(0, 100, by = 20) 
axis(2,
     at = y_axis_ticks,
     labels = paste0(y_axis_ticks, "%"),
     las = 1)

# # The same, but assuming a complete phase 1 roll out
# n_doses_phase_1 <- sum(age_populations$phase_1A, age_populations$phase_1B)
# coverage_phase_1 <- doses_by_age(n_doses_phase_1, age_populations) / age_distribution$pop
# barplot(100 * coverage_phase_1,
#         names.arg = age_distribution$age_class,
# axes = FALSE,
# xlab = "age group",
#         main = "Assumed vaccination coverage\nafter complete phase 1 roll out",
#         ylim = c(0, 100))
# y_axis_ticks <- seq(0, 100, by = 20) 
# axis(2,
#      at = y_axis_ticks,
#      labels = paste0(y_axis_ticks, "%"),
#      las = 1)
# abline(h = 100, lty = 3)

# population-wide baseline IFRs based on these numbers
ifr_baseline_odriscoll <- sum(age_distribution$fraction * ifr$odriscoll)
ifr_baseline_brazeau <- sum(age_distribution$fraction * ifr$brazeau)

# apply this to vaccine coverage (single dose) estimates to get new IFRs by age
# and overall


# # check against tables
# lapply(populations_1A, sum)
# lapply(populations_1B, sum)
# 
# # check these look sensible
# 
# # populations by age in each phase
# par(mfrow = c(2, 1))
# barplot(age_populations_1A, ylim = range(age_populations_1B))
# barplot(age_populations_1B)
# 
# # proportional coverage with single doses in each age
# par(mfrow = c(1, 1))
# barplot(age_coverage)
# 
# # check this is true
# identical(
#   sum(age_doses_1A, age_doses_1B),
#   n_doses
# )








