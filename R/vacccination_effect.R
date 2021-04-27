library(dplyr)
library(purrr)
library(readxl)
library(tidyr)

# vaccination_segments <- readxl::read_xlsx(
#   path = "data/vaccinatinon/vaccination_segment_populations.xlsx",
#   col_names = c(
#     "phase",
#     "segment",
#     "pop_under_50",
#     "pop_over_50",
#     "pop_total"
#   ),
#   skip = 2
# ) %>%
#   tidyr::fill(
#     phase,
#     .direction = "down"
#   ) %>%
#   mutate(
#     pop_under_50 = readr::parse_number(pop_under_50),
#     pop_over_50 = readr::parse_number(pop_over_50),
#     pop_total = readr::parse_number(pop_total)
#   )

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

disaggregate <- function(doses, mask) {
  masked_age_distribution <- age_distribution * mask
  disaggregation <- masked_age_distribution / sum(masked_age_distribution)
  doses * disaggregation
}

# total number of vaccine doses given out, and total australian population
n_doses <- 1583000
aust_population_total <- 25693000

# adjust as tp whether using up to 75 per Prem or expanded up to 100
age_classes <- c(
  "0-5",
  "5-10",
  "10-15",
  "15-20",
  "20-25",
  "25-30",
  "30-35",
  "35-40",
  "40-45",
  "45-50",
  "50-55",
  "55-60",
  "60-65",
  "65-70",
  "70-75",
  "75+"
)

# age classes we care about
ages_lower <- seq(0, 75, by = 5)
n_ages <- length(ages_lower)

# age structure of the Australian population
# This is a "standard" distribution data frame but old population size data from 2001 hence is adjusted later
aust_population_standard <- readxl::read_xls(
  path = "data/vaccinatinon/abs_standard_age_31010DO003_200106.xls",
  sheet = "Table_1",
  skip = 6,
  col_names = c(
    "age",
    "pop"
  )
) %>%
  filter(age != "Total", !is.na(age), age != "© Commonwealth of Australia 2013") %>%
  mutate(
    age = case_when(
      age == "100 and over" ~ "100",
      TRUE ~ age
    ) %>%
      as.integer,
  ) %>%
  mutate(
    index = c(
      rep(1:16, each = 5),
      rep(16, times = 21) # this is hard coding in length of data frame
    )
  )

pop_classes <- tibble(
  age_class = age_classes,
  standard_population = tapply(
    X = aust_population_standard$pop,
    INDEX = aust_population_standard$index,
    FUN = sum
  )
) %>%
  mutate(
    pop_proportion = standard_population/sum(standard_population),
    population = round(pop_proportion*aust_population_total)
  )



# Nick's code
#population_by_age <- sum(pop_classes$population)
#age_distribution <- population_by_age / sum(population_by_age)

# Gerry ?correction
population_by_age <- pop_classes$population
age_distribution <- pop_classes$pop_proportion

# disaggregation vectors for different vaccination groups

# masks for different age groups (can be combined)
all_ages <- ages_lower >= 0
over_50 <- ages_lower >= 50 
over_65 <- ages_lower >= 65 
under_50 <- ages_lower < 50 
working_age <- ages_lower >= 16 & ages_lower < 65
over_70 <- ages_lower >= 70
over_75 <- ages_lower >= 75

# population sizes in vaccination roll-out groups
populations_1A <- list(
  aged_care_residents = disaggregate(183000, over_65),
  disability_residents_u50 = disaggregate(5000, under_50),
  disability_residents_o50 = disaggregate(21000, over_50),
  border_workers_u50 = disaggregate(16000, working_age & under_50),
  border_workers_o50 = disaggregate(11000, working_age & over_50),
  care_staff_u50 = disaggregate(143000, working_age & under_50),
  care_staff_o50 = disaggregate(107000, working_age & over_50),
  health_staff_priority_u50 = disaggregate(222000, working_age & under_50),
  health_staff_priority_o50 = disaggregate(90000, working_age & over_50)
)

populations_1B <- list(
  elderly_o80 = disaggregate(915000, over_75), # put over-80s in the final bin (75+)
  elderly_70_79 = disaggregate(1857000, over_70),
  health_staff_other_u50 = disaggregate(267000, working_age & under_50),
  health_staff_other_o50 = disaggregate(126000, working_age & over_50),
  atsi_o50 = disaggregate(91000, over_50),
  medical_condition_u50 = disaggregate(896000, under_50),
  medical_condition_o50 = disaggregate(167000, over_50),
  priority_workers_u50 = disaggregate(201000, under_50),
  priority_workers_o50 = disaggregate(67000, over_50)
)

# combine these
age_populations_1A <- Reduce(`+`, populations_1A)
age_populations_1B <- Reduce(`+`, populations_1B)

# assuming a single dose per person, full coverage in 1A, and partial coverage
# in 1B, compute the number of doses given out in each age group coverage in each age group
age_doses_1A <- age_populations_1A
n_doses_1B <- n_doses - sum(age_doses_1A)
age_doses_1B <- n_doses_1B * age_populations_1B / sum(age_populations_1B)

# combine into total doses so far
age_doses <- age_doses_1A + age_doses_1B

# compute vaccination coverage
age_coverage <- age_doses / population_by_age

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

# Prem 2017 contact matrix
contact_matrix <- readxl::read_xlsx(
  path = "data/vaccinatinon/MUestimates_all_locations_1.xlsx",
  sheet = "Australia"
) %>%
  as.matrix

# quick and dirty expansion of this matrix to groups up to 100 - this is almost certainly invalid
#contact_matrix <- contact_matrix[c(1:16, 16, 16, 16, 16),c(1:16, 16, 16, 16, 16)]


dimnames(contact_matrix) <- list(
  age_classes,
  age_classes
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
  skip = 1
) %>%
  mutate(
    age_class = age_classes
  ) %>%
  dplyr::select(
    age_group,
    age_class,
    everything()
  )


# calculate relative infectiousness
# I think that this should be scaled so highest values = 1, which is q_scaled
relative_infectiousness <- age_susceptability$clinical_fraction*1 + 0.5*(1 - age_susceptability$clinical_fraction)
q <- relative_infectiousness*age_susceptability$relative_susceptability
q_scaled <- q/max(q)


# apply the q scaling before computing m
contact_matrix_scaled <- sweep(contact_matrix, 2, q_scaled, FUN = "*")

# calculate m - number of onward infections per relative contact 
m <- find_m(
  R_target = 2, # I can't find notes on this but I think I remember this being discussed as the basic R0 for SARS-CoV-2
  transition_matrix = contact_matrix_scaled
)

next_generation_matrix <- contact_matrix_scaled * m

# calculate basic reproduction numer 
R0 <- eigen(next_generation_matrix)$values[1] %>% Re

# reduction in onward transmission per doses for each vaccine
efficacy_pf_2_dose <- 0.9
efficacy_az_2_dose <- 0.8

efficacy_pf_1_dose <- efficacy_pf_2_dose/2
efficacy_az_1_dose <- efficacy_az_2_dose/2

# proportion of each vaccine
proportion_pf <- 0.5
proportion_az <- 1-proportion_pf

# proportion fully dosed
proportion_2_dose <- 0
proportion_1_dose <- 1 - proportion_2_dose

efficacy_mean <- proportion_2_dose * proportion_pf * efficacy_pf_2_dose +
  proportion_1_dose * proportion_pf * efficacy_pf_1_dose +
  proportion_2_dose * proportion_az * efficacy_az_2_dose +
  proportion_1_dose * proportion_az * efficacy_az_1_dose


age_efficacy <- age_coverage*(1-efficacy_mean)

vc_next_gen_matrix <- sweep(
  next_generation_matrix,
  2,
  age_efficacy,
  FUN = "*"
)
