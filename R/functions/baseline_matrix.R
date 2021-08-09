baseline_matrix <- function(
  R0 = 2.5,
  final_age_bin = 80,
  type = c("next generation", "exposure outcome", "contact")
  ) {

  type <- match.arg(type)
  
  # construct a next generation/exposure outcome/contact matrix for Australia
  # from Prem matrix
  
  # Prem 2017 contact matrix
  contact_matrix <- readxl::read_xlsx(
    path = "data/vaccinatinon/MUestimates_all_locations_1.xlsx",
    sheet = "Australia",
    col_types = rep("numeric", 16)
  ) %>%
    as.matrix
  
  # expand out to add an 80+ category the same as the 75-80 category
  matrix <- matrix(NA, 17, 17)
  matrix[17, 17] <- contact_matrix[16, 16]
  matrix[17, 1:16] <- contact_matrix[16, ]
  matrix[1:16, 17] <- contact_matrix[, 16]
  matrix[1:16, 1:16] <- contact_matrix
  
  # set names
  bin_names <- age_classes(80)$classes 
  dimnames(matrix) <- list(
    bin_names,
    bin_names
  )
  
  if (type != "contact") {
    
    age_disaggregation <- tibble::tribble(
      ~age_group_10y, ~age_group_5y,
      "0_9", "0-4",
      "0_9", "5-9",
      "10_19", "10-14",
      "10_19", "15-19",
      "20_29", "20-24",
      "20_29", "25-29",
      "30_39", "30-34", 
      "30_39", "35-39",
      "40_49", "40-44",
      "40_49", "45-49",
      "50_59", "50-54",
      "50_59", "55-59",
      "60_69", "60-64",
      "60_69", "65-69",
      "70+", "70-74",
      "70+", "75-79",
      "70+", "80+"
    )
    
    age_data_davies <- read_csv(
      "data/vaccinatinon/susceptibility_clinical_fraction_age_Davies.csv",
      col_types = cols(
        age_group = col_character(),
        rel_susceptibility_mean = col_double(),
        rel_susceptibility_median = col_double(),
        clinical_fraction_mean = col_double(),
        clinical_fraction_median = col_double()
      )
    )
    
    # calculate relative infectiousness (using relative differences in clinical
    # fraction by age and assumed relative infectiousness of asymptomatics) and
    # susceptibility by age
    asymp_rel_infectious <- 0.5
    age_contribution <- age_disaggregation %>%
      left_join(
        age_data_davies,
        by = c("age_group_10y" = "age_group")
      ) %>%
      mutate(
        rel_infectiousness = clinical_fraction_mean +
          asymp_rel_infectious * (1 - clinical_fraction_mean),
        rel_infectiousness = rel_infectiousness /
          max(rel_infectiousness),
        rel_susceptibility = rel_susceptibility_mean /
          max(rel_susceptibility_mean),
      ) %>%
      select(
        age_group_5y,
        rel_infectiousness,
        rel_susceptibility
      )
    
    # adjust infectiousness on columns and susceptibility on rows
    matrix <- sweep(matrix, 2, age_contribution$rel_infectiousness, FUN = "*")

    # for a next generation matrix (vector multiplier is distribution of
    # infectious state), apply susceptibility to rows. For an exposure outcome
    # matrix (vector multiplier is number of exposures age group is subjected
    # to) apply susceptibility to columns
    index <- switch(
      type,
      "next generation" = 1,
      "exposure outcome" = 2,
    )    
    matrix <- sweep(matrix, index, age_contribution$rel_susceptibility, FUN = "*")
    
  }
  
  # if a target R0 is provided, scale the matrix to match
  if (!is.null(R0)) {
    
    # calculate m - number of onward infections per relative contact 
    m <- find_m(
      R_target = R0,
      transition_matrix = matrix
    )
    
    matrix <- matrix * m
    
  }
  
  matrix
  
}
