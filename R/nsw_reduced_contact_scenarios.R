# load functions.
# can hash out the greta stuff on lines 1:12 if not installed.
# most of that file will be irrelevant but should be OK to just find
# and source relvant functions as necessary, but these ar often very nested
source("R/functions.R") 

# get a transmission matrix and next gen matrix from existing code:

age_breaks <- c(seq(0, 80, by = 5), Inf)

model <- polymod_model()

transmission_matrices <- get_setting_transmission_matrices(
  age_breaks = age_breaks
)


data <- abs_pop_age_lga_2020 %>%
  group_by(age_group) %>%
  summarise(
    population = sum(population)
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  mutate(
    country = "Australia"
  ) %>%
  nest(
    population = -country
  ) %>%
  rowwise() %>%
  mutate(
    per_capita_household_size = get_per_capita_household_size(),
    setting_matrices = list(
      predict_setting_contacts(
        contact_model = model,
        population = population,
        per_capita_household_size = per_capita_household_size,
        age_breaks = age_breaks
      )
    ),
    contact_matrices = list(
      setting_matrices[c("home", "school", "work", "other")]
    )
  )


# create contact matrices with reduced contact for over 65's
# in non-household settings, by either 25% or 50% 

contact_matrices_reduced_05  <- data$contact_matrices[[1]]
contact_matrices_reduced_025 <- data$contact_matrices[[1]]

contact_matrices_reduced_05[2:4] <- lapply(
  X = contact_matrices[2:4],
  FUN = function(x){
    y <- x
    
    y[14:17,] <- x[14:17,]*0.5
    y[1:13,14:17] <- x[1:13,14:17]*0.5
    
    return(y)
  }
)

contact_matrices_reduced_025[2:4] <- lapply(
  X = contact_matrices[2:4],
  FUN = function(x){
    y <- x
    
    y[14:17,] <- x[14:17,]*0.75
    y[1:13,14:17] <- x[1:13,14:17]*0.75
    
    return(y)
  }
)


# generate unscaled NGMs with this transmission matrix and contact matrices
ngm_u <- data %>%
  mutate(
    ngm_unscaled = list(
      all = get_unscaled_ngm(
        contact_matrices = contact_matrices,
        transmission_matrices = transmission_matrices
      )
    )
  ) %>%
  pull(ngm_unscaled) %>%
  `[[`(1)

ngm_u_05 <- data %>%
  mutate(
    contact_matrices = list(contact_matrices_reduced_05),
    ngm_unscaled = list(
      all = get_unscaled_ngm(
        contact_matrices = contact_matrices,
        transmission_matrices = transmission_matrices
      )
    )
  ) %>%
  pull(ngm_unscaled) %>%
  `[[`(1)

ngm_u_025 <- data %>%
  mutate(
    contact_matrices = list(contact_matrices_reduced_025),
    ngm_unscaled = list(
      all = get_unscaled_ngm(
        contact_matrices = contact_matrices,
        transmission_matrices = transmission_matrices
      )
    )
  ) %>%
  pull(ngm_unscaled) %>%
  `[[`(1)


# scale NGMs
R0 <- 3

m <- find_m(
  R_target = R0,
  transition_matrix = ngm_u
)

m_05 <- find_m(
  R_target = R0,
  transition_matrix = ngm_u_05
)

m_025 <- find_m(
  R_target = R0,
  transition_matrix = ngm_u_025
)

ngm     <- ngm_u     * m
ngm_05  <- ngm_u_05  * m_05
ngm_025 <- ngm_u_025 * m_025

# calculate effect sizes in change of relative growth rate against unmodified NGM
effect_05 <- 1 - get_R(ngm_05)/get_R(ngm)
effect_025 <- 1 - get_R(ngm_025)/get_R(ngm)

multiplier_05 <- 1 - effect_05
multiplier_025 <- 1 - effect_025


# effect size and TP multiplier for 50% reduction in non-hh contact for over 65's
effect_05
multiplier_05

# effect size and TP multiplier for 25% reduction in non-hh contact for over 65's
effect_025
multiplier_025