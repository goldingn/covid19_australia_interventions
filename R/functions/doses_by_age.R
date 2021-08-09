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
