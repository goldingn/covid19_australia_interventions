summarise_effect <- function(
  n_doses,
  age_populations,
  age_distribution,
  next_generation_matrix,
  ifr
) {
  
  # Given a number of doses, compute vaccine coverage in each age group, the
  # effect on reducing transmission among the whole population, and the efect on
  # reducing transmission in the most-vaccinated age group
  
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
