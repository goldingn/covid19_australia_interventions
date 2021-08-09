disaggregate <- function(population, mask) {
  # disaggregate a population across age groups, based on the age groups in that
  # population and the national age distribution
  
  masked_age_distribution <- age_distribution * mask
  disaggregation <- masked_age_distribution / sum(masked_age_distribution)
  population * disaggregation
}
