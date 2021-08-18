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
