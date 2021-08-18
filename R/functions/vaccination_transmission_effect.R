vaccination_transmission_effect <- function(
  age_coverage,
  efficacy_mean,
  next_generation_matrix
) {
  # given vaccination coverage in each age group, the average vaccine efficacy (by
  # age or overall), and the baseline next generation matrix, compute the
  # reduction in transmission for each age and overall
  
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
