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
