phase_age_populations <- function() {
  
  # allocation leads to over vaccination in some groups - try to fix this!
  # also cap coverage (of single doses) at 2?
  # disaggregation vectors for different vaccination groups
  
  
  # get the age classes and age distributions in national population
  ages <- age_classes(final_age_bin = 80)
  national_distribution <- get_age_distribution(final_age_bin = 80)$fraction
  
  # remove under-15s from all vaccination (vaccines not approved for these ages)
  distribution <- disaggregation_vec(ages$lower >= 15, national_distribution)
  
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
