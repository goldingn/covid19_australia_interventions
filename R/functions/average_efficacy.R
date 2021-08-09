# proportion_pf and proportion_2_dose are only used if the other proportion
# arguments are not specified
average_efficacy <- function(
  efficacy_pf_2_dose = 0.9685,
  efficacy_az_2_dose = 0.93,
  efficacy_pf_1_dose = 0.8317,
  efficacy_az_1_dose = 0.892,
  proportion_pf = 0.5,
  proportion_2_dose = 0.2,
  proportion_pf_2_dose = proportion_pf * proportion_2_dose,
  proportion_az_2_dose = (1 - proportion_pf) * proportion_2_dose,
  proportion_pf_1_dose = proportion_pf * (1 - proportion_2_dose),
  proportion_az_1_dose = (1 - proportion_pf) * (1 - proportion_2_dose)
) {
  
  # based on Pritchard MedRXiv / Harris via ATAGI advice paper
  # single dose calculations
  # AZ: 1 - (1 - 0.64) * (1 - 0.47) = 0.809
  # PF: 1 - (1 - 0.67) * (1 - 0.49) = 0.832
  
  # AZ: 1 - (1 - 0.8) * (1 - 0.65) = 0.93
  # Pf: 1 - (1 - 0.91) * (1 - 0.65) = 0.9685
  
  # compute the average efficacy of doses, given the proportion of vaccines from
  # each provider, and the proportion people who are vaccined with 2 doses - these
  # are estiamtes for b.1.1.7, from James Wood's email
  
  # proportion of each vaccine
  proportion_az <- 1 - proportion_pf
  
  # proportion fully dosed
  proportion_1_dose <- 1 - proportion_2_dose # may need edit when 
  
  efficacy_mean <- proportion_pf_2_dose * efficacy_pf_2_dose +
    proportion_pf_1_dose * efficacy_pf_1_dose +
    proportion_az_2_dose * efficacy_az_2_dose +
    proportion_az_1_dose * efficacy_az_1_dose 
  
  efficacy_mean 
  
}
