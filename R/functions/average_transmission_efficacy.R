average_transmission_efficacy <- function() {
  average_efficacy(
    efficacy_pf_2_dose = 0.9685,
    efficacy_az_2_dose = 0.93,
    #efficacy_pf_1_dose = efficacy_pf_2_dose/2,
    #efficacy_az_1_dose = efficacy_az_2_dose/2,
    efficacy_pf_1_dose = 0.8317,
    efficacy_az_1_dose = 0.892,
    proportion_pf = 0.5,
    proportion_2_dose = 0
  )
}
