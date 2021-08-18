baseline_contact_parameters <- function(gi_cdf) {
  
  # mean duration of infection in days
  infectious_days <- infectious_period(gi_cdf)
  
  # get the average number and duration contacts by household/non-household
  baseline_contact_params <- rolls_contact_data() %>%
    # contact duration in hours per day, multiply by infectious days *for
    # household contacts only* to get number of contact hours of entire
    # infectious period. For non-household contacts, we can assume they are
    # different individuals so transmission can exceed contacts, so multiply
    # infectious days by the daily R
    mutate(
      contact_duration = contact_duration * ifelse(hh_member == "household",
                                                   infectious_days,
                                                   1)
    ) %>%
    # summarise number and duration of household/non-household contacts per
    # respondent
    group_by(participant_id, hh_member, weight) %>%
    summarise(contacts = round(sum(contacts)),
              contact_duration = mean(contact_duration, na.rm = TRUE)) %>%
    ungroup() %>%
    # summarise over respondents
    group_by(hh_member) %>%
    summarise(
      mean_contacts = weighted_mean(
        contacts,
        w = weight,
        na.rm = TRUE
      ),
      se_contacts = weighted_se(
        contacts,
        mean_contacts,
        w = weight,
        na.rm = TRUE
      ),
      mean_duration = weighted_mean(
        contact_duration,
        w = weight,
        na.rm = TRUE
      ),
      se_duration = weighted_se(
        contact_duration,
        mean_duration,
        w = weight,
        na.rm = TRUE
      )
    )  
  
  # replace the prior over mean non-household contacts with a more comparable
  # estimate from Prem/Polymod
  TC_0_prior <- baseline_total_contacts()
  OC_0_prior <- tibble(
    mean = TC_0_prior$mean - baseline_contact_params$mean_contacts[1],
    se = sqrt(TC_0_prior$se ^ 2 + baseline_contact_params$se_contacts[1] ^ 2)
  )
  baseline_contact_params$mean_contacts[2] <- OC_0_prior$mean
  # increase the uncertainty on the baseline contact rate, since the survey
  # design of Polymod is not the same as those we have fielded, and the
  # demographic conversion in Prem may also make it less comparable
  baseline_contact_params$se_contacts[2] <- OC_0_prior$se * 5
  
  baseline_contact_params
  
}
