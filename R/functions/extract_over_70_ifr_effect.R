extract_over_70_ifr_effect <- function(x, which = c("odriscoll", "brazeau")) {
  # extract age-specific IFRs after vaccination  from a list of outputs from
  # summarise_effect, then population weight them to represent the IFR for the
  # population over 70
  
  which <- match.arg(which)
  age_effects <- x$ifr$age[[which]]
  weighting <- get_age_distribution(80) %>%
    mutate(
      mask = case_when(
        age_class %in% c("70-74", "75-79", "80+") ~ 1,
        TRUE ~ 0
      ),
      weights = mask * fraction,
      weights = weights / sum(weights)
    ) %>%
    pull(weights)
  
  sum(age_effects * weighting)
}
