# get a dataframe of vaccine coverage estimates
vaccination_coverage <- function() {
  
  # scrape a timeseries of the cumulative number of doses given out
  scrape_doses_timeseries() %>%
    # apply an allocation model to compute the cumulative number of people
    # receiving their second dose (fully vaccinated)
    mutate(
      fully_vaccinated = model_vaccination_coverage(new_doses)
    ) %>%
    # get the fraction of vaccinated people who are fully vaccinated
    mutate(
      partially_vaccinated = doses - (fully_vaccinated * 2),
      vaccinated = fully_vaccinated + partially_vaccinated,
      proportion_2_dose = fully_vaccinated / vaccinated
    )
  
}
