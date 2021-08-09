macrodistancing_params <- function(baseline_contact_params) {
  
  # baseline number of non-household contacts, from Prem and Rolls
  OC_0 <- normal(baseline_contact_params$mean_contacts[2],
                 baseline_contact_params$se_contacts[2],
                 truncation = c(0, Inf))
  
  # coefficients for change in average contacts as a function of mobility
  # indices (all set to be negative in first lockdown)
  mobility_coefs <- normal(0, 1, dim = 5, truncation = c(0, Inf))
  
  # coefficients for the fraction of weekly contacts that are on weekends, as a
  # function of the log diffference in contacts
  weekday_coefs <- normal(0, 1, dim = c(2, 6))

  list(
    OC_0 = OC_0,
    mobility_coefs = mobility_coefs,
    weekday_coefs = weekday_coefs
  )
  
}
