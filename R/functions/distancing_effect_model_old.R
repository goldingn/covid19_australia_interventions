#distancing_effect_model <- function(dates, gi_cdf) {
distancing_effect_model_old <- function(dates, gi_cdf) {
  
  # informative priors on variables for contacts at t = 0 (Hx = household, Ox =
  # non-household, Tx = total, xC = contacts. xD = duration)
  baseline_contact_params <- baseline_contact_parameters(gi_cdf)
  
  # prior on the probability of *not* transmitting, per hour of contact
  # (define to match moments of R0 prior)
  logit_p_params <- logit_p_prior(baseline_contact_params, gi_cdf)
  logit_p <- normal(logit_p_params$meanlogit, logit_p_params$sdlogit)
  p <- ilogit(logit_p)
  
  infectious_days <- infectious_period(gi_cdf)
  
  HC_0 <- normal(baseline_contact_params$mean_contacts[1],
                 baseline_contact_params$se_contacts[1],
                 truncation = c(0, Inf))
  HD_0 <- normal(baseline_contact_params$mean_duration[1],
                 baseline_contact_params$se_duration[1],
                 truncation = c(0, Inf))
  OD_0 <- normal(baseline_contact_params$mean_duration[2],
                 baseline_contact_params$se_duration[2],
                 truncation = c(0, Inf))
  
  # get HD_t in each state
  h_t <- h_t_state(dates)
  HD_t <- HD_0 * h_t
  
  # trends in non-household contacts in each state over time
  OC_t_state <- trends_date_state(
    "outputs/macrodistancing_trends.RDS",
    dates
  )
  OC_0 <- OC_t_state[1, 1]
  
  # model gamma_t: reduction in duration and transmission probability of
  # non-household contacts over time, per state
  
  # load probability of microdistancing and divide by the maximum value to get
  # an index of per-contact transmission probability
  microdistancing_prob <- trends_date_state(
    "outputs/microdistancing_trends.RDS",
    dates
  )
  d_t_state <- microdistancing_prob / max(microdistancing_prob)
  
  beta <- uniform(0, 1)
  gamma_t_state <- 1 - beta * d_t_state
  
  # compute component of R_eff for local cases
  household_infections <- HC_0 * (1 - p ^ HD_t)
  non_household_infections <- OC_t_state * gamma_t_state *
    infectious_days * (1 - p ^ OD_0)
  R_t <- household_infections + non_household_infections
  
  # return greta arrays
  list(R_t = R_t, 
       gamma_t_state = gamma_t_state,
       OC_t_state = OC_t_state,
       p = p,
       beta = beta,
       HC_0 = HC_0,
       HD_0 = HD_0,
       OC_0 = OC_0,
       OD_0 = OD_0,
       dates = dates)
  
}
