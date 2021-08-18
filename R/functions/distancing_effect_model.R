# greta sub-model for the component R_eff due to macro- and micro-distancing
distancing_effect_model <- function(
  dates,
  gi_cdf,
  voc_mixture = c("all", "alpha", "delta", "wt")
) {
  
  voc_mixture <- match.arg(voc_mixture)
  
  # informative priors on variables for contacts at t = 0 (Hx = household, Ox =
  # non-household, Tx = total, xC = contacts. xD = duration)
  baseline_contact_params <- baseline_contact_parameters(gi_cdf)
  
  
  prop_var <- prop_variant(dates = dates)
  prop_alpha <- prop_var$prop_alpha
  prop_delta <- prop_var$prop_delta
  prop_wt    <- prop_var$prop_wt
  
  if(voc_mixture == "alpha") {
    prop_alpha <- prop_alpha * 0 + 1
    prop_delta <- prop_wt <- prop_delta * 0
  }
  
  if(voc_mixture == "delta") {
    prop_delta <- prop_delta * 0 + 1
    prop_alpha <- prop_wt <- prop_alpha * 0
  }
  
  if(voc_mixture == "wt") {
    prop_wt <- prop_wt * 0 + 1
    prop_alpha <- prop_alpha * 0 
    prop_delta <- prop_delta * 0
  }
  
  
  # prior on the probability of *not* transmitting, per hour of contact
  # (define to match moments of R0 prior)
  logit_p_params <- logit_p_prior(baseline_contact_params, gi_cdf)
  logit_p <- normal(logit_p_params$meanlogit, logit_p_params$sdlogit)
  p <- ilogit(logit_p)
  
  phi_alpha       <- normal(1.454, 0.055, truncation = c(0, Inf))
  phi_delta_alpha <- normal(1.421, 0.033, truncation = c(0, Inf))
  
  phi_delta <- phi_alpha * phi_delta_alpha
  
  phi_star <- prop_wt * 1 + prop_alpha * phi_alpha + prop_delta * phi_delta

  p_star <- p ^ phi_star
  
  
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
  household_infections <- HC_0 * (1 - p_star ^ HD_t)
  non_household_infections <- OC_t_state * gamma_t_state *
    infectious_days * (1 - p_star ^ OD_0)
  R_t <- household_infections + non_household_infections
  
  # return greta arrays
  list(R_t = R_t, 
       gamma_t_state = gamma_t_state,
       OC_t_state = OC_t_state,
       p = p,
       p_star = p_star,
       beta = beta,
       HC_0 = HC_0,
       HD_0 = HD_0,
       OC_0 = OC_0,
       OD_0 = OD_0,
       dates = dates,
       phi_alpha = phi_alpha,
       phi_delta_alpha = phi_delta_alpha,
       phi_delta = phi_delta,
       phi_star = phi_star)
  
}
