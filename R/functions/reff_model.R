reff_model <- function(data) {
  
  # reduction in R due to surveillance detecting and isolating infectious people
  surveillance_reff_local_reduction <- surveillance_effect(
    dates = data$dates$infection_project,
    cdf = gi_cdf,
    states = data$states
  )
  
  extra_isolation_local_reduction <- extra_isolation_effect(
    dates = data$dates$infection_project,
    cdf = gi_cdf,
    states = data$states
  )
  
  # the reduction from R0 down to R_eff for imported cases due to different
  # quarantine measures each measure applied during a different period. Q_t is
  # R_eff_t / R0 for each time t, modelled as a monotone decreasing step function
  # over three periods with increasingly strict policies
  q_index <- case_when(
    data$dates$infection < quarantine_dates()$date[1] ~ 1,
    data$dates$infection < quarantine_dates()$date[2] ~ 2,
    TRUE ~ 3,
  )
  q_index <- c(q_index, rep(3, data$n_date_nums - data$n_dates))
  
  # q_raw <- uniform(0, 1, dim = 3)
  log_q_raw <- -exponential(1, dim = 3)
  log_q <- cumsum(log_q_raw)
  log_Qt <- log_q[q_index]
  
  # add likelihood for hotel quarantine spillovers - assume Poisson since
  # there's no reason to expect clustering with these rare events, and we'd
  # never be able to determine the number infected in each incident anyway
  expected_hotel_spillovers <- exp(log_q[3] + log(data$imported$total_hotel_cases))
  distribution(data$imported$total_hotel_spillovers) <- poisson(expected_hotel_spillovers)
  
  # The change in R_t for locally-acquired cases due to social distancing
  # behaviour, modelled as a sum of household R_t and non-household R_t
  # Non-household Reff is modelled as a function of the number of non-household
  # contacts per 24h (itself modelled from mobility data, calibrated against
  # contact surveys) and the relative transmission probability per contact,
  # inferred from surveys on micro-distancing behaviour.
  distancing_effect <- distancing_effect_model(data$dates$mobility, gi_cdf)
  
  # pull out R_t component due to distancing for locally-acquired cases, and
  # extend to correct length
  R_eff_loc_1_no_surv <- extend(distancing_effect$R_t, data$n_dates_project)
  
  # multiply by the surveillance effect
  R_eff_loc_1 <- R_eff_loc_1_no_surv * surveillance_reff_local_reduction *
    extra_isolation_local_reduction
  log_R_eff_loc_1 <- log(R_eff_loc_1)
  
  # extract R0 from this model and estimate R_t component due to quarantine for
  # overseas-acquired cases
  log_R0 <- log_R_eff_loc_1[1, 1]
  log_R_eff_imp_1 <- log_R0 + log_Qt
  R_eff_imp_1 <- exp(log_R_eff_imp_1)
  
  # hierarchical (marginal) prior sd on log(Reff12) by state 
  sigma <- normal(0, 0.5, truncation = c(0, Inf))
  sigma_state <- sigma * ones(data$n_states)
  var <- sigma ^ 2

  # hierarchical prior mean on log(Reff12) by state
  mu_prior <- log_R_eff_loc_1 - var

  # temporally correlated errors in R_eff for local cases - representing all the
  # stochastic transmission dynamics in the community, such as outbreaks in
  # communities with higher or lower tranmission rates
  # fixing the kernel variance at 1, and introducing the variance in v
  kernel_L <- rational_quadratic(
    lengthscales = lognormal(3, 1),
    variance = 1,
    alpha = lognormal(3, 1)
  )
  
  # de-centred temporally-correlated log Reff12 GP prior
  epsilon_L <- epsilon_gp(
    date_nums = data$dates$date_nums,
    n_states = data$n_states,
    inducing_date_nums = data$dates$inducing_date_nums,
    sigma_state = sigma_state,
    kernel = kernel_L
  )

  # add the prior mean back on to re-centre the posterior  
  log_R_eff_loc <- mu_prior + epsilon_L

  # expand out the Reff for locals
  log_R_eff_imp <- sweep(
    zeros(data$n_date_nums, data$n_states),
    1,
    log_R_eff_imp_1,
    FUN = "+"
  )
  
  R_eff_loc_12 <- exp(log_R_eff_loc)
  R_eff_imp_12 <- exp(log_R_eff_imp)
  
  # work out which elements to exclude (because there were no infectious people)
  valid <- which(data$valid_mat, arr.ind = TRUE)
  
  # combine everything as vectors, excluding invalid datapoints (remove invalid
  # elements here, otherwise it causes a gradient issue)
  R_eff_loc <- exp(log_R_eff_loc[1:data$n_dates, ])
  R_eff_imp <- exp(log_R_eff_imp[1:data$n_dates, ])
  new_from_loc_vec <- data$local$infectiousness[valid] * R_eff_loc[valid]
  new_from_imp_vec <- data$imported$infectiousness[valid] * R_eff_imp[valid]
  expected_infections_vec <- new_from_loc_vec + new_from_imp_vec
  
  # negative binomial likelihood for number of cases
  sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf), dim = data$n_states)
  size <- 1 / sqrt(sqrt_inv_size[valid[, 2]])
  prob <- 1 / (1 + expected_infections_vec / size)
  
  # Account for right truncation; underreporting of recent infections which have
  # had less time to be detected. Given the number of cases N_t infected on day t
  # (that will ever be detected), the number of cases N^*_t infected on that day
  # that are known about so far is drawn from a binomial sample with probability
  # p, from the time-to-detection distribution. Since N_t is drawn from a negative
  # binomial,  N^*_t is drawn from a compound binomial/negative binomial mixture
  # distribution. Fortunately that turns out to be a negative binomial with
  # modified probability parameter (NB is poisson-gamma, so binomial-NB is
  # binomial-poisson-gamma, but binomial-poisson is poisson with rate lambda * p and gamma times a constant is gamma,
  # so it's a poisson-gamma, which is NB).
  
  # There is an average of one day from specimen collection to confirmation, and
  # the linelist covers the previous day, so the date by which they need to have
  # been detected two days prior to the linelist date.
  detection_prob_vec <- data$detection_prob_mat[valid]
  
  # Modify the probability to account for truncation. When detection_prob_vec = 1,
  # this collapses to prob
  prob_trunc <- 1 / (1 + detection_prob_vec * (1 - prob) / prob)
  
  distribution(data$local$cases[valid]) <- negative_binomial(size, prob_trunc)
  
  m <- model(expected_infections_vec)

  list(
    greta_model = m,
    greta_arrays = module(
      expected_infections_vec,
      size,
      prob_trunc,
      R_eff_loc_1,
      R_eff_imp_1,
      R_eff_loc_12,
      R_eff_imp_12,
      log_R0,
      log_q,
      distancing_effect,
      surveillance_reff_local_reduction,
      extra_isolation_local_reduction,
      log_R_eff_loc,
      log_R_eff_imp,
      epsilon_L
    )
  )
  
  # p <- distancing_effect$p
  # phi <- distancing_effect$phi
  # phi_wt <- distancing_effect$phi_wt
  # 
  # 
  # m <- model(expected_infections_vec,
  #            p)
  # 
  # list(
  #   greta_model = m,
  #   greta_arrays = module(
  #     expected_infections_vec,
  #     size,
  #     prob_trunc,
  #     R_eff_loc_1,
  #     R_eff_imp_1,
  #     R_eff_loc_12,
  #     R_eff_imp_12,
  #     log_R0,
  #     log_q,
  #     distancing_effect,
  #     surveillance_reff_local_reduction,
  #     log_R_eff_loc,
  #     log_R_eff_imp,
  #     epsilon_L
  #   ),
  #   phi,
  #   phi_wt
  # )
  
  
}
