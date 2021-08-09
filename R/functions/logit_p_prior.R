# find a prior over logit(p) that corresponds to the prior over R0, at the mean
# values of the baseline contact data, by moment matching
logit_p_prior <- function(params, gi_cdf) {
  
  infectious_days <- infectious_period(gi_cdf)
  
  transform <- function(free) {
    list(meanlogit = free[1],
         sdlogit = exp(free[2]))
  }
  
  R0 <- function(p, HC_0, HD_0, OC_0, OD_0) {
    HC_0 * (1 - p ^ HD_0) + OC_0 * infectious_days * (1 - p ^ OD_0)
  }
  
  R0_params <- function(logit_p_params, n = 1e5) {
    logit_p <- rnorm(n, logit_p_params$meanlogit, logit_p_params$sdlogit)
    R0_draws <- R0(
      p = plogis(logit_p),
      HC_0 = rnorm(n, params$mean_contacts[1], params$se_contacts[1]),
      OC_0 = rnorm(n, params$mean_contacts[2], params$se_contacts[2]),
      HD_0 = rnorm(n, params$mean_duration[1], params$se_duration[1]),
      OD_0 = rnorm(n, params$mean_duration[2], params$se_duration[2])
    )
    log_R0_draws <- log(R0_draws)
    list(meanlog = mean(log_R0_draws),
         sdlog = sd(log_R0_draws))
  }
  
  obj <- function(free) {
    logit_p_params <- transform(free)
    R0_params <- R0_params(logit_p_params)
    R0_expected <- R0_prior()
    (R0_expected$meanlog - R0_params$meanlog) ^ 2 + sqrt((R0_expected$sdlog - R0_params$sdlog) ^ 2)
  }
  
  set.seed(2020-05-18)
  o <- optim(c(5, -2), fn = obj, method = "BFGS")
  
  transform(o$par)
  
}
