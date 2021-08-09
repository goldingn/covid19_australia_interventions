# the pre-intervention serial interval of Taslim Ali et al.
taslim_ali_cdf <- function() {
  
  params <- lognormal_prior(mean = 7.8, sd = 5.2)
  
  gi_cdf <- function(days) {
    plnorm(days, meanlog = params$mean, sdlog = params$sd)
  }
  
  gi_cdf
}
