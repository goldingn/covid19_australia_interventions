# assuming restrictions pulse between two states: baseline and lockdown, with R
# for baseline greater than 1 (cases grow) and R for lockdown less than 1 (cases
# shrink), and the aim is to keep the long-term average of R at 1 (maintain case
# counts below a critical threshold), compute the fraction of the time that
# would need to be in the lockdown state.
fraction_lockdown <- function(
  R_baseline,
  R_lockdown
) {
  
  # compute the fraction of the time we would need to be in lockdown to maintain
  # an average R of 1
  fraction <- -log(R_baseline) / (log(R_lockdown) - log(R_baseline))
  # if the baseline TP is not above 1, the fraction is 0 as no lockdowns are needed
  fraction[R_baseline <= 1] <- 0
  # if the lockdown TP is not below 1, there is no fraction that can keep R at average 1
  fraction[R_lockdown >= 1] <- NA
  
  fraction
}
