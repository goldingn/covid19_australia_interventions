# a discretised lognormal distribution (i.e. samplesd by applying the floor
# operation to samples from a lognormal). Due to the numerical instability of
# integrating across the distribution, a vector of breaks must be defined and
# the observations will be treated as censored within those breaks
discrete_lognormal <- function(meanlog, sdlog, breaks, dim = NULL) {
  greta:::distrib("discrete_lognormal", meanlog, sdlog, breaks, dim)
}
