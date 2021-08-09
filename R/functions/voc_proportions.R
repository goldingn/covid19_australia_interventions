# given a set of linelists, return a list of date-by-state matrices, each giving
# the proportion of infectiousness that comes from that linelist (infectiousness
# is that variant). 
voc_proportions <- function(...) {
  
  infectious_cases_list <- list(...)
  
  # get the denominator case count
  total_infectious <- Reduce('+', infectious_cases_list)
  total_infectiousness <- gi_convolution(
    total_infectious,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf
  )
  # set 0s to 1s to avoid divid-by-0 errors (replaces 0/0 = NA with 0/1 = 0)
  total_infectiousness[total_infectiousness == 0] <- 1
  
  # get list of infectiousnesses for each
  infectiousness_list <- lapply(
    X = infectious_cases_list,
    FUN = gi_convolution,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf
  )
  
  # compute proportions and return
  proportion_list <- lapply(
    X = infectiousness_list,
    FUN = "/",
    total_infectiousness
  )

  proportion_list
  
}
