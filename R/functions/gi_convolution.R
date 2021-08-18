# given a date-by-state matrix of case counts by date of infection,
# corresponding vectors of dates and states, and a function got the CDF of the
# continous version of a generation interval distribution, adjust the GI
# distribution by surveillance effectiveness (fraction of cases detected and
# isolated by each day post infection) and convolve the cases to get the
# combined infectiousness in each date and state.
gi_convolution <- function(cases, dates, states, gi_cdf, gi_bounds = c(0, 20)) {
  
  n_dates <- length(dates)
  n_states <- length(states)
  if (!identical(dim(cases), c(n_dates, n_states))) {
    stop ("cases does not match dates and states", call. = FALSE)
  }
  
  convolved <- cases * 0
  for (i in seq_len(n_states)) {
    # Circulant matrices of generation interval discrete probabilities
    # use Nishiura's serial interval as a generation interval
    # gi_cdf <- nishiura_cdf()
    gi_mat <- gi_matrix(
        gi_cdf = gi_cdf,
        dates = dates,
        state = states[i],
        gi_bounds = gi_bounds
      )
    
    convolved[, i] <- gi_mat %*% cases[, i]
    
  }
  
  convolved
  
  
}
