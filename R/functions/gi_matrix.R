# build a convolution matrix for the discrete generation interval for a single
# state, applying the effect of improving surveillance and normalising to
# integrate to 1
gi_matrix <- function(gi_cdf, dates, state,
                      gi_bounds = c(0, 20),
                      ttd_cdfs = NULL) {
  
  n_dates <- length(dates)
  
  # baseline GI matrix, without effects of improved surveillance
  day_diff <- time_difference_matrix(n_dates)
  gi_mat_naive <- gi_probability(gi_cdf, day_diff)
  
  # compute fraction surviving without detection in circulant matrix format,
  # multiply by GI matrix and rescale to get new GI distribution on each day
  ttd_mat <- day_diff
  ttd_mat[] <- ttd_survival(
    days = c(day_diff),
    dates = rep(dates, each = n_dates),
    target_state = state,
    cdfs = ttd_cdfs
  )
  scaling <- surveillance_effect(
    dates = dates,
    cdf = gi_cdf,
    state = state,
    gi_bounds = gi_bounds,
    ttd_cdfs = ttd_cdfs
  )
  rel_gi_mat <- gi_mat_naive * ttd_mat
  gi_mat <- sweep(rel_gi_mat, 2, scaling, FUN = "/")
  
  gi_mat
  
}
