# build a vector of discrete generation interval probability masses for a given
# date, applying the effect of improving surveillance and normalising to
# integrate to 1
gi_vector <- function(gi_cdf, date, state,
                      gi_bounds = c(0, 20),
                      ttd_cdfs = NULL) {
  
  # baseline GI vector, without effects of improved surveillance
  days <- seq(gi_bounds[1], gi_bounds[2])
  gi_vec_naive <- gi_probability(gi_cdf, days = days, bounds = gi_bounds)
  
  # compute fraction surviving without detection in circulant matrix format,
  # multiply by GI matrix and rescale to get new GI distribution on each day
  ttd_vec <- ttd_survival(
    days = days,
    dates = rep(date, each = length(days)),
    target_state = state,
    cdfs = ttd_cdfs
  )
  scaling <- surveillance_effect(
    dates = date,
    cdf = gi_cdf,
    state = state,
    gi_bounds = gi_bounds,
    ttd_cdfs = ttd_cdfs
  )
  rel_gi_vec <- gi_vec_naive * ttd_vec
  gi_vec <- rel_gi_vec / scaling
  
  gi_vec
  
}
