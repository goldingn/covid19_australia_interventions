# given a positive integer 'n_days' of the number of days for which to compute
# values and a discrete vector 'disaggregation_probs' of probabilities that data
# for one day should actually be assigned that number of days into the future,
# return a symmetric matrix that can be used in a matrix multiply to
# disaggregate daily data into expected counts on future days, according to that
# probability distribution. Doing this disaggregation using a circulant matrix
# of probabilities with masked lower values, is more efficient in greta than
# looping since it can easily be parallelised. Note this is the same operation
# as cases_known_outcome_matrix() in goldingn/australia_covid_ascertainment
disaggregation_matrix <- function (cdf, n_days, max_days = 20, ...) {
  
  diff <- time_difference_matrix(n_days, max_days)
  si_disaggregation <- gi_probability(cdf, days, ...)
  
}
