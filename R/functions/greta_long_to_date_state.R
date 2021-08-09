# take a vector greta array correpsonding to dates and states and convert to
# date-by-state wide format
greta_long_to_date_state <- function(long, dates, states) {
  wide_dim <- c(n_distinct(dates), n_distinct(states))
  greta_array(long, dim = wide_dim)
}
