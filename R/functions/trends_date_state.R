# get the index of microdistancing in each state as a date-by-state matrix
trends_date_state <- function (file, dates = NULL, states = NULL) {
  
  trends <- readRDS(file)
  
  date_seq <- seq(min(trends$date), max(trends$date), by = 1)
  
  if (is.null(states)) {
    states <- unique(trends$state)
  }
  
  index <- trends %>%
    # expand to all required dates and states
    select(state, date, mean) %>%
    old_right_join(
      expand_grid(
        state = states,
        date = date_seq
      )
    ) %>%
    # turn into a date-by-state matrix
    pivot_wider(
      names_from = state,
      values_from = mean
    ) %>%
    select(-date) %>%
    as.matrix()
  
  # crop to specified dates, extending either end out flat if missing
  if (!is.null(dates)) {
    idx <- dates - min(date_seq) + 1
    idx <- pmax(idx, 1)
    idx <- pmin(idx, nrow(index))
    index <- index[idx, ]
  }
  
  index
  
}
