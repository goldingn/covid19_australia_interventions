quarantine_dates <- function() {
  expand_grid(
    date = c("2020-03-15", "2020-03-28"),
    state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", 
      "VIC", "WA")
  ) %>%
    mutate(
      date = as.Date(date),
      state = factor(state)
    )
}
