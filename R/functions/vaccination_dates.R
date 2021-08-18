vaccination_dates <- function() {
  expand_grid(
    date = c("2021-02-22"),
    state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", 
              "VIC", "WA")
  ) %>%
    mutate(
      date = as.Date(date),
      state = factor(state)
    )
}
