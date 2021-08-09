write_mobility_dates <- function(mobility, dir = "outputs/"){
  mobility %>%
    group_by(datastream) %>%
    summarise(
      latest = max(date),
      earliest = min(date)
    ) %>%
    write_csv(
      file = file.path(dir, 'mobility_dates.csv')
    )
}
