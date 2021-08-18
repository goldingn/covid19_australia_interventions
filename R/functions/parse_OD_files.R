parse_OD_files <- function(dir, starts) {
  dir %>%
    list.files(
      pattern = starts,
      full.names = TRUE
    ) %>%
    tibble(
      filename = .
    ) %>%
    mutate(
      details = basename(filename),
      details = str_remove(details, starts),
      details = str_remove(details, ".csv$")
    ) %>%
    mutate(
      timestamp = str_sub(details, -4),
      start_date = str_sub(details, end = 10),
      end_date = str_sub(details, start = 15, end = 24)
    ) %>%
    select(
      -details
    ) %>%
    mutate_at(
      vars(ends_with("_date")),
      as.Date
    ) %>%
    filter(
      start_date != as.Date("2020-06-16")
    )
}
