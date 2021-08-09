# read in and parse all the respondent-level survey data
parse_all_surveys <- function() {
  bind_rows(
    parse_all_uom_surveys(),
    parse_barometer(),
    parse_all_doh_surveys()
  ) %>%
    mutate(
      weekend_fraction = weekend_weight(date)
    ) %>%
    group_by(wave) %>%
    mutate(
      wave_date = median(date),
      wave_duration = as.numeric(max(date) - min(date))
    ) %>%
    ungroup()
}
