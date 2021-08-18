# plot the combined impacts of vaccination and public health and social measures
# (PHSM) on control (reducing transmission potential to <1 of COVID-19)
summarise_samples <- function(samples_file) {
  delta_summary <-
    samples_file %>%
    read_csv(
      col_types = cols(
        .default = col_double(),
        date = col_date(),
        state = col_character(),
        date_onset = col_date()
      )
    ) %>%
    pivot_longer(
      cols = !(date:date_onset),
      names_to = "sim",
      values_to = "tp"
    ) %>%
    group_by(
      date, state
    ) %>%
    summarise(
      tp = mean(tp),
      .groups = "drop"
    )
}
