write_reff_key_dates <- function(model_data, dir = "outputs/") {
  # save these dates for Freya and Rob to check
  tibble(
    linelist_date = model_data$dates$linelist,
    latest_infection_date = model_data$dates$latest_infection,
    latest_reff_date = model_data$dates$latest_mobility,
    forecast_reff_change_date = model_data$dates$latest_mobility + 1
  ) %>%
    write_csv(
      file.path(dir, "output_dates.csv")
    )
}
