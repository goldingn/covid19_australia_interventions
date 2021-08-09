# save local case data, dates, and detection probabilities for Robs
write_local_cases <- function(model_data, dir = "outputs") {
  
  tibble::tibble(
    date_onset = rep(model_data$dates$onset, model_data$n_states),
    detection_probability = as.vector(model_data$detection_prob_mat),
    state = rep(model_data$states, each = model_data$n_dates),
    count = as.vector(model_data$local$cases_infectious),
    acquired_in_state = as.vector(model_data$local$cases)
  ) %>%
    write.csv(
      file.path(dir, "local_cases_input.csv"),
      row.names = FALSE
    )
  
}
