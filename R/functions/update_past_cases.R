# convert any new linelist files into formatted case data in past_cases
update_past_cases <- function(past_cases_dir = "outputs/past_cases") {
  
  # linelists already processed
  past_cases_files <- list.files(past_cases_dir, pattern = ".csv$") %>%
    tibble(file = .) %>%
    mutate(
      date = gsub("local_cases_input_", "", file),
      date = gsub(".csv", "", date),
      date = as.Date(date)
    )
  
  # find all nndss linelists that haven't yet been processed (using only the latest on each date)
  linelist_files <- linelist_date_times("~/not_synced/nndss") %>%
    mutate(
      date = as.Date(date_time),
      file = basename(file)
    ) %>%
    group_by(date) %>%
    filter(
      date_time == max(date_time)
    ) %>%
    ungroup() %>%
    anti_join(
      past_cases_files,
      by = "date"
    )
  
  # subset to only Wednesdays
  # mutate(wday = lubridate::wday(date, label = TRUE)) %>%
  # filter(wday == "Wed")
  
  linelists <- linelist_files %>%
    pull(date) %>%
    lapply(load_linelist)
  
  # format and write these out
  for (linelist in linelists) {
    
    model_data <- reff_model_data(linelist)
    
    linelist_date <- model_data$dates$linelist
    
    message("processing linelist: ", linelist_date)
    
    tibble::tibble(
      date_onset = rep(model_data$dates$onset, model_data$n_states),
      detection_probability = as.vector(model_data$detection_prob_mat),
      state = rep(model_data$states, each = model_data$n_dates),
      count = as.vector(model_data$local$cases_infectious),
      acquired_in_state = as.vector(model_data$local$cases)
    ) %>%
      write.csv(
        paste0(past_cases_dir, "/local_cases_input_",
               format(linelist_date, format = "%Y-%m-%d"), ".csv"),
        row.names = FALSE)
    
  }
  
}
