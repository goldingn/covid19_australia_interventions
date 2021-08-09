parse_all_doh_surveys <- function(dir = "data/survey_raw") {
  
  dir %>%
    list.files(
      pattern = ".csv$",
      full.names = TRUE
    ) %>%
    lapply(parse_doh_survey) %>%
    bind_rows() %>%
    remove_doh_duplicates()
  
}
