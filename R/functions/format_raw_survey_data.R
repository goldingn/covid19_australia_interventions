# read in the national survey raw data and create contact and microdistancing
# data in the same firmat as originally shared. I decided I can have a weird
# recursive function, as a treat.
format_raw_survey_data <- function(file = NULL, wave = NULL) {
  
  # in default mode, loop through all extra raw files and try to get them in order
  if (is.null(file)) {
    files <- list.files("data/survey_raw/", pattern = ".csv$", full.names = TRUE)
    lengths <- nchar(files)
    dates <- files %>%
      substr(lengths - 9, lengths - 4) %>%
      as.Date("%d%m%y")
    files <- files[order(dates)]
    waves <- seq_along(files) + 14
    mapply(format_raw_survey_data, files, waves)
    return(invisible(NULL))
  }
  
  raw <- read_csv(file)
  
  micro <- raw %>%
    select(
      state = S3,
      date = StartDate,
      "1.5m compliance" = Q65,
      "non-household contact" = Q109,
      "hand hygine" = Q110,
      "cough" = Q111
    ) %>%
    mutate(
      state = case_when(state == "ACT" ~ "Australian Capital Territory",
                        TRUE ~ state),
      state = abbreviate_states(state)
    ) %>%
    filter(!is.na(state)) %>%
    mutate(
      date = as.character(date),
      date = as.Date(date, format = "%Y%m%d"),
      date = min(date)
    ) %>%
    pivot_longer(
      cols = c(
        "1.5m compliance",
        "non-household contact",
        "hand hygine",
        "cough"
      ),
      names_to = "question",
      values_to = "response"
    ) %>%
    group_by(state, date, question) %>%
    mutate(respondents = n()) %>%
    group_by(state, date, question, respondents) %>%
    count(response) %>%
    select(date,
           state,
           question,
           response,
           count = n,
           respondents) %>%
    arrange(state, date, question, response)
  
  write_csv(micro,
            paste0(
              "data/microdistancing/Barometer wave ",
              wave,
              " compliance.csv"
            ))
  
  contacts <- raw %>%
    select(state = S3,
           num_contacts = Q138) %>%
    mutate(state = replace_na(state, "Other")) %>%
    group_by(num_contacts, state) %>%
    summarise(n = n()) %>%
    pivot_wider(
      names_from = state,
      values_from = n,
      values_fill = list(n = 0)
    )
  
  
  write_csv(contacts,
            paste0("data/contacts/barometer/contact numbers wave ", wave, ".csv"))
  
}
