# parsing the file of microdistancing measures from BETA
barometer_results <- function() {
  
  files <- list.files("data/microdistancing/",
                      pattern = ".csv$",
                      full.names = TRUE)
  
  tibbles <- lapply(
    files,
    read_csv,
    col_types = cols(
      date = col_date(format = ""),
      state = col_character(),
      question = col_character(),
      response = col_character(),
      count = col_double(),
      respondents = col_double()
    )
  )
  
  do.call(bind_rows, tibbles) %>%
    filter(
      !state %in% c("Australia", "Other")
    ) %>%
    mutate(
      question = recode(
        question,
        "hand hygine" = "Hand washing",
        "cough" = "Cough etiquette",
        "non-household contact" = "Physical contact"
      ), 
      state = recode(
        state,
        "New South Wales" = "NSW",
        "Northern Territory" = "NT",
        "Queensland" = "QLD",
        "South Australia" = "SA",
        "Tasmania" = "TAS",
        "Victoria" = "VIC",
        "Western Australia" = "WA"
      )
    ) %>%
    arrange(state, date, question, response)
  
}
