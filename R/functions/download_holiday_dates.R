download_holiday_dates <- function(destination) {
  
  # create the directory if needed
  directory <- dirname(destination)
  if (!file.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  list(
    "2020" = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/c4163dc4-4f5a-4cae-b787-43ef0fcf8d8b/download/australian_public_holidays_2020.csv",
    "2021" = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/2dee10ef-2d0c-44a0-a66b-eb8ce59d9110/download/australian_public_holidays_2021.csv",
    "2022" = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/d256f989-8f49-46eb-9770-1c6ee9bd2661/download/australian_public_holidays_2022.csv"
  ) %>%
    lapply(
      read_csv,
      col_types = 
        cols(
          Date = col_date(format = "%Y%m%d"),
          `Holiday Name` = col_character(),
          Information = col_character(),
          `More Information` = col_character(),
          Jurisdiction = col_character()
        )
    ) %>%
    do.call(
      bind_rows, .
    ) %>%
    mutate(
      state = toupper(Jurisdiction),
      state = unabbreviate_states(state),
      date = Date,
      name = `Holiday Name`
    ) %>%
    select(state, date, name) %>%
    write_csv(destination)
}
