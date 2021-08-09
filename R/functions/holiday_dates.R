# dates of public holidays by state, from:
# https://data.gov.au/dataset/ds-dga-b1bc6077-dadd-4f61-9f8c-002ab2cdff10/details?q=
holiday_dates <- function(holiday_file = "data/holidays/public_holidays.csv") {
  
  if (!file.exists(holiday_file)) {
    download_holiday_dates(holiday_file)
  }
  
  read_csv(
    holiday_file,
    col_types = cols(
      state = col_character(),
      date = col_date(format = ""),
      name = col_character()
    )
  )

}
