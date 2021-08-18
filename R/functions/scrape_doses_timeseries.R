scrape_doses_timeseries <- function() {
  
  url <- "https://covidlive.com.au/report/daily-vaccinations/aus"
  
  # scrape the cumulative number of doses and convert to daily new doses
  scraped <- url %>%
    read_html() %>%
    html_nodes(
      "table"
    ) %>%
    .[[2]] %>%
    html_table(
      fill = TRUE
    ) %>%
    mutate(
      date = as.Date(DATE, format = "%d %B %y"),
      cumulative_doses = DOSES,
      cumulative_doses = gsub(",", "", cumulative_doses),
      cumulative_doses = as.numeric(cumulative_doses)
    ) %>%
    # compute new doses per day
    arrange(
      date
    ) %>%
    mutate(
      new_doses = diff(c(0, cumulative_doses))
    ) %>%
    select(
      date,
      new_doses
    )
  
  # fill in zeroes for any missing datess
  missing_dates <- expand_grid(
    date = seq(
      min(scraped$date),
      max(scraped$date),
      by = 1
    ),
    new_doses = 0
  ) %>%
    anti_join(
      scraped,
      by = "date"
    )
  
  # combine, reorder, compute cumulative number, drop leading 0s
  scraped %>%
    bind_rows(missing_dates) %>%
    arrange(date) %>%
    mutate(
      doses = cumsum(new_doses)
    ) %>%
    filter(
      doses > 0
    )
  
}
