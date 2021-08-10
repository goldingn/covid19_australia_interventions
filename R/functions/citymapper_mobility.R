# load the Citymapper index (urban direction requests, mostly public transport)
# for a couple of cities
citymapper_mobility <- function() {
  
  data <- citymapper_url() %>%
    read_csv(
      skip = 3,
      col_types = cols(
        .default = col_double(),
        Date = col_date(format = "")
      )
    ) %>%
    pivot_longer(
      cols = -Date,
      names_to = "region",
      values_to = "trend"
    ) %>%
    filter(region %in% ideal_regions()) %>%
    filter(!is.na(trend)) %>%
    rename(date = Date) %>%
    mutate(
      trend = 100 * (trend - 1),
      state = case_when(
        region == "Sydney" ~ "New South Wales",
        region == "Melbourne" ~ "Victoria",
        region == "Brisbane" ~ "Queensland",
        region == "Perth" ~ "Western Australia",
        TRUE ~ as.character(NA)
      )
    )
  
}
