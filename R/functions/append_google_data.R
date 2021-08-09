# append any missing google mobility data to mobility data (because Google
# dropped a bunch of data out this one time)
append_google_data <- function(mobility_data, url = tidycovid_url) {
  
  mobility_data <- mobility_data %>%
    filter(!is.na(trend)) %>%
    filter(!is.na(state)) %>%
    select(state, date, trend, datastream)
  
  
  file <- "data/google_cmr/tidycovid_cache.RDS"
  if (!file.exists(file)) {
    dowload_tidycovid()
  }
  tmp <- readRDS(file)

  
  # get the previous data in the same format
  previous_data <- tmp$country_region %>%
    filter(iso3c == "AUS") %>%
    rename(
      retail_and_recreation = retail_recreation,
      grocery_and_pharmacy = grocery_pharmacy,
      state = region
    ) %>%
    pivot_longer(
      cols = c(
        "retail_and_recreation",
        "grocery_and_pharmacy",
        "parks",
        "transit_stations",
        "workplaces",
        "residential"
      ),
      values_to = "trend",
      names_to = "category"
    ) %>%
    mutate(
      category = str_replace_all(category, "_", " ")
    ) %>%
    mutate(
      datastream = str_c("Google: time at ", category)
    ) %>%
    select(
      state,
      date,
      trend,
      datastream
    )
  
  # find any observations that are missing in the mobility data
  missing_data <- mobility_data %>%
    anti_join(previous_data, ., by = c("state", "date", "datastream"))
  
  # add them and return
  mobility_data %>%
    anti_join(missing_data, by = c("state", "date", "datastream")) %>%
    bind_rows(missing_data) %>%
    arrange(datastream, state, date)
  
}
