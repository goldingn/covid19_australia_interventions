# download and format Apple's mobility data
apple_mobility <- function() {
  data <- apple_url() %>%
    readr::read_csv(
      col_types = cols(
        .default = col_double(),
        geo_type = col_character(),
        region = col_character(),
        transportation_type = col_character(),
        alternative_name = col_character(),
        `sub-region` = col_character(),
        country = col_character()
      )) %>%
    tidyr::pivot_longer(
      cols = !any_of(
        c(
          "geo_type",
          "region",
          "transportation_type",
          "alternative_name",
          "sub-region",
          "country"
        )
      ),
      names_to = "date",
      values_to = "trend"
    ) %>%
    mutate(
      date = lubridate::date(date)
    ) %>%
    filter(
      region %in% ideal_regions()
    )
  
  # transform to percentage change, and correct dates (they are agreegated
  # midnight-midnight in pacific central time, for which Australian daylight
  # hours fall on the next calendar day in Australia)
  data <- data %>%
    mutate(trend = trend - 100,
           date = date + 1)
  
  # pull out the states and cities
  states <- c("New South Wales", "Victoria", "Queensland", "Western Australia", 
              "South Australia", "Australian Capital Territory", "Tasmania", 
              "Northern Territory")
  
  cities <- c("Sydney", "Melbourne", "Brisbane", "Perth", 
              "Adelaide", "Canberra", "Hobart", "Darwin")
  
  # need to use state-level data for driving, but cities for other metrics
  data <- data %>%
    filter(
      case_when(
        transportation_type == "driving" & region %in% states ~ TRUE,
        transportation_type != "driving" & region %in% cities ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    mutate(
      region = case_when(
        region == "Sydney" ~ "New South Wales",
        region == "Melbourne" ~ "Victoria",
        region == "Brisbane" ~ "Queensland",
        region == "Perth" ~ "Western Australia",
        region == "Adelaide" ~ "South Australia",
        region == "Hobart" ~ "Tasmania",
        region == "Canberra" ~ "Australian Capital Territory",
        region == "Darwin" ~ "Northern Territory",
        TRUE ~ region
      )
    ) %>%
    rename(state = region)
  
  data
  
}
