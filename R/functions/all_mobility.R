# combine all mobility datasets
all_mobility <- function() {
  
  # load datasets and label their datastreams separately
  google <- google_mobility() %>%
    mutate(
      datastream = str_c("Google: time at ", category)
    ) %>%
    select(-category)
  
  apple <- apple_mobility() %>%
    mutate(
      datastream = str_c("Apple: directions for ", transportation_type)
    ) %>%
    select(
      -geo_type,
      -transportation_type
    )
  
  # facebook <- facebook_mobility() %>%
  #   mutate(
  #     datastream = str_c("Facebook: ", metric)
  #   ) %>%
  #   select(
  #     -metric
  #   )
  
  citymapper <- citymapper_mobility() %>%
    mutate(
      datastream = str_c("Citymapper: directions")
    )
  
  # combine the datasets
  bind_rows(
    google,
    apple,
    # facebook,
    citymapper
  )
  
}
