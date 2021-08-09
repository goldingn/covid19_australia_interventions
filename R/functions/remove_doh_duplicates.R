# remove duplicated data in DoH surveys caused by bots
remove_doh_duplicates <- function(doh_surveys) {
  
  # find clusters of more than responses of the same age gender and postcode in a given wave
  duplicates <-
    doh_surveys %>%
    filter(!is.na(postcode) & postcode != -99 & wave >= 22) %>%
    group_by(wave, age, gender, postcode) %>%
    summarise(count = n()) %>%
    filter(count > 3) %>%
    arrange(wave, postcode, gender, age)
  
  # print out the detected duplicates
  message("duplicates detected:")
  print(duplicates, n = Inf)
  
  # return the data with them removed
  doh_surveys %>%
    anti_join(duplicates)
  
}
