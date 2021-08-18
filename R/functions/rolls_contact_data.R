# get the numbers of household contacts and distribution among types of
# locations from Rolls et al.
rolls_contact_data <- function() {
  
  # load data on encounters of individuals and format
  individuals <- read_csv(
    file = "data/contacts/covid_full.csv",
    col_types = cols(
      participant_id = col_double(),
      contact_id = col_character(),
      contact_duration = col_double(),
      location_duration = col_double(),
      hh_member = col_double(),
      location_code = col_double(),
      location_type = col_character(),
      weight = col_double()
    ),
    na = c("NULL", "", "NA")
  ) %>%
    select(-location_duration, -location_code) %>%
    mutate(location = case_when(
      location_type == "Home" ~ "home",
      location_type == "Retail and hospitality (bars, cafes, shops, hair dressing, etc.)" ~ "retail",
      location_type == "Public spaces (parks, streets, stations, airports etc.)" ~ "public",
      location_type == "Public Transport (train, tram, bus or taxi)" ~ "transit",
      location_type == "Work" ~ "work",
      TRUE ~ "other",
    ))
  
  # convert encounters to numbers and total durations of unique contacts in each
  # locationand contact type
  contact_data <- individuals %>%
    # this encounter as a proportion of all encounters with this contact
    group_by(participant_id, contact_id) %>%
    mutate(proportion = 1 / n()) %>%
    # sum proportions and durations in each location, using the average duration with that contact where it's missing
    group_by(participant_id, contact_id, hh_member, location, weight) %>%
    summarise(
      proportion = sum(proportion),
      contact_duration = mean(contact_duration, na.rm = TRUE) / n(),
    ) %>%
    # count (proportional) unique contacts and average durations in household/non
    # household in each location for each participant
    group_by(participant_id, hh_member, location, weight) %>%
    summarise(
      contacts = sum(proportion),
      contact_duration = mean(contact_duration, na.rm = TRUE)
    ) %>%
    # convert duration to hours
    mutate(contact_duration = contact_duration / (60)) %>%
    ungroup() %>%
    mutate(
      hh_member = ifelse(hh_member == 1,
                         "household",
                         "non_household")
    ) %>%
    group_by(weight) %>%
    # expand out to include 0s for different categories, to averages are unbiased
    complete(participant_id, hh_member, location, fill = list(contacts = 0)) %>%
    arrange(participant_id, hh_member, location)
  
  contact_data
  
}
