# compute fractions of non-household (unique) contacts
# in each location 
location_contacts <- function() {
  
  rolls_contact_data() %>%
    filter(hh_member == "non_household") %>%
    group_by(participant_id, location, weight) %>%
    summarise(contacts = sum(contacts)) %>%
    group_by(location) %>%
    summarise(
      mean_contacts = weighted_mean(
        contacts,
        w = weight
      )
    ) %>%
    mutate(
      proportion_contacts = mean_contacts / sum(mean_contacts)
    )
  
}
