# load a csv of numbers of non-household contacts by state (where the first
# column gives numberts of contacts and the rest of each row gives counts of
# respondents with that number of contacts), tidy up the names, add a date and
# convert to long format (one row per response)
load_contacts_by_state <- function(csv, date) {
  
  contacts <- read_csv(
    csv,
    col_types = list(
      .default = col_double()
    )
  ) %>%
    rename_all(
      recode,
      contact_num = "contacts",
      num_contacts = "contacts",
      "Australian Capital Territory" = "ACT",
      "New South Wales" = "NSW",
      "Northern Territory" = "NT",
      "Queensland" = "QLD",
      "South Australia" = "SA",
      "Tasmania" = "TAS",
      "Victoria" = "VIC",
      "Western Australia" = "WA"
    ) %>%
    pivot_longer(cols = -contacts,
                 names_to = "state",
                 values_to = "respondents") %>%
    mutate(date = date) %>%
    filter(state != "Other") %>%
    uncount(respondents) 
  
  contacts
  
}
