# change in time at residential locations in each state
h_t_state <- function(dates) {
  
  location_change(dates) %>%
    select(state, date, home) %>%
    pivot_wider(names_from = state, values_from = home) %>%
    select(-date) %>%
    as.matrix()
  
}
