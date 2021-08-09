lga_to_state <- function (lga) {
  
  "data/spatial/abs/LGA19_to_STATE16.csv" %>%
    read_csv(
      col_types = cols(
        LGA_CODE_2019 = col_double(),
        LGA_NAME_2019 = col_character(),
        STATE_CODE_2016 = col_double(),
        STATE_NAME_2016 = col_character()
      )
    ) %>%
    select(
      lga = LGA_NAME_2019,
      state = STATE_NAME_2016
    ) %>%
    mutate(
      state = abbreviate_states(state)
    ) %>%
    old_right_join(
      tibble(lga = lga)
    ) %>%
    pull(state)
  
}
