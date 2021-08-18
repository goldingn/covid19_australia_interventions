yesno_to_logical <- function(x) {
  case_when(
    x == "Yes" ~ TRUE,
    x == "No" ~ FALSE,
    TRUE ~ NA
  )
}
