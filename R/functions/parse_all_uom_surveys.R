parse_all_uom_surveys <- function(dir = "~/not_synced/uom_surveys/unlocked") {
  
  dir %>%
    file.path(
      c(
        "SPSS Updated Data Covid19 Attitudes and Practices labels April2020.xlsx",
        "SPSS Wave 2 Data Covid19 Attitudes and Practices labels May2020.xlsx"
      )
    ) %>%
    mapply(
      parse_uom_survey,
      .,
      wave = c(-1, 0),
      SIMPLIFY = FALSE
    ) %>%
    bind_rows()
  
}
