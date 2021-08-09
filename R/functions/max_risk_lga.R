# which lga is highest risk?
max_risk_lga <- function(lga_infectious) {
  lga_infectious %>%
    arrange(desc(sink_import_potential)) %>%
    filter(row_number() == 1) %>%
    pull(lga) %>%
    lga_name()
}
