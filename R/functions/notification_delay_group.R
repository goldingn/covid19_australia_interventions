# split the dates and states into periods  with similar notification delay distributions
notification_delay_group <- function(date_confirmation, state) {
  
  stage <- case_when(
    date_confirmation < as.Date("2020-06-14") ~ 1,
    date_confirmation < as.Date("2020-08-01") ~ 2,
    date_confirmation < as.Date("2020-08-21") ~ 3,
    TRUE ~ 4,
  )
  
  group <- case_when(
    stage == 1 ~ "all states (start-Jun13)",
    stage == 2 & state == "VIC" ~ "VIC 1 (Jun14-Jul31)",
    stage == 3 & state == "VIC" ~ "VIC 2 (Aug1-Aug20)",
    stage == 4 & state == "VIC" ~ "VIC 3 (Aug21-now)",
    TRUE ~ "other states Jun14-now"
  )
  
  group
  
}
