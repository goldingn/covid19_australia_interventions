# fetch the state corresponding to each postcode
postcode_to_state <- function(postcode) {
  
  state <- case_when(
    grepl("^26", postcode) ~ "ACT",
    grepl("^2", postcode) ~ "NSW",
    grepl("^3", postcode) ~ "VIC",
    grepl("^4", postcode) ~ "QLD",
    grepl("^5", postcode) ~ "SA",
    grepl("^6", postcode) ~ "WA",
    grepl("^7", postcode) ~ "TAS",
    grepl("^08", postcode) ~ "NT",
    # QLD seems to be recording some locations as 93xx (QLD po-box or LVR?) 9399
    # is in NIR list as QLD, but 9301 is not)
    grepl("^93", postcode) ~ "QLD",
    TRUE ~ "NA"
  )
  
  state[state == "NA"] <- NA
  state
  
}
