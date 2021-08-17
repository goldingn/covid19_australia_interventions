#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param doh_path
#' @param abs_lga_postcodes_path
#' @return
#' @author Nicholas Tierney
#' @export
read_doh_add_weights <- function(doh_path = "~/not_synced/survey_data/",
                                 abs_lga_postcodes_path =
                                 "~/not_synced/CA_POSTCODE_2018_LGA_2018.xlsx") {
  
    doh_survey <- parse_all_doh_surveys(doh_path)
    
    postcodes <- doh_survey %>% 
      pull(postcode) %>% 
      unique()

    browser()
    weights_tbl <- create_abs_lga_weights(
      file = abs_lga_postcodes_path,
      postcode = postcodes
    )
    doh_survey_weights <- doh_survey %>% 
      left_join(weights_tbl,
                by = "postcode")
    
    doh_survey_weights

}
