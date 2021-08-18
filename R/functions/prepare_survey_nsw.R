#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param pmin_contact_num
#' @param vec_lga_concern
#' @return
#' @author Nicholas Tierney
#' @export
prepare_survey_nsw <- function(data,
                               pmin_contact_num = 100, 
                               vec_lgas_concern) {

  data %>% 
  filter(state == "NSW",
         age >= 16) %>% 
    relocate(date, .after = wave) %>% 
    prepare_survey_contact(pmin_contact_num = pmin_contact_num,
                           vec_lga_of_concern = vec_lgas_concern)

}
