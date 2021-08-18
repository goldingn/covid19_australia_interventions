#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pmin_contact_num
#' @param vec_lga_of_concern
#' @return
#' @author Nicholas Tierney
#' @export
prepare_survey_contact <- function(data, 
                                   pmin_contact_num = 100, 
                                   vec_lga_of_concern) {

    data %>% 
      add_date_week() %>% 
      filter(year == 2021) %>% 
      mutate(contact_num = pmin(pmin_contact_num, contact_num)) %>% 
      add_lga_of_concern(vec_lgas_of_concern = vec_lga_of_concern) %>% 
    mutate(youth_older = if_else(
      condition = between(age, 16, 39),
      true = "16-39",
      false = "40+"
    ),
    .after = lga_of_concern
    ) 

}
