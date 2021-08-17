#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vec_lgas_of_concern
#' @return
#' @author Nicholas Tierney
#' @export
add_lga_of_concern <- function(data, vec_lgas_of_concern) {

    data %>% 
      mutate(
        lga_of_concern = if_else(
          condition = str_detect(lga, vec_lgas_of_concern),
          true = "lga_of_concern",
          false = "lga_NOT_of_concern"
        ),
        .after = lga
      ) 

}
