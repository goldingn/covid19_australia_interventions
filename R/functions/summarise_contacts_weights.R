#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author Nicholas Tierney
#' @export
summarise_contacts_weights <- function(data) {

  data %>% 
    summarise(
      n_resp_weight = sum(n()*weight),
      across(
        .cols = contains("contact"),
        .fns = ~weighted.mean(x = .x, w = weight),
        .names = "avg_{.col}_weight"
      )
    )

}
