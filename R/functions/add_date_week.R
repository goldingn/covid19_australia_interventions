#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param date
#' @return
#' @author Nicholas Tierney
#' @export
add_date_week <- function(data) {

    data %>% 
      mutate(week = week(date),
             year = year(date),
             date_week = as.Date(glue("{year}-{week}-{1}"), "%Y-%U-%u"),
             .after = date)

  }
