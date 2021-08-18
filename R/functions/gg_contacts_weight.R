#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param group
#' @return
#' @author Nicholas Tierney
#' @export
gg_contacts_weight <- function(data, group) {

    ggplot(data,
           aes(x = date_week,
               y = avg_contact_num_weight,
               size = n_resp_weight,
               colour = {{group}})) + 
      geom_point(alpha = 0.75) +
      scale_colour_brewer(palette = "Dark2") +
      facet_wrap(facets = vars({{group}})) +
      theme(legend.position = "none")
    
}
