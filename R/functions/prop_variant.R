prop_variant <- function(dates){
  
  df <-  prop_variant_dates() %>%
    full_join(
      y = expand_grid(
        date = dates,
        state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
      )
    ) %>%
    arrange(state, date) %>%
    tidyr::fill(everything()) %>%
    filter(date %in% dates) # account for "2020-01-01" start date may not be in dates
    
  prop_wt <- df %>%
    dplyr::select(state, date, prop_wt) %>%
    pivot_wider(
      names_from = state,
      values_from = prop_wt
    ) %>%
    arrange(date) %>%
    dplyr::select(-date)%>%
    as.matrix
  
  prop_alpha <- df %>%
    dplyr::select(state, date, prop_alpha) %>%
    pivot_wider(
      names_from = state,
      values_from = prop_alpha
    ) %>%
    arrange(date) %>%
    dplyr::select(-date)%>%
    as.matrix
  
  prop_delta <- df %>%
    dplyr::select(state, date, prop_delta) %>%
    pivot_wider(
      names_from = state,
      values_from = prop_delta
    ) %>%
    arrange(date) %>%
    dplyr::select(-date)%>%
    as.matrix
  
  prop_variant <- list(
    "prop_wt"    = prop_wt,
    "prop_alpha" = prop_alpha,
    "prop_delta" = prop_delta
  )
  
  return(prop_variant)
}
