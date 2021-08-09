# prop_voc_date_state <- function() {
#   tibble::tribble(
#     ~state,        ~date, ~prop_voc,
#      "ACT", "2021-01-27",         1,
#      "NSW", "2021-01-27",         1,
#       "NT", "2021-01-27",         1,
#      "QLD", "2021-01-27",         1,
#       "SA", "2021-01-27",         1,
#      "TAS", "2021-01-27",         1,
#      "VIC", "2021-01-27",         1,
#       "WA", "2021-01-27",         1
#   ) %>%
#     mutate(
#       date = as.Date(date)
#     )
# }
# 
# prop_voc_date_state_long <- function(dates) {
#   
#   df <- expand_grid(
#     date = dates,
#     state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
#   ) %>%
#     full_join(
#       prop_voc_date_state()
#     )%>%
#     pivot_wider(
#       names_from = state,
#       values_from = prop_voc
#     ) %>%
#     dplyr::select(-date) %>%
#     as.matrix
#   
#   
#   df[1,] <- apply(
#     X = df[1,] %>% as.matrix,
#     MARGIN = 2,
#     FUN = function(x){
#       ifelse(is.na(x), 0, x)
#     }
#   ) %>%
#     t
#   
#   df <- df %>%
#     as_tibble %>%
#     fill(everything()) %>%
#     as.matrix
#   
#   
#   return(df)
# }
prop_variant_dates <- function(){
  tibble::tribble(
    ~state,        ~date, ~prop_wt, ~prop_alpha, ~prop_delta,
    "ACT",  "2020-01-01",        1,           0,          0,
    "NSW",  "2020-01-01",        1,           0,          0,
    "NT",   "2020-01-01",        1,           0,          0,
    "QLD",  "2020-01-01",        1,           0,          0,
    "SA",   "2020-01-01",        1,           0,          0,
    "TAS",  "2020-01-01",        1,           0,          0,
    "VIC",  "2020-01-01",        1,           0,          0,
    "WA",   "2020-01-01",        1,           0,          0,
    
    "ACT",  "2021-01-27",        0,           1,          0,
    "NSW",  "2021-01-27",        0,           1,          0,
    "NT",   "2021-01-27",        0,           1,          0,
    "QLD",  "2021-01-27",        0,           1,          0,
    "SA",   "2021-01-27",        0,           1,          0,
    "TAS",  "2021-01-27",        0,           1,          0,
    "VIC",  "2021-01-27",        0,           1,          0,
    "WA",   "2021-01-27",        0,           1,          0,
    
    "ACT",  "2021-06-07",        0,           0,          1,
    "NSW",  "2021-06-07",        0,           0,          1,
    "NT",   "2021-06-07",        0,           0,          1,
    "QLD",  "2021-06-07",        0,           0,          1,
    "SA",   "2021-06-07",        0,           0,          1,
    "TAS",  "2021-06-07",        0,           0,          1,
    "VIC",  "2021-06-07",        0,           0,          1,
    "WA",   "2021-06-07",        0,           0,          1
  ) %>%
    mutate(
      date = as.Date(date)
    )
}
