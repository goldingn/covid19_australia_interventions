#archived_functions



get_vic_linelist_wollert <- function(file = "~/not_synced/vic/Whittlesea outbreak dates 30 May 2021.xlsx"){
  
  file %>%
    read_excel(
      col_names = c(
        "ID",
        "CRM",
        "infectious_date",
        "inferred_onset",
        "positive"
      ),
      col_types = c(
        "text",
        "numeric",
        "date",
        "date",
        "date"
      ),
      skip = 1
    ) %>%
    mutate(
      date_onset = as.Date(inferred_onset),
      date_confirmation = as.Date(positive),
      state = "VIC",
      import_status = "local",
      postcode_of_acquisition = "8888",
      postcode_of_residence = "8888",
      state_of_acquisition = NA,
      state_of_residence = NA,
      report_delay = NA,
      date_linelist = NA,
      date_detection = date_confirmation - 1
    ) %>%
    dplyr::select(
      date_onset,
      date_detection,
      date_confirmation,
      state,
      import_status,
      postcode_of_acquisition,
      postcode_of_residence,
      state_of_acquisition,
      state_of_residence,
      report_delay,
      date_linelist
    )
  
}

## from get_vic_linelist

# ll_date <- max(linelist$date_linelist)
# 
# vic_linelist <- get_vic_linelist() %>%
#   mutate(date_linelist = ll_date) %>%
#   dplyr::select(
#     -postcode_of_acquisition,
#     -postcode_of_residence,
#     -state_of_acquisition,
#     -state_of_residence,
#     - report_delay
#   )
# 
# vic_nndss <- linelist %>%
#   filter(
#     (
#       state == "VIC" &
#         date_confirmation > as.Date("2021-05-20") &
#         import_status == "local"
#     )
#   ) #%>%
#   # dplyr::select(
#   #   -postcode_of_acquisition,
#   #   -postcode_of_residence,
#   #   -state_of_acquisition,
#   #   -state_of_residence,
#   #   - report_delay
#   # )
# 
# 
# nndss_match <- semi_join(
#   x = vic_nndss %>%
#     group_by(
#       date_onset,
#       date_confirmation
#     ) %>%
#     mutate(id = row_number()),
#   y = vic_linelist %>%
#     group_by(
#       date_onset,
#       date_confirmation
#     ) %>%
#     mutate(id = row_number()),
#   by = c(
#     "date_onset",
#     "date_confirmation",
#     "id"
#   )
# )
# 
# vic_match <- semi_join(
#   x = vic_linelist %>%
#     group_by(
#       date_onset,
#       date_confirmation
#     ) %>%
#     mutate(id = row_number()),
#   y = vic_nndss %>%
#     group_by(
#       date_onset,
#       date_confirmation
#     ) %>%
#     mutate(id = row_number()),
#   by = c(
#     "date_onset",
#     "date_confirmation",
#     "id"
#   )
# )
# 
# 
# nndss_anti <- anti_join(
#   x = vic_nndss %>%
#     group_by(
#       date_onset,
#       date_confirmation
#     ) %>%
#     mutate(id = row_number()),
#   y = vic_linelist %>%
#     group_by(
#       date_onset,
#       date_confirmation
#     ) %>%
#     mutate(id = row_number()),
#   by = c(
#     "date_onset",
#     "date_confirmation",
#     "id"
#   )
# )
# 
# nndss_anti_onset <- nndss_anti %>%
#   filter(!is.na(date_onset)) %>%
#   ungroup
# 
# nndss_anti_na <- nndss_anti %>%
#   filter(is.na(date_onset)) %>%
#   ungroup %>%
#   group_by(date_confirmation) %>%
#   mutate(id = row_number())
#   
# 
# vic_anti <- anti_join(
#   x = vic_linelist %>%
#     group_by(
#       date_onset,
#       date_confirmation
#     ) %>%
#     mutate(id = row_number()),
#   y = vic_nndss %>%
#     group_by(
#       date_onset,
#       date_confirmation
#     ) %>%
#     mutate(id = row_number()),
#   by = c(
#     "date_onset",
#     "date_confirmation",
#     "id"
#   )
# ) %>%
#   ungroup %>%
#   group_by(date_confirmation) %>%
#   arrange(date_detection, date_onset) %>%
#   mutate(id = row_number()) %>%
#   ungroup
# 
# nndss_splice <- left_join(
#   nndss_anti_na,
#   vic_anti %>%
#     select(date_onset, date_confirmation, id),
#   by = c(
#     "date_confirmation",
#     "id"
#   )
# ) %>%
#   mutate(
#     date_onset = date_onset.y
#   )%>%
#   select(
#     date_onset,
#     date_detection,
#     date_confirmation,
#     state,
#     import_status,
#     postcode_of_acquisition,
#     postcode_of_residence,
#     state_of_acquisition,
#     state_of_residence,
#     report_delay,
#     date_linelist
#   )
# 
# 
# vic_check <- anti_join(
#   vic_anti %>%
#     select(date_onset, date_confirmation, id),
#   nndss_anti_na,
#   by = c(
#     "date_confirmation",
#     "id"
#   )
# ) # leftover from vic dataset that are not matchable to nndss
# 
# vic_spliced <- bind_rows(
#   nndss_match,
#   nndss_anti_onset,
#   nndss_splice
# )
# 
# 
# linelist <- linelist %>%
#   filter(
#     !(state == "VIC" &
#         date_confirmation > as.Date("2021-05-20") &
#         import_status == "local")
#   ) %>%
#   bind_rows(
#     vic_spliced
#   )

