load_linelist <- function(date = NULL,
                          use_vic = FALSE,
                          use_sa = TRUE,
                          use_nsw = TRUE) {
  
  # load the latest NNDSS linelist (either the latest or specified file)
  linelist <- get_nndss_linelist(date = date)
  
  # optionally replace VIC data with DHHS direct upload
  if (use_vic) {
    
    vic_linelist <- linelist$date_linelist[1] %>%
      format(format = "%Y%m%d") %>%
      paste0("~/not_synced/vic/", ., "_linelist_reff.csv") %>%
      get_vic_linelist()
    
    linelist <- linelist %>%
      filter(state != "VIC") %>%
      bind_rows(vic_linelist)
    
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
    
  }
  
  if (use_sa) {
    
    linelist <- linelist %>%
      filter(
        !(state == "SA" &
            import_status == "local" &
            date_detection >= as.Date("2021-07-19") & 
            date_detection < as.Date("2021-07-26")
          )
      ) %>%
      
      bind_rows(
        get_sa_linelist()
      )
     
  }
  
  if (use_nsw) {
    
    nsw_ll <- get_nsw_linelist()
    nsw_ll_date <- nsw_ll$date_linelist[1]
    nsw_ll_start <- min(nsw_ll$date_confirmation)
    
    
    linelist <- linelist %>%
      filter(
        !(state == "NSW" &
            import_status == "local" &
            date_detection >= nsw_ll_start & 
            date_detection <= nsw_ll_date
        )
      ) %>%
      
      bind_rows(
        nsw_ll
      )
  }
  
  # flag whether each case is an interstate import
  linelist <- linelist %>%
    mutate(
      interstate_import = case_when(
        state != state_of_acquisition ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  linelist
  
}
