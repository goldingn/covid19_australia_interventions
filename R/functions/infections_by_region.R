# convert imputed linelist into matrix of new infections by date and state
infections_by_region <- function(linelist,
                                 region_type = c("state", "postcode_of_residence"),
                                 case_type = c("local", "imported", "both"),
                                 from = min(linelist$date),
                                 to = max(linelist$date)) {
  
  region_type <- match.arg(region_type)
  case_type <- match.arg(case_type)
  
  # get full range of dates (do this before dropping rows)
  dates <- seq(from, to, by = 1)
  
  # drop unneeded rows (and regions)
  if (case_type != "both") {
    linelist <- linelist %>%
      filter(import_status == case_type)
  }
  
  regions <- unique(linelist[[region_type]])
  
  # pad this with full set of dates, states, and import statuses
  grid <- expand_grid(
    date = dates,
    region = regions
  )
  
  # widen into matrices of date by state
  new_infections <- linelist %>%
    mutate(cases = 1) %>%
    rename(region = !!region_type) %>%
    old_right_join(grid) %>%
    group_by(region, date) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = region, values_from = cases) %>%
    select(-date) %>%
    as.matrix()
  
  new_infections
  
}
