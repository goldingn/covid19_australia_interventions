compute_infectious <- function(state_cases, lga, import_rate,
                               sources = NULL) {
  
  # compute the remaining total future infectious potential in each LGA at the latest date
  gi_vec <- gi_vector(gi_cdf(), max(state_cases$date))
  potential_remaining <- 1 - cumsum(gi_vec)
  
  infectious <- state_cases %>%
    mutate(
      days_ago = as.numeric(max(date) - date)
    ) %>%
    left_join(
      tibble(
        days_ago = seq_along(potential_remaining),
        potential = potential_remaining
      )
    ) %>%
    mutate(
      potential = replace_na(potential, 0),
      lga_code = as.character(lga_code)
    ) %>%
    group_by(lga, lga_code) %>%
    summarise(
      infectious_potential = sum(potential * infections)
    ) %>%
    ungroup() %>%
    # add on missing lgas and infectious import potential
    old_right_join(
      tibble(
        lga = colnames(import_rate)
      )
    ) %>%
    mutate(
      infectious_potential = replace_na(infectious_potential, 0),
      import_potential = (infectious_potential %*% import_rate)[1, ]
    )
  
  # define the source lgas if not provided
  if (is.null(sources)) {
    sources <- infectious %>%
      filter(infectious_potential > 0) %>%
      pull(lga)
  }
  
  infectious <- infectious %>%
    # compute the risk of non-lockdown areas bing infected *by lockdown areas*
    mutate(
      is_source = lga %in% sources,
      lockdown_export_potential = ifelse(is_source, infectious_potential, 0),
      sink_import_potential = (lockdown_export_potential %*% import_rate)[1, ],
      sink_import_potential = ifelse(is_source, 0, sink_import_potential),
      sink_import_potential = sink_import_potential / max(sink_import_potential),
    )
  
  lga %>%
    st_simplify(dTolerance = 0.001) %>%
    left_join(infectious) %>%
    mutate_at(
      vars(
        infectious_potential,
        import_potential,
        sink_import_potential
      ),
      ~replace_na(., 0)
    ) %>%
    mutate(
      infectious_potential_area = infectious_potential / area,
    )
}
