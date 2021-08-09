build_gravity_matrix <- function(
  OD_files,
  lga,
  lga_codes,
  start = as.Date("2000-01-01"),
  end = as.Date("2050-01-01")
) {
  # get distance matrix
  lga_distance <- lga %>%
    st_geometry() %>%
    st_centroid() %>%
    st_distance() %>%
    units::drop_units()
  
  base <- lga %>%
    st_drop_geometry() %>%
    select(lga, lga_code, pop)
  
  # get tibble for pairs of places
  pairs <- expand_grid(
    lga_from = lga$lga,
    lga_to = lga$lga
  ) %>%
    old_right_join(
      rename_all(base, paste0, "_from")
    ) %>%
    old_right_join(
      rename_all(base, paste0, "_to")
    ) %>%
    mutate(
      distance = as.vector(lga_distance),
      distance = distance / 1e3
    )
  
  idx <- match(lga$lga_code, lga_codes)
  
  # load aggregated OD matrices, subset to state, and turn into 3 column matrix
  ODs <- lapply(OD_files$filename,
                read_csv,
                col_names = FALSE) %>%
    lapply(as.matrix) %>%
    lapply(`[`, idx, idx) %>%
    lapply(as.vector) %>%
    lapply(
      cbind,
      expand_grid(
        lga_code_from = lga$lga_code,
        lga_code_to = lga$lga_code
      )
    ) %>%
    lapply(
      rename,
      flow = "X[[i]]"
    )
  
  add_meta <- function(idx, OD_list, meta) {
    meta <- meta[idx, ]
    mutate(OD_list[[idx]],
           start_date = meta$start_date,
           end_date = meta$end_date,
           timestamp = meta$timestamp)
  }
  
  # get all combinations, compute (censored) numbers of movements, and restrict to
  # pre-lockdown and inter-lga movements
  OD_data <- lapply(seq_along(ODs),
                    add_meta,
                    ODs,
                    OD_files) %>%
    bind_rows() %>%
    old_right_join(pairs) %>%
    tibble() %>%
    mutate(
      days = as.numeric(end_date - start_date) + 1,
      count = round(flow * days)
    ) %>%
    filter(
      end_date < end,
      start_date > start,
      lga_from != lga_to
    )
  
  # fit gravity model
  # flow ~ days * N1*N2/d
  # log(flow) ~ log(N1) + log(N2) - log(d) + log(days)
  m <- glm(count ~
             log(pop_from) +
             log(pop_to) +
             log(distance) +
             timestamp,
           offset = log(days),
           data = OD_data,
           family = stats::poisson)
  
  pred <- pairs %>%
    mutate(
      timestamp = "0000",
      days = 1
    ) %>%
    mutate(
      expected_flow = predict(m, newdata = ., type = "response")
    ) %>%
    mutate(
      expected_flow = ifelse(
        is.finite(expected_flow),
        expected_flow,
        NA
      )
    ) %>%
    select(lga_from,
           lga_to,
           expected_flow) %>%
    pivot_wider(
      names_from = lga_to,
      values_from = expected_flow
    ) %>%
    select(-lga_from) %>%
    as.matrix() %>%
    `rownames<-`(colnames(.))
  
  pred
  
}
