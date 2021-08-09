# aggregate infections and infectiousness for a given case type at lga level
lga_infections <- function(linelist, dates, gi_mat, case_type = c("local", "imported")) {
  
  case_type <- match.arg(case_type)
  
  # aggregate locally-acquired cases by postcode and date
  postcode_matrix <- linelist %>%
    infections_by_region(
      region_type = "postcode",
      case_type = case_type,
      from = min(dates),
      to = max(dates)
    )
  
  postcodes <- colnames(postcode_matrix)
  
  # read in postcode-lga lookup and weights
  weights_tbl <- read_xlsx(
    "data/spatial/abs/CA_POSTCODE_2018_LGA_2018.xlsx",
    sheet = 4,
    skip = 5
  ) %>%
    filter(
      row_number() > 1,
      !is.na(RATIO)
    ) %>%
    select(
      postcode = POSTCODE_2018...1,
      lga = LGA_NAME_2018,
      lga_code = LGA_CODE_2018,
      weight = RATIO
    ) %>%
    # subset to observed postcodes
    old_right_join(
      tibble(
        postcode = postcodes,
      )
    ) %>%
    # assign unrecognised/unknown/overseas postcodes to a separate class
    mutate(
      lga = replace_na(lga, "other"),
      weight = replace_na(weight, 1)
    )
  
  # convert to matrix for weighting
  weights_matrix <- weights_tbl %>%
    select(-lga_code) %>%
    pivot_wider(
      names_from = lga,
      values_from = "weight",
      values_fill = list(weight = 0)
    ) %>%
    select(-postcode) %>%
    as.matrix() %>%
    `rownames<-`(postcodes)
  
  # normalise so we don't lose any cases
  weights_matrix <- sweep(weights_matrix, 1, rowSums(weights_matrix), FUN = "/")
  
  # aggregate cases to lga level
  lga_matrix <- postcode_matrix %*% weights_matrix
  lga <- lengthen(lga_matrix,
                  dates,
                  "lga",
                  "infections")
  
  # get infectiousness of these cases
  postcode_infectious_matrix <- gi_mat %*% postcode_matrix
  lga_infectious_matrix <- postcode_infectious_matrix %*% weights_matrix
  lga_infectious <- lengthen(lga_infectious_matrix,
                             dates,
                             "lga",
                             "infectiousness")
  
  # convert both to long form and combine
  lga_long <- lga %>%
    left_join(
      lga_infectious
    ) %>%
    # remove empty entries
    filter(
      infections > 0 | infectiousness > 0
    ) %>%
    # add LGA codes
    left_join(
      weights_tbl %>%
        select(lga, lga_code) %>%
        filter(!duplicated(.))
    ) %>%
    mutate(
      state = lga_to_state(lga)
    ) %>%
    arrange(state, lga, date)
  
  lga_long
  
}
