#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param file
#' @param postcode
#' @return
#' @author Nicholas Tierney
#' @export
create_abs_lga_weights <- function(file =
                                   "~/not_synced/CA_POSTCODE_2018_LGA_2018.xlsx",
                                   postcode = postcodes) {

    weights_tbl <- read_xlsx(
      file,
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
          postcode = postcode,
        )
      ) %>%
      # assign unrecognised/unknown/overseas postcodes to a separate class
      mutate(
        lga = replace_na(lga, "other"),
        weight = replace_na(weight, 1)
      )

}
