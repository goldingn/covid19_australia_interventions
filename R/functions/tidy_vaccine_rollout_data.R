#' Clean and tidy vaccine rollout data
#'
#' @param path file path
#'
#' @return tidied rollout data.
#'
#' @examples
#' \dontrun{
#' tidy_vaccine_rollout_data("path/to/data")
#' }
tidy_vaccine_rollout_data <- function(vacc_path) {

  # find the names of the sheets
  vacc_sheet_names <- excel_sheets(vacc_path)

  # read in one row at a time
  read_excel_slice_names <- function(n = 1) {
    read_excel(
      path = vacc_path,
      sheet = "3. Age x Brand x Rollout Week",
      skip = n,
      n_max = n - 1,
      .name_repair = make_clean_names,
    ) %>% names()
  }

  # lop off the first 1-2 names as we want to construct new column names
  # which contain all the information as the multiple headers, but in one row
  row_1_names <- read_excel_slice_names(1) %>% vec_slice(-1)
  row_2_names <- read_excel_slice_names(2) %>% vec_slice(-c(1:2))
  row_3_names <- read_excel_slice_names(3) %>% vec_slice(-c(1:2))

  # so eventually, we'll have column names something like:
  # week_1__qld__individuals_received_dose_1"
  # and we can then pivot that into long format, and separate out those
  # parts into columns, "week", "state", and "dose"

  # clean up the names of these rows, removing extra repeated content
  row_1_names_tidy <- str_sub(
    string = row_1_names,
    start = 1,
    end = 7
  ) %>%
    str_replace(
      pattern = "_1_",
      replacement = "_1"
    )

  row_2_names_tidy <- str_remove_all(
    string = row_2_names,
    pattern = "_[[:digit:]]+"
  ) %>%
    str_replace(
      pattern = "x",
      replacement = "unknown"
    )

  row_3_names_tidy <- str_extract(
    string = row_3_names,
    pattern = "individuals_received_dose_[1:2]"
  )

  # construct the new column names, which will look like:
  # week_1__qld__individuals_received_dose_1"
  # variables are separated by a doule _ so we can parse them out easily later
  new_col_names <- c(
    "vaccine_type",
    "age_group",
    glue("{row_1_names_tidy}__{row_2_names_tidy}__{row_3_names_tidy}")
  )

  # read in the excel sheet, skipping to the data, and providing our own names
  excel_raw <- read_excel(
    path = vacc_path,
    sheet = "3. Age x Brand x Rollout Week",
    skip = 4,
    col_names = new_col_names
  )

  # pivot the data into long format, then separate it out into our desired columns
  tidy_vacc_data <- excel_raw %>%
    pivot_longer(
      cols = -c(
        vaccine_type,
        age_group
      ),
      names_to = "name",
      values_to = "count",
      values_transform = list(count = as.character)
    ) %>%
    separate(
      col = name,
      into = c("week", "state", "dose"),
      sep = "__"
    ) %>%
    # remove suppressed data into a new column
    mutate(count_without_suppressed = parse_number(count)) %>%
    # convert weeks into numbers, adjusting week_1 to week 20
    # (which was week_1_week_20)
    mutate(
      week_number = parse_number(week),
      week_number = case_when(
        week_number == 1 ~ 20,
        TRUE ~ week_number
      ),
      # weeks are defined as follows:
      # "Monday - Sunday, starting on Sunday the 21st February 2021"
      # "Week 25 is a partial week."
      days_since_feb_21 = week_number * 7
    ) %>%
    mutate(
      date = as_date("2021-02-21") + days_since_feb_21,
      .after = week_number
    ) %>%
    # having an issue with `conflicted` and `greta`, so need to namespace
    # for the time being
    dplyr::select(
      -week,
      -days_since_feb_21
    ) %>% 
    mutate(
      dose = parse_number(dose),
      vaccine_type = case_when(
        vaccine_type == "COVID-19 Vaccine AstraZeneca" ~ "astra_zeneca",
        vaccine_type == "Pfizer Comirnaty" ~ "pfizer_comirnaty"
      )
    ) %>% 
    dplyr::select(
      date,
      week_number,
      age_group,
      vaccine_type,
      state,
      dose,
      count,
      everything()
    )

  tidy_vacc_data
}
