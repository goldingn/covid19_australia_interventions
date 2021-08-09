format_nsw_vaccination_data <- function (
  file, sheet,
  skip_rows = 4,
  min_date = as.Date("2021-06-26"),
  age_names = c("15-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
  vaccines = c("AstraZeneca", "Pfizer"),
  deliverers = c("NSW Health","GP", "Commercial", "Other")
) {

  # read oin the sheet, and skip the bumpf at the top
  readxl::read_excel(
    path = file,
    sheet = sheet,
    skip = skip_rows
  ) %>%
    # keep the cumulative count up to this date
    rename(
      previous_dates = `...2`
    ) %>%
    # remove total columns on the RHS
    select(
      -starts_with("...")
    ) %>%
    # remove the first regional grouping label
    filter(
      Group != "Special Interest LGAs"
    ) %>%
    # work out what level of grouping each row indicates
    mutate(
      grouping_level = case_when(
        Group %in% age_names ~ "age",
        Group %in% vaccines ~ "vaccine",
        Group %in% deliverers ~ "deliverer",
        TRUE ~ "lga" 
      ),
      .after = Group
    ) %>%
    # remove other two regional grouping labels (need to do it here as they duplicate the LGA names)
    filter(
      !(duplicated(.) & grouping_level == "lga")
    ) %>%
    # unpack the grouping levels
    mutate(
      lga = case_when(
        grouping_level == "lga" ~ Group,
        TRUE ~ NA_character_
      ),
      deliverer = case_when(
        grouping_level == "deliverer" ~ Group,
        TRUE ~ NA_character_
      ),
      vaccine = case_when(
        grouping_level == "vaccine" ~ Group,
        TRUE ~ NA_character_
      ),
      age = case_when(
        grouping_level == "age" ~ Group,
        TRUE ~ NA_character_
      ),
      .after = Group
    ) %>%
    # fill down to label each age-specific coverafge with the other groupings
    fill(
      c(lga, deliverer, vaccine, age)
    ) %>%
    # throw out grouping totals and extraneous columns
    filter(
      grouping_level == "age"
    ) %>%
    select(
      -Group,
      -grouping_level
    ) %>%
    # convert to long format
    pivot_longer(
      cols = -c(lga, deliverer, vaccine, age),
      values_to = "count",
      names_to = "date"
    ) %>%
    # rename the date columns
    mutate(
      date = case_when(
        date == "previous_dates" ~ min_date - 1,
        # stringr::str_starts(date, "...") ~ NA,
        TRUE ~ as.Date(
          suppressWarnings(
            as.numeric(
              date
            )
            ),
          origin = "1899-12-30"
        )
      )
    ) %>%
    # convert to cumulative vaccinations
    group_by(
      lga, deliverer, vaccine, age
    ) %>%
    arrange(
      lga, deliverer, vaccine, age, date
    ) %>%
    mutate(
      cumulative_count = cumsum(count)
    ) %>%
    # collapse the over 80 age group
    mutate(
      age = case_when(
        age == "80-89" ~ "80+",
        age == "90+" ~ "80+",
        TRUE ~ age
      )
    ) %>%
    group_by(
      lga, vaccine, age, date
    ) %>%
    summarise(
      across(
        c(count, cumulative_count),
        sum
      ),
      .groups = "drop"
    ) %>%
    select(
      -count
    )
  
}
