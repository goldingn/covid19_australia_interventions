nsw_lga_populations <- function(
  file = "data/vaccinatinon/nsw/Coverage_data_July27/AIR Data 20210727 rates - v2.xlsx",
  sheet = "LGA Sex x Age Dose 1 rate"
) {
  read_excel(
    path = file,
    sheet = sheet,
    skip = 5
  ) %>%
    select(
      lga = `...1`,
      sex = `...2`,
      age = `...3`,
      population = `Population...5`
    ) %>%
    filter(
      sex == "Total",
      age != "15+",
      age != "All ages"
    ) %>%
    select(
      -sex
    ) %>%
    mutate(
      age = case_when(
        age == "80-84" ~ "80+",
        age == "85+" ~ "80+",
        TRUE ~ age
      )
    ) %>%
    group_by(
      lga, age
    ) %>%
    summarise(
      population = sum(population),
      .groups = "drop"
    )
}
