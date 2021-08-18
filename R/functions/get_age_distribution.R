get_age_distribution <- function(
  final_age_bin = 85,
  by = 5,
  population_total = 25693000
) {
  
  # check the final age bin in sensible
  if (final_age_bin > 85) {
    stop(
      "No age-specific population data for ages greater than 85",
      call. = TRUE
    )
  }
  
  ages <- age_classes(
    final_age_bin = final_age_bin,
    by = by
  )
  
  # Age structure of the Australian population by year of age, up to 100+
  # This is a "standard" distribution data frame but old population size data
  # from 2001 hence is adjusted later
  # aust_population_standard <- read_xls(
  #   path = "data/vaccinatinon/abs_standard_age_31010DO003_200106.xls",
  #   sheet = "Table_1",
  #   skip = 6,
  #   col_names = c(
  #     "age",
  #     "pop"
  #   )
  # ) %>%
  #   filter(age != "Total", !is.na(age), age != "© Commonwealth of Australia 2013") %>%
  #   mutate(
  #     age = case_when(
  #       age == "100 and over" ~ "100",
  #       TRUE ~ age
  #     ) %>%
  #       as.integer,
  #   )
  
  # use 2020 population, as this better matches proportion 80+
  aust_population_2020 <- read_xls(
    path = "data/vaccinatinon/abs_population_2020.xls",
    sheet = "Table_8",
    range = cell_rows(c(223:328)),
    col_names = as.character(1:10)
  ) %>%
    select(1, 10) %>%
    rename(
      age = "1",
      pop = "10"
    ) %>%
    select(
      age,
      pop
    ) %>%
    mutate(
      age = case_when(
        age == "85-89" ~ "85",
        age == "90-94" ~ "85",
        age == "95-99" ~ "85",
        age == "100 and over" ~ "85",
        TRUE ~ age
      )
    ) %>%
    filter(
      !grepl("-", age)
    ) %>%
    group_by(
      age
    ) %>%
    summarise(
      pop = sum(pop),
      .groups = "drop"
    ) %>%
    mutate(
      age = as.integer(age)
    ) %>%
    arrange(age)
  
  # aggregate into age classes and return
  age_class_fractions <- aust_population_2020 %>%
    mutate(
      age_class = cut(
        age,
        breaks = c(ages$lower - 1, Inf),
        labels = ages$classes
      ),
      age_class = as.character(age_class),
      age_class = factor(age_class, levels= unique(age_class))
    ) %>%
    group_by(
      age_class
    ) %>%
    summarise(
      pop = sum(pop),
      .groups = "drop"
    ) %>%
    mutate(
      fraction = pop / sum(pop),
      pop = fraction * population_total
    )
  
  age_class_fractions
  
}
