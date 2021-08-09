# get a (mean and se) number of total contacts at baseline that is comparable to
# the numbers in Freya's survey from Prem/polymod (similar survey methodology)
baseline_total_contacts <- function() {
  
  # load Prem contact matrix for Australia
  f <- "data/contacts/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx"
  all_aus <- readxl::read_xlsx(
    path = f,
    sheet = "Australia"
  ) %>%
    as.matrix()
  
  # load Australian population data
  pop <- read_csv(
    file = "data/contacts/ERP_QUARTERLY_20052020195358149.csv",
    col_types = cols(
      MEASURE = col_double(),
      Measure = col_character(),
      STATE = col_double(),
      State = col_character(),
      SEX_ABS = col_double(),
      Sex = col_character(),
      AGE = col_character(),
      Age = col_character(),
      FREQUENCY = col_character(),
      Frequency = col_character(),
      TIME = col_character(),
      Time = col_character(),
      Value = col_double(),
      `Flag Codes` = col_logical(),
      Flags = col_logical()
    )
  ) %>%
    filter(
      Measure == "Estimated Resident Population",
      Sex == "Persons",
      State == "Australia",
      Time == "Sep-2019",
      Age != "All ages"
    ) %>%
    select(age = Age, pop = Value) %>%
    mutate(age = as.numeric(age)) %>%
    arrange(age) %>%
    mutate(
      age_bin = cut(age, seq(0, 100, by = 5), include.lowest = TRUE, right = FALSE)
    ) %>%
    group_by(age_bin) %>%
    summarise(
      min = min(age),
      max = max(age),
      pop = sum(pop)
    )
  
  # get the age-binned population represented by the survey (of adults)
  survey_pop <- pop %>%
    group_by(age_bin) %>%
    mutate(
      fraction = mean((min:max) >= 18),
      weighted_pop = pop * fraction
    ) %>%
    ungroup() %>%
    filter(min < 80)
  
  mean_wt_aus <- weighted_mean(colSums(all_aus), survey_pop$weighted_pop)
  
  # get standard error of the total number of contacts in the UK arm of polymod,
  # as a measure of prior uncertainty in the Australian estimate of total
  # contacts
  download.file("https://raw.githubusercontent.com/goldingn/comix_covid-19-first_wave/master/data/polymod_contacts_part.rds",
                (f <- tempfile()), method='wget')
  
  standard_error_uk <- readRDS(f) %>%
    filter(part_age_group != "[0,18)") %>%
    group_by(part_id) %>%
    summarise(
      contacts = n_distinct(cont_id)
    ) %>%
    ungroup() %>%
    summarise(
      se = sd(contacts) / sqrt(n())
    ) %>%
    pull(se)
  
  tibble::tibble(
    mean = mean_wt_aus,
    se = standard_error_uk
  )
  
}
