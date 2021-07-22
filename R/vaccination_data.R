
source("R/functions.R")

library(readxl)

vaccine_files_dates <- function(
  dir = "~/not_synced/vaccination/vaccination_data/",
  name_pattern = " Individuals by Dose Number, Dose Type, and Demographics.xlsx"
) {
  files <- list.files(
    path = "~/not_synced/vaccination/vaccination_data/",
    pattern = ".xlsx$",
    full.names = TRUE
  )
  
  dates <- gsub(
    pattern = " Individuals by Dose Number, Dose Type, and Demographics.xlsx",
    replacement = "",
    basename(files)
  ) %>%
    as.Date(format = "%Y.%m.%d")
  
  tibble::tibble(
    file = files,
    date = dates
  )
  
}



# file <- vaccine_files_dates() %>%
#   pull(file)
# 
# file <- file[1]

read_vaccine_data <- function(
  file
){
  
  data_date <- sub(
    pattern = " Individuals by Dose Number, Dose Type, and Demographics.xlsx",
    replacement = "",
    basename(file)
  ) %>%
    as.Date(format = "%Y.%m.%d")
  
  header <- readxl::read_xlsx(
    path = file,
    n_max = 2,
    col_names = FALSE,
    sheet = "Vaccine Brand Split"
  ) %>%
    t %>%
    as_tibble %>%
    tidyr::fill(V1, .direction = "down") %>%
    mutate(name = paste(V1, V2, sep = "_")) %>%
    pull(name)
  
  over_80 <- c(
    "80+",
    "80-84",
    "85+",
    "85-89",
    "90+",
    "90-94",
    "95+",
    "95-99",
    "100+"
  )
  
  readxl::read_xlsx(
    path = file,
    skip = 2,
    col_names = header,
    sheet = "Vaccine Brand Split",
    n_max = 37
  ) %>%
    tidyr::fill(
      `NA_Vaccine Name`,
      .direction = "down"
    ) %>%
    dplyr::rename(
      vaccine = `NA_Vaccine Name`,
      age_class = `Administered State_Age at Encounter Date - 5 Year`
    ) %>%
    pivot_longer(
      cols = -vaccine:-age_class,
      names_to = "name",
      values_to = "doses"
    ) %>%
    mutate(
      state = sub(
        pattern = "_.*",
        replacement = "",
        x = name
      ),
      dose_number = sub(
        pattern = ".* ", # splits in trailing space - could get caught if format changes
        replacement = "",
        x = name
      ) %>%
        as.integer
    ) %>%
    dplyr::select(state, age_class, vaccine, dose_number, doses) %>%
      filter(age_class != "Totals", state != "Totals") %>%
    mutate(
      vaccine = case_when(
        vaccine == "COVID-19 Vaccine AstraZeneca" ~ "az",
        TRUE ~ "pf"
      )
    ) %>%
    pivot_wider(
      names_from = dose_number,
      values_from = doses
    ) %>%
    mutate(
      `1` = `1` - `2`
    ) %>%
    pivot_longer(
      cols = `1`:`2`,
      names_to = "dose_number",
      values_to = "doses"
    ) %>%
    rowwise %>%
    mutate(
      age_class = case_when(
        any(age_class == over_80) ~ "80+",
        TRUE ~ age_class
      )
    ) %>%
    group_by(state, age_class, vaccine, dose_number) %>%
    summarise(
      doses = sum(doses)
    ) %>%
    ungroup %>%
    mutate(
      dose_number = as.integer(dose_number),
      data_date = data_date,
    )
  
}

# vaccine_data <- sapply(
#   X = vaccine_files_dates() %>%
#     pull(file),
#   FUN = read_vaccine_data,
#   simplify = FALSE
# ) %>%
#   bind_rows

age_lookup <- tibble::tribble(
  ~age_lower, ~age_upper, ~age_band_quantium, ~age_band_5y,
  0,         4, "0-9", "0-4",
  5,         9, "0-9", "5-9",
  10,        10, "10-19", "10-14",
  11,        11, "10-19", "10-14",
  12,        12, "10-19", "10-14",
  13,        13, "10-19", "10-14",
  14,        14, "10-19", "10-14",
  15,        15, "10-19", "15-19",
  16,        16, "10-19", "15-19",
  17,        17, "10-19", "15-19",
  18,        19, "10-19", "15-19",
  20,        24, "20-29", "20-24",
  25,        29, "20-29", "25-29",
  30,        34, "30-39", "30-34",
  35,        39, "30-39", "35-39",
  40,        44, "40-49", "40-44",
  45,        49, "40-49", "45-49",
  50,        54, "50-59", "50-54",
  55,        59, "50-59", "55-59",
  60,        64, "60-69", "60-64",
  65,        69, "60-69", "65-69",
  70,        74, "70-79", "70-74",
  75,        79, "70-79", "75-79",
  80,        84, "80+", "80+",
  85,        89, "80+", "80+",
  90,        94, "80+", "80+",
  95,        99, "80+", "80+",
  100,       999, "80+",  "80+"
)


pop_data <- read_csv(
  "data/vaccinatinon/2021-07-13-census-populations.csv",
  col_types = cols(
    ste_name16 = col_character(),
    sa3_name16 = col_character(),
    sa2_name16 = col_character(),
    mmm2019 = col_double(),
    age_lower = col_double(),
    age_upper = col_double(),
    is_indigenous = col_logical(),
    is_comorbidity = col_logical(),
    vaccine_segment = col_character(),
    population = col_double()
  )
)

age_pops_eligible_5y <- pop_data %>%
  mutate(
    eligible = as.integer(age_lower >= 16)
  ) %>%
  left_join(
    age_lookup,
    by = c("age_lower", "age_upper")
  ) %>%
  group_by(
    age_band_5y,
  ) %>%
  summarise(
    eligible_population = sum(population * eligible),
    total_population = sum(population),
    .groups = "drop"
  )


vaccine_data <- vaccine_files_dates() %>%
  pull(file) %>%
  magrittr::extract(1) %>%
  read_vaccine_data() %>%
  pivot_wider(
    names_from = c(vaccine, dose_number),
     values_from = doses
  ) %>%
  mutate(
    vaccinated = az_1 + az_2 + pf_1 + pf_2,
    across(
      c(az_1, az_2, pf_1, pf_2),
      .fns = c("frac" = ~ . / vaccinated),
      .names = "{.fn}_{.col}"
    )
  ) %>%
  left_join(
    age_pops_eligible_5y,
    by = c("age_class" = "age_band_5y")
  ) %>%
  mutate(
    eligible_coverage = vaccinated / eligible_population,
    coverage = vaccinated / total_population,
    average_efficacy_transmission = average_efficacy(
      efficacy_pf_2_dose = combine_efficacy(0.79, 0.65),
      efficacy_az_2_dose = combine_efficacy(0.60, 0.65),
      efficacy_pf_1_dose = combine_efficacy(0.30, 0.46),
      efficacy_az_1_dose = combine_efficacy(0.18, 0.48),
      proportion_pf_2_dose = frac_pf_2,
      proportion_az_2_dose = frac_az_2,
      proportion_pf_1_dose = frac_pf_1,
      proportion_az_1_dose = frac_az_1
    )
  )
  

