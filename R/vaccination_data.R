source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)
source("./objects_and_settings.R")

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
  
  tibble(
    file = files,
    date = dates
  )
  
}



file <- vaccine_files_dates() %>%
  pull(file)

file <- file[2]

read_vaccine_data <- function(
  file
){
  
  data_date <- sub(
    pattern = " Individuals by Dose Number, Dose Type, and Demographics.xlsx",
    replacement = "",
    basename(file)
  ) %>%
    as.Date(format = "%Y.%m.%d")
  
  header <- read_xlsx(
    path = file,
    n_max = 2,
    col_names = FALSE,
    sheet = "Vaccine Brand Split"
  ) %>%
    t %>%
    as_tibble %>%
    fill(V1, .direction = "down") %>%
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
  
  read_xlsx(
    path = file,
    skip = 2,
    col_names = header,
    sheet = "Vaccine Brand Split",
    n_max = 37
  ) %>%
    fill(
      `NA_Vaccine Name`,
      .direction = "down"
    ) %>%
    rename(
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
    select(state, age_class, vaccine, dose_number, doses) %>%
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

vaccine_data <- sapply(
  X = vaccine_files_dates() %>%
    pull(file),
  FUN = read_vaccine_data,
  simplify = FALSE
) %>%
  bind_rows
