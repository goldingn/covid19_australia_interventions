source("R/functions.R")

get_quantium_lookups <- function(dir) {
  
  list(
    age = read_csv(
      sprintf(
        "%s/dim_age_band.csv",
        dir
      ),
      col_types = cols(
        age_band_id = col_double(),
        age_band = col_character()
      )
    ),
    product = read_csv(
      sprintf(
        "%s/dim_vaccine.csv",
        dir
      ),
      col_types = cols(
        vaccine = col_double(),
        name = col_character(),
        short_name = col_character()
      )
    ),
    date = read_csv(
      sprintf(
        "%s/dim_time.csv",
        dir
      ),
      col_types = cols(
        time = col_double(),
        week_starting = col_date("%d/%m/%Y")
      )
    ),
    scenario = read_csv(
      sprintf(
        "%s/dim_scenario.csv",
        dir
      ),
      col_types = cols(
        scenario = col_double(),
        `5-11 uptake curve` = col_character(),
        booster_shape = col_character(),
        booster_scale_by_age = col_character(),
        booster_uptake_terminal = col_double(),
        booster_uptake_months = col_double()
      )
    )
  )
  
}



read_quantium_vaccination_data <- function(
  date = NULL
){
  
  # get most recent forecast
  dir_dates <- list.dirs(
    path = "~/not_synced/vaccination/quantium_forecasts/",
    full.names = FALSE,
    recursive = FALSE
  ) %>%
    as.Date
  
  if (is.null(date)) {
    dir_index <- which.max(dir_dates)
  } else {
    dir_index <- which(dir.dates == date)
    if (length(dir_index) != 1){
      stop("Either no directory or too many directories match this date")
    }
  }
  
  dir <- list.dirs(
    path = "~/not_synced/vaccination/quantium_forecasts/",
    full.names = TRUE,
    recursive = FALSE
  )[dir_index]
  
  
  # load all vaccine data
  vaccines <- read_csv(
    file = sprintf(
      "%s/vaccines.csv",
      dir
    )
  )
  
  # load quantium lookup tables
  lookups <- get_quantium_lookups(dir = dir)

  # add on times, age bands, and products, and return
  vaccines %>%
    # join on the dates
    ungroup() %>%
    left_join(
      lookups$date,
      by = c(
        "time_dose_1" = "time"
      )
    ) %>%
    rename(
      date_dose_1 = week_starting
    ) %>%
    left_join(
      lookups$date,
      by = c(
        "time_dose_2" = "time"
      )
    ) %>%
    rename(
      date_dose_2 = week_starting
    ) %>%
    left_join(
      lookups$date,
      by = c(
        "time_booster" = "time"
      )
    ) %>%
    rename(
      date_booster = week_starting
    ) %>%
    select(
      -starts_with("time")
    ) %>%
    # join on the products
    left_join(
      lookups$product,
      by = "vaccine"
    ) %>%
    select(
      -vaccine,
      -short_name
    ) %>%
    rename(
      vaccine = name
    ) %>%
    left_join(
      lookups$product,
      by = c("vaccine_booster" = "vaccine")
    ) %>%
    select(
      -vaccine_booster,
      -short_name
    ) %>%
    rename(
      vaccine_booster = name
    ) %>%
    # join on the age bands and convert to a factor
    left_join(
      lookups$age,
      by = "age_band_id"
    ) %>%
    select(
      -age_band_id
    ) %>%
    mutate(
      age_band = factor(
        age_band,
        levels = sort_age_groups(unique(age_band))
      )
    )
  
  
}


  vaccine_data <- read_csv("~/not_synced/vaccination/2021-12-08-magenta7352-booster-uptake-scenarios/vaccines.csv")

dim_sa4 <- read_csv("~/not_synced/vaccination/2021-12-08-magenta7352-booster-uptake-scenarios/dim_sa4.csv") %>%
  mutate(
    state = as.factor(STE_NAME16)
  )
dim_vaccine <- read_csv("~/not_synced/vaccination/2021-12-08-magenta7352-booster-uptake-scenarios/dim_vaccine.csv") %>%
  
dim_time <- read_csv("~/not_synced/vaccination/2021-12-08-magenta7352-booster-uptake-scenarios/dim_time.csv")
dim_age <- read_csv("~/not_synced/vaccination/2021-12-08-magenta7352-booster-uptake-scenarios/dim_age_band.csv")
dim_scenario <- read_csv("~/not_synced/vaccination/2021-12-08-magenta7352-booster-uptake-scenarios/dim_scenario.csv")
