library(readxl)

vax_files_dates <- function(
  dir = "~/not_synced/vaccination/vaccination_data/"
) {
  
  files <- list.files(
    path = dir,
    pattern = ".xl",
    full.names = TRUE
  )
  
  filenames <- list.files(
    path = dir,
    pattern = ".xl",
    full.names = FALSE
  )
  
  dates <- sapply(
    X = filenames,
    FUN = function(x){
      # this deals with the variable naming and format of files
      # but may need to be tweaked further if file names change again
      if (is.na(anytime::anydate(x))) {
        xsplit <- strsplit(x, split = "_")[[1]] %>%
          anytime::anydate(.)
        
        xsplit[!is.na(xsplit)]
      } else {
        anytime::anydate(x)
      }

    },
    USE.NAMES = FALSE
  ) %>% as.Date(origin = as.Date("1970-01-01"))
  
  tibble::tibble(
    file = files,
    date = dates
  )
  
}




read_vax_data <- function(
  file,
  date
){
  
 header <- readxl::read_excel(
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
  
  readxl::read_excel(
    path = file,
    skip = 2,
    col_names = header,
    sheet = "Vaccine Brand Split",
    n_max = 38
  ) %>%
    tidyr::fill(
      `NA_Vaccine Name`,
      .direction = "down"
    ) %>%
    dplyr::rename(
      vaccine = `NA_Vaccine Name`,
      age_class = 2 # need to fix this so works with either naming convention
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
      date = date,
    )

}

load_vax_data <- function(){
 filesdates  <- vaccine_files_dates()
 
 mapply(
   FUN = read_vax_data,
   file = filesdates$file,
   date = filesdates$date,
   SIMPLIFY = FALSE,
   USE.NAMES = FALSE
 ) %>%
   bind_rows
 
}

vdat <- load_vax_data()



immunity_lag_correction <- function(
  date,
  doses,
  dose_number
) {
  
  if (dose_number[1] == 1) {
    weeks_increase <- 2
    weeks_wait <- 1
  } else if (dose_number[1] == 2) {
    weeks_increase <- 2
    weeks_wait <- 0
  }
  
  # compute the current doses (by dose/vaccine)
  max_date <- which.max(date)
  latest_doses <- doses[max_date]
  
  # compute the diff of dosess for all dates to get the proportion of the
  # population added on/by that date
  new_doses <- diff(c(0, doses))
  
  # compute the ratio of the proportion added on each previous date to the
  # current doses (should sum to 1)
  date_weights <- new_doses / latest_doses
  
  # multiply each of those ratios by the relative effect based on the date difference¿
  week_diff <- as.numeric(date[max_date] - date) / 7
  relative_effect <- pmax(0, pmin(1, (week_diff - weeks_wait) / weeks_increase))
  
  # sum the ratios to get the correction multiplier
  correction <- sum(relative_effect * date_weights)
  
  if (is.na(correction)) {
    correction <- 0
  }
  
  correction
  
  
}


dose_data <- vdat %>%
  arrange(state, age_class, vaccine, dose_number, date) %>%
  group_by(state, age_class, vaccine, dose_number) %>% 
  mutate(
    correction = slider::pslide_dbl(
      .l = list(
        date,
        doses,
        dose_number
      ),
      .f = immunity_lag_correction,
      .before = Inf
    ) %>%
      unlist,
    effective_doses = correction * doses
  ) %>%
  ungroup %>%
  group_by(state, age_class, date) %>%
  mutate(
    any_vaccine = sum(doses),
    effective_any_vaccine = sum(effective_doses),
  ) %>%
  ungroup %>%
  mutate(
    fraction = doses / any_vaccine,
    effective_fraction = effective_doses / effective_any_vaccine
  ) %>%
  arrange(state, age_class, date) 

efficacy_data <- dose_data %>%
  pivot_wider(
    names_from = c(vaccine, dose_number),
    values_from = c(doses, correction, effective_doses, fraction, effective_fraction)
  )