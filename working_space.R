load_air_data <- function(
  data_dir = "~/not_synced/vaccination/vaccination_data_with_booster/"
){
  
  do_dir <- file.path(data_dir, "dose_ordering") 
  un_dir <- file.path(data_dir, "unknowns")
  
  do_dates <- list.files(
    path = do_dir
  ) %>%
    sub(
      pattern = "DohertyTimeseriesExport_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "DoseOrdering_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "_.*",
      replacement = "",
      x = .
    ) %>%
    as.Date
  
  un_dates <- list.files(
    path = un_dir
  ) %>%
    sub(
      pattern = "DohertyTimeseriesExport_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "Unknowns_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "_.*",
      replacement = "",
      x = .
    ) %>%
    as.Date
  
  if(max(do_dates) != max(un_dates)){
    stop("Most recent dose ordering and unknowns files have different dates")
  }
  
  do_index <- which.max(do_dates)
  un_index <- which.max(un_dates)
  
  extraction_date <- do_dates[do_index]
  
  do_path <- list.files(
    path = do_dir,
    full.names = TRUE
  )[do_index]
  
  un_path <-list.files(
    path = un_dir,
    full.names = TRUE
  )[un_index]
  
  do_raw <- read_csv(
    file = do_path
  ) %>%
    rename(
      "state" = PATIENT_MEDICARE_STATE
    )
  
  un_raw <- read_csv(
    file = un_path
  ) %>%
    rename(
      "state" = PROVIDER_STATE
    )
  
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
  
  bind_rows(
    do_raw %>%
      filter(
        state != "Unknown",
        state != "UNK"
      ),
    un_raw
  ) %>%
    rename(
      "age_class" = CURRENT_AGE_GROUP,
      "week" = AS_OF_ENCOUNTER_WEEK,
      "date" = AS_OF_WEEK_COMMENCING,
      "dose1" = FIRST_DOSE,
      "dose2" = SECOND_DOSE,
      "dose3" = THIRD_DOSE,
      "count" = CUMULATIVE_UNIQUE_INDIVIDUALS_VACCINATED
    ) %>%
    mutate(
      date = as.Date(
        date,
        format = "%d/%m/%Y"
      ),
      age_class = case_when(
        age_class %in% over_80 ~ "80+",
        TRUE ~ age_class
      ),
      vaccine = case_when(
        !is.na(dose3) ~ dose3,
        !is.na(dose2) ~ dose2,
        TRUE ~ dose1
      ),
      # assuming here that moderna spikevax and pfizer comirnaty are equivalent
      # and that vaccination efficacy depends on most recent vaccine
      vaccine = case_when(
        vaccine == "COMIRN" ~ "pf",
        vaccine == "MODERN" ~ "pf",
        vaccine == "COVAST" ~ "az"
      ),
      # not accounting for effect of booster as yet, counting only as second dose
      dose_number = case_when(
        !is.na(dose3) ~ 2,
        !is.na(dose2) ~ 2,
        TRUE ~ 1
      ),
    )
  
}


tibble(
  date = rep(
    seq.Date(
      from = as.Date("2021-02-22"),
      to = as.Date("2021-03-29"),
      by = 7
    ),
    times = 2
  ),
  doses = c(10, 20, 20, 20, 10,  0,
            0,  0, 10, 20, 30, 40),
  dose_number = c(
    rep(1, 6),
    rep(2, 6)
  )
) %>%
  group_by(dose_number) %>%
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
  )
