# run reff for past linelists
source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)
source("./objects_and_settings.R")

# list of linelist dates to run for
sync_nndss()

# those we want
linelist_dates <- seq(as.Date("2020-04-01"), Sys.Date(), by = 7)

# those that are available
dates_available <- linelist_date_times("~/not_synced/nndss/") %>%
  mutate(date = as.Date(date_time)) %>%
  pull(date)

keep <- linelist_dates %in% dates_available
linelist_dates <- rev(linelist_dates[keep])

for (i in seq_along(linelist_dates)) {
  
  # create output directory
  date <- linelist_dates[i]
  date_string <- as.character(date, format = "%Y%m%d")
  
  dir <- file.path("outputs/historic", date_string)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  
  print(date_string)
  
  linelist <- load_linelist(date)
  data <- reff_model_data(linelist)
  
  data$dates$linelist
  
  # save the key dates for Freya and David to read in, and tabulated local cases
  # data for the Robs
  write_reff_key_dates(data, dir)
  write_local_cases(data, dir)
  
  # define the model (and greta arrays) for Reff, and sample until convergence
  fitted_model <- fit_reff_model(data, max_tries = 0)
  
  # output Reff trajectory draws for Rob M
  write_reff_sims(fitted_model, dir, write_reff_1 = FALSE)
  
}
