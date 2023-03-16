
source("R/functions.R")
## Vaccination effect ---------
# Vaccination effect calculations  --------

# find most recent data or specify date, check dir printed is sensible
get_quantium_data_dates()

dir <- get_quantium_data_dir()
dir

# check dir date is sensible and get date of data set
data_date <- sub(
  pattern = ".*\\/",
  replacement = "",
  x = dir
) %>%
  as.Date

data_date
data_date_save <- format(data_date, "%Y%m%d")
data_date_save


# reaad-in lookups
# object
lookups <- get_quantium_lookups(dir = dir)

# read in and label data
vaccine_raw <- read_quantium_vaccination_data()

# check scenarios and assign appropriate one for use
# currently only difference is % booster uptake (100, 80, 75)
# choose 75
unique(vaccine_raw$scenario)

scenario_to_use <- lookups$scenario$scenario[grep("Realistic", lookups$scenario$booster_uptake)]

# this may fail if scenario lookup table is not up to date so check this is TRUE or will cause failure later
# otherwise may need to check email for appropriate scenario number and assign manually
scenario_to_use %in% unique(vaccine_raw$scenario)
#scenario_to_use <- 141

scenario_to_use

# aggregate to state
vaccine_state <- aggregate_quantium_vaccination_data_to_state(vaccine_raw) %>%
  filter(scenario == scenario_to_use)

vaccine_state



state_population <- vaccine_state %>%
  filter(scenario == max(scenario)) %>%
  group_by(state) %>%
  summarise(
    population = sum(num_people, na.rm = TRUE),
    .groups = "drop"
  )

state_population_by_age_band <- vaccine_state %>%
  filter(scenario == max(scenario)) %>%
  group_by(age_band, state) %>%
  summarise(
    population = sum(num_people, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(state) %>%
  mutate(prop_age = population / sum(population))

saveRDS(
  object = vaccine_state,
  file = sprintf(
    "outputs/vaccine_state_%s.RDS",
    data_date_save
  )
)
