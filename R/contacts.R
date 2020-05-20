# estimate the baseline numbers and durations of contacts with household members
# and non-household members in Australia using data from Rolls et al.

source("R/functions.R")


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

baseline_total_contacts <- weighted_mean(colSums(all_aus), survey_pop$weighted_pop)

# get the numbers of household contacts and distribution among types of
# locations from Rolls et al.

# load data on encounters of individuals and format
individuals <- read_csv(
  file = "data/contacts/covid_full.csv",
  col_types = cols(
    participant_id = col_double(),
    contact_id = col_character(),
    contact_duration = col_double(),
    location_duration = col_double(),
    hh_member = col_double(),
    location_code = col_double(),
    location_type = col_character(),
    weight = col_double()
  ),
  na = c("NULL", "", "NA")
) %>%
  select(-location_duration, -location_code) %>%
  mutate(location = case_when(
    location_type == "Home" ~ "home",
    location_type == "Retail and hospitality (bars, cafes, shops, hair dressing, etc.)" ~ "retail",
    location_type == "Public spaces (parks, streets, stations, airports etc.)" ~ "public",
    location_type == "Public Transport (train, tram, bus or taxi)" ~ "transit",
    location_type == "Work" ~ "work",
    TRUE ~ "other",
  ))

# convert encounters to numbers and total durations of unique contacts in each
# locationand contact type
contact_data <- individuals %>%
  # this encounter as a proportion of all encounters with this contact
  group_by(participant_id, contact_id) %>%
  mutate(proportion = 1 / n()) %>%
  # sum proportions and durations in each location, using the average duration with that contact where it's missing
  group_by(participant_id, contact_id, hh_member, location, weight) %>%
  summarise(
    proportion = sum(proportion),
    contact_duration = mean(contact_duration, na.rm = TRUE) / n(),
  ) %>%
  # count (proportional) unique contacts and average durations in household/non
  # household in each location for each participant
  group_by(participant_id, hh_member, location, weight) %>%
  summarise(
    contacts = sum(proportion),
    contact_duration = mean(contact_duration, na.rm = TRUE)
  ) %>%
  # convert duration to hours
  mutate(contact_duration = contact_duration / (60)) %>%
  ungroup() %>%
  mutate(
    hh_member = ifelse(hh_member == 1,
                       "household",
                       "non_household")
  ) %>%
  group_by(weight) %>%
  # expand out to include 0s for different categories, to averages are unbiased
  complete(participant_id, hh_member, location, fill = list(contacts = 0)) %>%
  arrange(participant_id, hh_member, location)

# get the average number and duration contacts by household/non-household
baseline_contact_params <- contact_data %>%
  group_by(participant_id, hh_member, weight) %>%
  summarise(contacts = round(sum(contacts)),
            contact_duration = mean(contact_duration, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(hh_member) %>%
  summarise(
    mean_contacts = weighted_mean(
      contacts,
      w = weight,
      na.rm = TRUE
    ),
    se_contacts = weighted_se(
      contacts,
      mean_contacts,
      w = weight,
      na.rm = TRUE
    ),
    mean_duration = weighted_mean(
      contact_duration,
      w = weight,
      na.rm = TRUE
    ),
    se_duration = weighted_se(
      contact_duration,
      mean_duration,
      w = weight,
      na.rm = TRUE
    )
  )

# compute fractions of non-household (unique) contacts
# in each location 
location_weights <- contact_data %>%
  filter(hh_member == "non_household") %>%
  group_by(participant_id, location, weight) %>%
  summarise(contacts = sum(contacts)) %>%
  group_by(location) %>%
  summarise(
    mean_contacts = weighted_mean(
      contacts,
      w = weight
    )
  ) %>%
  mutate(
    proportion_contacts = mean_contacts / sum(mean_contacts)
  )


# saveRDS(baseline_contact_params,
#         "outputs/baseline_contact_params.RDS")

# find a prior over logit(p) that corresponds to the prior over R0, at the mean
# values of the baseline contact data
# R0 = HC0(1 - p ^ HD0) + OC0(1 - p ^ OD0)

HC0 <- baseline_contact_params$mean_contacts[1]
OC0 <- baseline_contact_params$mean_contacts[2]
HD0 <- baseline_contact_params$mean_duration[1]
OD0 <- baseline_contact_params$mean_duration[2]

# get the infectious period in hours
si_pmf <- serial_interval_probability(0:100, fixed = TRUE)
si_cdf <- cumsum(si_pmf)
infectious_days <- which(si_cdf >= 0.95)[1]
infectious_period <- infectious_days * 24

transform <- function(free) {
  list(meanlogit = free[1],
       sdlogit = exp(free[2]))
}

R0 <- function(logit_p) {
  p <- plogis(logit_p)
  daily_infections <- HC0 * (1 - p ^ HD0) + OC0 * (1 - p ^ OD0)
  infectious_period * daily_infections
}

R0_params <- function(logit_p_params) {
  logit_p_draws <- rnorm(1e5, logit_p_params$meanlogit, logit_p_params$sdlogit)
  R0_draws <- vapply(logit_p_draws, R0, FUN.VALUE = numeric(1))
  log_R0_draws <- log(R0_draws)
  list(meanlog = mean(log_R0_draws),
       sdlog = sd(log_R0_draws))
}

obj <- function(free) {
  logit_p_params <- transform(free)
  R0_params <- R0_params(logit_p_params)
  R0_expected <- R0_prior()
  (R0_expected$meanlog - R0_params$meanlog) ^ 2 + sqrt((R0_expected$sdlog - R0_params$sdlog) ^ 2)
}

set.seed(2020-05-18)
o <- optim(c(5, -2), fn = obj, method = "BFGS")
logit_p_params <- transform(o$par)
# R0_params(logit_p_params)
# R0_prior()
# summary(rlnorm(1e4, R0_params(logit_p_params)[[1]], R0_params(logit_p_params)[[2]]))
# summary(rlnorm(1e4, R0_prior()[[1]], R0_prior()[[2]]))

# # infection probability per hour of contact
# summary(1 - plogis(rnorm(1e4, logit_p_params$meanlogit, logit_p_params$sdlogit)))
saveRDS(logit_p_params,
        "outputs/logit_p_params.RDS")

# model OC_t using the Rolls et al. baseline by location, Freya's survey
# results, and the trends in time change for google locations from the mobility
# model.




# get output for HD_t from HD_0 and h_t

# In the Reff model, input HD_t, OC_t, HC_0, OD_0, duration of infectiousness,
# prior on p, and model using d_t



# to do:
# - compute change in time spent at home from Google residential in model
# - add model for change in non-houseehold contacts (use parameter for OC_0 and
#   add Freya's contact survey as observations?)
# - implement R(t) model for each state