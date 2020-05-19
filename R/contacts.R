# estimate the baseline numbers and durations of contacts with household members
# and non-household members in Australia using data from Rolls et al.

source("R/functions.R")

rolls <- read_csv(
  file = "data/contacts/rolls_et_al.csv",
  col_types = cols(
    participant_id = col_double(),
    contact_id = col_double(),
    contact_duration = col_double(),
    location_duration = col_double(),
    hh_member = col_double(),
    weight = col_double()
  ),
  na = c("NULL", "", "NA")
)

contact_data <- rolls %>%
  # sum durations for each contact
  group_by(participant_id, contact_id, hh_member, weight) %>%
  summarise(
    contact_duration = sum(contact_duration),
    location_duration = sum(location_duration)
  ) %>%
  # count contacts and average durations in household/non household for each
  # participant
  group_by(participant_id, hh_member, weight) %>%
  summarise(
    contacts = n_distinct(contact_id),
    contact_duration = mean(contact_duration, na.rm = TRUE)
  ) %>%
  # convert duration to hours
  mutate(contact_duration = contact_duration / (60)) %>%
  ungroup() %>%
  mutate(
    hh_member = ifelse(hh_member == 1,
                       "household",
                       "non_household")
  )
  
# compute weighted mean and standard error of numbers and average durations of contacts,
# with household and non-household members
baseline_contact_params <- contact_data %>%
  group_by(hh_member) %>%
  summarise(
    mean_contacts = weighted_mean(
      contacts,
      w = weight
    ),
    se_mean_contacts = weighted_se(
      contacts,
      mean_contacts,
      w = weight
    ),
    mean_avg_duration = weighted_mean(
      contact_duration,
      w = weight,
      na.rm = TRUE
    ),
    se_avg_duration = weighted_se(
      contact_duration,
      mean_avg_duration,
      w = weight,
      na.rm = TRUE
    )
  )

saveRDS(baseline_contact_params,
        "outputs/baseline_contact_params.RDS")

# find a prior over logit(p) that corresponds to the prior over R0, at the mean
# values of the baseline contact data
# R0 = HC0(1 - p ^ HD0) + OC0(1 - p ^ OD0)

HC0 <- baseline_contact_params$mean_contacts[1]
OC0 <- baseline_contact_params$mean_contacts[2]
HD0 <- baseline_contact_params$mean_avg_duration[1]
OD0 <- baseline_contact_params$mean_avg_duration[2]

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
