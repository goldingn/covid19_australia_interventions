# omicron TP

# this script needs serious tidying up but the result is fine


source("R/functions.R")

fitted_model <- readRDS("outputs/fitted_reff_model.RDS")


gas <- which(
  sapply(
    sapply(
      fitted_model$greta_arrays$distancing_effect,
      class
    ),
    function(x){"greta_array" %in% x}
  )
)

vector_list <- lapply(fitted_model$greta_arrays$distancing_effect[gas], c)

# simulate from posterior for these quantities of interest
args <- c(vector_list, list(values = fitted_model$draws, nsim = 2000))


contact_sims <- do.call(calculate, args)

#sim_medians <- lapply(contact_sims, median)

sim_medians <- lapply(
  contact_sims,
  FUN = function(x){
    apply(
      x,
      MARGIN = 2,
      FUN = median
    )
  }
)

# original model


voc_mixture <- match.arg(voc_mixture)

# informative priors on variables for contacts at t = 0 (Hx = household, Ox =
# non-household, Tx = total, xC = contacts. xD = duration)
baseline_contact_params <- baseline_contact_parameters(gi_cdf)


prop_var <- prop_variant(dates = dates)
prop_alpha <- prop_var$prop_alpha
prop_delta <- prop_var$prop_delta
prop_wt    <- prop_var$prop_wt

if(voc_mixture == "alpha") {
  prop_alpha <- prop_alpha * 0 + 1
  prop_delta <- prop_wt <- prop_delta * 0
}

if(voc_mixture == "delta") {
  prop_delta <- prop_delta * 0 + 1
  prop_alpha <- prop_wt <- prop_alpha * 0
}

if(voc_mixture == "wt") {
  prop_wt <- prop_wt * 0 + 1
  prop_alpha <- prop_alpha * 0 
  prop_delta <- prop_delta * 0
}


# prior on the probability of *not* transmitting, per hour of contact
# (define to match moments of R0 prior)
logit_p_params <- logit_p_prior(baseline_contact_params, gi_cdf)
logit_p <- normal(logit_p_params$meanlogit, logit_p_params$sdlogit)
p <- ilogit(logit_p)

phi_alpha       <- normal(1.454, 0.055, truncation = c(0, Inf))
phi_delta_alpha <- normal(1.421, 0.033, truncation = c(0, Inf))

phi_delta <- phi_alpha * phi_delta_alpha

phi_star <- prop_wt * 1 + prop_alpha * phi_alpha + prop_delta * phi_delta

p_star <- p ^ phi_star


infectious_days <- infectious_period(gi_cdf)


# HC, HD_0, OD_0, ID: baseline household contacts and contact durations from Aus
# model
baseline_contact_params <- baseline_contact_parameters(gi_cdf)

HC_0 <- normal(baseline_contact_params$mean_contacts[1],
               baseline_contact_params$se_contacts[1],
               truncation = c(0, Inf))
HD_0 <- normal(baseline_contact_params$mean_duration[1],
               baseline_contact_params$se_duration[1],
               truncation = c(0, Inf))
OD_0 <- normal(baseline_contact_params$mean_duration[2],
               baseline_contact_params$se_duration[2],
               truncation = c(0, Inf))

# get HD_t in each state
h_t <- h_t_state(dates)
HD_t <- HD_0 * h_t

# trends in non-household contacts in each state over time
OC_t_state <- trends_date_state(
  "outputs/macrodistancing_trends.RDS",
  dates
)
OC_0 <- OC_t_state[1, 1]

# model gamma_t: reduction in duration and transmission probability of
# non-household contacts over time, per state

# load probability of microdistancing and divide by the maximum value to get
# an index of per-contact transmission probability
microdistancing_prob <- trends_date_state(
  "outputs/microdistancing_trends.RDS",
  dates
)
d_t_state <- microdistancing_prob / max(microdistancing_prob)

beta <- uniform(0, 1)
gamma_t_state <- 1 - beta * d_t_state

# compute component of R_eff for local cases
household_infections <- HC_0 * (1 - p_star ^ HD_t)
non_household_infections <- OC_t_state * gamma_t_state *
  infectious_days * (1 - p_star ^ OD_0)
R_t <- household_infections + non_household_infections

#

HC_0 <- sim_medians$HC_0
HD_0 <- sim_medians$HD_0
h_t <- h_t_state(fitted_model$greta_arrays$distancing_effect$dates)
HD_t <- HD_0 * h_t
OD_0 <- sim_medians$OD_0
p <- sim_medians$p
phi_delta <- sim_medians$phi_delta

p_star_delta <- p ^ phi_delta

OC_t_state <- trends_date_state(
  "outputs/macrodistancing_trends.RDS",
  fitted_model$greta_arrays$distancing_effect$dates
)

microdistancing_prob <- trends_date_state(
  "outputs/microdistancing_trends.RDS",
  fitted_model$greta_arrays$distancing_effect$dates
)
d_t_state <- microdistancing_prob / max(microdistancing_prob)

beta <- sim_medians$beta
gamma_t_state <- 1 - beta * d_t_state


infectious_days <- infectious_period(gi_cdf)

R_t_o <- 6.8
R_t_d <- 8

HD_t <- mean(HD_t[1,])

OC_t_state <- mean(OC_t_state[1,])

# 
R_t <- household_infections + non_household_infections

R_t <- HC_0 * (1 - p_star ^ HD_t) +
  OC_t_state * gamma_t_state *
  infectious_days * (1 - p_star ^ OD_0)

R_t <- HC_0 - HC_0 * p_star ^ HD_t + OC_t_state * gamma_t_state * infectious_days - OC_t_state * gamma_t_state * infectious_days * p_star ^ OD_0

HC_0 * p_star ^ HD_t + OC_t_state * gamma_t_state * infectious_days * p_star ^ OD_0 <- HC_0 + OC_t_state * gamma_t_state * infectious_days - R_t

R_t <- HC_0 - HC_0 * p_star ^ HD_t + OC_t_state * infectious_days - OC_t_state * infectious_days * p_star ^ OD_0

HC <- HC_0
HD <- HD_0
OC <- OC_t_state
OD <- OD_0

Rt <- function(p_star, HC, HD, OC, OD) {
  household <- HC * (1 - p_star ^ HD) 
  non_household <- OC * (1 - p_star ^ OD) 
  household + non_household
}

objective <- function(p_star_omicron) {
  R0_delta <- Rt(p_star_delta, HC, HD, OC, OD)
  R0_omicron <- Rt(p_star_omicron, HC, HD, OC, OD)
  ratio <- R0_omicron / R0_delta
  (ratio - 0.85) ^ 2
}

p_star_delta <- 0.1
HC <- 3
HD <- 0.5
OC <- 10
OD <- 0.1

o <- optimise(objective, interval = c(0, 1))
p_star_omicron <- o$minimum

phi_omicron <- log(p_star_omicron) / log(p)
# 1.64473
