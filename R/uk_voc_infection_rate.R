# Analysis of the potential increase in per-unit-contact-time infectiousness of
# the UK VOC 202012/1 relative to wildtype.

# We carried out an analysis to independently estimate the relative
# transmissability of the UK VOC strain of SARS-CoV-2 compared with UK wild-type
# strains, and to account for variability in relative transmissability between
# high-restriction and low-restriction scenarios. We sought to directly estimate
# the impact of the UK VOC strain on the probability of transmission to a
# contact per unit of contact time. A change to this parameter is consistent
# with the hypothesis that the increased case growth rates associated with UK
# VOC are due to increased viral shedding during infection. With an estimate of
# this parameter we can modify our estimates of transmission potential in
# Australia, whilst accounting for estimated changes to the fraction of contacts
# that are with household members, the duration of time spent in the household,
# and changes to microdistancing behaviours. We estimated this key parameter by
# adapting the mathematical model of household and non-household transmission
# that forms part of our methodology for estimating transmission potential in
# Australia, and fitting it to data from Public Health England on secondary
# attack rates among contacts for UK VOC and wild-type strains in 9 English
# regions.

# # Model for attack rates

# Our existing transmission potential model explicitly considers secondary
# attack rates among household members and non-household members, modelled as a
# function of the probability of transmission per unit contact time, the average
# duration of contacts with household and non-household members, and
# modification of the non-household attack rate. The latter is a combined effect
# of reductions in the per-unit-contact-time transmission probability and in the
# average duration of non-household contacts.

# We explicitly model the household secondary attack rate at time/location $i$
# as:

#   HSAR_i = 1 - (1 - p) ^ HD_i

# where $p$ is the probability of transmission per unit of contact time, and
# $HD_i$ is the average duration of household contacts at time and place $i$,
# summed over the full course of infection.

# We model the secondary attack rate for non-household members as:

#   OSAR_i = \gamma_i * (1 - (1 - p) ^ OD_0)

# where $OD_0$ is the average duration of non-household contacts per 24 hours at
# baseline (prior to the pandemic and restrictions), and $\gamma_i$ is the
# reduction in non-household secondary attack rates as a function of
# microdistancing behaviour.

# We infer the parameters $HD_i$ and $gamma_i$ from data on mobility and
# behavioural change as:

#   HD_i = HD_0 * h_i
# \gamma_i = 1 - \beta * d_i

# where $HD_0$ is the average duration of household contacts over the full
# infectious period at baseline, $h_i$ is proportional change in the amount of
# time spent in the household, inferred from the Google mobility metric 'Time at
# Residential', d_i is the degree of adherence of microdistancing behaviour,
# scaled to range from 0 at baseline to 1 at the peak of microdistancing, and
# $\beta$ is a free parameter controlling the impact of microdistancing on
# reducing non-household transmission that is fitted to Australian case data.

# # Effect of UK VOC

# We model the effect of the UK VOC on per-unit-contact time probability of
# transmission via a parameter for the power of the probability of *not
# transmitting* per unit of contact time:

#   p^* = 1 - (1 - p) ^ \phi

# where $p^*$ and $p$ are the per-unit-contact time probabilities of
# transmission for the VOC strain and wild type strains, respectively, and
# $\phi$ is a free parameter constrained to be positive that controlls the
# relative infectiousness. $\phi = 1$ would imply the two strains have the same
# transmissability. The aim of this analysis is to infer $\phi$ from UK attack
# rate data.

# # Fitting to UK attack rate data

# Public Health England's Technical Report 3 on the UK VOC 202012/01 Table 6
# reports numbers of contacts of cases with VOC and wild-type strains in 9
# English regions, and the number of those contacts that became cases, between
# 2020/09/20 and 2021/01/04:
# https://www.gov.uk/government/publications/investigation-of-novel-sars-cov-2-variant-variant-of-concern-20201201
# We fit a model that separately considers attack rates in each of these
# regions, using separate estimates of mobility, microdistancing, and
# macrodistancing in each.  By considering all 9 regions as independent
# observations (rather than aggegating the data for all of England) we increase
# statistical power, consider the effect of the strain at different levels of
# restrictions, and reduce the probability the high attack rates may be due to
# founder effects or confounding with outbreaks in specific settings.

# Unfortunately, these data are not provided disaggregated at a finer temporal resolution. Nor are the attack rate estimates dissagregated by whether or not the contacts were houshold members. We must therefore adapt our model to estimate an overall attack rate over contacts, and adjust it for non-random ascertainment of contacts in the PHE dataset.

# We can estimate the overall secondary attack rate for each region $SAR_i$ as a
# combination of household and non-household secondary attack rates weighted by
# $w_i$, the fraction of contacts that are household members:

#   SAR_i = w_i * HSAR_i + (1 - w_i) * OSAR_i
#   w_i = HC / (HC + OC_i * ID)

# where $HC$ is the average number of household contacts (assumed the same for
# each region), $OC_i$ is the average number of non-household contacts per 24
# hours, and ID is the average duration of infectiousness in days. Our model
# assumes that household contacts stay the same throughout the course of
# infection, but that there is a different set of non-household contacts on each
# day.

#  The overall secondary attack rates estimated by this model correspond to the
#  average number of contacts specified as $HC$ and $OC_i$. Whilst the number of
#  household contacts is likely to be consistent between analyses, the
#  operational contact definition used by the contact tracing teams that
#  provided the PHE data is likely to yield a smaller number of contacts than
#  the contact surveys used to estimate $OC_i$. Moreover, the number of contacts
#  will not be a random sample of the larger number of contacts, since
#  operational contact tracing will target those individuals with a greater risk
#  of transmission. The consequence of this is that observed attack rates are
#  biased-up. This will also affect estimates of the relative transmissability
#  of the VOC strain from these raw data - reducing the apparent
#  transmissability. We account for these issues by introducing a free parameter
#  $\psi$ to relate the 'true' and observed attack rates:

#    SAR_i ^ \psi

# # Full model

# We specified a Bayesian statistical model to estimate $\phi$ and the other
# parameters from UK attack rate data as follows:

#   C_i ~ Binomial(N_i, SAR_i ^ \psi)
#   C^*_i ~ Binomial(N^*_i, SAR^*_i ^ \psi)
# 
#   SAR_i = w_i * HSAR_i + (1 - w_i) * OSAR
#   SAR^*_i = w_i * HSAR^*_i + (1 - w_i) * OSAR^*
#   w_i = HC / (HC + OC_i * ID)
# 
#   HSAR_i <- 1 - (1 - p) ^ HD_i
#   HSAR^*_i <- 1 - (1 - p^*) ^ HD_i
# 
#   OSAR_i <- \gamma_i * (1 - (1 - p) ^ OD_0)
#   OSAR^*_i <- \gamma_i * (1 - (1 - p^*) ^ OD_0)
# 
#   HD_i = HD_0 * h_i
#   \gamma_i = 1 - \beta * d_i

# where N_i and C_i are the number of contacts, and the number of those contacts
# that became cases in each English region i, and all variables with superscript
# $*$ correspond to infection with the UK VOC strain, and those without
# correspond to wild type strains.

# The model was fitted by MCMC using the same algorithm and software as the
# model for R_effective. The model was run until there were at least 1000
# effective samples of each parameter. Convergence was assessed visually and by
# the potential scale reduction factor (less than 1.01 for all parameters).
# Calibration of the model was assessed by posterior predictive checks over each
# of $C_i$, $C^*_i$, and the empirical estimate of the ratio of attack rates
# between strains for each region: $\frac{C^*_i / N^*_i}{C_i / N_i}$, and
# indicated good fit.

# # Parameter values and priors

# When fitting the R effective model for Australia, the parameters $OC_i$
# (non-household contacts per 24 hours), $h_i$ (relative time spent at home),
# and $d_i$ (relative microdistancing effect) are all informed by bespoke
# statistical models tailored to the Australian situation and surveys carried
# out only in Australia. We developed equivalent estimates of these parameters
# for the UK from a range of other sources.

# To estimate $OC_i$ we used the macrodistancing model fitted to Australian
# contact survey data to predict the number of non-household contacts per days
# in each English region, based on the values of Google mobility metrics for
# those regions. Google mobility data were downloaded for each English county,
# aggregated up to compute the average value over each region, and then averaged
# for each region over the period over which attack rate data were collected.
# Predictions of the Australian contact model were visually compared with
# summary statistics of non-household contact rates from April to August 2020 as
# estimated by the UK's CoMix survey series and found to have good calibration.
# We used the aggregated estimate of change in Google's time at residential to
# inform $h_i$. To estimate $d_i$ we analysed data on adherence to the UK's 2m
# rule using data for each English region from you.gov surveys. We calculated
# the number of people responding that they had not broken the 2m rule ("come
# into physical contact with (within 2 meters / 6 feet)") in the past 7 days.
# This is analogous to the 1.5m rule question used to define our microdistancing
# metric in Australia. This time series was analysed using a Binomial
# Generalised Additive Model with a smooth on date, to estimate a timeseries of
# the metric for each region over the course of the pandemic. This timeseries
# was rescaled to have maximum value 1 and then averaged over the time period
# over which attack rate data were collected.

# The model comprised 6 parameters; 4 for which we have existing estimates ($p$,
# $HC$, $HD_0$, and $OD_0$) and 2 for which we do not ($\phi$ and $\psi$). We
# defined an informative prior for $p$ based on a normal approximation to the
# posterior for this parameter from the Australian R effective model. This
# assumes \emph{a priori} that the wild-type strains in the UK have equivalent
# infectiousness to the strains that have circulated in Australia, though the
# parameter can be amended by the UK attack rate model fitting procedure if this
# is inconsistent with the data. For $HC$, $HD_0$, and $OD_0$ we used the same
# priors as we use in fitting the Australian model of R effective - based on
# surveys of contact behaviour in Australian prior to the pandemic. The average
# number of household contacts in each English region as reported in the you.gov
# surveys agreed closely with this Australian prior for $HC$. We chose to use
# the Australian estimate rather than the UK estimates since the posterior
# estimate of $p$ was estimated contingent on this distribution. Both $\phi and
# $\psi$ must be positive and a value of 1 indicates no effect (of the strain or
# of bias in contact acquisition, respectively). We therefore specified
# minimally informative positive-truncated normal priors for both parameters,
# with mode ($\mu$ parameter of the normal distribution) of 1. For $\psi$ we set
# the $\sigma$ parameter of the normal distribution to 1 to allow a large range
# of values, and for $\phi$ we set it to 0.25. Prior predictive checks on the
# ratio of attack rates between VOC and wild-type strains
# ($\frac{SAR^*_i}{SAR_i}$) with this prior on $\phi$ confirmed that the prior
# was vague with respect to the relative transmissability of the VOC strain
# versus wild type. I.e. multiplicative increases in transmissability of the VOC
# strain estimated from other studies were within the bulk of the prior
# distribution, as were larger increases and decreases in transmissability.

# $\beta$ was fixed at the posterior mean as estimated from the Australian
# model. In the absence of a time series of attack rate data, it is not possible
# to estimate this parameter independently for the UK, and the value of the
# parameter is poorly statistically identified in this model due to potential
# confounding with other parameters - especially $\psi$. For this reason,
# uncertainty in $\beta$ was not considered in this analysis.

source("spartan/lib.R")
source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)


set.seed(2021-01-17)

# load data

# UK attack rate data:
# manually copying from Table 6 in Technical briefing
# update 9/02/2021 table 4 technical briefing 5
# https://www.gov.uk/government/publications/investigation-of-novel-sars-cov-2-variant-variant-of-concern-20201201

uk_attack <- tibble::tribble(
  ~region,                ~contacts_voc, ~cases_voc, ~contacts_wt, ~cases_wt, 
  "East Midlands",                  868,        100,         1734,       185,
  "East of England",               6801,        876,         2358,       243,
  "London",                       12883,       1624,         3464,       299,
  "North East",                    1104,        122,         1795,       178,
  "North West",                    3577,        542,         5121,       495,
  "South East",                    7503,        912,         2044,       167,
  "South West",                    1011,        131,         1459,       160,
  "West Midlands",                 2806,        392,         2891,       279,
  "Yorkshire and Humber",           949,        130,         3301,       329
)

# regions and dates of attack rate data
uk_data_regions <- uk_attack$region
uk_data_start_date <- as.Date("2020-11-30")
uk_data_end_date <- as.Date("2021-01-10") 


# lookup between UK counties and regions:
# https://geoportal.statistics.gov.uk/datasets/0fa948d8a59d4ba6a46dce9aa32f3513_0/data

county_region_lookup <- read_csv(
  "data/uk_voc/Ward_to_Local_Authority_District_to_County_to_Region_to_Country__December_2018__Lookup_in_United_Kingdom_.csv",
  col_types = cols(
    .default = col_character(),
    FID = col_double()
  )
) %>%
  select(
    sub_region_1 = CTY18NM,
    region = GOR10NM
  ) %>%
  filter(
    !is.na(sub_region_1) & !is.na(region)
  ) %>%
  mutate(
    sub_region_1 = case_when(
      sub_region_1 == "Inner London" ~ "Greater London",
      sub_region_1 == "Outer London" ~ "Greater London",
      TRUE ~ sub_region_1
    )
  ) %>%
  mutate(
    region = case_when(
      region == "Yorkshire and The Humber" ~ "Yorkshire and Humber",
      TRUE ~ region
    )
  ) %>%
  filter(
    !duplicated(.)
  )

# load Google mobility metrics for UK regions

# get link from: https://www.google.com/covid19/mobility/index.html
url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
uk_google_mobility <- readr::read_csv(
  url, 
  col_types = cols(
    country_region_code = col_character(),
    country_region = col_character(),
    sub_region_1 = col_character(),
    sub_region_2 = col_character(),
    date = col_date(format = "%Y-%m-%d"),
    retail_and_recreation_percent_change_from_baseline = col_double(),
    grocery_and_pharmacy_percent_change_from_baseline = col_double(),
    parks_percent_change_from_baseline = col_double(),
    transit_stations_percent_change_from_baseline = col_double(),
    workplaces_percent_change_from_baseline = col_double(),
    residential_percent_change_from_baseline = col_double(),
    census_fips_code = col_character()
  )
) %>%
  filter(
    country_region == "United Kingdom"
  ) %>%
  left_join(
    county_region_lookup,
    by = "sub_region_1"
  ) %>%
  filter(
    !is.na(region)
  ) %>%
  tidyr::pivot_longer(
    ends_with("_percent_change_from_baseline"),
    names_to = "category",
    values_to = "trend"
  ) %>%
  dplyr::select(
    county = sub_region_1,
    region = region,
    category = category,
    date = date,
    trend = trend
  ) %>%
  mutate(
    category = str_remove_all(category, "_percent_change_from_baseline"),
    category = str_replace_all(category, "_", " ")
  ) %>%
  group_by(
    date, category, region
  ) %>%
  summarise(
    trend = mean(trend, na.rm = TRUE)
  ) %>%
  ungroup()

# smooth through them in a similar way to the Aus mobility model  
uk_google_mobility_smoothed <- uk_google_mobility %>%
  group_by(
    region,
    category
  ) %>%
  # smooth out day-of-the-week effects
  mutate(
    trend = slider::slide_dbl(
      trend,
      mean,
      na.rm = TRUE,
      .before = 3,
      .after = 3
    )
  ) %>%
  # smooth across symptom onset delay distribution
  mutate(
    trend = slider::slide_dbl(
      trend,
      gaussian_smooth,
      na.rm = TRUE,
      sd = 2.8,
      .before = 5,
      .after = 5
    )
  ) %>%
  ungroup()

# (should ideally population-weight the average trend over counties)

uk_google_mobility_smoothed %>%
  filter(category == "workplaces") %>%
  ggplot() +
  aes(date, trend) +
  geom_line() +
  facet_wrap("region") +
  theme_minimal() +
  ylab("") +
  xlab("") +
  ggtitle("Percentage change in visits to workplaces in UK regions")

# extrapolate contact rates in the UK using the macro model fitted to Australian data

uk_location_change_trends <- uk_google_mobility_smoothed %>%
  mutate(
    category = case_when(
      category == "residential" ~ "home",
      category == "transit stations" ~ "transit",
      category == "parks" ~ "public",
      category == "workplaces" ~ "work",
      category == "retail and recreation" ~ "retail",
      TRUE ~ "other"
    )
  ) %>%
  filter(
    category != "other"
  ) %>%
  mutate(
    change = 1 + (trend / 100),
  ) %>%
  select(
    -trend
  ) %>%
  pivot_wider(
    names_from = category,
    values_from = change
  ) %>%
  mutate(
    state = region,
  ) %>%
  select(
    state,
    date,
    public,
    home,
    retail,
    transit,
    work
  )

fitted_macro_model <- readRDS("outputs/fitted_macro_model.RDS")
data <- fitted_macro_model$data
data$location_change_trends <- uk_location_change_trends
uk_predictions <- macrodistancing_model(data, fitted_macro_model$params)
OC_t_i <- uk_predictions$mean_daily_contacts

pred_sim <- calculate(c(OC_t_i), values = fitted_macro_model$draws, nsim = 1000)[[1]][, , 1]
quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))

# predicted trends in non-household contact rates
pred_trend <- uk_location_change_trends %>%
  select(date, state) %>%
  # add predictions
  mutate(mean = colMeans(pred_sim)) %>%
  bind_cols(as_tibble(quants))

# averages over the UK attack rate data period
OC_i <- pred_trend %>%
  select(date, state, mean) %>%
  filter(
    date >= uk_data_start_date &
      date <= uk_data_end_date
  ) %>%
  group_by(
    state
  ) %>%
  summarise(
    mean = mean(mean)
  ) %>%
  ungroup()

# plot these against CoMix data, as an external validation of the model
comix_contacts <- read_csv("data/uk_voc/comix_data.csv",
                           col_types = cols(
                             start_date = col_date(format = "%d/%m/%y"),
                             mean_contacts = col_double(),
                             lower_4 = col_double(),
                             upper_4 = col_double(),
                             truncation = col_double(),
                             region = col_character()
                           ))

pred_trend %>%
  filter(state == "London") %>%
  ggplot() +
  aes(date, mean) +
  geom_ribbon(
    aes(x = date, ymin = `5%`, ymax = `95%`),
    alpha = 0.3
  ) +
  geom_line() +
  geom_point(
    aes(
      start_date,
      mean_contacts,
      colour = truncation,
      shape = region
    ),
    data = comix_contacts %>%
      mutate(truncation = factor(truncation))
  ) +
  scale_color_discrete() +
  theme_minimal() +
  ylab("Non-household contacts per day") +
  xlab("") +
  ggtitle("Predicted macrodistancing trend for London",
          "with observed national average contact rates")

# parse you.gov surveys to get a micro proxy and average household size
# https://github.com/goldingn/covid-19-tracker/raw/master/data/united-kingdom.zip

yougov <- read_csv(
  "data/uk_voc/united-kingdom.csv",
  col_types = cols(
    .default = col_character(),
    RecordNo = col_double(),
    endtime = col_datetime(format = "%d/%m/%Y %H:%M"),
    qweek = col_character(),
    i1_health = col_double(),
    i2_health = col_double(),
    i7a_health = col_double(),
    household_size = col_double()
  )
) %>%
  mutate(
    date = as.Date(endtime),
    always_apart = i2_health == 0
  ) %>%
  select(
    date,
    region,
    household_size,
    always_apart
  )

# Note that reported household sizes are very slightly larger than those we
# assume based on Australian data. However we
yougov %>%
  group_by(region) %>%
  summarise(
    household_mean = mean(household_size, na.rm = TRUE)
  ) %>%
  filter(
    region %in% uk_data_regions
  )

# compute time series of reported adherence to 2m rule in UK regions
micro_data <- yougov %>%
  group_by(region, date) %>%
  summarise(
    respondents = n(),
    adhering = sum(always_apart)
  ) %>%
  mutate(
    not_adhering = respondents - adhering,
    date_num = as.numeric(date - min(date)),
    region = case_when(
      region == "Yorkshire and the Humber" ~ "Yorkshire and Humber",
      TRUE ~ region
    ),
    region = factor(region),
  )


micro_responses <- cbind(micro_data$adhering, micro_data$not_adhering)
micro_gam <- mgcv::gam(
  micro_responses ~ s(date_num) + s(date_num, by = region),
  family = stats::binomial,
  data = micro_data,
  gamma = 2
)

micro_pred <- expand_grid(
  region = sort(unique(micro_data$region)),
  date = seq(
    min(micro_data$date),
    max(micro_data$date),
    by = 1
  )
) %>%
  mutate(
    date_num = as.numeric(date - min(date)),
    region = factor(region)
  ) %>%
  mutate(
    pred = predict(
      micro_gam,
      newdata = .,
      type = "response"
    )
  )

micro_pred %>%
  ggplot() +
  aes(date, pred) +
  geom_line() +
  facet_wrap("region") +
  ylim(0, 1) +
  theme_minimal() +
  ylab("") +
  xlab("") +
  ggtitle("Proportion of UK respondents 'always' adhering to the 2m rule")

# get the micro effect relative to the peak, and summarise over the attack rate
# data period

d_i <- micro_pred %>%
  group_by(region) %>%
  mutate(
    peak = max(pred),
    effect = pred / peak
  ) %>%
  filter(
    date >= uk_data_start_date,
    date <= uk_data_end_date,
    region %in% uk_data_regions
  ) %>%
  summarise(
    micro_effect = mean(effect)
  )

# compute the average of the household metric over the attack rate data time
# period in each region
h_i <- uk_location_change_trends %>%
  filter(
    date >= uk_data_start_date &
      date <= uk_data_end_date
  ) %>%
  group_by(state) %>%
  summarise(
    home = mean(home)
  )

# base priors on p and beta on Reff model posteriors

fitted_reff_model <- readRDS("outputs/fitted_reff_model.RDS")

# approximate beta as half-normal centred at 1
beta_posterior <- calculate(
  fitted_reff_model$greta_arrays$distancing_effect$beta,
  values = fitted_reff_model$draws,
  nsim = 5000
)[[1]]

# beta_mu <- 1
# beta_sigma <- sqrt(sum((beta_posterior - 1) ^ 2) / length(beta_posterior))
# beta <- normal(beta_mu, beta_sigma, truncation = c(0, 1))
beta <- mean(beta_posterior)
# hist(beta_posterior, breaks = 20, xlim = c(0, 1))
# hist(calculate(beta, nsim = 5000)[[1]], breaks = 20, xlim = c(0, 1))

# approximate q (q = 1 - p) as a normal. In the Reff model we call the
# probability of not transmitting o, but here we call it q
q_posterior <- calculate(
  fitted_reff_model$greta_arrays$distancing_effect$p,
  values = fitted_reff_model$draws,
  nsim = 1000
)[[1]]

q <- normal(mean(q_posterior), sd(q_posterior), truncation = c(0, 1))
p <- 1 - q

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

# prior on the relative per-unit-contact-time infectiousness of the UK VOC strain
phi <- normal(1, 1, truncation = c(0, Inf))

# per-unit-contact-time infectiousness of the UK VOC strain
p_star <- 1 - (1 - p) ^ phi

# bias in sampling of contacts and cases in UK attack rate data (uniformly
# increases/decreases the observed attack rate fromt he hypothetical 'true'
# attack rate, where the true attack rate is defined such that the number of
# non-household contacts is that from the contact surveys). We could get a prior
# for this by comparing the numbers of contacts per case to the average numbers
# of non-household contacts, however the UK attack rate report does not clearly
# state the number of cases considered.

psi <- normal(1, 1, truncation = c(0, Inf))

infectious_days <- infectious_period(gi_cdf)

# overall microdistancing effect
gamma_i <- 1 - beta * d_i$micro_effect

# household and non-household secondary attack rates for the two strains
hsar_i <- 1 - (1 - p) ^ (HD_0 * h_i$home)
hsar_star_i <- 1 - (1 - p_star) ^ (HD_0 * h_i$home)
osar_i <- gamma_i * (1 - (1 - p) ^ OD_0)
osar_star_i <- gamma_i * (1 - (1 - p_star) ^ OD_0)

# proportion of contacts that are household contacts
w_i <- HC_0 / (HC_0 + OC_i$mean * infectious_days)

# overall secondary attack rates
sar_i <- w_i * hsar_i + (1 - w_i) * osar_i
sar_star_i <- w_i * hsar_star_i + (1 - w_i) * osar_star_i

# account for biased undersampling of contacts in the SAR data (e.g. sampling
# all household contacts but only a fraction of non-household contacts) which
# would cause the observed and expected attack rates to differ.
sar_observed_i <- sar_i ^ psi
sar_star_observed_i <- sar_star_i ^ psi

distribution(uk_attack$cases_wt) <- binomial(uk_attack$contacts_wt, sar_observed_i)
distribution(uk_attack$cases_voc) <- binomial(uk_attack$contacts_voc, sar_star_observed_i)

m <- model(phi, HC_0, HD_0, OD_0, p, psi)

draws <- mcmc(m, chains = 10)
draws <- extra_samples(draws, 2000)
convergence(draws)
bayesplot::mcmc_trace(draws)

# # calculate Reff for household, non-household, and overall for the two strains
# reff_household_wt <- HC_0 * hsar_wt_i
# reff_household_voc <- HC_0 * hsar_voc_i

# compute the ratio of Reff for household, non-household, and overall infections
r_household <- hsar_star_i / hsar_i
r_non_household <- osar_star_i / osar_i
r_overall <- mean(sar_star_i / sar_i)
r_overall_observed <- mean(sar_star_observed_i / sar_observed_i)

# simulate prior and posterior over the ratio of attack rates (and therefore of Reff)
r_sim_prior <- calculate(r_overall, nsim = 5000)[[1]][, 1, 1]
r_sim_posterior <- calculate(r_overall, values = draws, nsim = 5000)[[1]][, 1, 1]
r_observed_sim_prior <- calculate(r_overall_observed, nsim = 5000)[[1]][, 1, 1]
r_observed_sim_posterior <- calculate(r_overall_observed, values = draws, nsim = 5000)[[1]][, 1, 1]

par(mfrow = c(1, 2))
hist(r_sim_prior, breaks = 50, xlim = c(0, 3))
abline(v = 1, lty = 2)
hist(r_sim_posterior, breaks = 50, xlim = c(0, 3))
abline(v = 1, lty = 2)

mean(r_sim_prior)
mean(r_observed_sim_prior)

mean(r_sim_posterior)
quantile(r_sim_posterior, c(0.05, 0.95))
mean(r_observed_sim_posterior)
quantile(r_observed_sim_posterior, c(0.05, 0.95))

r_household_posterior <- calculate(r_household, values = draws, nsim = 5000)[[1]][, , 1]
r_non_household_posterior <- calculate(r_non_household, values = draws, nsim = 5000)[[1]][, , 1]

colMeans(r_household_posterior)
colMeans(r_non_household_posterior)

# compare priors and posteriors for all parameters

hist_prior_posterior <- function(greta_array, draws, nsim = 1000, ...)  {
  
  prior_sim <- calculate(greta_array, nsim = nsim)[[1]]
  posterior_sim <- calculate(greta_array, values = draws, nsim = nsim)[[1]]
  
  prior_sim <- c(prior_sim)
  posterior_sim <- c(posterior_sim)
  xlim <- range(c(prior_sim, posterior_sim))
  
  op <- par()
  on.exit(par(op))
  par(mfrow = c(1, 2))
  
  hist(c(prior_sim), xlim = xlim, ...)
  hist(c(posterior_sim), xlim = xlim, ...)
  
}



# compare priors and posteriors

# parameters we want to estimate
hist_prior_posterior(phi, draws)
hist_prior_posterior(psi, draws)

# parameters we provide informative priors for
hist_prior_posterior(HC_0, draws)
hist_prior_posterior(HD_0, draws)
hist_prior_posterior(OD_0, draws)
hist_prior_posterior(p, draws)

# derived parameters of interest
hist_prior_posterior(r_overall, draws)
hist_prior_posterior(r_household, draws)
hist_prior_posterior(r_non_household, draws)

# do posterior predictive checks to make sure these align with raw ratios from PHE data
cases_voc_ga <- binomial(uk_attack$contacts_voc, sar_star_observed_i)
cases_wt_ga <- binomial(uk_attack$contacts_wt, sar_observed_i)
cases_voc_sim <- calculate(cases_voc_ga, values = draws, nsim = 1000)[[1]][, , 1]
cases_wt_sim <- calculate(cases_wt_ga, values = draws, nsim = 1000)[[1]][, , 1]
bayesplot::ppc_ecdf_overlay(
  uk_attack$cases_voc,
  cases_voc_sim,
  discrete = TRUE
)
bayesplot::ppc_ecdf_overlay(
  uk_attack$cases_wt,
  cases_wt_sim,
  discrete = TRUE
)

# do posterior predictive check on the empirical ratio of attack rates
empirical_r_observed <- uk_attack %>%
  mutate(
    attack_rate_voc = cases_voc / contacts_voc,
    attack_rate_wt = cases_wt / contacts_wt,
    ratio = attack_rate_voc / attack_rate_wt
  ) %>%
  pull(ratio)

empirical_sar_voc_ga <- cases_voc_ga / uk_attack$contacts_voc
empirical_sar_wt_ga <- cases_wt_ga / uk_attack$contacts_wt
empirical_r_ga <- empirical_sar_voc_ga / empirical_sar_wt_ga
empirical_r_sim <- calculate(empirical_r_ga, values = draws, nsim = 1000)[[1]][, , 1]

bayesplot::ppc_ecdf_overlay(
  empirical_r_observed,
  empirical_r_sim,
  discrete = FALSE
)


# summarise the posterior of phi for use in counterfactuals
phi_sim <- calculate(phi, values = draws, nsim = 5000)[[1]][, 1, 1]
hist(phi_sim)     

# approximate as a normal distribution
mean(phi_sim)
#Update 2021-02-09
# 1.454
# Original
# 1.512
sd(phi_sim)
# Update 2021-02-09
# 0.051
# Original
# 0.084

# p_star_sim <- calculate(p_star, values = draws, nsim = 5000)[[1]][, 1, 1]
# summary(p_star_sim)
# 
# p_sim <- calculate(p, values = draws, nsim = 5000)[[1]][, 1, 1]
# summary(p_sim)

# summarise relative infectiousness in UK
r_draws <- calculate(
  r_overall,
  r_household,
  r_non_household,
  values = draws,
  nsim = 5000
)

lapply(r_draws,
       function(x) {
         x <- 100 * (x - 1)
         stats <- c(
           mean = mean(x),
           quantile(x, c(0.05, 0.5, 0.95))
         )
         round(stats, 2)
       }
)


#Update 2021-02-09
# $r_overall
# mean    5%   50%   95% 
# 40.39 33.45 40.29 47.94 
# 
# $r_household
# mean    5%   50%   95% 
# 36.89 30.49 36.77 43.95 
# 
# $r_non_household
# mean    5%   50%   95% 
# 43.75 36.21 43.69 51.73 

# Original
# $r_overall
# mean    5%   50%   95% 
# 43.55 32.34 43.39 55.38 
# 
# $r_household
# mean    5%   50%   95% 
# 39.14 29.20 39.01 49.53 
# 
# $r_non_household
# mean    5%   50%   95% 
# 49.02 36.08 48.77 62.64 