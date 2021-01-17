# Analysis of the potential increase in per-unit-contact-time infectiousness of
# the UK VOC 202012/1 relative to wildtype.

# We want to infer the ratio of the per-unit-contact-time-transmission
# probabilities between wildtype and UK VOC strains of SARS-CoV-2. This would
# enable us to provide more reliable estimates of what Reff could be expected to
# be achieved in Australia under different lockdown scenarios.

# NHS track and trace provide data on secondary attack rates for the two
# strains, disaggregated by a range of ages and regions. It may be possible to
# calculate the ratio of per-unit-contact-time transmission probabilities from
# these data using external estimates of the duration and number of contacts
# (household and non-household) for UK regions during this period. However the
# UK attack rate estimates are not disaggregate by household vs non-household
# contacts, which poses a problem.
 
# We can adapt part of the Reff model to estimate overall attack rates in the
# UK, in order to parameterise the relative infectiousness of the VOC strain.

# Household secondary attack rate (HSAR):
#   HSAR = 1 - p ^ (HD_0 * h)
 
# where p is the per-unit-contact-time transmission probability, HD_0 is the
# baseline duration of household contacts over the whole infectious period, and
# h is proportional change in the amount of time spent at home (~~Google
# residential)
 
# Non-household secondary attack rate (OSAR):
#   OSAR = gamma * (1 - p ^ OD_0)
 
# where OD_0 is the average duration of non-household contacts per 24h at
# baseline, and gamma is the reduction in per-contact transmission rate for
# non-household contacts due to microdistancing behaviours

# The overall secondary attack rate (SAR) is a mixture of the two, weighted by
# the fraction of contacts that are household member versus not:
#   SAR = (HC * HSAR + OC * ID * OSAR) / (HC + OC * ID)
#   SAR = w * HSAR + (1 - w) * OSAR
#   w = HC / (HC + OC * ID)

# where ID is the average duration of the infectious period in days, HC is the
# number of household contacts (assumed to be constant throughout the infectious
# period), and OC is the number of non-household contacts per 24 hours. Note
# that we assume that non-household contacts are unique to each day. Though this
# only affects depletion of susceptible non-household contacts and violation of
# this assumption is unlikely to have an appreciable impact on estimates unless
# the per-contact transmission probability for non-household contacts is very
# high.
 
# We can fit this with a Bayesian statistical model over data for each UK region i
#   infected_WT_i ~ Binomial(contacts_WT_i, SAR_WT_i)
#   infected_VOC_i ~ Binomial(contacts_VOC_i, SAR_VOC_i)
#   SAR_WT_i = w_i * HSAR_WT_i + (1 - w_i) * OSAR_WT
#   SAR_VOC_i = w_i * HSAR_VOC_i + (1 - w_i) * OSAR_VOC
#   w_i = HC / (HC + OC_i * ID)
#   HSAR_WT_i = 1 - p_WT ^ (HD_0 * h_i)
#   HSAR_VOC_i = 1 - p_VOC ^ (HD_0 * h_i)
#   OSAR_WT = gamma_i * (1 - p_WT ^ OD_0)
#   OSAR_VOC = gamma_i * (1 - p_VOC ^ OD_0)
#   gamma_i = 1 - beta * d_i
#   p_VOC = 1 - (1 - p_WT) ^ phi

# where phi is the relative per-contact-time infectiousness of VOC
 
# data sources:

# infected_WT_i, contacts_WT_i, infected_VOC_i, contacts_VOC_i: NHS track and
# trace report:
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/950823/Variant_of_Concern_VOC_202012_01_Technical_Briefing_3_-_England.pdf
 
# h_i: Google residential metric for location i

# OC_i: Aus OC model applied to Google mobility metrics for UK location i

# HC, HD_0, OD_0, ID: baseline household contacts and contact durations from Aus
# model
 
# d_i: find a similar question to our 1.5 rule from the you.gov survey
# timeseries for UK (check trends of question for Aus against our micro), scale
# relative to maximum, and multiply by posterior mean of beta from the fitted
# Reff model https://github.com/YouGov-Data/covid-19-tracker/tree/master/data
 
# p_WT prior: as per posterior from Aus model
 
# r prior: as minimally-informative as possible, contrained to be positive, but
# not constrained to be greater than 1. E.g. N+(1, 10)

source("R/functions.R")

set.seed(2021-01-17)

# load data

# UK attack rate data:
# manually copying from Table 6 in Technical briefing 3
# https://www.gov.uk/government/publications/investigation-of-novel-sars-cov-2-variant-variant-of-concern-20201201

uk_attack <- tibble::tribble(
  ~region, ~contacts_voc, ~cases_voc, ~contacts_wt, ~cases_wt, 
  "East Midlands", 150, 15, 1008, 117,
  "East of England", 1869, 263, 1199, 153,
  "London", 3507, 505, 1844, 197,
  "North East", 235, 29, 738, 79,
  "North West", 400, 65, 2182, 223,
  "South East", 2419, 377, 1155, 107,
  "South West", 230, 43, 380, 50,
  "West Midlands", 299, 47, 1388, 155,
  "Yorkshire and Humber", 109, 16, 1339, 158
)

# regions and dates of attack rate data
uk_data_regions <- uk_attack$region
uk_data_start_date <- as.Date("2020-09-20")
uk_data_end_date <- as.Date("2021-01-04")

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

# base priors on p_WT and beta on Reff model posteriors

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

# approximate p_wt as a normal
p_wt_posterior <- calculate(
  fitted_reff_model$greta_arrays$distancing_effect$p,
  values = fitted_reff_model$draws,
  nsim = 1000
)[[1]]

p_wt <- normal(mean(p_wt_posterior), sd(p_wt_posterior), truncation = c(0, 1))


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
phi <- normal(1, 0.25, truncation = c(0, Inf))

# per-unit-contact-time infectiousness of the UK VOC strain
p_voc <- 1 - (1 - p_wt) ^ phi

# bias in sampling of contacts and cases in UK attack rate data (uniformly
# increases/decreases the observed attack rate fromt he hypothetical 'true'
# attack rate, where the true attack rate is defined such that the number of
# non-household contacts is that from the contact surveys). We could get a prior
# for this by comparing the numbers of contacts per case to the average numbers
# of non-household contacts, however the UK attack rate report does not clearly
# state the number of cases considered.

bias <- normal(1, 1, truncation = c(0, Inf))

infectious_days <- infectious_period(gi_cdf)

# overall microdistancing effect
gamma_i <- 1 - beta * d_i$micro_effect

# household and non-household secondary attack rates for the two strains
hsar_wt_i <- 1 - p_wt ^ (HD_0 * h_i$home)
hsar_voc_i <- 1 - p_voc ^ (HD_0 * h_i$home)
osar_wt_i <- gamma_i * (1 - p_wt ^ OD_0)
osar_voc_i <- gamma_i * (1 - p_voc ^ OD_0)

# proportion of contacts that are household contacts
w_i <- HC_0 / (HC_0 + OC_i$mean * infectious_days)

# overall secondary attack rates
sar_wt_i <- w_i * hsar_wt_i + (1 - w_i) * osar_wt_i
sar_voc_i <- w_i * hsar_voc_i + (1 - w_i) * osar_voc_i

# account for biased undersampling of contacts in the SAR data (e.g. sampling
# all household contacts but only a fraction of non-household contacts) which
# would cause the observed and expected attack rates to differ.
sar_wt_observed_i <- sar_wt_i ^ bias
sar_voc_observed_i <- sar_voc_i ^ bias

distribution(uk_attack$cases_wt) <- binomial(uk_attack$contacts_wt, sar_wt_observed_i)
distribution(uk_attack$cases_voc) <- binomial(uk_attack$contacts_voc, sar_voc_observed_i)

m <- model(phi, HC_0, HD_0, OD_0, p_wt, bias)

draws <- mcmc(m, chains = 10)
convergence(draws)
bayesplot::mcmc_trace(draws)

# # calculate Reff for household, non-household, and overall for the two strains
# reff_household_wt <- HC_0 * hsar_wt_i
# reff_household_voc <- HC_0 * hsar_voc_i

# compute the ratio of Reff for household, non-household, and overall infections
r_household <- hsar_voc_i / hsar_wt_i
r_non_household <- osar_voc_i / osar_wt_i
r_overall <- mean(sar_voc_i / sar_wt_i)
r_overall_observed <- mean(sar_voc_observed_i / sar_wt_observed_i)

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

hist_prior_posterior(phi, draws)
hist_prior_posterior(HC_0, draws)
hist_prior_posterior(HD_0, draws)
hist_prior_posterior(OD_0, draws)
hist_prior_posterior(p_wt, draws)
hist_prior_posterior(bias, draws)
hist_prior_posterior(r_overall, draws)
hist_prior_posterior(r_household, draws)
hist_prior_posterior(r_non_household, draws)

# do posterior predictive checks to make sure these align with raw ratios from PHE data
cases_voc_ga <- binomial(uk_attack$contacts_voc, sar_voc_observed_i)
cases_wt_ga <- binomial(uk_attack$contacts_wt, sar_wt_observed_i)
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
# 0.852
sd(phi_sim)
# 0.021


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


