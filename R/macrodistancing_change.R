# analyse change in macrodistancing behaviour (non-household contact rate) by
# state, using a baseline rate, and survey questions from Freya's survey and the
# BETA barometer
source("R/functions.R")
library(greta)

# make sure we have the latest survey data
format_raw_survey_data()

# modelled change (after/before ratio) in time at types of locations from Google
location_change_trends <- location_change() %>%
  mutate_at(
    vars(public, home, retail, transit, work),
    ~replace_na(., 1)
  ) %>%
  mutate(state = abbreviate_states(state))

# state-level numbers of non-household contacts by state and date from Freya's
# survey and the BETA barometer. Remove implausible responses, from the
# reporting clump at 999 and above (short conversation with 999 or more people
# in a day is implausible, and probably an entry/reporting/understanding error)
contacts <- contact_survey_data() %>%
  filter(contacts < 999)

# gi_cdf <- nishiura_cdf()

params <- macrodistancing_params(location_change_trends, gi_cdf)
OC_0 <- params$OC_0
relative_weights <- params$relative_weights
scaling <- params$scaling

OC_t_state <- macrodistancing_model(
  location_change_trends,
  baseline = OC_0,
  relative_weights = relative_weights,
  scaling = scaling
)

out <- macrodistancing_likelihood(OC_t_state, contacts, location_change_trends)

# fit model
set.seed(2020-05-30)
m <- model(relative_weights, scaling, OC_0, out$size)
draws <- mcmc(m,
              sampler = hmc(Lmin = 10, Lmax = 15),
              chains = 10)

draws <- extra_samples(draws, 1000)
convergence(draws)

nsim <- coda::niter(draws) * coda::nchain(draws)
nsim <- min(10000, nsim)

# check posterior calibration
contacts_ga <- negative_binomial(out$size, out$prob)
contacts_sim <- calculate(contacts_ga, values = draws, nsim = nsim)[[1]][, , 1]
bayesplot::ppc_ecdf_overlay(
  contacts$contacts,
  contacts_sim[1:1000, ],
  discrete = TRUE
)

# get trend predictions
pred_sim <- calculate(c(OC_t_state), values = draws, nsim = nsim)[[1]][, , 1]
quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

# get point estimates for plotting
# gi_cdf <- nishiura_cdf()
baseline_contact_params <- baseline_contact_parameters(gi_cdf)
baseline_point <- tibble::tribble(
  ~date, ~estimate, ~sd,
  as.Date("2020-03-01"),
  baseline_contact_params$mean_contacts[2],
  baseline_contact_params$se_contacts[2]
) %>%
  mutate(type = "Nowcast") %>%
  mutate(lower = estimate - sd * 1.96,
         upper = estimate + sd * 1.96)

# slim down dataframe to get independent estimates for surveys
survey_points <- contacts %>%
  group_by(state, date) %>%
  summarise(n = n())  %>%
  ungroup()

library(INLA)
contacts_inla <- contacts %>%
  mutate(date = as.character(date),
         date_state = paste(date, state))

glm <- inla(contacts ~ f(date_state, model = "iid"),
            family = "nbinomial",
            data = contacts_inla,
            control.predictor = list(link = 1))

# pull out fitted values for each date/state combination
survey_indices <- contacts_inla %>%
  mutate(id = row_number()) %>%
  distinct(date, state, .keep_all = TRUE) %>%
  mutate(date = as.Date(date))

idx <- survey_points %>%
  left_join(survey_indices) %>%
  pull(id)

sry <- glm$summary.fitted.values[idx, ]
survey_points <- survey_points %>%
  mutate(
    date = date + 2,
    estimate = sry$mean,
    lower = sry$`0.025quant`,
    upper = sry$`0.975quant`
  ) %>%
  mutate(type = "Nowcast")

# get holiday dates and subset to where they overlap with surveys
holiday_lines <- survey_points %>%
  mutate(date_start = date - 3,
         date_end = date + 3) %>%
  select(state, date_start, date_end) %>%
  left_join(
    holiday_dates() %>%
      mutate(state = abbreviate_states(state))
  ) %>%
  filter(date < date_end & date > date_start)

type <- 1
states <- unique(location_change_trends$state)
dates <- unique(location_change_trends$date)
n_states <- length(states)
# non-household contacts
p <- plot_trend(pred_sim,
                dates = dates,
                multistate = TRUE,
                base_colour = purple,
                vline_at = intervention_dates()$date,
                ylim = c(0, 15),
                hline_at = NULL,
                vline2_at = NA) + 
  ggtitle(label = "Macro-distancing trend",
          subtitle = "Rate of non-household contacts") +
  ylab("Estimated mean number of non-household contacts per day") + 
  
  # add baseline estimate
  geom_point(
    aes(date, estimate),
    data = baseline_point,
    size = 0.5,
    colour = grey(0.5)
  ) +
  geom_errorbar(
    aes(
      date,
      estimate,
      ymin = lower,
      ymax = upper
    ),
    data = baseline_point,
    width = 0,
    colour = grey(0.5)
  ) + 
  
  # rug marks for holidays
  geom_rug(
    aes(date),
    data = holiday_lines,
    col = green,
    size = 1,
    length = unit(0.1, "npc"),
    sides = "b",
    inherit.aes = FALSE
  ) +
  
  # add survey results estimate
  geom_point(
    aes(date, estimate),
    data = survey_points,
    size = 4,
    pch = "_"
  ) +
  geom_errorbar(
    aes(
      date,
      estimate,
      ymin = lower,
      ymax = upper
    ),
    data = survey_points,
    size = 4,
    alpha = 0.2,
    width = 0
  )


p

save_ggplot("macrodistancing_effect.png")

# prepare outputs for plotting
pred_trend <- location_change_trends %>%
  select(date, state) %>%
  # add predictions
  mutate(mean = colMeans(pred_sim)) %>%
  bind_cols(as_tibble(quants))

# save the model fit
saveRDS(pred_trend,
        file = "outputs/macrodistancing_trends.RDS")

# estimates at peak and at latest date
pred_summary <- pred_trend %>%
  group_by(state) %>%
  summarise(peak = which.min(mean),
            peak_estimate = mean[peak],
            peak_low = ci_90_lo[peak],
            peak_high = ci_90_hi[peak],
            peak_date = date[peak],
            latest = which.max(date),
            latest_estimate = mean[latest],
            latest_low = ci_90_lo[latest],
            latest_high = ci_90_hi[latest],
            latest_date = date[latest]) %>%
  select(-peak, -latest)

saveRDS(pred_summary,
        file = "outputs/macrodistancing_trend_summary.RDS")

# save the raw contact survey data
contact_survey_data() %>%
 saveRDS("outputs/contact_survey_data.RDS")

