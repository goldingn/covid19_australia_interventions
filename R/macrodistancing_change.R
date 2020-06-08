# analyse change in macrodistancing behaviour (non-household contact rate) by
# state, using a baseline rate, and survey questions from Freya's survey and the
# BETA barometer
source("R/functions.R")
library(greta)

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

params <- macrodistancing_params(location_change_trends)
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

convergence(draws)

nsim <- coda::niter(draws) * coda::nchain(draws)
nsim <- min(10000, nsim)

# check posterior calibration
contacts_ga <- negative_binomial(out$size, out$prob)
contacts_sim <- calculate(contacts_ga, values = draws, nsim = nsim)[[1]][, , 1]
bayesplot::ppc_ecdf_overlay(
  contacts$contacts,
  contacts_sim,
  discrete = TRUE
)

# get trend predictions
pred_sim <- calculate(c(OC_t_state), values = draws, nsim = nsim)[[1]][, , 1]
quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

# get point estimates for plotting
baseline_contact_params <- baseline_contact_parameters()
baseline_point <- tibble::tribble(
  ~date, ~estimate, ~sd,
  as.Date("2020-03-01"),
  baseline_contact_params$mean_contacts[2],
  baseline_contact_params$se_contacts[2]
) %>%
  mutate(type = "Nowcast") %>%
  mutate(lower = estimate - sd * 1.96,
         upper = estimate + sd * 1.96)

# slim down dataframe to get indeopendent estimates for surveys
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
idx <- contacts_inla %>%
  mutate(id = row_number()) %>%
  distinct(date, state, .keep_all = TRUE) %>%
  mutate(date = as.Date(date)) %>%
  right_join(survey_points) %>%
  pull(id)

sry <- glm$summary.fitted.values[idx, ]
survey_points <- survey_points %>%
  mutate(estimate = sry$mean,
         lower = sry$`0.025quant`,
         upper = sry$`0.975quant`) %>%
  mutate(type = "Nowcast")


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
  
  # add survey results estimate
  geom_point(
    aes(date, estimate),
    data = survey_points,
    size = 0.5
  ) +
  geom_errorbar(
    aes(
      date,
      estimate,
      ymin = lower,
      ymax = upper
    ),
    data = survey_points,
    width = 0
  )

p

# get required aspect ratio
panel_width <- 11.69 / 2
panel_height <- 8.27 / 3
panel_ratio <- panel_height / panel_width

# work out dimensions for 4x2 panels for reports
multi_mfrow <- c(4, 2)
multi_width <- 8.27
multi_height <- (multi_width / multi_mfrow[2]) * panel_ratio * multi_mfrow[1]

# add a bit of space for the title
multi_height <- multi_height * 1.2

ggsave("outputs/figures/macrodistancing_effect.png",
       width = multi_width,
       height = multi_height,
       scale = 0.8)

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

