# analyse change in microdistancing behaviour by state, using survey questions
# from the BETA barometer
source("R/functions.R")

# recode and collapse responses into percentage adherence
barometer <- barometer_results() %>%
  # recode 1.5m compliance question as yes/no (whether they mostly did it)
  mutate(
    response = case_when(
      question == "1.5m compliance" &
        response %in% c("Always") ~ "yes",
      question == "1.5m compliance" &
        response %in% c("Often", "Sometimes", "Rarely", "No") ~ "no",
      TRUE ~ response) 
  ) %>%
  # recode hand washing to yes/no (whether they did it immediately afterwards)
  mutate(
    response = case_when(
      question == "Hand washing" & response != "No" ~ "yes",
      question == "Hand washing" & response == "No" ~ "no",
      TRUE ~ response) 
  ) %>%
  # recode cough etiquette to yes/no (whether they covered their mouth with anything)
  mutate(
    response = case_when(
      question == "Cough etiquette" & response != "Nothing" ~ "yes",
      question == "Cough etiquette" & response == "Nothing" ~ "no",
      TRUE ~ response) 
  ) %>%
  # recode physical contact to the opposite, to reflect avoidance
  mutate(
    response = case_when(
      question == "Physical contact" & response == "No" ~ "yes",
      question == "Physical contact" & response == "Yes" ~ "no",
      TRUE ~ response) 
  ) %>%
  # combine responses into yes/no
  group_by(state, date, question, response) %>%
  summarise(count = sum(count),
            respondents = mean(respondents)) %>%
  mutate(proportion = count / respondents) %>%
  # now we can just keep the proportion responding 'yes'
  filter(response == "yes") %>%
  select(-response) %>%
  arrange(state, question, date)

# load latent factors
# assume adoption of microdistancing follows the same trend as macrodistancing,
# and that waning starts at the same time, butdon't assume it wanes at the same
# rate

distancing <- readRDS("outputs/social_distancing_latent.RDS")

# get data to predict to
pred_data <- distancing %>%
  rename(distancing = mean) %>%
  select(date, distancing) %>%
  left_join(
    expand_grid(
      date = .$date,
      state = unique(barometer$state)
    )
  ) %>%
  mutate(
    state_id = match(state, unique(state)),
    time = as.numeric(date - max(intervention_dates()$date)),
    time = time / max(time)
  )

# subset to 1.5m question and add data for modelling
barometer_distance <- barometer %>%
  filter(question == "1.5m compliance") %>%
  left_join(pred_data)

# Need Bayesian fit as lme4 fit is singular, and doesn't have right structure
library(greta)

n_locations <- max(barometer_distance$state_id)

# timing of peak microdistancing between the date of the last intervention and
# today's date
peak <- normal(0, 1, truncation = c(0, 1))

# hierarchical structure on state-level waning
logit_waning_effects_mean <- normal(0, 10)
logit_waning_effects_sd <- normal(0, 0.5, truncation = c(0, Inf))
logit_waning_effects_raw <- normal(0, 1, dim = n_locations)
logit_waning_effects <- logit_waning_effects_mean + logit_waning_effects_raw * logit_waning_effects_sd
waning_effects <- -ilogit(logit_waning_effects)

# hierarchical structure on state-level peak effect (proportion adhering) 
logit_distancing_effects_mean <- normal(0, 10)
logit_distancing_effects_sd <- normal(0, 0.5, truncation = c(0, Inf))
logit_distancing_effects_raw <- normal(0, 1, dim = n_locations)
logit_distancing_effects <- logit_distancing_effects_mean + logit_distancing_effects_raw * logit_distancing_effects_sd
distancing_effects <- ilogit(logit_distancing_effects)

prob <- microdistancing_model(
  data = barometer_distance,
  peak = peak,
  distancing_effects = distancing_effects,
  waning_effects = waning_effects
)

distribution(barometer_distance$count) <- binomial(
  barometer_distance$respondents,
  prob
)

m <- model(waning_effects, distancing_effects, peak)

set.seed(2020-05-30)
draws <- mcmc(m, chains = 4)
convergence(draws)

prob_pred <- microdistancing_model(
  data = pred_data,
  peak = peak,
  distancing_effects = distancing_effects,
  waning_effects = waning_effects
)

prob_pred_sim <- calculate(prob_pred, values = draws, nsim = 5000)[[1]][, , 1]
quants <- t(apply(prob_pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

# prepare outputs for plotting
pred_plot <- pred_data %>%
  select(date, state) %>%
  # add predictions
  mutate(mean = colMeans(prob_pred_sim)) %>%
  bind_cols(as_tibble(quants))
  
library(ggplot2)
line_df <- pred_plot %>%
  mutate_at(
    vars(mean, ci_90_lo, ci_90_hi, ci_50_lo, ci_50_hi),
    ~ . * 100
  ) %>%
  filter(date >= as.Date("2020-03-01")) %>%
  mutate(type = "Nowcast")

point_df <- barometer_distance %>%
  # duplicate ACT/NT
  ungroup() %>%
  mutate(
    percentage = proportion * 100
  ) %>%
  mutate(type = "Nowcast")

# Compute confidence intervals for the proportions for plotting. Need to fudge
# the sample size for one survey round with 100% adherence on a small sample
pred <- point_df %>%
  mutate(
    id = factor(row_number()),
    respondents = ifelse(respondents == count,
                         respondents + 1,
                         respondents)
  ) %>%
  glm(cbind(count, respondents - count) ~ id,
      data = .,
      family = stats::binomial) %>%
  predict(se.fit = TRUE)

# Monte Carlo integration based on normal approximation to logit-probability
logit_sims <- replicate(
  10000,
  rnorm(length(pred$fit),
        pred$fit,
        pred$se.fit)
)
p_sims <- plogis(logit_sims)
estimate <- rowMeans(p_sims)
cis <- t(apply(p_sims, 1, quantile, c(0.025, 0.975)))

point_df <- point_df %>%
  mutate(
    percentage = estimate * 100,
    lower = cis[, 1] * 100,
    upper = cis[, 2] * 100
  )

base_colour <- purple

p <- ggplot(line_df) +
  
  aes(date, mean, fill = type) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "1 months", date_labels = "%b %d") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_vline(
    xintercept = intervention_dates()$date,
    colour = "grey80"
  ) +
  
  geom_ribbon(aes(ymin = ci_90_lo,
                  ymax = ci_90_hi),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = ci_50_lo,
                  ymax = ci_50_hi),
              alpha = 0.5) +
  geom_line(aes(y = ci_90_lo),
            colour = base_colour,
            alpha = 0.8) + 
  geom_line(aes(y = ci_90_hi),
            colour = base_colour,
            alpha = 0.8) + 
  
  facet_wrap(~ state, ncol = 2, scales = "free") +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines")) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage),
    data = point_df,
    size = 0.5
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper),
    data = point_df,
    width = 0
  ) +
  
  # and titles  
  ggtitle(
    label = "Micro-distancing trend",
    subtitle = "Calibrated against self-reported adherence to physical distancing"
  ) +
  ylab("Estimate of percentage 'always' keeping 1.5m distance")

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

ggsave("outputs/figures/microdistancing_effect.png",
       width = multi_width,
       height = multi_height,
       scale = 0.8)

# save the model fit
saveRDS(pred_plot, file = "outputs/microdistancing_trends.RDS")

# estimates at peak and at latest date
pred_summary <- pred_plot %>%
  group_by(state) %>%
  summarise(peak = which.max(mean),
            peak_estimate = mean[peak] * 100,
            peak_low = ci_90_lo[peak] * 100,
            peak_high = ci_90_hi[peak] * 100,
            peak_date = date[peak],
            latest = which.max(date),
            latest_estimate = mean[latest] * 100,
            latest_low = ci_90_lo[latest] * 100,
            latest_high = ci_90_hi[latest] * 100,
            latest_date = date[latest]) %>%
  select(-peak, -latest)

saveRDS(pred_summary,
        file = "outputs/microdistancing_trend_summary.RDS")

