# analyse change in microdistancing behaviour by state, using survey questions
# from the BETA barometer
source("R/functions.R")

data <- microdistancing_data()
barometer_distance <- data$barometer_data
pred_data <- data$prediction_data

library(greta)

n_locations <- max(barometer_distance$state_id)

params <- microdistancing_params(n_locations)

peak <- params$peak
distancing_effects <- params$distancing_effects
waning_effects <- params$waning_effects

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
    size = 3,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper),
    data = point_df,
    size = 3,
    alpha = 0.2,
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

