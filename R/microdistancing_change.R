# analyse change in microdistancing behaviour by state, using survey questions
# from the BETA barometer
source("R/functions.R")

barometer <- barometer_results()

# recode and collapse responses into percentage adherence
state <- barometer %>%
  # recode 1.5m compliance question as yes/no (whether they mostly did it)
  mutate(
    response = case_when(
      question == "1.5m compliance" &
        response %in% c("Always", "Often") ~ "yes",
      question == "1.5m compliance" &
        response %in% c("Sometimes", "Rarely", "No") ~ "no",
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
  group_by(location, date, question, response) %>%
  summarise(count = sum(count),
            respondents = mean(respondents)) %>%
  mutate(proportion = count / respondents) %>%
  # now we can just keep the proportion responding 'yes'
  filter(response == "yes") %>%
  select(-response) %>%
  arrange(location, question, date)

# load latent factors
# assume adoption of microdistancing follows the same trend as macrodistancing,
# and that waning starts at the same time, butdon't assume it wanes at the same
# rate

distancing <- readRDS("~/Dropbox/github/covid19_australia_distancing/outputs/social_distancing_latent.RDS")
waning <- readRDS("~/Dropbox/github/covid19_australia_distancing/outputs/waning_distancing_latent.RDS")

# get data to predict to
pred_data <- distancing %>%
  rename(distancing = mean) %>%
  select(date, distancing) %>%
  left_join(
    waning %>%
      rename(waning = mean) %>%
      select(date, waning)
  ) %>%
  left_join(
    expand_grid(
      date = .$date,
      location = unique(state$location)
    )
  ) %>%
  mutate(
    location_id = match(location, unique(location))
  )
  

# subset to 1.5m question and add distancing and waning indices
state_distance <- state %>%
  filter(question == "1.5m compliance") %>%
  left_join(pred_data)

# Need Bayesian fit as lme4 fit is singular, and doesn't have quite the right
# structure so go Bayesian
library(greta)

n_locations <- max(state_distance$location_id)

# hierarchical structure on state-level waning
logit_waning_effect_mean <- normal(0, 10)
logit_waning_effect_sd <- normal(0, 0.5, truncation = c(0, Inf))
logit_waning_effect_raw <- normal(0, 1, dim = n_locations)
logit_waning_effect <- logit_waning_effect_mean + logit_waning_effect_raw * logit_waning_effect_sd
waning_effect <- -ilogit(logit_waning_effect)
distancing_effect <- uniform(0, 1)

# try a power transform on the distancing coefficient, to get a better fit from
# the peak

expected_proportion <- function(data, distancing_coef, waning_coefs) {
  data$distancing * distancing_coef + data$waning * waning_coefs[data$location_id]
}

prob <- expected_proportion(state_distance, distancing_effect, waning_effect)
distribution(state_distance$count) <- binomial(state_distance$respondents, prob)

m <- model(waning_effect, distancing_effect)

set.seed(2020-05-30)
draws <- mcmc(m, chains = 4)

# check convergence
r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
n_eff <- coda::effectiveSize(draws)
max(r_hats)
min(n_eff)

prob_pred <- expected_proportion(pred_data, distancing_effect, waning_effect)

prob_pred_sim <- calculate(prob_pred, values = draws, nsim = 5000)[[1]][, , 1]
quants <- t(apply(prob_pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

# prepare outputs for plotting
pred_plot <- pred_data %>%
  select(date, location) %>%
  # add predictions
  mutate(mean = colMeans(prob_pred_sim)) %>%
  bind_cols(as_tibble(quants)) %>%
  # duplicate ACT/NT
  mutate(
    duplicate = case_when(
      location == "ACT/NT" ~ 2,
      TRUE ~ 1
    )
  ) %>%
  uncount(duplicate, .id = "ACT_or_NT") %>%
  mutate(
    state = case_when(
      location == "ACT/NT" & ACT_or_NT == 1 ~ "ACT",
      location == "ACT/NT" & ACT_or_NT == 2 ~ "NT",
      TRUE ~ location
    )
  ) %>%
  select(-location, -ACT_or_NT)
  
library(ggplot2)
line_df <- pred_plot %>%
  mutate_at(
    vars(mean, ci_90_lo, ci_90_hi, ci_50_lo, ci_50_hi),
    ~ . * 100
  ) %>%
  filter(date >= as.Date("2020-03-01")) %>%
  mutate(type = "Nowcast")

point_df <- state_distance %>%
  # duplicate ACT/NT
  ungroup() %>%
  mutate(
    percentage = proportion * 100,
    duplicate = case_when(
      location == "ACT/NT" ~ 2,
      TRUE ~ 1
    )
  ) %>%
  uncount(duplicate, .id = "ACT_or_NT") %>%
  mutate(
    state = case_when(
      location == "ACT/NT" & ACT_or_NT == 1 ~ "ACT",
      location == "ACT/NT" & ACT_or_NT == 2 ~ "NT",
      TRUE ~ location
    )
  ) %>%
  select(-location, -ACT_or_NT) %>%
  mutate(type = "Nowcast")

# compute a confidence interval for the proportion
glm <- lme4::glmer(cbind(count, respondents - count) ~ (1 | id),
           data = point_df %>%
             mutate(id = factor(row_number())),
           family = stats::binomial)

# Monte Carlo integration based on normal approximation to logit-probability
meanlogit <- predict(glm)
sdlogit <- glm@theta
logit_sims <- replicate(
  1000,
  rnorm(length(meanlogit),
        meanlogit,
        sdlogit)
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

base_colour <- "purple"

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
  ylab("Estimate of percentage 'often' or 'always' keeping 1.5m distance")

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


