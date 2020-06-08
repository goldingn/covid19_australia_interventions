# model reporting delays in the linelist to detect impact of the app

library(dplyr)
library(readr)
library(tidyr)
library(RColorBrewer)
source("R/functions.R")

set.seed(2020-04-29)
linelist <- readRDS("~/not_synced/nnds/linelist_formatted.RDS")

# Delay is time from date of symptom onset to specimen collection, only consider
# it valid if 0 or greater. There are a very few, data points in the 2-3 days
# before symptom onset (when virus detectable) and a number of clearly erroneous
# ones before this. These are so few that they a) are clearyly not from the same
# distribution as the positive delays, and b) are so few (<1.5%) that their
# omission should not substantially bias the estimate of mean time to detection.
delay_data <- linelist %>%
  filter(import_status == "local") %>%
  mutate(date_infection = date_onset - 5) %>%
  filter(!is.na(date_infection),
         !is.na(date_detection)) %>%
  mutate(time_to_detection = as.numeric(date_detection - date_onset),
         date_num = date_infection - min(date_infection) + 1,
         date_num = as.numeric(date_num)) %>%
  filter(time_to_detection >= 0) %>%
  select(date_infection,
         date_num,
         delay = time_to_detection,
         state = region)

# get data on numbers of tests
test_data <- get_tests() %>%
  na.omit()

# jointly model numbers of tests and the reporting delay:

# tests ~ poisson(expected_tests)
# delay ~ negative_binomial(expected_delay)
# log(expected_delay) = beta * log(expected_tests) + e1
# log(expected_tests) = e2


n_dates <- max(delay_data$date_num)
date_nums <- seq_len(n_dates)
states <- sort(unique(delay_data$state))
n_states <- length(states)

# fit a hierarchical gp
library(greta.gp)

kernel <- function(alpha_time = lognormal(2, 0.5),
                   lengthscale_time = lognormal(2, 0.5),
                   sigma_time = normal(0, 0.5, truncation = c(0, Inf)),
                   sigma_bias = NULL) {
  
  kernel <- rational_quadratic(
    lengthscales = lengthscale_time,
    variance = sigma_time ^ 2,
    alpha = alpha_time)
  
  
  if (!is.null(sigma_bias)) {
    bias <- bias(sigma_bias ^ 2)
    kernel <- kernel + bias
  }
  
  kernel
  
  
}

state_kernel <- kernel()
national_kernel <- kernel(sigma_bias = 0.1)

inducing_date_nums <- seq(n_dates, 1, by = -5)

national_trend <- gp(x = date_nums,
                     kernel = national_kernel,
                     inducing = inducing_date_nums)

state_errors <- gp(x = date_nums,
                   kernel = state_kernel,
                   inducing = inducing_date_nums,
                   n = n_states)

state_trends <- sweep(state_errors, 1, national_trend, FUN = "+")
mu <- exp(state_trends)

square_inv_size <- normal(0, 0.5, truncation = c(0, Inf))
size <- 1 / sqrt(square_inv_size)

prob <- 1 / (1 + mu / size)

row_idx <- match(delay_data$date_num, date_nums)
col_idx <- match(delay_data$state, states)
idx <- cbind(row_idx, col_idx)

distribution(delay_data$delay) <- negative_binomial(size = size, prob = prob[idx])

m <- model(mu)
draws <- mcmc(m, chains = 20, one_by_one = TRUE)

convergence(draws)

# summarise posterior over mean delay
draws_mat <- as.matrix(draws)
mean <- colMeans(draws_mat)
ci <- apply(draws_mat, 2, quantile, c(0.025, 0.975))

# and get CI over predictive posterior (variation in delay)
obs <- negative_binomial(size = size, prob = prob)

obs_sim <- calculate(obs, values = draws, nsim = 20000)[[1]]
obs_lower <- c(apply(obs_sim, 2:3, quantile, 0.025))
obs_upper <- c(apply(obs_sim, 2:3, quantile, 0.975))

pred_df <- tibble(
  date_num = rep(date_nums, n_states),
  state = rep(states, each = n_dates),
  delay = mean,
  lower = ci[1, ],
  upper = ci[2, ],
  obs_lower = obs_lower,
  oobs_upper = obs_upper
) %>%
  mutate(date = min(delay_data$date_infection) + date_num - 1)

library(ggplot2)
ggplot(pred_df) +
  aes(date, delay) +
  geom_point(aes(date_infection, delay),
             data = delay_data,
             size = 0.1,
             alpha = 0.2,
             colour = "blue") +
  ggtitle(label = "Average time to detection has declined",
          subtitle = "among locally-acquired cases") +
  ylab("days from symptom onset to specimen collection") +
  xlab(element_blank()) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(date, obs_lower), linetype = 3) +
  geom_line(aes(date, obs_upper), linetype = 3) +
  geom_line() +
  facet_wrap(~state, ncol = 2) + 
  theme_minimal()
  
ggsave("outputs/figures/time_to_detection_fit.png",
       width = 8.27,
       height = 11.69,
       scale = 0.8)

pred_df %>%
  filter(date_num == max(date_num))

# priors for the parameters of the lognormal distribution over the serial interval from Nishiura et
# al., as stored in the EpiNow source code 
si_param_samples <- read_csv(
  file = "https://raw.githubusercontent.com/epiforecasts/EpiNow/758b706599244a545d6b07f7be4c10ffe6c8cf50/data-raw/nishiura-lognormal-truncated.csv",
  col_types = cols(param1 = col_double(),
                   param2 = col_double())
)
meanlog <- mean(si_param_samples$param1)
sdlog <- mean(si_param_samples$param2)
# beta <- function(x) {
#   plnorm(x + 5, meanlog, sdlog)
# }
# gamma <- function(x) {
#   beta(x) / beta(10)
# }
# # 
# pred_df$prop_R_eff <- gamma(pmin(10, pred_df$delay))
# par(mfrow = c(4, 2))
# for (state in states) {
#   plot(prop_R_eff ~ date, data = pred_df[pred_df$state == state, ],
#        type = "l",
#        ylim = c(0, 1))
#   title(main = state)
# }

# need to incorporate uncertainty on earlier delay here, so the later delay
# becomes uncertain

# so need to compute lognormal cdf in greta
beta_dt <- iprobit((log(mu + 5) - meanlog) / sdlog)
gamma_dt <- sweep(beta_dt, 2, t(beta_dt[1, ]), FUN = "/")
gamma_dt_sim <- calculate(gamma_dt, values = draws, nsim = 2000)[[1]]
gamma_dt_mean <- c(apply(gamma_dt_sim, 2:3, FUN = base::mean))
gamma_dt_lower <- c(apply(gamma_dt_sim, 2:3, FUN = quantile, 0.025))
gamma_dt_upper <- c(apply(gamma_dt_sim, 2:3, FUN = quantile, 0.975))

pred_df$rel_reff_mean <- gamma_dt_mean
pred_df$rel_reff_lower <- gamma_dt_lower
pred_df$rel_reff_upper <- gamma_dt_upper
  
ggplot(pred_df) +
  aes(date, rel_reff_mean) +
  
  ggtitle(label = expression(Reduction~of~R[eff]~due~to~improved~contact~tracing),
          subtitle = "among locally-acquired cases") +
  ylab(expression(Proportion~of~original~R[eff])) +
  xlab(element_blank()) +
  
  geom_ribbon(aes(ymin = rel_reff_lower, ymax = rel_reff_upper), alpha = 0.2) +
  geom_line() +
  
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~state, ncol = 2) + 
  theme_minimal()

ggsave("outputs/figures/contact_tracing_effect.png",
       width = 8.27,
       height = 11.69,
       scale = 0.8)
  
