
write_reff_sims <- function(fitted_model,
                            dir = "outputs/projection",
                            write_reff_1 = TRUE,
                            write_reff_12 = TRUE,
                            write_reff_2 = FALSE) {
  
  if (write_reff_1) {
    
    reff_1 <- reff_sims(fitted_model, which = "R_eff_loc_1")
    
    reff_1 %>%
      write_csv(
        file.path(dir, "r_eff_1_local_samples.csv")
      )
    
  }
  
  if (write_reff_12) {
    
    # find the dates for clamping into the future (where 50%/95% cases so far detected)
    clip_idx_50 <- (fitted_model$data$detection_prob_mat > 0.5) %>%
      apply(1, all) %>%
      which() %>%
      max()
    
    clip_idx_95 <- (fitted_model$data$detection_prob_mat > 0.95) %>%
      apply(1, all) %>%
      which() %>%
      max()
    
    date_50 <- fitted_model$data$dates$infection[clip_idx_50]
    date_95 <- fitted_model$data$dates$infection[clip_idx_95]
    
    reff_12 <- reff_sims(fitted_model, which = "R_eff_loc_12")
    
    reff_12 %>%
      write_csv(
        file.path(dir, "r_eff_12_local_samples.csv")
      )
    
    reff_12 %>%
      soft_clamp(date_50) %>%
      write_csv(
        file.path(dir, "r_eff_12_local_samples_soft_clamped_50.csv")
      )
    
    reff_12 %>%
      soft_clamp(date_95) %>%
      write_csv(
        file.path(dir, "r_eff_12_local_samples_soft_clamped_95.csv")
      )
  }
  
  
  if (write_reff_2) {
    
    reff_2 <- reff_sims(fitted_model, which = "epsilon_L")
    
    reff_2 %>%
      write_csv(
        file.path(dir, "r_eff_2_local_samples.csv")
      )
    
  }
  
  
}


source("R/lib.R")

set.seed(2020-04-29)
source("R/functions.R")


fitted_model <- readRDS("outputs/fitted_reff_model.RDS")

# redo this as function to enable running with all voc or non-voc for outputs.

# Results with Oz analysis - use estimate of the relative per-unit-contact-time
# infection probability and reconstruct component 1 timeseries
phi <- normal(1.454, 0.051, truncation = c(0, Inf))


data <- fitted_model$data
dates <- data$dates$mobility

de <- fitted_model$greta_arrays$distancing_effect

q <- de$p
p <- 1 - q
p_star <- 1 - (1 - p) ^ phi


infectious_days <- infectious_period(gi_cdf)

h_t <- h_t_state(dates)
HD_t <- de$HD_0 * h_t

household_infections <- de$HC_0 * (1 - (1 - p_star) ^ HD_t)
non_household_infections <- de$OC_t_state * de$gamma_t_state *
  infectious_days * (1 - (1 - p_star) ^ de$OD_0)
R_t <- household_infections + non_household_infections
R_eff_loc_1_no_surv <- extend(R_t, data$n_dates_project)


# multiply by the surveillance effect to get component 1
surveillance_reff_local_reduction <- surveillance_effect(
  dates = data$dates$infection_project,
  cdf = gi_cdf,
  states = data$states
)


# multiply by the surveillance effect
R_eff_loc_1 <- R_eff_loc_1_no_surv * surveillance_reff_local_reduction
log_R_eff_loc_1 <- log(R_eff_loc_1)


# hierarchical (marginal) prior sd on log(Reff12) by state 
sigma <- normal(0, 0.5, truncation = c(0, Inf))
sigma_state <- sigma * ones(data$n_states)
var <- sigma ^ 2

# hierarchical prior mean on log(Reff12) by state
mu_prior <- log_R_eff_loc_1 - var

# temporally correlated errors in R_eff for local cases - representing all the
# stochastic transmission dynamics in the community, such as outbreaks in
# communities with higher or lower tranmission rates
# fixing the kernel variance at 1, and introducing the variance in v
kernel_L <- rational_quadratic(
  lengthscales = lognormal(3, 1),
  variance = 1,
  alpha = lognormal(3, 1)
)

# de-centred temporally-correlated log Reff12 GP prior
epsilon_L <- epsilon_gp(
  date_nums = data$dates$date_nums,
  n_states = data$n_states,
  inducing_date_nums = data$dates$inducing_date_nums,
  sigma_state = sigma_state,
  kernel = kernel_L
)

# add the prior mean back on to re-centre the posterior  
log_R_eff_loc <- mu_prior + epsilon_L

R_eff_loc_12 <- exp(log_R_eff_loc)


oz_fitted_model <- fitted_model
oz_fitted_model$greta_arrays$R_eff_loc_1 <- R_eff_loc_1
oz_fitted_model$greta_arrays$R_eff_loc_12 <- R_eff_loc_12

oz_dir <- "outputs/projection/b117_oz_style"
dir.create(oz_dir, showWarnings = FALSE)
write_reff_sims(oz_fitted_model, oz_dir, write_reff_12 = TRUE, write_reff_2 = TRUE)

#write_reff_sims(fitted_model, write_reff_1 =FALSE, write_reff_12 = FALSE, write_reff_2 = TRUE)

# also calculate and write out the equivalent multiplicative factor over time
ratio <- oz_fitted_model$greta_arrays$R_eff_loc_1 / fitted_model$greta_arrays$R_eff_loc_1
ratio_vec <- c(ratio)
ratio_sims <- calculate(ratio_vec, values = fitted_model$draws, nsim = 2000)
ratio_samples <- t(ratio_sims[[1]][, , 1])
colnames(ratio_samples) <- paste0("sim", 1:2000)

tibble(
  date = rep(fitted_model$data$dates$infection_project, fitted_model$data$n_states),
  state = rep(fitted_model$data$states, each = fitted_model$data$n_dates_project),
) %>%
  mutate(date_onset = date + 5) %>%
  cbind(ratio_samples) %>%
  write_csv(
    file.path(oz_dir, "r_eff_1_ratio_samples.csv")
  )

read_csv("outputs/projection/b117_oz_style/r_eff_1_local_samples.csv") %>%
  filter(date == as.Date("2020-04-11")) %>%
  pivot_longer(
    cols = starts_with("sim"),
    names_to = "sim"
  ) %>%
  group_by(state, date) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    p_exceedance = mean(value > 1)
  )


voc1 <- read_csv(file = "outputs/projection/b117_oz_style/r_eff_1_local_samples.csv")
voc12 <- read_csv(file = "outputs/projection/b117_oz_style/r_eff_12_local_samples.csv")
voc2 <- read_csv(file = "outputs/projection/b117_oz_style/r_eff_2_local_samples.csv")

wt1 <- read_csv(file = "outputs/projection/r_eff_1_local_samples.csv")
wt12 <- read_csv(file = "outputs/projection/r_eff_12_local_samples.csv")
wt2 <- read_csv(file = "outputs/projection/r_eff_2_local_samples.csv")




v1 <- voc1 %>%
  tidyr::pivot_longer(
    cols = starts_with("sim")
  ) %>%
  group_by(date, state) %>%
  summarise(reff = mean(value)) %>%
  mutate(strain = "VOC", component = "1")

v12 <- voc12 %>%
  tidyr::pivot_longer(
    cols = starts_with("sim")
  ) %>%
  group_by(date, state) %>%
  summarise(reff = mean(value)) %>%
  mutate(strain = "VOC", component = "12")

v2 <- voc2 %>%
  tidyr::pivot_longer(
    cols = starts_with("sim")
  ) %>%
  group_by(date, state) %>%
  summarise(reff = mean(value)) %>%
  mutate(strain = "VOC", component = "2")

w1 <- wt1 %>%
  tidyr::pivot_longer(
    cols = starts_with("sim")
  ) %>%
  group_by(date, state) %>%
  summarise(reff = mean(value)) %>%
  mutate(strain = "WT", component = "1")

w12 <- wt12 %>%
  tidyr::pivot_longer(
    cols = starts_with("sim")
  ) %>%
  group_by(date, state) %>%
  summarise(reff = mean(value)) %>%
  mutate(strain = "WT", component = "12")

w2 <- wt2 %>%
  tidyr::pivot_longer(
    cols = starts_with("sim")
  ) %>%
  group_by(date, state) %>%
  summarise(reff = mean(value)) %>%
  mutate(strain = "WT", component = "2")


r1 <- bind_rows(v1, w1)

r12 <- bind_rows(v12, w12)

r2 <- bind_rows(v2, w2)

rall <- bind_rows(v1, v12, v2, w1, w12, v2)


ggplot(r1) +
  geom_line(
    aes(
      x = date,
      y = reff,
      colour = strain
    )
  ) +
  facet_wrap(~state, ncol = 2)


ggplot(r12) +
  geom_line(
    aes(
      x = date,
      y = reff,
      colour = strain
    )
  ) +
  facet_wrap(~state, ncol = 2)



ggplot(r2) +
  geom_line(
    aes(
      x = date,
      y = reff,
      colour = strain
    )
  ) +
  facet_wrap(~state, ncol = 2)


ggplot(rall) +
  geom_line(
    aes(
      x = date,
      y = reff,
      colour = strain,
      linetype = component
    )
  ) +
  facet_wrap(~state, ncol = 2)

