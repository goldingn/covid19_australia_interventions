# model the changing times to case detection throughout the first wave

source("R/functions.R")
set.seed(2020-06-10)

linelist <- readRDS("~/not_synced/nnds/linelist_formatted.RDS")

# make the detection date the earliest of peciment conllection or notification
detection <- linelist %>%
  # remove any cases where first specimen collection is being reported as after
  # case confirmation (data entry error with dates, so detection date could be
  # erroneous)
  filter(
    date_detection <= date_confirmation
  ) %>%
  filter(
    import_status == "local"
  ) %>%
  mutate(
    date_infection = date_onset - 5,
    days_to_detection = date_detection - date_onset,
    days_to_detection = as.numeric(days_to_detection)
  ) %>%
  filter(days_to_detection >= -12) %>%
  # also consider the number tested 1 or 2+ days prior to symptom onset
  mutate(
    days_to_detection = pmax(days_to_detection, -2),
    negative = as.numeric(days_to_detection < 0)
  ) %>%
  na.omit()

# index to non-negative ttds
nonneg <- which(!detection$negative)

# create a date-by-state matrix of days
dates <- seq(min(detection$date_infection), max(detection$date_infection), by = 1)
states <- sort(unique(detection$region))
n_dates <- length(dates)
n_states <- length(states)
zeros <- matrix(0, n_dates, n_states)
days <- row(zeros)

# the test positivity rates appeared to be higher around March then dropped to a
# plateau around the end of May. Define a sigmoid prior that broadly matches
# this. Flat prior to 1st of March, trend over next three months, then flat. Set
# ends of a logit to start around then. Given prior on one end and the width,
# get the parameters.
ends <- match(as.Date(c("2020-03-01", "2020-05-01")), dates)
start <- ends[1] + normal(0, 10)
diff <- diff(ends) + normal(0, 10)

# from this, get location of centre and the slope (slope 1 means a plateau at
# +5/-5)
location <- start + diff / 2
slope <- -10 / diff

# relationship between the sigmoid curve and log mean positive time to detection
intercept_ttd <- normal(0, 10) # exp(hierarchical_normal(n_states))
slope_ttd <- normal(0, 10) # exp(hierarchical_normal(n_states))

# relationship between the sigmoid curve and logit probability of a negative ttd
intercept_neg <- normal(0, 10) # exp(hierarchical_normal(n_states))
slope_neg <- normal(0, 10) # exp(hierarchical_normal(n_states))

# shared sigmoid curve
sigmoid <- ilogit(slope * (days - location))

# expected mean time to detection
mean_ttd <- exp(intercept_ttd + slope_ttd * sigmoid)

# expected mean time to detection
p_neg <- ilogit(intercept_neg + slope_neg * sigmoid)

# indices
row_idx <- match(detection$date_infection, dates)
col_idx <- match(detection$region, states)
idx <- cbind(row_idx, col_idx)

# negative time to detection likelihood
neg <- detection$negative
distribution(neg) <- bernoulli(p_neg[idx])

# positive time to detection likelihood
sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf))
size <- 1 / sqrt(sqrt_inv_size)
prob <- 1 / (1 + mean_ttd / size)
ttd <- detection$days_to_detection[nonneg]
distribution(ttd) <- negative_binomial(size, prob[idx[nonneg]])

m <- model(location, slope,
           intercept_ttd, slope_ttd)

draws <- mcmc(m, chains = 10)
convergence(draws)

nsim <- coda::niter(draws) * coda::nchain(draws)
nsim <- min(10000, nsim)

# posterior predictive checks
neg_dist <- bernoulli(p_neg)
ttd_dist <- negative_binomial(size, prob)
sims <- calculate(neg_dist[idx], ttd_dist[idx[nonneg]], values = draws, nsim = 1000)
neg_sim <- sims[[1]][, , 1]
ttd_sim <- sims[[2]][, , 1]

# overall PPC check on each part of the likelihood
bayesplot::ppc_ecdf_overlay(
  detection$days_to_detection[nonneg],
  ttd_sim,
  discrete = TRUE
)
bayesplot::ppc_ecdf_overlay(
  detection$negative,
  neg_sim,
  discrete = TRUE
)

# get samples of the different parts of the distribution

# probability of a negative time
neg_samples <- calculate(p_neg[, 1], values = draws, nsim = nsim)[[1]][, , 1]
neg_probability <- tibble(
  p_neg = colMeans(neg_samples),
  date = dates
)

# probability of each positive time
# switch to analytic solution with posterior samples of NB parameters
ttd_samples <- calculate(ttd_dist[, 1], values = draws, nsim = nsim)[[1]][, , 1]
pos_probability <- tibble(
  days = c(ttd_samples),
  date = rep(dates, each = nsim)
) %>%
  group_by(date) %>%
  count(days) %>%
  mutate(
    days = case_when(
      days >= 20 ~ 20,
      TRUE ~ days)
  ) %>%
  full_join(
    expand_grid(
      days = 0:20,
      date = dates,
      n = 0
    )
  ) %>%
  group_by(date, days) %>%
  summarise(n = sum(n)) %>%
  group_by(date) %>%
  mutate(probability = n / sum(n))

# combine these, expand negative out into -1 and -2, and pad with 0s for -3:5
probability <- pos_probability %>%
  full_join(
    expand_grid(
      days = -5:-1,
      date = dates,
      probability = 0
    )
  ) %>%
  left_join(neg_probability) %>%
  mutate(
    probability = case_when(
      days <= -3 ~ 0,
      days %in% c(-1, -2) ~ p_neg / 2,
      days >= 0 ~ probability * (1 - p_neg)
    )
  ) %>%
  select(-`n`, -p_neg) %>%
  ungroup() %>%
  arrange(date, days)

# convert to a matrix
probability_mat <- probability %>%
  pivot_wider(names_from = days, values_from = probability) %>%
  select(-date) %>%
  as.matrix()

# probability of lasting this long without being detected
cdf_mat <- t(apply(probability_mat, 1, cumsum))

get_quantile <- function (cdf, prob) {
  matches <- which(cdf < prob)
  idx <- matches[length(matches)]
  days <- seq_along(cdf) - 6
  days[idx]
}


get_mean <- function (cdf) {
  days <- seq_along(cdf) - 6
  prob <- c(0, diff(cdf))
  sum(days * prob)
}

get_quantile_mat <- function(cdf, prob) {
  apply(cdf, 1, get_quantile, prob)
}

get_mean_mat <- function(cdf) {
  apply(cdf, 1, get_mean)
}

# get a tibble of statistics to plot
df <- tibble(
  date = dates,
  mean = get_mean_mat(cdf_mat),
  median = get_quantile_mat(cdf_mat, 0.5),
  ci_50_lo = get_quantile_mat(cdf_mat, 0.25),
  ci_50_hi = get_quantile_mat(cdf_mat, 0.75),
  ci_90_lo = get_quantile_mat(cdf_mat, 0.05),
  ci_90_hi = get_quantile_mat(cdf_mat, 0.95),
  type = "Nowcast"
)


library(ggplot2)
base_colour <- yellow

p <- ggplot(df) + 
  
  aes(date, mean, fill = type) +
  
  xlab(element_blank()) +
  
  # coord_cartesian(ylim = ylim) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "1 months", date_labels = "%b %d") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_vline(xintercept = intervention_dates()$date, colour = "grey80") +
  
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
  
  
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines")) +
  ggtitle(label = "Surveillance trend",
          subtitle = "Time to detection of locally-acquired cases") +
  ylab("Days from symptom onset to testing")

# add points for true delays
df_obs <- detection %>%
  rename(state = region) %>%
  mutate(type = "Nowcast")

p <- p + geom_point(
  aes(date_infection, days_to_detection),
  data = df_obs,
  size = 0.5,
  alpha = 0.2
)

p

panel_width <- 11.69 / 2
panel_height <- 8.27 / 3

ggsave("outputs/figures/surveillance_effect.png",
       width = panel_width,
       height = panel_height * 1.25,
       scale = 1)


