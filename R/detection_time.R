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
  mutate(
    date_infection = date_onset - 5,
    days_to_detection = date_detection - date_onset,
    days_to_detection = as.numeric(days_to_detection)
  ) %>%
  filter(days_to_detection >= -12) %>%
  # also consider the number tested 1 or 2+ days prior to symptom onset
  mutate(days_to_detection = pmax(days_to_detection, -2)) %>%
  na.omit()

# for those with non-negative ttd, model the distribution of days.
nonneg <- detection %>%
  filter(days_to_detection >= 0)

# start with a Poisson distribution, then check fit and consider NB

# model the log rate with a linear model

# create a date-by-state matrix of lambda
dates <- seq(min(nonneg$date_infection), max(nonneg$date_infection), by = 1)
states <- sort(unique(nonneg$region))
n_dates <- length(dates)
n_states <- length(states)
zeros <- matrix(0, n_dates, n_states)
days <- row(zeros)

# the test positivity rate in VIC (which had lots of cases and still has cases)
# appeared to peak around the start of April, and dropped to a plateau around
# the end of May. Define a logit that broadly matches this.

# Want a clamped linear function to reflect change
# flat prior to 1st of March
# trend over next three months, then flat
# flat after 1st May
# set ends of a logit to start/end around then
# given prior on one end, set prior on the other
ends <- match(as.Date(c("2020-03-01", "2020-05-01")), dates)
start <- ends[1] + normal(0, 1)
diff <- diff(ends) + normal(0, 1)

# from this, get location of centre
location <- start + diff / 2
# and the slope (slope 1 means a plateau at +5/-5)
slope <- -10 / diff

# relationship between the sigmoid curve and log mean time to detection
intercept_ttd <- normal(0, 10) # exp(hierarchical_normal(n_states))
slope_ttd <- normal(0, 10) # exp(hierarchical_normal(n_states))

# shared sigmoid curve
sigmoid <- ilogit(slope * (days - location))

# expected mean time to detection
mean_ttd <- exp(intercept_ttd + slope_ttd * sigmoid)

# time to detection likelihood
sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf))
size <- 1 / sqrt(sqrt_inv_size)
prob <- 1 / (1 + mean_ttd / size)
ttd_row_idx <- match(nonneg$date_infection, dates)
ttd_col_idx <- match(nonneg$region, states)
ttd_idx <- cbind(ttd_row_idx, ttd_col_idx)
distribution(nonneg$days_to_detection) <- negative_binomial(size,
                                                            prob[ttd_idx])

m <- model(location, slope,
           intercept_ttd, slope_ttd)

draws <- mcmc(m)
convergence(draws)

nsim <- coda::niter(draws) * coda::nchain(draws)
nsim <- min(10000, nsim)

# posterior predictive checks
ttd_dist <- negative_binomial(size, prob)
sims <- calculate(ttd_dist[ttd_idx], values = draws, nsim = nsim)
ttd_sim <- sims[[1]][, , 1]

# overall PPC check
bayesplot::ppc_ecdf_overlay(
  nonneg$days_to_detection,
  ttd_sim,
  discrete = TRUE
)

# plot model fit by date and state
ttd_samples <- calculate(c(ttd_dist), values = draws, nsim = nsim)[[1]]

nonneg_plot <- nonneg %>%
  mutate(
    state = region,
    type = "Nowcast"
  ) %>%
  filter(date_infection > as.Date("2020-03-01"))

p <- plot_trend(ttd_samples,
                dates,
                base_colour = yellow,
                multistate = TRUE,
                hline_at = NULL,
                ylim = range(nonneg$days_to_detection),
                min_date = as.Date("2020-03-01")) +
  
  geom_point(
    aes(date_infection, jitter(days_to_detection)),
    data = nonneg_plot,
    size = 0.25,
    alpha = 0.1
  )

p


# get probabilities of different numbers of days from symptom onset to detection
ttd_draws <- calculate(ttd_dist[, 1], values = draws, nsim = nsim)[[1]][, , 1]
counts <- apply(ttd_draws, 2, table)
prep <- function(x) {
  x <- as.data.frame(x)
  names(x) <- c("days", "count")
  tibble(x)
}
counts <- lapply(counts, prep)
for (i in seq_along(counts)) {
  counts[[i]]$date <- dates[i]
}

probabilities <- do.call(bind_rows, counts) %>%
  mutate(
    days = case_when(
      as.numeric(days) >= 20 ~ 20,
      TRUE ~ as.numeric(days))
  ) %>%
  full_join(
    expand_grid(
      days = 0:20,
      date = dates,
      count = 0
    )
  ) %>%
  group_by(date, days) %>%
  summarise(count = sum(count)) %>%
  group_by(date) %>%
  mutate(probability = count / sum(count))
