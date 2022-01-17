library(tidyverse)
library(surveillance)

get_pmf_infection_admission <- function(file = "data/nsw_onset_to_admission.csv") {

  # get the probability mass function of the delay distribution from infection to
  # admission, for integer days starting at 0
  pmf <- file %>%
    read_csv(
      col_types = cols(
        Var1 = col_double(),
        Freq = col_double()
      )
    ) %>%
    rename(
      delay = Var1
    ) %>%
    arrange(
      delay
    ) %>%
    mutate(
      delay = delay + 5
    ) %>%
    filter(
      delay >= 0
    ) %>%
    mutate(
      prob = Freq / sum(Freq)
    ) %>%
    pull(
      prob
    )

}

deconvolve <- function(timeseries, pmf, verbose = TRUE, truncate = 6) {

  timeseries_sts <- sts(timeseries)

  args <- list(
    k = 4,
    eps = rep(0.005, 2),
    iter.max = rep(250, 2),
    B = -1,
    eq3a.method = "C",
    verbose = verbose
  )

  backproj <- backprojNP(
    sts = timeseries_sts,
    incu.pmf = pmf,
    control = args
  )

  deconvolved <- upperbound(backproj)[, 1]

  # censor the last 5 days of estimates (dips down with increasing trend)

  n <- length(deconvolved)
  dodgy <- (n - truncate):n
  deconvolved[dodgy] <- NA

  # and round before returning
  round(deconvolved)

}

shift <- function(timeseries, pmf) {

  fixed_delay <- round(sum((seq_along(pmf) - 1) * pmf))
  n <- length(timeseries)

  c(timeseries[(fixed_delay + 1):n], rep(NA, fixed_delay))

}

# simulate delays
r_delay <- function(n) {
  sample(
    seq_along(pmf) - 1,
    size = n,
    replace = TRUE,
    prob = pmf
  )
}

# simulate fake infeciton and admission data
sim_data <- function(pmf, n_days = 100) {

  # simulate exponential daily infections
  days <- seq_len(n_days)
  infections <- tibble(
    day = days,
    cases = rpois(n_days, exp(5 + 0.3 * days - 0.003 * days ^ 2))
  )
  
  # simulate admissions from true distribution
  admissions <- infections %>%
    # sample some number of admiissions with a time-varying admission probability
    mutate(
      admission_prob = plogis(-2 +  -0.025 * day),
      # cases = rbinom(n(), cases, admission_prob)
      cases = round(cases * admission_prob)
    ) %>%
    # time-shift them
    uncount(
      cases
    ) %>%
    mutate(
      day = day + r_delay(n())
    ) %>%
    group_by(
      day
    ) %>%
    summarise(
      cases = n()
    ) %>%
    complete(
      day = 1:max(day),
      fill = list(cases = 0)
    ) %>%
    # remove those not yet hospitalised
    filter(
      day <= n_days
    )

  data <- admissions %>%
    rename(
      admissions = cases
    ) %>%
    full_join(
      infections,
      by = "day"
    ) %>%
    rename(
      infections = cases
    )

  data

}

# simulation estimation check

# load pmf
pmf <- get_pmf_infection_admission()

# use it to simulate data
#data <- sim_data(pmf, n_days = 40)

data <- nsw_daily_combined %>% 
  rename(admissions = count_admit,
         infections = count,
         day = date_num) %>% select(admissions,infections,day)

# shift back the admissions
data_shifted <- data %>%
  mutate(
    admissions_deconvolved = deconvolve(admissions, pmf),
    admissions_shifted = shift(admissions, pmf)
  )

# learn the time-varying admission probabilities from a smaller window
# hospitalisation rate will be a mix of two, for two different variants
admission_prob_model <- data_shifted %>%
  filter(
    day >= 5 & day <= 20
  ) %>%
  glm(
    cbind(admissions_deconvolved, infections - admissions_deconvolved)  ~ day,
    family = stats::binomial,
    data = .
  )

# predict the infections
data_imputed <- data_shifted %>%
  mutate(
    admission_prob = predict(
      admission_prob_model,
      newdata = .,
      type = "response"
    ),
    infections_predicted = round(admissions_deconvolved / admission_prob)
  )

# plot to compare
data_imputed %>%
  pivot_longer(
    cols = -day,
    names_to = "type",
    values_to = "count"
  ) %>%
  ggplot(
    aes(
      x = day,
      y = count,
      colour = type
    )
  ) +
  geom_line(
    size = 1
  ) +
  theme_minimal()



admission_prob_model






