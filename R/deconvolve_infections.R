#calculate admission/infection ratio from hospitalisation and case linelists
source("R/lib.R")
source("R/functions.R")

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


#read in hospitalisation linelist

files <- list.files(
  "../not_synced/nsw/clinical_linelists/",
  pattern = "NSW_out_episode",
  full.names = TRUE
)

dates <- files %>%
  basename() %>%
  substr(17, 26) %>%
  as.Date(format = "%Y_%m_%d")
latest <- which.max(dates)
file <- files[latest]
date <- dates[latest]

nsw_clinic_linelist <- file %>%
  read_xlsx(sheet = 2,range = cell_cols("A:O"),
            col_types = c(
              person_id = "numeric",
              age = "numeric",
              roh_score = "numeric",
              roh3_band_txt = "text",
              load_date = "date",
              VISIT_FACILITY_NAME = "text",
              VISIT_FACILITY_ID = "numeric",
              admit_date = "date",
              discharge_date = "date",
              admit_date_dt = "date",
              discharge_date_dt = "date",
              still_in_hosp = "numeric",
              los_hours = "numeric",
              any_icu_flag = "numeric",
              still_in_icu = "numeric"
            )
  ) %>%    # remove some bogus dates
  mutate(across(
    all_of(c(
      "admit_date",
      "discharge_date"
    )),
    clean_date
  )
  ) %>% select(person_id, age, VISIT_FACILITY_NAME,
               admit_date,
               discharge_date,los_hours)

nsw_clinic_linelist_calculation_period <- nsw_clinic_linelist %>% 
  filter(admit_date < date)

# count daily hospitalisation
nsw_daily_admit <- as.data.frame(table(nsw_clinic_linelist_calculation_period$admit_date),
                                 stringsAsFactors = FALSE) %>% 
  rename(date = Var1, count_admit = Freq) %>% mutate(date = as_date(date)) %>% arrange(date)


# #some quick codes to compare recent linelists
# compare_admit <- left_join(data_imputed,nsw_daily_admit, by = c("date" = "date")) %>% 
#   rename(admission_04 = count_admit) %>% select(date, admissions, admission_04)
# 
# compare_admit <- left_join(compare_admit,nsw_daily_admit, by = c("date" = "date")) %>% 
#   rename(admission_12_21 = count_admit) 

# 
# #plot compare
# compare_admit  %>% 
#   filter(date >= "2021-12-01" & date <= "2022-01-11") %>% 
#   rename("linelist_12_21" = admission_12_21,
#          "linelist_01_04" = admission_04,
#          "linelist_01_11" = admissions) %>% 
#   pivot_longer(
#     cols = -c(date),
#     names_to = "type",
#     values_to = "count"
#   ) %>%
#   ggplot(
#     aes(
#       x = date,
#       y = count,
#       colour = type
#     )
#   ) +
#   geom_line(
#     size = 1
#   ) +
#   theme_minimal()
# 
# ggsave("outputs/figures/admission_linelist_compare.png", bg = 'white')

#get daily infection from local cases input
nsw_daily_infect <- read_csv("outputs/local_cases_input.csv") %>% 
  filter(date_onset < date & date_onset >= min(nsw_daily_admit$date), state == "NSW")


nsw_daily_combined <- left_join(nsw_daily_infect,nsw_daily_admit,by = c("date_onset" = "date"))
#   
#   
#   cbind(nsw_daily_infect,nsw_daily_admit$count_admit) %>% 
#   rename(count_admit = "nsw_daily_admit$count_admit")
# nsw_daily_combined$ratio <- nsw_daily_combined$count/nsw_daily_combined$count_admit
# mean(nsw_daily_combined$ratio)

nsw_daily_combined$date_num <- seq(1,nrow(nsw_daily_combined))




data <- nsw_daily_combined %>% 
  rename(admissions = count_admit,
         infections = count,
         day = date_num,
         date = date_onset) %>% 
  mutate(admissions = ifelse(is.na(admissions),0,admissions)) %>% 
  select(admissions,infections,day,date)

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
    day >= 436 & day <= 456
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
data_imputed  %>% filter(date >= "2021-12-01" & date <= date) %>% 
  pivot_longer(
    cols = -c(day,date,admission_prob),
    names_to = "type",
    values_to = "count"
  ) %>%
  ggplot(
    aes(
      x = date,
      y = count,
      colour = type
    )
  ) +
  geom_line(
    size = 1
  ) +
  theme_minimal()

ggsave("outputs/figures/imputed_case_from_admission.png", bg = 'white')

data_imputed %>% filter(date >= "2021-12-01" & date <= date) %>% 
  pivot_longer(
    cols = -c(day,date,infections,infections_predicted,admission_prob),
    names_to = "type",
    values_to = "count"
  ) %>%
  ggplot(
    aes(
      x = date,
      y = count,
      colour = type
    )
  ) +
  geom_line(
    size = 1
  ) +
  theme_minimal() 

ggsave("outputs/figures/deconvulved_admission.png", bg = 'white')


admission_prob_model






