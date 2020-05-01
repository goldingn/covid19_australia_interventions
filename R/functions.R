library(readr)
library(dplyr)
library(stringr)

# read in and tidy up Facebook movement data
facebook_mobility <- function() {
  
  file <- "data/fb_data/au_gadm_mobility_statistics.20200427.csv"
  data <- readr::read_csv(file) %>%
    dplyr::select(
      state = polygon_name,
      date = ds,
      "staying still" = all_day_ratio_single_tile_users
    ) %>%
    tidyr::pivot_longer(
      cols = c("staying still"),
      names_to = "metric",
      values_to = "trend"
    ) %>%
    mutate(date = lubridate::date(date)) %>%
    mutate(weekday = lubridate::wday(date))  
  
  # set the staying home variable against a baseline of the first two weeks
  baseline <- data %>%
    filter(date < lubridate::date("2020-03-15")) %>%
    group_by(state, metric, weekday) %>%
    summarise(baseline = median(trend))
  
  data <- data %>%
    left_join(baseline) %>%
    mutate(
      corrected = (trend - baseline) / abs(baseline)
    ) %>%
    mutate(
      trend = ifelse(metric == "staying still", corrected, trend)
    ) %>%
    select(
      -corrected,
      -baseline,
      -weekday
      ) %>%
    mutate(
      trend = trend * 100,
    )
  
  # add a composite national trend with population weights
  relative_population <- state_populations() %>%
    arrange(state) %>%
    mutate(fraction = population / sum(population)) %>%
    dplyr::select(-population)
  
  national_data <- data %>%
    left_join(relative_population) %>%
    group_by(date, metric) %>%
    summarise(trend = sum(trend * fraction)) %>%
    ungroup() %>%
    mutate(state = NA)
  
  bind_rows(
    national_data,
    data
  )
  
}

# load and format Jono Carroll's scraping of Aus mobility data
# - remove the grocery and pharmacy category
# (affected by panic buying, not interventions)
google_mobility <- function() {
  # get link from: https://www.google.com/covid19/mobility/index.html?hl=en
  url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  data <- readr::read_csv(url, ) %>%
    filter(country_region == "Australia") %>%
    tidyr::pivot_longer(
      ends_with("_percent_change_from_baseline"),
      names_to = "category",
      values_to = "trend"
    ) %>%
    dplyr::select(
      state = sub_region_1,
      category = category,
      date = date,
      trend = trend
    ) %>%
    mutate(
      category = str_remove_all(category, "_percent_change_from_baseline"),
      category = str_replace_all(category, "_", " ")
    )
  data
}

# download and format Apple's mobility data - will need to update the url regularly
apple_mobility <- function() {
  # get link from: https://www.apple.com/covid19/mobility
  url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev20/v2/en-us/applemobilitytrends-2020-04-27.csv"
  data <- readr::read_csv(url) %>%
    tidyr::pivot_longer(
      cols = starts_with("2020-"),
      names_to = "date",
      values_to = "trend"
    ) %>%
    mutate(
      date = lubridate::date(date)
    ) %>%
    filter(
      region %in% ideal_regions()
    )
  
  # transform to percentage change, and correct dates (they are agreegated
  # midnight-midnight in pacific central time, for which Australian daylight
  # hours fall on the next calendar day in Australia)
  data <- data %>%
    mutate(trend = trend - 100,
           date = date + 1)
  
  # add on a state label, rename the transportation type to 'category' to match
  # google, and add on the data source
  data <- data %>%
    mutate(
      state = case_when(region == "Sydney" ~ "New South Wales",
                        region == "Melbourne" ~ "Victoria",
                        region == "Brisbane" ~ "Queensland",
                        region == "Perth" ~ "Western Australia",
                        TRUE ~ as.character(NA))
    )
  
  data
   
}

# load the Citymapper index (direction requests) for a couple of cities
citymapper_mobility <- function() {
  
  # get link from: https://citymapper.com/cmi/about
  url <- "https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_20200429.csv"
  data <- readr::read_csv(url, skip = 3) %>%
    tidyr::pivot_longer(cols = -Date,
                        names_to = "region",
                        values_to = "trend") %>%
    filter(region %in% ideal_regions()) %>%
    filter(!is.na(trend)) %>%
    rename(date = Date) %>%
    mutate(trend = 100 * (trend - 1),
           state = case_when(
             region == "Sydney" ~ "New South Wales",
             region == "Melbourne" ~ "Victoria",
             region == "Brisbane" ~ "Queensland",
             region == "Perth" ~ "Western Australia",
             TRUE ~ as.character(NA)
           )
    )
  
}

# combine all mobility datasets
all_mobility <- function() {
  
  # load datasets and label their datastreams separately
  google <- google_mobility() %>%
    mutate(
      datastream = str_c("Google: time at ", category)
    ) %>%
    dplyr::select(-category)
  
  apple <- apple_mobility() %>%
    mutate(
      datastream = str_c("Apple: directions for ", transportation_type)
    ) %>%
    dplyr::select(
      -geo_type,
      -region,
      -transportation_type
    )

  # facebook <- facebook_mobility() %>%
  #   mutate(
  #     datastream = str_c("Facebook: ", metric)
  #   ) %>%
  #   dplyr::select(
  #     -metric
  #   )
  
  citymapper <- citymapper_mobility() %>%
    mutate(
      datastream = str_c("Citymapper: directions")
    )
  
  # combine the datasets
  bind_rows(
    google,
    apple,
    # facebook,
    citymapper
  )
  
}

abbreviate_states <- function(state_names) {
  case_when(
    state_names == "Australian Capital Territory" ~ "ACT",
    state_names == "New South Wales" ~ "NSW",
    state_names == "Northern Territory" ~ "NT",
    state_names == "Queensland" ~ "QLD",
    state_names == "South Australia" ~ "SA",
    state_names == "Tasmania" ~ "TAS",
    state_names == "Victoria" ~ "VIC",
    state_names == "Western Australia" ~ "WA"
  )
}

# a list of the regions we'se ideally be interested in. Apple only provides
# data for a handful of these (Australia and the four biggest cities)
ideal_regions <- function() {
  c(
    "Australia",
    "New South Wales",
    "Victoria",
    "Queensland",
    "Western Australia",
    "South Australia",
    "Australian Capital Territory",
    "Tasmania",
    "Northern Territory",
    "Sydney",
    "Melbourne",
    "Brisbane",
    "Perth",
    "Adelaide",
    "Canberra",
    "Hobart",
    "Darwin"
  )
}

intervention_dates <- function() {
  tibble::tribble(~date, ~stage, ~text,
                  "2020-03-16", 1, "public gatherings <= 500 people",
                  "2020-03-24", 2, "venues closed, advised to stay home",
                  "2020-03-29", 3, "public gatherings <= 2 people") %>%
    mutate(date = lubridate::date(date))
}

# dates of public holidays by state, from:
# https://www.australia.gov.au/about-australia/special-dates-and-events/public-holidays
holiday_dates <- function() {
  dplyr::bind_rows(
    tibble::tibble(
      state = "Australian Capital Territory",
      tibble::tribble(~date, ~name,
                      "2020-01-01", "New Year's Day",
                      "2020-01-27", "Australia Day",
                      "2020-03-09", "Canberra Day",
                      "2020-04-10", "Good Friday",
                      "2020-04-11", "Easter Saturday",
                      "2020-04-12", "Easter Sunday",
                      "2020-04-13", "Easter Monday",
                      "2020-04-25", "ANZAC Day",
                      "2020-06-01", "Reconciliation Day",
                      "2020-06-08", "Queen's Birthday",
                      "2020-10-05", "Labour Day",
                      "2020-12-25", "Christmas Day",
                      "2020-12-28", "Boxing Day"
      )
    ),
    tibble::tibble(
      state = "New South Wales",
      tibble::tribble(~date, ~name,
                      "2020-01-01", "New Year's Day",
                      "2020-01-27", "Australia Day",
                      "2020-04-10", "Good Friday",
                      "2020-04-11", "Easter Saturday",
                      "2020-04-12", "Easter Sunday",
                      "2020-04-13", "Easter Monday",
                      "2020-04-25", "ANZAC Day",
                      "2020-06-01", "Reconciliation Day",
                      "2020-06-08", "Queen's Birthday",
                      "2020-08-03", "Bank Holiday",
                      "2020-10-05", "Labour Day",
                      "2020-12-25", "Christmas Day",
                      "2020-12-26", "Christmas Day",
                      "2020-12-28", "Boxing Day (Additional day)"
      )
    ),
    tibble::tibble(
      state = "Northern Territory",
      tibble::tribble(~date, ~name,
                      "2020-01-01", "New Year's Day",
                      "2020-01-27", "Australia Day",
                      "2020-04-10", "Good Friday",
                      "2020-04-11", "Saturday before Easter Sunday",
                      "2020-04-13", "Easter Monday",
                      "2020-04-25", "ANZAC Day",
                      "2020-05-04", "May Day",
                      "2020-06-08", "Queen's Birthday",
                      "2020-08-03", "Picnic Day",
                      "2020-10-05", "Labour Day",
                      "2020-12-24", "Christmas Eve",
                      "2020-12-25", "Christmas Day",
                      "2020-12-28", "Boxing Day",
                      "2020-12-31", "New Year's Eve"
      )
    ),
    tibble::tibble(
      state = "Queensland",
      tibble::tribble(~date, ~name,
                      "2020-01-01", "New Year's Day",
                      "2020-01-27", "Australia Day",
                      "2020-04-10", "Good Friday",
                      "2020-04-11", "Easter Saturday",
                      "2020-04-12", "Easter Sunday",
                      "2020-04-13", "Easter Monday",
                      "2020-04-25", "ANZAC Day",
                      "2020-05-04", "Labour Day",
                      "2020-10-05", "Queen's Birthday",
                      "2020-12-25", "Christmas Day",
                      "2020-12-26", "Boxing Day",
                      "2020-12-28", "Boxing Day (Additional day)"
      )
    ),
    tibble::tibble(
      state = "South Australia",
      tibble::tribble(~date, ~name,
                      "2020-01-01", "New Year's Day",
                      "2020-01-26", "Australia Day",
                      "2020-01-27", "Australia Day (Additional day)",
                      "2020-03-09", "Adelaide Cup Day",
                      "2020-04-10", "Good Friday",
                      "2020-04-11", "Easter Saturday",
                      "2020-04-13", "Easter Monday",
                      "2020-04-25", "ANZAC Day",
                      "2020-06-08", "Queen's Birthday",
                      "2020-10-05", "Labour Day",
                      "2020-12-24", "Christmas Eve",
                      "2020-12-25", "Christmas Day",
                      "2020-12-28", "Proclamation Day",
                      "2020-12-31", "New Year's Eve"
      )
    ),
    tibble::tibble(
      state = "Tasmania",
      tibble::tribble(~date, ~name,
                      "2020-01-01", "New Year's Day",
                      "2020-01-27", "Australia Day",
                      "2020-03-09", "Eight Hours Day",
                      "2020-04-10", "Good Friday",
                      "2020-04-13", "Easter Monday",
                      "2020-04-14", "Easter Tuesday",
                      "2020-04-25", "ANZAC Day",
                      "2020-06-08", "Queen's Birthday",
                      "2020-12-25", "Christmas Day",
                      "2020-12-28", "Boxing Day"
      )
    ),
    tibble::tibble(
      state = "Victoria",
      tibble::tribble(~date, ~name,
                      "2020-01-01", "New Year's Day",
                      "2020-01-27", "Australia Day",
                      "2020-03-09", "Labour Day",
                      "2020-04-10", "Good Friday",
                      "2020-04-11", "Saturday before Easter Sunday",
                      "2020-04-12", "Easter Sunday",
                      "2020-04-13", "Easter Monday",
                      "2020-04-25", "ANZAC Day",
                      "2020-06-08", "Queen's Birthday",
                      "2020-09-25", "Friday before AFL Grand Final",
                      "2020-11-03", "Melbourne Cup",
                      "2020-12-25", "Christmas Day",
                      "2020-12-26", "Boxing Day",
                      "2020-12-28", "Boxing Day (Additional day)"
      )
    ),
    tibble::tibble(
      state = "Western Australia",
      tibble::tribble(~date, ~name,
                      "2020-01-01", "New Year's Day",
                      "2020-01-27", "Australia Day",
                      "2020-03-02", "Labour Day",
                      "2020-04-10", "Good Friday",
                      "2020-04-13", "Easter Monday",
                      "2020-04-25", "ANZAC Day",
                      "2020-04-27", "ANZAC Day",
                      "2020-06-01", "Western Australia Day",
                      "2020-09-28", "Queen's Birthday",
                      "2020-12-25", "Christmas Day",
                      "2020-12-26", "Boxing Day",
                      "2020-12-28", "Boxing Day (Additional day)"
      )
    )
  ) %>%
    mutate(date = lubridate::date(date))
}

# define a latent factor for state-switching behaviour. kappa are lengths of the
# tails for early- and late-adopters; lambda are the relative contribution of
# the triggers; tau are dates of triggers
latent_behaviour_switch <- function(date_num,
                                    tau,
                                    kappa = normal(2, 1, truncation = c(0, Inf), dim = length(tau)),
                                    lambda = uniform(0, 1, dim = length(tau))) {
  
  if (length(tau) == 1) {
    lag <- date_num - tau
    result <- ilogit(lag / kappa)
  } else {
    lambda <- lambda / sum(lambda)
    lags <- outer(date_num, tau, FUN = "-")
    lag_delays <- ilogit(sweep(lags, 2, kappa, FUN = "/"))
    result <- lag_delays %*% lambda
  }
  result
  
}


# define a latent factor for a single symmetric distribution of behavioural events
latent_behavioural_event <- function(date_num, tau, kappa = normal(3, 1, truncation = c(0, Inf)), lambda = 1) {
  # work with logs for numerical stability
  # e <- exp(-lag / kappa)
  # 4 * lambda * e / (1 + e) ^ 2
  lag <- date_num - tau
  le <- -lag / kappa
  log_bump <- log(4 * lambda) + le - 2 * log1pe(le)
  exp(log_bump)
}

# a b-spline constrained to 0-1 for day of the week, with Sunday (day 1)
# fixed at 1 (for identifiability)
latent_spline <- function (days = 1:7, knots = 4) {
  bases <- splines::bs(days, df = knots)
  # for identifiability, make one weight positive. That way the rest can define
  # their sign relative to this, and the loading defines the overall sign
  weights <- normal(0, 1, truncation = c(-Inf, 0), dim = knots)#, 1, dim = knots, truncation = c(-1, 1))
  spline <- bases %*% weights
  spline <- spline - min(spline)
  spline <- spline / max(spline)
  spline
  
}

state_populations <- function() {
  tibble::tribble(
    ~state, ~population,
    "Australian Capital Territory", 426709,
    "New South Wales", 8089526,
    "Northern Territory", 245869,
    "Queensland", 5095100,
    "South Australia", 1751693,	
    "Tasmania", 534281,
    "Victoria", 6594804,
    "Western Australia", 2621680
  )
}

posterior_sims <- function(vector, draws, nsim = NULL) {
  if (is.null(nsim)) {
    nsim <- coda::niter(draws) * coda::nchain(draws)
  }
  calculate(vector, values = draws, nsim = nsim)[[1]][, , 1]
}

# summarise the posterior for a vector greta array
summarise_vec_posterior <- function(vector,
                                    draws,
                                    quantiles = c(0.025, 0.975),
                                    nsim = 1000) {
  vector_sim <- posterior_sims(vector, draws, nsim = nsim)[, , 1]
  posterior_mean <- colMeans(vector_sim)
  posterior_ci <- t(apply(vector_sim, 2, quantile, quantiles))
  cbind(mean = posterior_mean, posterior_ci)
}

# add a polygon for a credible interval to a base plot
add_mean_ci <- function(posterior_summary,
                        dates,
                        col = grey(0.8),
                        border_col = grey(0.6),
                        line_col = grey(0.4),
                        lwd = 3) {
  polygon(x = c(dates, rev(dates)),
          y = c(posterior_summary[, 2],
                rev(posterior_summary[, 3])),
          lwd = 0.5,
          col = col,
          border = border_col)
  lines(posterior_summary[, 1] ~ dates,
        lwd = lwd,
        col = line_col)
}

add_gridlines <- function(key_dates, vertical = TRUE, horizontal = TRUE) {
  if (horizontal) {
    abline(h = 0, col = grey(0.4), lty = 3)
  }
  if (vertical) {
    abline(v = key_dates, col = grey(0.6), lwd = 2)
  }
}

# colours for plotting
pal <- function(colour = "green") {
  
  # look up the base colour
  brewer_cols <- brewer.pal(8, "Set2")
  # display.brewer.pal(8, "Set2")
  colour_names <- c(
    "green",
    "red",
    "blue",
    "pink",
    "bright green",
    "yellow",
    "beige",
    "grey"
  )
  idx <- match(colour, colour_names)
  base_colour <- brewer_cols[idx]
  
  # create a four-colour palette based on this colour
  pal <- colorRampPalette(c("white", base_colour, "black"))(10)
  pal[c(4, 5, 6, 7)]
  
}

plot_latent_factor <- function (factor, draws, dates, key_dates, cols = grey(c(0.9, 0.7, 0.5, 0.3)), title = "") {
  
  est <- summarise_vec_posterior(factor, draws)
  plot(est[, 1] ~ dates,
       type = "n",
       ylim = c(0, 1),
       ylab = "relative effect",
       xlab = "",
       las = 1)
  add_gridlines(key_dates, horizontal = FALSE)
  add_mean_ci(est,
              dates,
              col = cols[1],
              border_col = cols[2],
              line_col = cols[3])
  title(main = title,
        col.main = grey(0.3))
}

# greta function for the lognormal CDF (equivalent to plnorm(x, meanlog, sdlog))
# x must not be a greta array, though meanlog and sdlog can be. A greta array of
# CDF values is returned, equal to 0 wherex was 0 or lower.
lognormal_cdf <- function(x, meanlog, sdlog) {
  
  # filter out any invalid dates, to replace their CDF with 0
  valid <- x > 0
  x <- x[valid]
  
  p <- iprobit((log(x) - meanlog) / sdlog)
  
  # if any were missing, replace their cumulative density with 0
  if (any(!valid)) {
    result <- zeros(length(valid))
    result[valid] <- p
    p <- result
  }
  
  p
  
}

exponential_cdf <- function (x, rate) {
  
  # filter out any invalid dates, to replace their CDF with 0
  valid <- x > 0
  x <- x[valid]
  
  p <- 1 - exp(-rate * x)
  
  # if any were missing, replace their cumulative density with 0
  if (any(!valid)) {
    result <- zeros(length(valid))
    result[valid] <- p
    p <- result
  }
  
  p
  
}

# Given samples 'x' of a parameter, approximate the distribution by a (possibly
# truncated) normal distribution, and return a variable greta array following
# that distribution
parameter <- function(x, truncation = c(-Inf, Inf)) {
  normal(mean(x), sd(x), truncation = truncation)
}

# given a positive integer vector of (days since a case became symptomatic,
# compute the probability that the serial interval values falls in that day
serial_interval_probability <- function(days, fixed = FALSE) {
  
  # priors for the parameters of the lognormal distribution over the serial interval from Nishiura et
  # al., as stored in the EpiNow source code 
  si_param_samples <- read_csv(
    file = "https://raw.githubusercontent.com/epiforecasts/EpiNow/758b706599244a545d6b07f7be4c10ffe6c8cf50/data-raw/nishiura-lognormal-truncated.csv",
    col_types = cols(param1 = col_double(),
                     param2 = col_double())
  )
  
  # compute the CDF of the lognormal distribution, for this and the next day
  days_lower <- days
  days_upper <- days + 1
  
  if (fixed) {
    # if fixed, we can do this in R
    meanlog <- mean(si_param_samples$param1)
    sdlog <- mean(si_param_samples$param2)
    
    p_lower <- plnorm(days_lower, meanlog, sdlog)
    p_upper <- plnorm(days_upper, meanlog, sdlog)
    
  } else {
    # otherwise create greta arrays for the parameters, with priors based on
    # these samples
    meanlog <- parameter(si_param_samples$param1)
    sdlog <- parameter(si_param_samples$param2, c(0, Inf))
    
    p_lower <- lognormal_cdf(days_lower, meanlog, sdlog)
    p_upper <- lognormal_cdf(days_upper, meanlog, sdlog)
  }
  

  # get the integral of the density over this day
  p_upper - p_lower
  
}

# given a positive integer vector (of days between a case becoming symptomatic
# and being reported) compute the probability that the delay falls in that day
delay_probability <- function(days) {
  
  # define the parameter of the exponential distribution via samples provided by
  # David
  delay_samples <- read_csv("data/cases/sampled_report_delay.csv",
                            col_types = cols(x = col_double())) %>%
    pull(x)

  rate <- lognormal(0, 1)
  distribution(delay_samples) <- exponential(rate)
  
  # compute the CDF of the lognormal distribution, for this and the next day
  days_lower <- days
  days_upper <- days + 1
  
  # get the integral of the density over this day
  p_upper <- exponential_cdf(days_upper, rate)
  p_lower <- exponential_cdf(days_lower, rate)
  p_upper - p_lower
  
}

fake_linelist <- function() {
  
  library(readr)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  
  # create a synthetic linelist from data provided by David and Freya
  cases <- read_csv(
    file = "data/cases/cases.csv",
    col_types = cols(
      date = col_date(format = ""),
      import_status = col_character(),
      region = col_character(),
      cases = col_double()
    )
  ) %>%
    mutate(
      date = ymd(date),
      import_status = recode(
        import_status, 
        "Unknown origin" = "local",  # conservative approach assuming all unknown cases are locally acquired
        "Locally acquired" = "local",
        "Overseas acquired" = "imported"
      )
    ) %>% 
    group_by(date, region, import_status) %>%
    tally(wt = cases) %>%
    rename(cases = n) %>%
    ungroup()
  
  # Load gamma distributed reporting delays
  delay_samples <- read_csv(
    file = "data/cases/sampled_report_delay.csv",
    col_types = cols(x = col_integer())
  ) %>%
    pull(x)
  
  # Create a "linelist" from the data, as input into EpiNow::regional_rt_pipeline
  linelist <- cases %>%
    group_by(date, import_status, region) %>% 
    expand(count = seq(1:cases)) %>%
    select(-count) %>% 
    ungroup() %>% 
    mutate(
      report_delay = sample(
        x = delay_samples,
        size = nrow(.),
        replace = TRUE)
    ) %>% 
    mutate(date_onset = ymd(date) - report_delay) %>% 
    rename(date_confirmation = date)
  
  # throw away some of the true onset dates
  n_cases <- nrow(linelist)
  remove <- sample.int(n_cases, floor(n_cases * 0.06))
  linelist$date_onset[remove] <- NA
  
  linelist
  
}

# given a positive integer 'n_days' of the number of days for which to compute
# values and a discrete vector 'disaggregation_probs' of probabilities that data
# for one day should actually be assigned that number of days into the future,
# return a symmetric matrix that can be used in a matrix multiply to
# disaggregate daily data into expected counts on future days, according to that
# probability distribution. Doing this disaggregation using a circulant matrix
# of probabilities with masked lower values, is more efficient in greta than
# looping since it can easily be parallelised. Note this is the same operation
# as cases_known_outcome_matrix() in goldingn/australia_covid_ascertainment
disaggregation_matrix <- function (n_days, disaggregation_probs) {

  # build an upper-triangular circulant matrix (contribution of each day's cases
  # to each other's)
  mat <- matrix(0, n_days, n_days)
  
  # if we're doing this with a greta array, mat must be coerced to on in advance
  if (inherits(disaggregation_probs, "greta_array")) {
    mat <- as_data(mat)
  }
  
  indices <- row(mat) - col(mat) + 1
  mask <- indices > 0 & indices <= length(disaggregation_probs)
  mat[mask] <- disaggregation_probs[indices[mask]]
  mat
  
}

# apply the serial interval a date-by-state matrix 'cases' of case counts to get
# the expected number of infectious people at each time. If 'fixed', use a
# deterministic serial interval distribution base don prior means, othrwise
# treeat the parameters of the distribution as unknown parameters, to account
# for uncrtainty in the distribution.
apply_serial_interval <- function(cases, fixed = FALSE) {
  
  # get vector of probabilities (either an R or a greta array, based on 'fixed')
  probabilities <- serial_interval_probability(0:45, fixed = fixed)
  
  # get a square matrix of contributions of each date to each other, and
  # matrix-multiply to disaggregate
  si_disaggregation <- disaggregation_matrix(nrow(cases), probabilities)
  si_disaggregation %*% cases
  
}

# given the eman and standard deviation of a lognormal distribution, compute the
# parameters (mean and standard deviation of the normal distribution over the
# variable's log)
lognormal_prior <- function(mean, sd) {
  
  var <- sd ^ 2
  list(
    mean = log((mean ^ 2) / sqrt(var + mean ^ 2)),
    sd = sqrt(log(1 + var / (mean ^ 2)))
  )
  
}

tf_as_float <- greta:::tf_as_float
fl <- greta:::fl

greta_kernel <- greta.gp:::greta_kernel
tf_cols <- greta.gp:::tf_cols
tf_distance <- greta.gp:::tf_distance
check_active_dims <- greta.gp:::check_active_dims


iid <- function(variance, columns = 1) {
  greta_kernel("iid",
               tf_name = "tf_iid",
               parameters = list(variance = variance),
               arguments = list(
                 active_dims = check_active_dims(columns,
                                                 rep(1, length(columns))
                 )))
}

tf_iid <- function(X, X_prime, variance, active_dims) {
  
  # pull out active dimensions
  X <- tf_cols(X, active_dims)
  X_prime <- tf_cols(X_prime, active_dims)
  
  # find where these values match and assign the variance as a covariance there
  # (else set it to 0)
  distance <- tf_distance(X, X_prime)
  tf_as_float(distance < fl(1e-12)) * variance

}

R0_prior <- function() {
  # Visually estimated values of the posterior distributions  (means and lower
  # and upper 95% CIs) over R0 in 11 European countries from Flaxman et al.
  # (Imperial report 13). Rather than use the estimates in the main text, we
  # usee those fromm the SI under the shortest serial interval - a mean of 5
  # days, corresponding to a median of 4.49 days. Note, we assumed based on
  # Figure 7 that the SI gamma distribution parameter values given are the mean
  # and the rate parameter of the distribution), since no other combination of
  # mean, sd, shape, scale, or rate could be found to match the figure closely.
  # We use this lower estimate because it is closest to the estimate of Nishiura
  # et al. of a median serial interval of 4 days, or 4.6 days taking only the
  # pairs of cases with greatest confidence. This is also the most conseervative
  # eestimate of R0 and therefore of the impact of social distancing interventions
  prior_estimate <- tibble::tribble(
    ~country, ~lower, ~mean, ~upper,
    "Denmark", 2, 2.6, 3.3,
    "Italy", 2.4, 2.6, 2.8,
    "Germany", 2.5, 3, 3.75,
    "Spain", 2.7, 3.2, 3.6,
    "United Kingdom", 2.4, 2.6, 3.1,
    "France", 2.5, 2.8, 3.2,
    "Norway", 1.7, 2.4, 3.3,
    "Belgium", 2.4, 2.9, 3.7,
    "Austria", 2.3, 2.7, 3.7,
    "Sweden", 2.3, 2.8, 3.6,
    "Switzerland", 2.3, 2.7, 3.4
  ) %>%
    summarise(lower = mean(lower),
              mean = mean(mean),
              upper = mean(upper))
  
  # to find a lognormal prior that matches this, find the parameters of a
  # lognormal distribution that give the minimum mean squared error against
  # these estiamtes
  obj <- function(par) {
    meanlog <- par[1]
    sdlog <- exp(par[2])
    mean <- exp(meanlog + sdlog ^ 2 / 2)
    cis <- qlnorm(c(0.025, 0.975), meanlog, sdlog)
    mean_error <- mean - prior_estimate$mean
    ci_error <- cis - c(prior_estimate$lower, prior_estimate$upper)
    mse <- mean(mean_error ^ 2 + ci_error[1] ^ 2 + ci_error[2] ^ 2)
    mse
  }
  
  par <- optim(par = c(0, 0), obj)$par
  
  list(meanlog = par[1], sdlog = exp(par[2]))
  
}

plot_trend <- function(simulations,
                       base_colour = grey(0.4),
                       multistate = FALSE,
                       hline_at = 1,
                       ylim = c(0, 3),
                       vline_at = NA,
                       keep_only_rows = NULL) {
  
  library(ggplot2)
  
  mean <- colMeans(simulations)
  ci_90 <- apply(simulations, 2, quantile, c(0.05, 0.95))
  ci_50 <- apply(simulations, 2, quantile, c(0.25, 0.75))
  
  if (multistate) {
    dates <- rep(dates, n_states)
    states <- rep(states, each = n_dates)
  } else {
    states <- NA
  }
  
  df <- tibble(date = dates,
               state = states,
               mean = mean,
               ci_50_lo = ci_50[1, ],
               ci_50_hi = ci_50[2, ],
               ci_90_lo = ci_90[1, ],
               ci_90_hi = ci_90[2, ])
  
  if (!is.null(keep_only_rows)) {
    df <- df[keep_only_rows, ]
  }
  
  df <- df %>%
    filter(date >= as.Date("2020-03-01")) %>%
    mutate(type = "Nowcast")
  
  if (is.null(ylim)) {
    ylim <- c(min(df$ci_90_lo), max(df$ci_90_hi)) 
  }
  
  p <- ggplot(df) + 
    
    aes(date, mean, fill = type) +
    
    xlab(element_blank()) +
    
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    geom_vline(xintercept = vline_at, colour = "grey80") +
    
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
    

    geom_hline(yintercept = hline_at, linetype = "dotted") +
    
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90))
  
  if (multistate) {
    p <- p + facet_wrap(~ state, ncol = 2, scales = "free")
  }
  
  p    
  
}


# interpolate proportion of infectious cases that are imports
proportion_imported <- function (local_infectious, imported_infectious, X) {
  
  all_infectious <- local_infectious + imported_infectious
  prop_imported <- imported_infectious / all_infectious
  
  # transform to vector on unconstrained scale
  prop_imported <- pmax(0.01, prop_imported)
  prop_imported <- pmin(0.99, prop_imported)
  q_imported <- c(qlogis(prop_imported))
  
  # interpolate state-by-state  
  df <- as.data.frame(X)
  df$state <- factor(df$state)
  model <- mgcv::gam(response_q ~ s(date) +
                       s(date, by = state, k = 30),
                     data = df)
  q_imported <- predict(model, newdata = df)
  
  # transform back and return
  p_imported <- plogis(q_imported)
  dim(p_imported) <- dim(all_infectious)
  p_imported
  
}
