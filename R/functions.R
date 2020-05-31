library(readr)
library(dplyr)
library(stringr)
library(rjson)
library(tidyr)
library(greta)
library(readxl)

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
  # get link from: https://www.google.com/covid19/mobility/index.html
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
  url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2008HotfixDev40/v3/en-us/applemobilitytrends-2020-05-22.csv"
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

  # pull out the states and cities
  states <- c("New South Wales", "Victoria", "Queensland", "Western Australia", 
              "South Australia", "Australian Capital Territory", "Tasmania", 
              "Northern Territory")
  
  cities <- c("Sydney", "Melbourne", "Brisbane", "Perth", 
              "Adelaide", "Canberra", "Hobart", "Darwin")
  
  # need to use state-level data for driving, but cities for other metrics
  data <- data %>%
    filter(
      case_when(
        transportation_type == "driving" & region %in% states ~ TRUE,
        transportation_type != "driving" & region %in% cities ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    mutate(
      region = case_when(
        region == "Sydney" ~ "New South Wales",
        region == "Melbourne" ~ "Victoria",
        region == "Brisbane" ~ "Queensland",
        region == "Perth" ~ "Western Australia",
        region == "Adelaide" ~ "South Australia",
        region == "Hobart" ~ "Tasmania",
        region == "Canberra" ~ "Australian Capital Territory",
        region == "Darwin" ~ "Northern Territory",
        TRUE ~ region
      )
    ) %>%
    rename(state = region)
  
  data
   
}

# load the Citymapper index (direction requests) for a couple of cities
citymapper_mobility <- function() {
  
  # get link from: https://citymapper.com/cmi/about
  url <- "https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_20200524.csv"
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
  vector_sim <- posterior_sims(vector, draws, nsim = nsim)
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

gamma_cdf <- function(x, shape, rate) {
  stop("not yet implemented")
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

# given a positive integer vector of days (since an infection began), compute
# the probability that the generation interval values falls in that day
generation_interval_probability <- function(days, fixed = FALSE) {
  
  # priors for the parameters of the lognormal distribution over the serial interval from Nishiura et
  # al., as stored in the EpiNow source code 
  
  download.file(
    "https://github.com/epiforecasts/EpiNow/raw/master/data-raw/gi.rds",
    (f <- tempfile()),
    quiet = TRUE
  )
  gi_param_samples <- readRDS(f)
  
  # compute the CDF of the lognormal distribution, for this and the next day
  days_lower <- days
  days_upper <- days + 1
  
  if (fixed) {
    
    # if fixed, we can do this in R
    mean <- mean(gi_param_samples$mean)
    sd <- mean(gi_param_samples$sd)
    scale <- sd ^ 2 / mean
    shape <- mean / scale
    
    p_lower <- pgamma(days_lower, shape = shape, scale = scale)
    p_upper <- pgamma(days_upper, shape = shape, scale = scale)
    
  } else {
    # otherwise create greta arrays for the parameters, with priors based on
    # these samples
    scale_draws <- gi_param_samples$sd ^ 2 / gi_param_samples$mean
    shape_draws <- gi_param_samples$mean / scale_draws
    rate_draws <- 1 / scale_draws
    
    rate <- parameter(rate_draws, c(0, Inf))
    shape <- parameter(shape_draws, c(0, Inf))
    
    p_lower <- gamma_cdf(days_lower, shape, rate)
    p_upper <- gamma_cdf(days_upper, shape, rate)
  }
  
  # get the integral of the density over this day
  p_upper - p_lower
  
}

# given a positive integer vector of days (since a case became symptomatic),
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

apply_generation_interval <- function(cases, fixed = FALSE) {
  
  # get vector of probabilities (either an R or a greta array, based on 'fixed')
  probabilities <- generation_interval_probability(0:45, fixed = fixed)
  
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
                       dates,
                       base_colour = grey(0.4),
                       multistate = FALSE,
                       hline_at = 1,
                       ylim = c(0, 3),
                       vline_at = NA,
                       vline2_at = NA,
                       keep_only_rows = NULL) {
  
  library(ggplot2)
  
  mean <- colMeans(simulations)
  ci_90 <- apply(simulations, 2, quantile, c(0.05, 0.95))
  ci_50 <- apply(simulations, 2, quantile, c(0.25, 0.75))
  
  if (multistate) {
    states <- rep(states, each = length(dates))
    dates <- rep(dates, n_states)
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
    scale_x_date(date_breaks = "1 months", date_labels = "%b %d") +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    geom_vline(xintercept = vline_at, colour = "grey80") +
    geom_vline(xintercept = vline2_at, linetype = "dashed", colour = "grey60") +
    
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
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"))
  
  if (multistate) {
    p <- p + facet_wrap(~ state, ncol = 2, scales = "free")
  }
  
  if (!is.na(vline2_at)) {
    p <- p +
      annotate("rect",
             xmin = vline2_at,
             xmax = max(df$date),
             ymin = -Inf,
             ymax = Inf,
             fill = grey(0.5), alpha = 0.1) +
      geom_text(aes(x = vline2_at, y = 3, label = "projection"),
                hjust = -0.1, size = 3, colour = grey(0.6))
  }
  
  p    
  
}


# interpolate proportion of infectious cases that are imports
proportion_imported <- function (local_infectious, imported_infectious) {
  
  all_infectious <- local_infectious + imported_infectious
  prop_imported <- imported_infectious / all_infectious
  
  # transform to vector on unconstrained scale
  prop_imported <- pmax(0.01, prop_imported)
  prop_imported <- pmin(0.99, prop_imported)
  q_imported <- c(qlogis(prop_imported))
  
  n_dates <- nrow(all_infectious)
  n_states <- ncol(all_infectious)
  date_nums <- seq_len(n_dates)
  states <- factor(seq_len(n_states))
  # interpolate state-by-state  
  df <- data.frame(
    date = rep(date_nums, n_states),
    state = rep(states, each = n_dates)
  )
  model <- mgcv::gam(q_imported ~ s(date) +
                       s(date, by = state, k = 30),
                     data = df)
  q_imported <- predict(model, newdata = df)
  
  # transform back, clamp to numerically stable values, and return
  p_imported <- plogis(q_imported)
  eps <- sqrt(.Machine$double.eps)
  p_imported <- pmax(eps, p_imported)
  p_imported <- pmin(1 - eps, p_imported)
  dim(p_imported) <- dim(all_infectious)
  p_imported
  
}

app_downloads <- function () {
  tibble::tribble(
    ~date, ~number,
    "04-25", 0,
    "04-26", 1113000,
    "04-27", 2e6,
    "04-28", 2440000,
    "04-29", 3e6,
    "04-30", 3310000,
    "05-01", 3500000,
    "05-02", 4000000,
    "05-03", 4250000,
    "05-05", 4950000,
    "05-06", 5100000
  ) %>%
    mutate(
      date = paste0("2020-", date),
      date = as.Date(date),
      uptake = number / 16e6
    )
}

as.greta_array <- greta:::as.greta_array

# copy of greta.gp::gp with v manually passed in, enabling different
# (hierarchical) variance on each gp draw
multi_gp <- function (x, v, kernel, inducing = NULL, tol = 1e-04) {
  
  sparse <- !is.null(inducing)
  
  x <- as.greta_array(x)
  
  if (!sparse)
    inducing <- x
  else
    inducing <- as.greta_array(inducing)
  
  # calculate key objects
  Kmm <- kernel(inducing)
  
  m <- nrow(v)
  
  if (!identical(tol, 0))
    Kmm <- Kmm + diag(m) * tol
  
  Lm <- t(chol(Kmm))
  
  # evaluate gp at x
  if (sparse) {
    
    Kmn <- kernel(inducing, x)
    A <- forwardsolve(Lm, Kmn)
    f <- t(A) %*% v
    
  } else {
    
    f <- Lm %*% v
    
  }
  
  # add the info to the greta array
  attr(f, "gp_info") <- list(kernel = kernel,
                             inducing = inducing,
                             v = v,
                             Lm = Lm)
  f
}

get_tests <- function() {
  
  # scrape data on daily numbers of tests by state from covid19data.com.au
  url <- "https://e.infogram.com/_/3osqzRmYBiJsJafg79YC?parent_url=https%3A%2F%2Fwww-covid19data-com-au.filesusr.com%2Fhtml%2F2aed08_944ecbfd558f24812177bca5a8a74000.html&src=embed"
  
  text <- readLines(url) %>%
    paste(collapse = "\n")
  
  start <- str_locate(text, "window.infographicData=")[2] + 1
  text <- substr(text, start, 1000000L)
  end <- str_locate(text, ";</script>")[1]
  text <- substr(text, start = 1, stop = end)
  json <- rjson::fromJSON(text)
  
  data_lines <- json$elements[[2]]$data[[1]]
  states <- data_lines[[1]][-1]
  dates <- vapply(data_lines[-1], `[`, 1, FUN.VALUE = character(1))
  dates <- paste0(dates, "/2020")
  dates <- as.Date(dates, format = "%d/%m/%Y")
  
  values_text <- sapply(data_lines[-1], `[`, -1)
  values_text <- gsub(",", "", values_text)
  values <- as.numeric(values_text)
  values <- matrix(values, ncol = length(states), byrow = TRUE)
  colnames(values) <- states
  
  df <- values %>%
    as.data.frame() %>%
    cbind(date = dates) %>%
    pivot_longer(cols = -date,
                 names_to = "state",
                 values_to = "cumulative_tests") %>%
    group_by(state) %>%
    arrange(date) %>%
    mutate(daily_tests = c(NA, diff(cumulative_tests)),
           daily_tests = pmax(0, daily_tests))
  
  df
}

# weighted mean and standard error of the weighted mean, computed with a
# bootstrap
weighted_mean <- weighted.mean

weighted_se <- function(x, mu, w, na.rm = TRUE) {
  x_sims <- replicate(10000,
                      sample(x, length(x), replace = TRUE),
                      simplify = FALSE)
  means <- vapply(x_sims,
                  weighted_mean,
                  w,
                  na.rm = na.rm,
                  FUN.VALUE = numeric(1))
  sd(means)
}

# get a (mean and se) number of total contacts at baseline that is comparable to
# the numbers in Freya's survey from Prem/polymod (similar survey methodology)
baseline_total_contacts <- function() {
  
  # load Prem contact matrix for Australia
  f <- "data/contacts/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx"
  all_aus <- readxl::read_xlsx(
    path = f,
    sheet = "Australia"
  ) %>%
    as.matrix()
  
  # load Australian population data
  pop <- read_csv(
    file = "data/contacts/ERP_QUARTERLY_20052020195358149.csv",
    col_types = cols(
      MEASURE = col_double(),
      Measure = col_character(),
      STATE = col_double(),
      State = col_character(),
      SEX_ABS = col_double(),
      Sex = col_character(),
      AGE = col_character(),
      Age = col_character(),
      FREQUENCY = col_character(),
      Frequency = col_character(),
      TIME = col_character(),
      Time = col_character(),
      Value = col_double(),
      `Flag Codes` = col_logical(),
      Flags = col_logical()
    )
  ) %>%
    filter(
      Measure == "Estimated Resident Population",
      Sex == "Persons",
      State == "Australia",
      Time == "Sep-2019",
      Age != "All ages"
    ) %>%
    select(age = Age, pop = Value) %>%
    mutate(age = as.numeric(age)) %>%
    arrange(age) %>%
    mutate(
      age_bin = cut(age, seq(0, 100, by = 5), include.lowest = TRUE, right = FALSE)
    ) %>%
    group_by(age_bin) %>%
    summarise(
      min = min(age),
      max = max(age),
      pop = sum(pop)
    )
  
  # get the age-binned population represented by the survey (of adults)
  survey_pop <- pop %>%
    group_by(age_bin) %>%
    mutate(
      fraction = mean((min:max) >= 18),
      weighted_pop = pop * fraction
    ) %>%
    ungroup() %>%
    filter(min < 80)
  
  mean_wt_aus <- weighted_mean(colSums(all_aus), survey_pop$weighted_pop)
  
  # get standard error of the total number of contacts in the UK arm of polymod,
  # as a measure of prior uncertainty in the Australian estimate of total
  # contacts
  download.file("https://raw.githubusercontent.com/goldingn/comix_covid-19-first_wave/master/data/polymod_contacts_part.rds",
                (f <- tempfile()))
  
  standard_error_uk <- readRDS(f) %>%
    filter(part_age_group != "[0,18)") %>%
    group_by(part_id) %>%
    summarise(
      contacts = n_distinct(cont_id)
    ) %>%
    ungroup() %>%
    summarise(
      se = sd(contacts) / sqrt(n())
    ) %>%
    pull(se)
  
  tibble::tibble(
    mean = mean_wt_aus,
    se = standard_error_uk
  )
  
}

# get the numbers of household contacts and distribution among types of
# locations from Rolls et al.
rolls_contact_data <- function() {
  
  # load data on encounters of individuals and format
  individuals <- read_csv(
    file = "data/contacts/covid_full.csv",
    col_types = cols(
      participant_id = col_double(),
      contact_id = col_character(),
      contact_duration = col_double(),
      location_duration = col_double(),
      hh_member = col_double(),
      location_code = col_double(),
      location_type = col_character(),
      weight = col_double()
    ),
    na = c("NULL", "", "NA")
  ) %>%
    select(-location_duration, -location_code) %>%
    mutate(location = case_when(
      location_type == "Home" ~ "home",
      location_type == "Retail and hospitality (bars, cafes, shops, hair dressing, etc.)" ~ "retail",
      location_type == "Public spaces (parks, streets, stations, airports etc.)" ~ "public",
      location_type == "Public Transport (train, tram, bus or taxi)" ~ "transit",
      location_type == "Work" ~ "work",
      TRUE ~ "other",
    ))
  
  # convert encounters to numbers and total durations of unique contacts in each
  # locationand contact type
  contact_data <- individuals %>%
    # this encounter as a proportion of all encounters with this contact
    group_by(participant_id, contact_id) %>%
    mutate(proportion = 1 / n()) %>%
    # sum proportions and durations in each location, using the average duration with that contact where it's missing
    group_by(participant_id, contact_id, hh_member, location, weight) %>%
    summarise(
      proportion = sum(proportion),
      contact_duration = mean(contact_duration, na.rm = TRUE) / n(),
    ) %>%
    # count (proportional) unique contacts and average durations in household/non
    # household in each location for each participant
    group_by(participant_id, hh_member, location, weight) %>%
    summarise(
      contacts = sum(proportion),
      contact_duration = mean(contact_duration, na.rm = TRUE)
    ) %>%
    # convert duration to hours
    mutate(contact_duration = contact_duration / (60)) %>%
    ungroup() %>%
    mutate(
      hh_member = ifelse(hh_member == 1,
                         "household",
                         "non_household")
    ) %>%
    group_by(weight) %>%
    # expand out to include 0s for different categories, to averages are unbiased
    complete(participant_id, hh_member, location, fill = list(contacts = 0)) %>%
    arrange(participant_id, hh_member, location)
  
  contact_data
  
}


baseline_contact_parameters <- function() {
  
  # mean duration of infection in days
  infectious_days <- infectious_period()
  
  # get the average number and duration contacts by household/non-household
  baseline_contact_params <- rolls_contact_data() %>%
    # contact duration in hours per day, multiply by infectious days *for
    # household contacts only* to get number of contact hours of entire
    # infectious period. For non-household contacts, we can assume they are
    # different individuals so transmission can exceed contacts, so multiply
    # infectious days by the daily R
    mutate(
      contact_duration = contact_duration * ifelse(hh_member == "household",
                                                   infectious_days,
                                                   1)
    ) %>%
    # summarise number and duration of household/non-household contacts per
    # respondent
    group_by(participant_id, hh_member, weight) %>%
    summarise(contacts = round(sum(contacts)),
              contact_duration = mean(contact_duration, na.rm = TRUE)) %>%
    ungroup() %>%
    # summarise over respondents
    group_by(hh_member) %>%
    summarise(
      mean_contacts = weighted_mean(
        contacts,
        w = weight,
        na.rm = TRUE
      ),
      se_contacts = weighted_se(
        contacts,
        mean_contacts,
        w = weight,
        na.rm = TRUE
      ),
      mean_duration = weighted_mean(
        contact_duration,
        w = weight,
        na.rm = TRUE
      ),
      se_duration = weighted_se(
        contact_duration,
        mean_duration,
        w = weight,
        na.rm = TRUE
      )
    )  
  
  # replace the prior over mean non-household contacts with a more comparable
  # estimate from Prem/Polymod
  TC_0_prior <- baseline_total_contacts()
  OC_0_prior <- tibble::tibble(
    mean = TC_0_prior$mean - baseline_contact_params$mean_contacts[1],
    se = sqrt(TC_0_prior$se ^ 2 + baseline_contact_params$se_contacts[1] ^ 2)
  )
  baseline_contact_params$mean_contacts[2] <- OC_0_prior$mean
  baseline_contact_params$se_contacts[2] <- OC_0_prior$se
  
  baseline_contact_params
  
}

  # Results of Freya's survey
freya_survey_results <- function() {
  
  results <- tibble::tribble(
    ~date,        ~estimate, ~lower, ~upper,
    "2020-04-04",      2.78,   2.44,   3.17,
    "2020-05-02",      3.80,     NA,     NA,
  ) %>%
    mutate(
      date = as.Date(date),
      sd = mean(c(estimate[1] - lower[1], upper[1] - estimate[1])) / qnorm(0.95)
    )
  
  results
  
}

# mean infectious period in days
infectious_period <- function() {
  days <- 0:100
  si_pmf <- serial_interval_probability(days, fixed = TRUE)
  infectious_days <- weighted_mean(days, si_pmf)
  infectious_days
}

# find a prior over logit(p) that corresponds to the prior over R0, at the mean
# values of the baseline contact data, by moment matching
logit_p_prior <- function(params) {
  
  infectious_days <- infectious_period()
  
  transform <- function(free) {
    list(meanlogit = free[1],
         sdlogit = exp(free[2]))
  }
  
  R0 <- function(p, HC_0, HD_0, OC_0, OD_0) {
    HC_0 * (1 - p ^ HD_0) + OC_0 * infectious_days * (1 - p ^ OD_0)
  }
  
  R0_params <- function(logit_p_params, n = 1e5) {
    logit_p <- rnorm(n, logit_p_params$meanlogit, logit_p_params$sdlogit)
    R0_draws <- R0(
      p = plogis(logit_p),
      HC_0 = rnorm(n, params$mean_contacts[1], params$se_contacts[1]),
      OC_0 = rnorm(n, params$mean_contacts[2], params$se_contacts[2]),
      HD_0 = rnorm(n, params$mean_duration[1], params$se_duration[1]),
      OD_0 = rnorm(n, params$mean_duration[2], params$se_duration[2])
    )
    log_R0_draws <- log(R0_draws)
    list(meanlog = mean(log_R0_draws),
         sdlog = sd(log_R0_draws))
  }
  
  obj <- function(free) {
    logit_p_params <- transform(free)
    R0_params <- R0_params(logit_p_params)
    R0_expected <- R0_prior()
    (R0_expected$meanlog - R0_params$meanlog) ^ 2 + sqrt((R0_expected$sdlog - R0_params$sdlog) ^ 2)
  }
  
  set.seed(2020-05-18)
  o <- optim(c(5, -2), fn = obj, method = "BFGS")
  
  transform(o$par)
  
}


# get change in visits to locations - used as covariates for numbers of
# non-household contacts, and residential as proportional to household contact
# duration 
location_change <- function() {
  
  google_change_trends <- readRDS("outputs/google_change_trends.RDS") 
  
  location_change_trends <- google_change_trends %>%
    mutate(location = case_when(
      datastream == "Google: time at residential" ~ "home",
      datastream == "Google: time at transit stations" ~ "transit",
      datastream == "Google: time at parks" ~ "public",
      datastream == "Google: time at workplaces" ~ "work",
      datastream == "Google: time at retail and recreation" ~ "retail",
      TRUE ~ "other"
    )) %>%
    filter(location != "other") %>%
    select(-state_datastream, -datastream) %>% 
    pivot_wider(names_from = location, values_from = change)
  
  location_change_trends
  
}

# change in time at residential locations in each state
h_t_state <- function(dates) {
  
  location_change() %>%
    group_by(date, state, home) %>%
    select(date, state, home) %>%
    right_join(
      expand_grid(
        state = unique(.$state),
        date = dates
      )
    ) %>%
    ungroup() %>%
    mutate(
      home = replace_na(home, 1)
    ) %>%
    pivot_wider(names_from = state, values_from = home) %>%
    select(-date) %>%
    as.matrix()
  
}

# compute fractions of non-household (unique) contacts
# in each location 
location_contacts <- function() {
  
  rolls_contact_data() %>%
    filter(hh_member == "non_household") %>%
    group_by(participant_id, location, weight) %>%
    summarise(contacts = sum(contacts)) %>%
    group_by(location) %>%
    summarise(
      mean_contacts = weighted_mean(
        contacts,
        w = weight
      )
    ) %>%
    mutate(
      proportion_contacts = mean_contacts / sum(mean_contacts)
    )
  
}

# get the overall index of distancing (no waning) on the current dates and
# optionally add extra 1s at the end
social_distancing_national <- function(dates, n_extra = 0) {
  
  distancing_file <- "outputs/social_distancing_latent.RDS"
  distancing_index <- distancing_file %>%
    readRDS() %>%
    select(mean, date) %>%
    right_join(tibble(date = dates)) %>%
    replace_na(list(mean = 0)) %>%
    pull(mean)
  
  distancing_index <- c(distancing_index, rep(1, n_extra))
  distancing_index
  
}

# greta sub-model for the component R_eff due to macro- and micro-distancing
distancing_effect_model <- function(dates) {
  
  # modelled change (after/before ratio) in time at types of locations, from
  # Google, aligned with dates
  location_change_trends <- location_change() %>%
    right_join(
      expand_grid(
        state = unique(.$state),
        date = dates
      )
    ) %>%
    mutate_at(
      vars(public, home, retail, transit, work),
      ~replace_na(., 1)
    )

  # reshape to state/date x location matrix
  location_matrix <- location_change_trends %>%
    select(-state, -date) %>%
    as.matrix()
  
  # prior weights on the relationship between numbers of non-houshold contacts and
  # time spent in those locations, from baseline proportions of contacts in those
  # locations
  location_weights <- location_contacts()
  location_idx <- match(colnames(location_matrix), location_weights$location)
  relative_weights_prior <- location_weights[location_idx, ]$proportion_contacts
  
  # state population weights, to relate to national survey
  states <- unique(location_change_trends$state)
  state_weights <- state_populations()$population / sum(state_populations()$population)
  
  # Freya's survey results and index to dates
  dates <- unique(location_change_trends$date)
  freya_survey <- freya_survey_results()
  survey_date_idx <- match(freya_survey$date, dates)
  
  # informative priors on variables for contacts at t = 0 (Hx = household, Ox =
  # non-household, Tx = total, xC = contacts. xD = duration)
  baseline_contact_params <- baseline_contact_parameters()
  
  # prior on the probability of *not* transmitting, per hour of contact
  # (define to match moments of R0 prior)
  logit_p_params <- logit_p_prior(baseline_contact_params)
  logit_p <- normal(logit_p_params$meanlogit, logit_p_params$sdlogit)
  p <- ilogit(logit_p)
  
  infectious_days <- infectious_period()
  
  HC_0 <- normal(baseline_contact_params$mean_contacts[1],
                 baseline_contact_params$se_contacts[1])
  OC_0 <- normal(baseline_contact_params$mean_contacts[2],
                 baseline_contact_params$se_contacts[2])
  HD_0 <- normal(baseline_contact_params$mean_duration[1],
                 baseline_contact_params$se_duration[1])
  OD_0 <- normal(baseline_contact_params$mean_duration[2],
                 baseline_contact_params$se_duration[2])
  
  # get HD_t in each state
  h_t <- h_t_state(dates)
  HD_t <- HD_0 * h_t
  
  # relative contribution of time in each location type to the number of
  # non-household contacts
  relative_weights <- dirichlet(t(relative_weights_prior * 50))
  
  # scaling to account for greater/lower reductions than implied by the mobility
  scaling <- lognormal(0, 1)
  
  location_changes <- as_data(location_matrix)
  relative_contacts <- location_changes %*% t(relative_weights)
  relative_contacts_scaled <- relative_contacts ^ scaling

  # this is constrained  so that OC_t = OC_0 before distancing
  OC_t_state_long <- OC_0 * relative_contacts_scaled
  
  # reshape to time-by-state matrix
  wide_dim <- c(n_distinct(location_change_trends$date),
                n_distinct(location_change_trends$state))
  OC_t_state <- greta_array(OC_t_state_long, dim = wide_dim)
  
  # compute likelihood of national survey result
  OC_t <- OC_t_state %*% as.matrix(state_weights)
  distribution(freya_survey$estimate) <- normal(OC_t[survey_date_idx], freya_survey$sd)
  
  # model gamma_t: reduction in duration and transmission probability of
  # non-household contacts over time
  d_t <- social_distancing_national(dates)
  beta <- uniform(0, 1)
  gamma_t <- 1 - beta * d_t
  
  # compute component of R_eff for local cases
  household_infections <- HC_0 * (1 - p ^ HD_t)
  non_household_transmission <- infectious_days * (1 - p ^ OD_0) * gamma_t
  non_household_infections <- sweep(OC_t_state,
                                    1,
                                    non_household_transmission,
                                    FUN = "*")
  R_t <- household_infections + non_household_infections
  
  # return greta arrays
  list(R_t = R_t, 
       gamma_t = gamma_t,
       OC_t = OC_t,
       OC_t_state = OC_t_state,
       p = p,
       relative_weights = relative_weights,
       scaling = scaling,
       beta = beta,
       HC_0 = HC_0,
       HD_0 = HD_0,
       OC_0 = OC_0,
       OD_0 = OD_0,
       dates = dates)
  
}

plot_fit <- function(observed_cases, cases_sim, valid) {
  
  # compute quantiles and plot timeseries for each state
  quants <- apply(
    cases_sim,
    2,
    quantile,
    c(0.025, 0.25, 0.5, 0.75, 0.975)
  )
  
  # PPC check for each state
  par(mfrow = c(4, 2),
      mar = c(2, 3, 2, 0.5))
  for(i in 1:8) {
    idx <- valid[, 2] == i
    y <- observed_cases[idx]
    x <- dates[valid[idx, 1]]
    plot(y ~ x,
         type = "n",
         ylim = range(c(quants[, idx], y)),
         xlim = range(dates[valid[, 1]]),
         ylab = "",
         las = 1,
         xlab = "")
    mtext(states[i], side = 3, adj = 0, cex = 0.8)
    polygon(c(x, rev(x)),
            c(quants[1, idx], rev(quants[5, idx])),
            col = grey(0.9),
            lty = 0)
    polygon(c(x, rev(x)),
            c(quants[2, idx], rev(quants[4, idx])),
            col = grey(0.8),
            lty = 0)
    points(y ~ x, pch = 16, cex = 0.5)
    
  }
  
}

# parsing the excel file of microdistancing measures from BETA
barometer_results <- function() {
  file <- "data/microdistancing/BETA Barometer data - Adherence by States 20200525.xlsx"
  
  # lookup table
  lookup <- file %>%
    read_xlsx(
      sheet = 1,
      n_max = 6,
      skip = 2,
      col_names = c("sheet", "title", "question")
    ) %>%
    mutate(sheet = gsub("Table ", "", sheet),
           sheet = as.numeric(sheet)) %>%
    select(sheet, title)
  
  response_tibbles <- list()
  
  # loop through the sheets reading in data
  for (sheet in lookup$sheet) {
    
    # read the sheet in, skipping some lines and columns
    raw <- file %>%
      read_xlsx(
        col_names = FALSE,
        sheet = sheet + 1,
        range = cell_limits(c(3, 2), c(NA, NA))
      ) %>%
      # remove a redundant row and transpose to sort out columns
      dplyr::slice(-2) %>%
      t()
    
    # sort out column names
    raw[1, 1:2] <- c("location", "date")
    colnames(raw) <- raw[1, ]
    
    # finish formatting
    response_tibbles[[sheet]] <- raw %>%
      as_tibble() %>%
      dplyr::slice(-1) %>%
      # format locations
      mutate(
        location = ifelse(location == "Week", "Australia", location),
        location = gsub("^State \\(", "", location),
        location = gsub("\\)$", "", location)
      ) %>%
      fill(location) %>%
      # format dates
      mutate(
        date = gsub("Apr ", "April ", date),
        date = as.Date(date, format = "%e %B '%y")
      ) %>%
      # format number of respondents
      rename(respondents = `Column n`) %>%
      mutate(respondents = as.numeric(respondents)) %>%
      pivot_longer(
        cols = c(-location, -date, -respondents),
        names_to = "response",
        values_to = "percentage"
      ) %>%
      mutate(
        question = lookup$title[sheet],
        percentage = as.numeric(percentage),
        percentage = replace_na(percentage, 0)
      ) %>%
      mutate(count = as.integer(percentage * respondents)) %>%
      filter(location != "Australia") %>%
      select(
        location,
        date,
        question,
        response,
        percentage,
        count,
        respondents
      )
    
  }
  
  # combine them all and return
  do.call(bind_rows, response_tibbles)
  
}
# model for the trend in microdistancing
# compared to the macrodistancing effect, this has a linear transform of the
# distancing coefficient on the logit scale - corresponding to a different
# spread in adoption of microdistancing behaviour - and a different date of peak
# microdistancing and therefore different waning shape
microdistancing_model <- function(data,
                                  peak,
                                  distancing_effects,
                                  distancing_scale,
                                  waning_effects) {
  
  # shape of unscaled waning effect (0 at/before peak, to 1 at latest date)
  waning_shape <- (data$time - peak) / (1 - peak)
  nullify <- (sign(waning_shape) + 1) / 2
  waning_shape <- waning_shape * nullify
  
  # multiply by coefficient to get waning effect
  waning <- waning_shape * waning_effects[data$location_id]  
  
  # rescale distancing effects on logit scale
  logit_distancing <- qlogis(data$distancing) * distancing_scale
  distancing <- ilogit(logit_distancing) * distancing_effects[data$location_id]
  
  # combine the two
  distancing + waning
  
}

