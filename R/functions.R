library(readr)
library(dplyr)
library(stringr)
library(rjson)
library(tidyr)
library(greta)
library(readxl)
library(RColorBrewer)
library(tensorflow)

tfp <- reticulate::import("tensorflow_probability")

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
  data <- readr::read_csv(
    url, 
    col_types = cols(
      country_region_code = col_character(),
      country_region = col_character(),
      sub_region_1 = col_character(),
      sub_region_2 = col_character(),
      date = col_date(format = "%Y-%m-%d"),
      retail_and_recreation_percent_change_from_baseline = col_double(),
      grocery_and_pharmacy_percent_change_from_baseline = col_double(),
      parks_percent_change_from_baseline = col_double(),
      transit_stations_percent_change_from_baseline = col_double(),
      workplaces_percent_change_from_baseline = col_double(),
      residential_percent_change_from_baseline = col_double(),
      census_fips_code = col_character()
    )) %>%
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
  url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2011HotfixDev17/v3/en-us/applemobilitytrends-2020-07-07.csv"
  data <- readr::read_csv(
    url,
    col_types = cols(
      .default = col_double(),
      geo_type = col_character(),
      region = col_character(),
      transportation_type = col_character(),
      alternative_name = col_character(),
      `sub-region` = col_character(),
      country = col_character()
    )) %>%
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
  url <- "https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_20200708.csv"
  data <- readr::read_csv(
    url,
    skip = 3,
    col_types = cols(
      .default = col_double(),
      Date = col_date(format = "")
    )
  ) %>%
    tidyr::pivot_longer(
      cols = -Date,
      names_to = "region",
      values_to = "trend"
    ) %>%
    filter(region %in% ideal_regions()) %>%
    filter(!is.na(trend)) %>%
    rename(date = Date) %>%
    mutate(
      trend = 100 * (trend - 1),
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

# a hinge effect, 0 before the inflection point, then linear to 1 at the most recent period
latent_hinge <- function(inflection_dates, date_num) {
  last_date_num <- max(date_num)
  effect_periods <- last_date_num - inflection_dates
  time_remaining <- date_num - last_date_num
  distancing_effects <- 1 + kronecker(time_remaining, effect_periods, FUN = "/")
  nullify <- (sign(distancing_effects) + 1) / 2
  distancing_effects * nullify
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

plot_latent_factor <- function (factor, draws, dates, key_dates,
                                cols = grey(c(0.9, 0.7, 0.5, 0.3)), title = "") {
  
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
# CDF values is returned, equal to 0 where x was 0 or lower.
# lognormal_cdf <- function(x, meanlog, sdlog) {
#   
#   # filter out any invalid dates, to replace their CDF with 0
#   valid <- x > 0
#   x <- x[valid]
#   
#   p <- iprobit((log(x) - meanlog) / sdlog)
#   
#   # if any were missing, replace their cumulative density with 0
#   if (any(!valid)) {
#     result <- zeros(length(valid))
#     result[valid] <- p
#     p <- result
#   }
#   
#   p
#   
# }

lognormal_cdf <- function(x, meanlog, sdlog) {
  
  op("lognormal_cdf",
     x,
     meanlog,
     sdlog,
     tf_operation = "tf_lognormal_cdf",
     dim = dim(x))
  
}

# tensorflow function for the CDF of a lognormal distribution
tf_lognormal_cdf <- function(x, meanlog, sdlog) {
  
  d <- tfp$distributions$LogNormal(meanlog, sdlog)
  
  # This does not support zero or negative values (returns NA instead of 0), so
  # we need to avoid those. We also can't compute the CDF on negative values
  # then replace the bad values, since that breaks the gradients
  supported <- tf$greater(x, greta:::fl(0))
  ones <- tf$ones_like(x)
  zeros <- tf$zeros_like(x)
  
  x_clean <- tf$where(supported, x, ones)
  cdf_clean <- d$cdf(x_clean)
  mask <- tf$where(supported, ones, zeros)
  cdf_clean * mask
  
}

# probability mass function of a negative binomial distribution
negative_binomial_pmf <- function(x, size, prob) {
  
  op("negative_binomnial_pmf",
     x,
     size,
     prob,
     tf_operation = "tf_negative_binomial_pmf",
     dim = dim(x))
  
}

tf_negative_binomial_pmf <- function(x, size, prob) {
  
  
  d <- tfp$distributions$NegativeBinomial(
    total_count = size,
    probs = greta:::fl(1) - prob)
  
  d$prob(x)
  
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

nishiura_samples <- function () {
  
  # priors for the parameters of the lognormal distribution over the serial interval from Nishiura et
  # al., as stored in the EpiNow source code 
  read_csv(
    file = "data/parameters/nishiura_samples.csv",
    col_types = cols(param1 = col_double(),
                     param2 = col_double())
  )
  
}

# Pull out generation interval distribution estimates from Ganyani et al. Use
# baseline GI estimates for China from (Table 1, row 3). This corresponds to a
# GI/SI mean of 3.95, which is around the 4 that seems to be a consensus for the
# SI mean. We do not use the version allowing for a negative serial interval
# (which is suggested by other including Du et al., and could possibly be an
# artefact of incorrect alignment of infector/infectee pairs), though when this is
# used as a prior the model is able to estimate parameters corresponding to
# that scenario.
ganyani_gi <- function(which = c("Tianjin", "Singapore")) {

  tianjin <- tibble::tribble(
    ~which, ~est, ~lower, ~upper,
    "mean", 3.95, 3.01, 4.91,
    "sd", 1.51, 0.74, 2.97
  )

  singapore <- tibble::tribble(
    ~which, ~est, ~lower, ~upper,
    "mean", 5.20, 3.78, 6.78,
    "sd", 1.72, 0.91, 3.93
  )
  
  ganyani <- switch(match.arg(which),
                    Tianjin = tianjin,
                    Singapore = singapore)
  
  ganyani <- ganyani %>%
    mutate(
      sd_above = (upper - est) / 1.96,
      sd_below = (est - lower) / 1.96,
      sd = (sd_above + sd_below) / 2
    ) %>%
    select(which, est, sd)
  
  ganyani %>%
    group_by(which) %>%
    nest() %>%
    pull(data) %>%
    `names<-`(ganyani$which)
    
}

# The two (baseline) generation intervals of Ganyani et al.
ganyani_cdf <- function(which) {
  
  gi_params <- ganyani_gi(which)
  
  beta <- gi_params$mean$est / (gi_params$sd$est ^ 2)
  alpha <- gi_params$mean$est * beta
  
  gi_cdf <- function(days) {
    pgamma(days, alpha, beta)
  }
  
  gi_cdf
  
}

# the serial interval of Nishiura et al.
nishiura_cdf <- function() {
  
  # generation interval distribution; use SI distribution from Nishiura et al.
  nishiura <- nishiura_samples()
  meanlog <- mean(nishiura$param1)
  sdlog <- mean(nishiura$param2)
  
  gi_cdf <- function(days) {
    plnorm(days, meanlog, sdlog)
  }
  
  gi_cdf
  
}

# the serial interval distribution of Bi et al. (used in Impeerial Report 13 /
# Flaxman et al.)
bi_cdf <- function() {
  
  gi_cdf <- function(days) {
    pgamma(days, 2.29, 0.36)
  }
  
  gi_cdf
  
}

# Discretised generation interval with truncated continuous distribution. Given
# a number of days post-infection, compute the probability of the generation
# interval having that length as the density of a truncated distribution (given
# by an R function for its cumulative density function 'cdf') on time
# over the duration of that day.
gi_probability <- function(cdf,
                           days = seq(bounds[1], bounds[2]),
                           bounds = c(0, 20)) {
  
  # days of infectiousness
  n_days <- length(days)
  # set invalid days to -1 (density 0)
  out_of_bounds <- days < bounds[1] | days > bounds[2]
  days[out_of_bounds] <- -1
  
  # get discretised probability, without accounting for truncation  
  p <- cdf(days + 1) - cdf(days)
  
  # adjust density for truncation
  upper_bound <- cdf(bounds[2] + 1)
  lower_bound <- cdf(bounds[1])
  p <- p / (upper_bound - lower_bound)
  
  p
  
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

# build an upper-triangular circulant matrix of the number of days from one set
# of times to another, set to -999 if the difference is not supported (negative
# or exceeds max_days)
time_difference_matrix <- function (n_days) {
  
  mat <- matrix(0, n_days, n_days)
  row(mat) - col(mat)

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
disaggregation_matrix <- function (cdf, n_days, max_days = 20, ...) {
  
  diff <- time_difference_matrix(n_days, max_days)
  si_disaggregation <- gi_probability(cdf, days, ...)
  
}

# apply a fixed serial interval a date-by-state matrix 'cases' of case counts to get
# the expected number of infectious people at each time. If 'fixed', use a
# deterministic serial interval distribution base don prior means, othrwise
# treeat the parameters of the distribution as unknown parameters, to account
# for uncrtainty in the distribution.
apply_serial_interval <- function(cdf, cases) {
  
  # get a square matrix of contributions of each date to each other, and
  # matrix-multiply to disaggregate
  si_disaggregation <- disaggregation_matrix(cdf, nrow(cases))
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
                       keep_only_rows = NULL,
                       min_date = as.Date("2020-03-01")) {
  
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
    filter(date >= min_date) %>%
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


baseline_contact_parameters <- function(gi_cdf) {
  
  # mean duration of infection in days
  infectious_days <- infectious_period(gi_cdf)
  
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
infectious_period <- function(cdf) {
  days <- 0:100
  gi_pmf <- gi_probability(cdf, days)
  infectious_days <- sum(days * gi_pmf)
  infectious_days
}

# find a prior over logit(p) that corresponds to the prior over R0, at the mean
# values of the baseline contact data, by moment matching
logit_p_prior <- function(params, gi_cdf) {
  
  infectious_days <- infectious_period(gi_cdf)
  
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
location_change <- function(dates = NULL) {
  
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
  
  # optionally add all dates and states, and pad missing values (prior to first
  # mobility data) with 1s
  if (!is.null(dates)) {
    location_change_trends <- location_change_trends %>%
      group_by_all() %>%
      right_join(
        expand_grid(
          state = unique(.$state),
          date = dates
        )
      ) %>%
      ungroup() %>%
      mutate_at(vars(-state, -date), replace_na, 1)
  }

  location_change_trends
  
}

# change in time at residential locations in each state
h_t_state <- function(dates) {
  
  location_change(dates) %>%
    select(state, date, home) %>%
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

# get the index of microdistancing in each state as a date-by-state matrix
microdistancing_state <- function (dates, states) {
  
  microdistancing_file <- "outputs/microdistancing_trends.RDS"
  microdistancing_index <- microdistancing_file %>%
    readRDS() %>%
    # expand to all required dates and states
    select(state, date, mean) %>%
    right_join(
      expand_grid(
        state = states,
        date = dates
      )
    ) %>%
    replace_na(list(mean = 0)) %>%
    # scale to have a maximum of 1
    mutate(mean = mean / max(mean)) %>%
    # turn into a date-by-state matrix
    pivot_wider(
      names_from = state,
      values_from = mean
    ) %>%
    select(-date) %>%
    as.matrix()
  
  microdistancing_index
  
}

# get the index of macrodistancing in each state as a date-by-state matrix
macrodistancing_state <- function (dates, states) {
  
  macrodistancing_file <- "outputs/macrodistancing_trends.RDS"
  macrodistancing_index <- macrodistancing_file %>%
    readRDS() %>%
    # expand to all required dates and states
    select(state, date, mean) %>%
    right_join(
      expand_grid(
        state = states,
        date = dates
      )
    ) %>%
    # turn into a date-by-state matrix
    pivot_wider(
      names_from = state,
      values_from = mean
    ) %>%
    select(-date) %>%
    as.matrix()
  
  macrodistancing_index
  
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
distancing_effect_model <- function(dates, gi_cdf) {
  
  # informative priors on variables for contacts at t = 0 (Hx = household, Ox =
  # non-household, Tx = total, xC = contacts. xD = duration)
  baseline_contact_params <- baseline_contact_parameters(gi_cdf)
  
  # prior on the probability of *not* transmitting, per hour of contact
  # (define to match moments of R0 prior)
  logit_p_params <- logit_p_prior(baseline_contact_params, gi_cdf)
  logit_p <- normal(logit_p_params$meanlogit, logit_p_params$sdlogit)
  p <- ilogit(logit_p)
  
  infectious_days <- infectious_period(gi_cdf)
  
  HC_0 <- normal(baseline_contact_params$mean_contacts[1],
                 baseline_contact_params$se_contacts[1],
                 truncation = c(0, Inf))
  HD_0 <- normal(baseline_contact_params$mean_duration[1],
                 baseline_contact_params$se_duration[1],
                 truncation = c(0, Inf))
  OD_0 <- normal(baseline_contact_params$mean_duration[2],
                 baseline_contact_params$se_duration[2],
                 truncation = c(0, Inf))
  
  # get HD_t in each state
  h_t <- h_t_state(dates)
  HD_t <- HD_0 * h_t

  # model for non-household contacts in each state over time
  # modelled change (after/before ratio) in time at types of locations from Google
  location_change_trends <- location_change(dates) %>%
    mutate_at(
      vars(public, home, retail, transit, work),
      ~replace_na(., 1)
    ) %>%
    mutate(state = abbreviate_states(state))
  
  # state-level numbers of non-household contacts by state and date from Freya's
  # survey and the BETA barometer. Remove implausible responses, from the
  # reporting clump at 999 and above (short conversation with 999 or more people
  # in a day is implausible, and probably an entry/reporting/understanding error)
  contacts <- contact_survey_data() %>%
    filter(contacts < 999)
  
  params <- macrodistancing_params(location_change_trends, gi_cdf)
  OC_0 <- params$OC_0
  relative_weights <- params$relative_weights
  scaling <- params$scaling
  
  OC_t_state <- macrodistancing_model(
    location_change_trends,
    baseline = OC_0,
    relative_weights = relative_weights,
    scaling = scaling
  )
  
  # define the likelihood
  macrodistancing_likelihood(OC_t_state, contacts, location_change_trends)
    
  # model gamma_t: reduction in duration and transmission probability of
  # non-household contacts over time, per state
  
  data <- microdistancing_data(dates)
  barometer_distance <- data$barometer_data
  pred_data <- data$prediction_data
  
  params <- microdistancing_params()
  peak <- params$peak
  distancing_effects <- params$distancing_effects
  waning_effects <- params$waning_effects

  # define likelihood  
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

  # predict to new data  
  prob_pred <- microdistancing_model(
    data = pred_data,
    peak = peak,
    distancing_effects = distancing_effects,
    waning_effects = waning_effects
  )
  
  pred_data_wide <- pred_data %>%
    pivot_wider(names_from = state, values_from = distancing)
  
  # reshape to date-by-state matrix
  wide_dim <- c(n_distinct(pred_data$date), n_distinct(pred_data$state))
  microdistancing_prob <- greta_array(prob_pred, dim = wide_dim)
  
  # divide by the maximum value, so that beta can scale it properly
  d_t_state <- microdistancing_prob / max(microdistancing_prob)
  
  # d_t_state <- microdistancing_state(dates, states)
  beta <- uniform(0, 1)
  gamma_t_state <- 1 - beta * d_t_state
  
  # compute component of R_eff for local cases
  household_infections <- HC_0 * (1 - p ^ HD_t)
  non_household_infections <- OC_t_state * gamma_t_state *
    infectious_days * (1 - p ^ OD_0)
  R_t <- household_infections + non_household_infections
  
  # return greta arrays
  list(R_t = R_t, 
       gamma_t_state = gamma_t_state,
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

# parsing the file of microdistancing measures from BETA
barometer_results <- function() {
  
  files <- list.files("data/microdistancing/",
                      pattern = ".csv$",
                      full.names = TRUE)
  
  tibbles <- lapply(
    files,
    read_csv,
    col_types = cols(
      date = col_date(format = ""),
      state = col_character(),
      question = col_character(),
      response = col_character(),
      count = col_double(),
      respondents = col_double()
    )
  )

  do.call(bind_rows, tibbles) %>%
    filter(
      !state %in% c("Australia", "Other")
    ) %>%
    mutate(
      question = recode(
        question,
        "hand hygine" = "Hand washing",
        "cough" = "Cough etiquette",
        "non-household contact" = "Physical contact"
      ), 
      state = recode(
        state,
        "New South Wales" = "NSW",
        "Northern Territory" = "NT",
        "Queensland" = "QLD",
        "South Australia" = "SA",
        "Tasmania" = "TAS",
        "Victoria" = "VIC",
        "Western Australia" = "WA"
      )
    ) %>%
    arrange(state, date, question, response)
  
}

# model for the trend in microdistancing
# compared to the macrodistancing effect, this has a linear transform of the
# distancing coefficient on the logit scale - corresponding to a different
# spread in adoption of microdistancing behaviour - and a different date of peak
# microdistancing and therefore different waning shape
microdistancing_model <- function(data,
                                  peak,
                                  distancing_effects,
                                  waning_effects) {
  
  # shape of unscaled waning effect (0 at/before peak, to 1 at latest date)
  waning_shape <- (data$time - peak) / (1 - peak)
  nullify <- (sign(waning_shape) + 1) / 2
  waning_shape <- waning_shape * nullify
  
  # multiply by coefficients to get trends for each state
  waning <- waning_shape * waning_effects[data$state_id]  
  distancing <- data$distancing * distancing_effects[data$state_id]
  
  # combine the two
  distancing + waning
  
}

# model for the trend in macrodistancing a weighted sum of time at location
# types, and an overall scaling coefficient, multiplied by a scalar baseline
# contact rate
macrodistancing_model <- function(data,
                                  baseline,
                                  relative_weights,
                                  scaling) {
  
  # format data into a date/state by location greta array
  location_changes <- data %>%
    select(-state, -date) %>%
    as.matrix() %>%
    as_data()
  
  # weight locations and multiply by scaling factor to get change in
  # non-household contacts
  relative_contacts <- location_changes %*% t(relative_weights)
  relative_contacts_scaled <- relative_contacts ^ scaling
  
  # convert change to absolute estimate of nonhousehold contacts
  state_long <- baseline * relative_contacts_scaled
  
  # reshape to date-by-state matrix
  wide_dim <- c(n_distinct(data$date), n_distinct(data$state))
  greta_array(state_long, dim = wide_dim)
  
}

# contruct multiple GPs for epsilons in the Reff model
epsilon_gp <- function(
  date_nums,
  n_states,
  kernel,
  inducing_date_nums = date_nums,
  sigma_state = normal(0, 0.5, truncation = c(0, Inf), dim = n_states),
  tol = 1e-6) {
  
  # whitened representation of GP
  n_inducing <- length(inducing_date_nums)
  v_raw <- normal(0, 1, dim = c(n_inducing, n_states))
  v <- sweep(v_raw, 2, sigma_state, FUN = "*")
  
  # GP  
  epsilon <- multi_gp(
    x = date_nums,
    v = v,
    kernel = kernel,
    inducing = inducing_date_nums,
    tol = tol
  )
  
  epsilon
  
}

op <- greta::.internals$nodes$constructors$op

# Given an date-by-state matrix of 'infectiousness' due to imports and previous
# locally-acquired cases, and a corresponding matrix 'R_local' of Reff for
# locally-acquired infections, compute the expected numbers of locally-acquired
# cases into the future. Also requires 'disaggregation_probs' giving the density
# of the serial interval over a sequence of days.
project_local_cases <- function(
  infectiousness,
  R_local,
  disaggregation_probs
) {
  
  if (!identical(dim(infectiousness), dim(R_local))) {
    stop ("infectiousness and R_local must have the same dimensions")
  }
  
  op("project_local_cases",
     infectiousness,
     R_local,
     disaggregation_probs,
     operation_args = list(
       T = nrow(infectiousness),
       K = length(disaggregation_probs)
     ),
     tf_operation = "tf_project_local_cases",
     dim = dim(infectiousness))
  
}

# tensorflow function to project the expected number of locally-acquired cases
# into the future
tf_project_local_cases <- function(infectiousness, R_local, disaggregation_probabilities, T, K) {
  
  # continuing condition of TF while loop
  cond <- function(C, I, R, p, t, T, K, sequence) {
    tf$less(t, T)
  }
  
  # body of TF while loop
  body <- function(C, I, R, p, t, T, K, sequence) {
    
    # increase the expected infectiousness on subsequent days due to cases
    # infected on this day
    new_C <- R[, t, ] * I[, t, ]
    new_C <- tf$expand_dims(new_C, 1L)

    # distribute infectiousness of these cases across their infectious profile
    new_I <- new_C * p
    
    # add to cases and cumulative infectiousness
    perm_to <- c(1L, 2L, 0L)
    perm_from <- c(2L, 0L, 1L)
    
    I_t <- tf$transpose(I, perm_to)
    new_I_t <- tf$transpose(new_I, perm_to)
    I_t <- tf$tensor_scatter_nd_add(I_t,
                                    indices = t + sequence,
                                    updates = new_I_t)
    I <- tf$transpose(I_t, perm_from)
    
    C_t <- tf$transpose(C, perm_to)
    new_C_t <- tf$transpose(new_C, perm_to)
    C_t <- tf$tensor_scatter_nd_add(C_t,
                                    indices = tf$reshape(t, shape = c(1L, 1L)),
                                    updates = new_C_t)
    C <- tf$transpose(C_t, perm_from)
    
    list(C, I, R, p, t + 1L, T, K, sequence)
    
  }

  # pad I and R with K zeros so we don't need to mess with indexing inside the loop
  batch_size <- greta:::get_batch_size()
  n_locations <- dim(infectiousness)[[3]]
  pad <- tf$zeros(
    shape = tf$stack(list(batch_size, K + 1L, n_locations)),
    dtype = greta:::tf_float()
  )
  I <- tf$concat(list(infectiousness, pad), axis = 1L)
  R <- tf$concat(list(R_local, pad), axis = 1L)
  C <- tf$zeros(
    shape = tf$stack(list(batch_size, T, n_locations)),
    dtype = greta:::tf_float()
  )
  
  # initial variables for loop
  values <- list(
    C,
    I,
    R,
    disaggregation_probabilities,
    0L,
    T,
    K,
    as.matrix((1:K) - 1L)
  )

  # iterate to compute infectiousness
  result <- tf$while_loop(cond, body, values)
  
  # return expected numbers of new local cases
  result[[1]]

}

# check fit of projected cases against national epi curve
check_projection <- function(draws, R_eff_local, R_eff_imported,
                             gi_mat, gi_vec,
                             local_infectious, imported_infectious,
                             local_cases, dates,
                             start_date = as.Date("2020-02-28")) {
  
  
  n_states <- ncol(local_infectious)
  n_dates <- nrow(local_infectious)
  
  # national-level Reff - no clusters and weighted by state populations
  local_weights <- sweep(local_infectious, 1, rowSums(local_infectious), FUN = "/")
  local_weights[is.na(local_weights)] <- 1 / n_states
  import_weights <- sweep(imported_infectious, 1, rowSums(imported_infectious), FUN = "/")
  import_weights[is.na(import_weights)] <- 1 / n_states
  
  R_eff_loc_ntnl <- rowSums(R_eff_loc_12[seq_len(n_dates), ] * local_weights)
  R_eff_imp_ntnl <- rowSums(R_eff_imp_12[seq_len(n_dates), ] * import_weights)
  
  # subset to from the first of March, when transmission became established (the
  # model is not designed to work with the stochastic extinctions we saw at the beginning of the outbreak)
  start <- which(dates == start_date)
  sub_idx <- start:n_dates
  
  # simulate local-local transmission dynamics, at national level; forced using
  # (observed) case importation and local cases prior to the start of the
  # simulation
  
  # locally-acquired infections present prior to the start of the simulation
  previous_local_cases <- rowSums(local_cases)
  previous_local_cases[sub_idx] <- 0
  previous_local_infectiousness <- gi_mat %*% as.matrix(previous_local_cases)
  
  # compute infectious forcing from local cases emerging during this period that
  # were directly infected by imported cases (can't just include the import
  # infectiousness, since they are subject to a different Reff). Get expected
  # number of new local cases from imports, then disaggregate according to their
  # infectiousness profile to get force of local infection
  
  # expected number of new locally-acquired cases during the simulation period due
  # to infection from imports
  import_local_cases <- rowSums(imported_infectious) * R_eff_imp_ntnl[seq_len(n_dates)]
  import_local_infectiousness <- gi_mat %*% import_local_cases
  
  # combine these to get forcing from existing and import-associated local cases,
  # and disaggregate to get infectiousness of these
  local_infectiousness <- previous_local_infectiousness + import_local_infectiousness
  
  # Given this basic force of infection, R for locally-acquired cases (mean trend,
  # no clusters), and the infectiousness profile, iterate the dynamics to compute
  # the numbers of local cases
  secondary_locals <- project_local_cases(
    infectiousness = local_infectiousness[sub_idx],
    R_local = R_eff_loc_ntnl[sub_idx],
    disaggregation_probs = gi_vec
  )
  
  # compute locally-acquired cases
  local_cases_project_ntnl <- import_local_cases[sub_idx] + secondary_locals
  local_cases_project_ntnl_sim <- calculate(local_cases_project_ntnl,
                                            values = draws,
                                            nsim = 1000)[[1]]
  
  local_cases_ntnl <- rowSums(local_cases[sub_idx, ])
  plot_trend(local_cases_project_ntnl_sim,
             multistate = FALSE,
             ylim = c(0, 2 * max(local_cases_ntnl)),
             hline_at = NULL,
             dates = dates[sub_idx],
             base_colour = green,
             vline_at = intervention_dates()$date,
             min_date = min(dates)) +
    ggtitle("Projected national locally-acquired cases") +
    ylab("daily infections") +
    geom_line(data = data.frame(mean = local_cases_ntnl,
                                date = dates[sub_idx],
                                type = "Nowcast"))
  
}

# load a csv of numbers of non-household contacts by state (where the first
# column gives numberts of contacts and the rest of each row gives counts of
# respondents with that number of contacts), tidy up the names, add a date and
# convert to long format (one row per response)
load_contacts_by_state <- function(csv, date) {
  
  contacts <- read_csv(
    csv,
    col_types = list(
      .default = col_double()
    )
  ) %>%
    rename_all(
      recode,
      contact_num = "contacts",
      num_contacts = "contacts",
      "Australian Capital Territory" = "ACT",
      "New South Wales" = "NSW",
      "Northern Territory" = "NT",
      "Queensland" = "QLD",
      "South Australia" = "SA",
      "Tasmania" = "TAS",
      "Victoria" = "VIC",
      "Western Australia" = "WA"
    ) %>%
    pivot_longer(cols = -contacts,
                 names_to = "state",
                 values_to = "respondents") %>%
    mutate(date = date) %>%
    filter(state != "Other") %>%
    uncount(respondents) 
 
  contacts
     
}

# load in data from the contact surveys
contact_survey_data <- function() {
  
  bind_rows(
    
    # Freya's survey waves
    load_contacts_by_state(
      "data/contacts/freya_survey/contact_counts_NG_wave1.csv",
      as.Date("2020-04-04")
    ),
    
    load_contacts_by_state(
      "data/contacts/freya_survey/contact_counts_NG_wave2.csv",
      as.Date("2020-05-02")
    ),
    
    # barometer waves
    load_contacts_by_state(
      "data/contacts/barometer/contacts_by_state.csv",
      as.Date("2020-05-27")
    ),
    
    load_contacts_by_state(
      "data/contacts/barometer/contact_numbers_wave_11.csv",
      as.Date("2020-06-03")
    ),
    
    load_contacts_by_state(
      "data/contacts/barometer/contact numbers wave 12.csv",
      as.Date("2020-06-10")
    ),
    
    load_contacts_by_state(
      "data/contacts/barometer/contact numbers wave 13.csv",
      as.Date("2020-06-17")
    ),
    
    load_contacts_by_state(
      "data/contacts/barometer/contact numbers wave 14.csv",
      as.Date("2020-06-24")
    )
    
  )
  
}

macrodistancing_params <- function(location_change_trends, gi_cdf) {
  # baseline number of non-household contacts, from Prem and Rolls
  baseline_contact_params <- baseline_contact_parameters(gi_cdf)
  
  OC_0 <- normal(baseline_contact_params$mean_contacts[2],
                 baseline_contact_params$se_contacts[2],
                 truncation = c(0, Inf))
  
  # prior weights on the relationship between numbers of non-houshold contacts and
  # time spent in those locations, from baseline proportions of contacts in those
  # locations
  location_weights <- location_contacts()
  location_names <- colnames(location_change_trends)[-(1:2)]
  location_idx <- match(location_names,
                        location_weights$location)
  relative_weights_prior <- location_weights[location_idx, ]$proportion_contacts
  
  # relative contribution of time in each location type to the number of
  # non-household contacts
  relative_weights <- dirichlet(t(relative_weights_prior * 1))
  
  # scaling to account for greater/lower reductions than implied by the mobility
  scaling <- lognormal(0, 1)
  
  list(OC_0 = OC_0,
       relative_weights = relative_weights,
       scaling = scaling)
  
}


# define the likelihood for the macrodistancing model
macrodistancing_likelihood <- function(OC_t_state, contacts, location_change_trends) {
  
  # pull out the expected number of non-household contacts by state and date
  dates <- unique(location_change_trends$date)
  states <- unique(location_change_trends$state)
  date_idx <- match(contacts$date, dates)
  state_idx <- match(contacts$state, states)
  idx <- cbind(date_idx, state_idx)
  
  # likelihood for state-level contact rate data (truncate contacts at 500 for numerical stability)
  mean_contacts <- OC_t_state[idx]
  sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf))
  size <- 1 / sqrt(sqrt_inv_size)
  prob <- 1 / (1 + mean_contacts / size)
  distribution(contacts$contacts) <- negative_binomial(size, prob)
  
  result <- list(size = size,
                 prob = prob)
  
  invisible(result)
  
}

# check convergence
convergence <- function(draws) {
  
  r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
  n_eff <- coda::effectiveSize(draws)
  
  cat(sprintf("maximum R-hat: %.2f\nminimum n effective: %.2f",
              max(r_hats),
              min(n_eff)))
  
  result <- list(r_hats = r_hats,
                 n_eff = n_eff)
  
  invisible(result)
  
}

# define a zero-mean hierarchical normal prior over a vector of length n
hierarchical_normal <- function(n, index = NULL, mean_sd = 10, sd_sd = 0.5) {
  
  indexed <- !is.null(index)
  hyper_n <- ifelse(indexed, max(index), 1)
  
  mean <- normal(0, mean_sd, dim = hyper_n)
  sd <- normal(0, sd_sd, truncation = c(0, Inf), dim = hyper_n)
  raw <- normal(0, 1, dim = n)
  
  if (indexed) {
    mean <- mean[index]
    sd <- sd[index]
  }
  
  mean + raw * sd
  
}

microdistancing_params <- function(n_locations = 8) {
  
  # timing of peak microdistancing between the date of the last intervention and
  # today's date
  peak <- normal(0, 1, truncation = c(0, 1))
  
  # hierarchical structure on state-level waning
  logit_waning_effects <- hierarchical_normal(n_locations)
  waning_effects <- -ilogit(logit_waning_effects)
  
  # hierarchical structure on state-level peak effect (proportion adhering) 
  logit_distancing_effects <- hierarchical_normal(n_locations)
  distancing_effects <- ilogit(logit_distancing_effects)
  
  list(peak = peak,
       distancing_effects = distancing_effects,
       waning_effects = waning_effects)
}

# get data for fitting and predicting from microdistancing model
microdistancing_data <- function(dates = NULL) {
  
  # recode and collapse responses into percentage adherence
  barometer <- barometer_results() %>%
    # recode 1.5m compliance question as yes/no (whether they mostly did it)
    mutate(
      response = case_when(
        question == "1.5m compliance" &
          response %in% c("Always") ~ "yes",
        question == "1.5m compliance" &
          response %in% c("Often", "Sometimes", "Rarely", "No") ~ "no",
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
    group_by(state, date, question, response) %>%
    summarise(count = sum(count),
              respondents = mean(respondents)) %>%
    mutate(proportion = count / respondents) %>%
    # now we can just keep the proportion responding 'yes'
    filter(response == "yes") %>%
    select(-response) %>%
    arrange(state, question, date)
  
  # load latent factors
  # assume adoption of microdistancing follows the same trend as macrodistancing,
  # and that waning starts at the same time, butdon't assume it wanes at the same
  # rate
  
  distancing <- readRDS("outputs/social_distancing_latent.RDS")
  
  # use these dates if no others are specified
  if (is.null(dates)) {
    dates <- distancing$date
  }
  
  # get data to predict to
  pred_data <- distancing %>%
    rename(distancing = mean) %>%
    select(date, distancing) %>%
    right_join(
      expand_grid(
        date = dates,
        state = unique(barometer$state)
      )
    ) %>%
    replace_na(list(distancing = 0)) %>%
    mutate(
      state_id = match(state, unique(state)),
      time = as.numeric(date - max(intervention_dates()$date)),
      time = time / max(time)
    ) %>%
    arrange(state, date)
  
  # subset to 1.5m question and add data for modelling
  barometer_distance <- barometer %>%
    filter(question == "1.5m compliance") %>%
    left_join(pred_data)
  
  result <- list(barometer_data = barometer_distance,
                 prediction_data = pred_data)
  
  result
  
}

# given vectors of dates and numbers of days post infection, return the fraction of cases not being detected by that point
ttd_survival <- function(days, dates) {
  
  # load fitted CDFs over time
  cdf <- readRDS("outputs/time_to_detection_cdf.RDS")
  ttd_dates <- cdf$date
  cdf_mat <- cdf %>%
    select(-date) %>%
    as.matrix()
  
  # line up dates
  dates <- pmin(dates, max(ttd_dates))
  dates <- pmax(dates, min(ttd_dates))
  dates_idx <- match(dates, ttd_dates)
  
  # line up days (direct index against ttd elements since we're counting from
  # infection, not symptom onset)
  days_idx <- days + 1
  days_idx <- pmax(days_idx, 1)
  days_idx <- pmin(days_idx, ncol(cdf_mat))
  
  # pull out elements of CDF
  idx <- cbind(dates_idx, days_idx)
  cdf_vec <- cdf_mat[idx]
  
  # return probability of not being detected by this point
  1 - cdf_vec
  
}

# reduction in R due to faster detection of cases
surveillance_effect <- function(dates, cdf, gi_bounds = c(0, 20)) {
  
  n_dates <- length(dates)
  gi_range <- diff(gi_bounds) + 1
  day_vec <- seq_len(gi_range) - 1 + gi_bounds[1]
  day_mat <- col(matrix(0, n_dates, gi_range)) - 1
  
  # times to detection for each date  
  ttd_days <- day_mat
  ttd_days[] <- ttd_survival(c(day_mat), rep(dates, gi_range))
  
  # generation interval probability on each day post-infection
  gi_days <- gi_probability(cdf, day_vec, bounds = gi_bounds)
  
  # weighted sum to get reduction due to impeded transmission
  c(ttd_days %*% gi_days)
  
}

# get the mean date of symptom onset give a date of detection (using the
# time-varying time to detection distribution)
impute_one_onset <- function(detection_date, method = c("expected", "random"), max_days = 40) {
  
  method <- match.arg(method)
  
  # get possible dates of infection
  delays <- seq_len(max_days) 
  possible_infection_dates <- detection_date - delays
  possible_infection_dates
  
  # probability of being detected this many days later (probability of detection
  # by this day, minus probability of detection by the previous day)
  surv_from <- ttd_survival(delays, possible_infection_dates)
  surv_to <- ttd_survival(delays + 1, possible_infection_dates)
  prob <- surv_from - surv_to

  # normalise to get probabilities of different delays
  prob <- prob / sum(prob)

  # compute either the expected time since infection, or draw a random one
  TSI <- switch(method,
                expected = round(sum(delays * prob)),
                random = sample(delays, 1, prob = prob))
  
  # subtract 5 to get expcted time since symptom onset, round, and convert to the date
  onset_date <- detection_date - (TSI - 5)
  onset_date
  
}

impute_onsets <- function(detection_dates,
                          method = c("expected", "random"),
                          max_days = 40) {
  
  method <- match.arg(method)
  onset_dates <- lapply(detection_dates,
                        impute_one_onset,
                        method  = method,
                        max_days = max_days)
  do.call(c, onset_dates)
  
}

# clean up some weird date encoding in the linelist
clean_date <- function (original_date) {
  weird_date <- !grepl("2020", original_date)
  corrected_date <- gsub("^\\d\\d\\d\\d", "2020", original_date)
  # don't use ifelse as it converts to a numeric
  date <- original_date
  date[weird_date] <- corrected_date[weird_date]
  date
}

# read in the latest linelist and format for analysis
get_linelist <- function (file = NULL, dir = "~/not_synced/nndss") {
  
  if (is.null(file)) {
    # find the latest file
    files <- list.files(dir, pattern = ".xlsx$", full.names = TRUE)
    date_time_text <- gsub("^COVID-19 UoM ", "", basename(files)) 
    date_time_text <- gsub(".xlsx$", "", date_time_text)
    date_times <- as.POSIXct(date_time_text, format = "%d%b%Y %H%M")
    latest <- which.max(date_times)
    ll_date <- date_times[latest]
    file <- files[latest]
  } else {
    file <- file.path(dir, file)
    date_time_text <- gsub("^COVID-19 UoM ", "", basename(file)) 
    date_time_text <- gsub(".xlsx$", "", date_time_text)
    ll_date <- as.POSIXct(date_time_text, format = "%d%b%Y %H%M")
  }
  
  dat <- readxl::read_xlsx(
    file,
    col_types = c(
      STATE = "text",
      CONFIRMATION_STATUS = "numeric",
      TRUE_ONSET_DATE = "date",
      SPECIMEN_DATE = "date",
      NOTIFICATION_DATE = "date",
      NOTIFICATION_RECEIVE_DATE = "date",
      Diagnosis_Date = "date",
      AGE_AT_ONSET = "numeric",
      SEX = "numeric",
      DIED = "numeric",
      PLACE_OF_ACQUISITION = "text",
      HOSPITALISED = "numeric",
      CV_ICU = "numeric",
      CV_VENTILATED = "numeric",
      OUTBREAK_REF = "text",
      CASE_FOUND_BY = "numeric",
      CV_SYMPTOMS = "text",
      CV_OTHER_SYMPTOMS = "text",
      CV_COMORBIDITIES = "text",
      CV_OTHER_COMORBIDITIES = "text",
      CV_GESTATION = "numeric",
      CV_CLOSE_CONTACT = "numeric"
    ))
  
  # Remove cases without a state
  dat <- dat %>%
    filter(!is.na(STATE))
  
  
  # tidy up dates and parsse place of acquisition to local (Australia) vs. overseas
  dat <- dat %>%
    mutate(
      TRUE_ONSET_DATE = clean_date(TRUE_ONSET_DATE),
      NOTIFICATION_RECEIVE_DATE = clean_date(NOTIFICATION_RECEIVE_DATE)
    ) %>%
    mutate(
      import_status = ifelse(
        is.na(PLACE_OF_ACQUISITION) |
          grepl("^1101|^00038888", PLACE_OF_ACQUISITION),
        "local",
        "imported"
      )
    )
  
  # record state of acquisition
  dat <- dat %>%
    # fill in missing places of acquisition with correct code
    mutate(
      PLACE_OF_ACQUISITION = ifelse(
        is.na(PLACE_OF_ACQUISITION),
        "00038888",
        PLACE_OF_ACQUISITION)
    ) %>%
    mutate(
      postcode = substr(PLACE_OF_ACQUISITION, 5, 8),
      state_of_acquisition = case_when(
        grepl("^26", postcode) ~ "ACT",
        grepl("^2", postcode) ~ "NSW",
        grepl("^3", postcode) ~ "VIC",
        grepl("^4", postcode) ~ "QLD",
        grepl("^5", postcode) ~ "SA",
        grepl("^6", postcode) ~ "WA",
        grepl("^7", postcode) ~ "TAS",
        grepl("^08", postcode) ~ "NT",
        TRUE ~ "NA"
      ),
      state_of_acquisition = ifelse(
        state_of_acquisition == "NA",
        NA,
        state_of_acquisition
      )
    )
  
  # Generate linelist data
  linelist <- dat %>%
    # notification receive date seems buggy, and is sometimes before the notification date and speecimen collection
    mutate(
      date_confirmation = pmax(NOTIFICATION_RECEIVE_DATE,
                               NOTIFICATION_DATE,
                               na.rm = TRUE),
      # # when the above is mixed with NAs, the return value is a numeric :/
      # date_confirmation = as.POSIXct(date_confirmation,
      #                             origin = as.Date("1970-01-01"))
    ) %>%
    select(
      date_onset = TRUE_ONSET_DATE,
      date_detection = SPECIMEN_DATE,
      date_confirmation,
      region = STATE,
      import_status,
      state_of_acquisition
    ) %>%
    mutate(
      report_delay = as.numeric(date_confirmation - date_onset),
      date_linelist = as.Date(ll_date),
      region = as.factor(region)
    ) %>%
    
    # Remove those with onset date after confirmation date
    # only keep individuals with date of confirmation after onset date if less than 2 days (inclusive) because
    # we assume some individuals tested via contact tracing will test positive before symptom onset and therefore plausible
    # (noting that reporting delay distribution only calculated from positive differences)
    # also remove any individuals with NA for both notification and symptom onset dates
    filter(
      date_confirmation >= (date_onset - 2) | is.na(date_confirmation) | is.na(date_onset)
    ) %>%
    filter(
      !(is.na(date_confirmation) & is.na(date_onset))
    ) %>%
    mutate_at(
      vars(starts_with("date_")),
      ~as.Date(.)
    ) %>%
    # for cases missing a date of detection, assume it's the day before the date
    # of confirmation (1 days is the median and mode of this delay distribution)
    mutate(
      date_detection = case_when(
        is.na(date_detection) ~ date_confirmation - 1,
        TRUE ~ date_detection
      )
    )
  
  # save a formatted copy, and return
  saveRDS(
    linelist, 
    file.path(dir, "linelist_formatted.RDS")
  )
  
  linelist
  
}

# function to get a greta array forecasting numbers of locally-acquired cases
# in each state into the future.

# local cases and imported cases should be matrices conntaining integer (or
# fractional) numbers of observed or assumed cases infected on each date.
# Reff_locals and Reff_imports should be either matrices or 2D greta arrays of
# transmission potential for locally-acquired and imported cases. All four of
# these arguments must have the same number of columns. Reff_locals and
# Reff_imports must have the same number of rows, which should be greater than
# or equal to the numbers of rows in local_cases and imported_cases. Where
# local_cases and imported_cases have fewer rows than the other matrices, they
# will be padded with zeros to represent an assumption of no imported cases or
# other local cases injecxted into the local population. dates must be a vector
# of dates with as many elements as rows in the Reff matrices, andgi_cdf must be
# a function returning the continuous version of the generation interval
# distribution.

# the function first computes the number of *primary* local cases - those
# infected by imported cases - and then uses a discrete convolution to compute
# the expected number of secondary local infections (local-local) into the
# future. Note this a deterministic simulation of real-valued case counts, so it
# is impossible for a simulated outbreak to go extinct, and a case count of
# close to 0 cases will inevitably lead to a large outbreak if Reff exceeds 1.
forecast_locals <- function (local_cases, imported_cases,
                             Reff_locals, Reff_imports,
                             dates, gi_cdf,
                             simulation_start = dates[nrow(local_cases)],
                             gi_bounds = c(0, 20)) {
  
  n_dates <- length(dates)
  n_states <- ncol(Reff_locals)
  
  # check inputs
  if (nrow(Reff_locals) != n_dates |
      nrow(Reff_imports) != n_dates) {
    stop("Reff_locals and Reff_imports must have the same number of rows ",
         "as there are elements in dates")
  }
  
  if (ncol(Reff_imports) != n_states |
      ncol(local_cases) != n_states |
      ncol(imported_cases) != n_states) {
    stop("all input matrices must have the same number of columns")
  }
  
  if (nrow(local_cases) > n_dates |
      nrow(imported_cases) > n_dates) {
    stop("local_cases and imported_cases must fewer or equal numbers ",
         "of rows to the Reff matrices")
  }
  
  # pad the cases data if needed
  local_cases <- pad_cases_matrix(local_cases, n_dates, which = "after")
  imported_cases <- pad_cases_matrix(imported_cases, n_dates, which = "after")
  
  # create the generation interval matrix and vector
  gi_mat <- gi_matrix(gi_cdf, dates, gi_bounds = gi_bounds)
  gi_vec <- gi_vector(gi_cdf, max(dates), gi_bounds = gi_bounds)
  
  # infectiousness of imported cases over time
  imported_infectious <- gi_mat %*% imported_cases
  
  # expected number of primary (import-local) locally-acquired cases
  primary_local_cases <- imported_infectious * Reff_imports
  
  # infectiousness of primary locally-acquired cases
  primary_local_infectiousness <- gi_mat %*% primary_local_cases
  
  # infectiousness of observed (or assumed) locally-acquired cases
  existing_local_infectiousness <- gi_mat %*% local_cases
  
  # sum get local infectiousness not caused by dynamic cases
  local_infectiousness <- existing_local_infectiousness +
    primary_local_infectiousness
  
  # work out where to simulate from
  start_idx <- match(simulation_start, dates)
  sim_idx <- seq(start_idx, n_dates, by = 1)
  
  # simulate the expected numbers of secondary local cases
  secondary_local_cases <- project_local_cases(
    infectiousness = local_infectiousness[sim_idx, ],
    R_local = Reff_locals[sim_idx, ],
    disaggregation_probs = gi_vec
  )
  
  # pad the result with 0s to represent simulated cases
  secondary_local_cases <- pad_cases_matrix(secondary_local_cases,
                                            n_dates,
                                            "before")
  
  # matrix of just the primary local cases after the observed cases
  new_primary_local_cases <- zeros(n_dates, n_states)
  new_primary_local_cases[sim_idx, ] <- primary_local_cases[sim_idx, ]
  
  # combine with primary local cases to get the total number of new
  # locally-acquired cases
  forecast_local_cases <- local_cases + new_primary_local_cases + secondary_local_cases
  
  # get the probability (poisson assumption) of one for more new
  # locally-acquired cases
  forecast_local_infectious <- gi_mat %*% forecast_local_cases
  expected_transmission <- forecast_local_infectious * Reff_locals + 
    primary_local_cases
  p_cases <- 1 - exp(-expected_transmission)

  list(
    local_cases = forecast_local_cases,
    seecondary_local_cases = secondary_local_cases,
    probability_of_cases = p_cases
  )
  
}

pad_cases_matrix <- function(cases, n_dates, which = c("after", "before")) {
  
  which <- match.arg(which)
  
  if (nrow(cases) < n_dates) {
    
    pad <- matrix(0,
                  nrow = n_dates - nrow(cases),
                  ncol = ncol(cases))

    if (inherits(cases, "greta_array")) {
      pad <- as_data(pad)
    }
    
    cases <- switch(which,
                    before = rbind(pad, cases),
                    after = rbind(cases, pad))
  }
  
  cases
  
}

# build a convolution matrix for the discrete generation interval, applying the
# effect of improving surveillance and normalising to integrate to 1
gi_matrix <- function(gi_cdf, dates, gi_bounds = c(0, 20)) {
  
  n_dates <- length(dates)
  
  # baseline GI matrix, without effects of improved surveillance
  day_diff <- time_difference_matrix(n_dates)
  gi_mat_naive <- gi_probability(gi_cdf, day_diff)
  
  # compute fraction surviving without detection in circulant matrix format,
  # multiply by GI matrix and rescale to get new GI distribution on each day
  ttd_mat <- day_diff
  ttd_mat[] <- ttd_survival(days = c(day_diff),
                            dates = rep(dates, each = n_dates))
  rel_gi_mat <- gi_mat_naive * ttd_mat
  scaling <- surveillance_effect(
    dates = dates,
    cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  gi_mat <- sweep(rel_gi_mat, 2, scaling, FUN = "/")
  
  gi_mat
  
}

# build a vector of discrete generation interval probability masses for a given
# date, applying the effect of improving surveillance and normalising to
# integrate to 1
gi_vector <- function(gi_cdf, date, gi_bounds = c(0, 20)) {

  # baseline GI vector, without effects of improved surveillance
  days <- seq(gi_bounds[1], gi_bounds[2])
  gi_vec_naive <- gi_probability(gi_cdf, days = days, bounds = gi_bounds)
  
  # compute fraction surviving without detection in circulant matrix format,
  # multiply by GI matrix and rescale to get new GI distribution on each day
  ttd_vec <- ttd_survival(days = days, dates = rep(date, each = length(days)))
  rel_gi_vec <- gi_vec_naive * ttd_vec
  scaling <- surveillance_effect(
    dates = date,
    cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  gi_vec <- rel_gi_vec / scaling
  
  gi_vec
  
}

save_ggplot <- function (filename,
                         dir = "outputs",
                         subdir = "figures",
                         multi = TRUE,
                         width = 11.69 / 2,
                         height = 8.27 / 3,
                         scale = 1,
                         dpi = 150) {
  
  # david does 8.27 x 11.69 (landscape A4) for 3x2 panels
  # aspect ratio of 0.707:1 h:w
  # want A4 *portrait* width (8.27) with same aspect ratio
  
  if (multi) {
    
    # work out dimensions for 4x2 panels for reports
    ratio <- height / width
    mfrow <- c(4, 2)
    width <- height * 3
    height <- (width / mfrow[2]) * ratio * mfrow[1] * 1.2
    scale <- 0.8
    
  } else {
    
    height <- height * 1.25
    
  }
  
  path <- file.path(dir, subdir, filename)
  ggsave(path,
         width = width,
         height = height,
         scale = scale,
         dpi = dpi)
  
}

prep_melbourne_postcodes <- function(
  box = c(xmin = 144.8, ymin = -38, xmax = 145.3, ymax = -37.6),
  filepath = "data/abs/melbourne_postal.RDS"
) {
  
  library(sf)
  
  # crop post office areas to Melbourne region
  # find populations for them
  # pull out S2 point locations for them from Google data
  poa <- st_read(
    "data/abs/POA_2016_AUST.shp",
    stringsAsFactors = FALSE
  ) %>%
    filter(
      stringr::str_starts(POA_CODE16, "3")
    )
  
  # crop postal areas to Melbourne-ish
  melbourne_crop <- st_crop(poa, box)
  inter <- st_intersects(poa, melbourne_crop)
  keep <- vapply(inter, length, FUN.VALUE = numeric(1)) > 0
  melbourne <- poa[keep, ]
  
  # load meshblocks and find those intersecting with these postal areas
  mesh <- st_read(
    "data/abs/MB_2016_VIC.shp",
    stringsAsFactors = FALSE
  )
  inter <- st_intersects(mesh, melbourne)
  keep <- vapply(inter, length, FUN.VALUE = numeric(1)) > 0
  melbourne_mesh <- mesh[keep, ]
  
  # load populations of all meshblocks
  mesh_pop <- read_csv(
    "data/abs/2016 census mesh block counts.csv",
    col_types = cols(
      MB_CODE_2016 = col_character(),
      MB_CATEGORY_NAME_2016 = col_character(),
      AREA_ALBERS_SQKM = col_double(),
      Dwelling = col_double(),
      Person = col_double(),
      State = col_double()
    )
  ) %>%
    rename(
      MB_CODE16 = MB_CODE_2016
    )
  
  # add populations onto shapefile
  melbourne_mesh <- melbourne_mesh %>%
    left_join(
      mesh_pop
    )
  
  # get intersection with postal areas to sum populations
  coverages <- st_covered_by(melbourne_mesh, melbourne)
  idx <- rep(NA, length(coverages))
  idx[lengths(coverages) > 0] <- unlist(coverages)
  
  postal_pop <- melbourne_mesh %>%
    st_drop_geometry() %>%
    mutate(
      POA_CODE16 = melbourne$POA_CODE16[idx]
    ) %>%
    group_by(POA_CODE16) %>%
    summarise(
      POP = sum(Person)
    )
  
  melbourne %>%
    left_join(postal_pop) %>%
    mutate(
      POP_DENS = POP / AREASQKM16
    ) %>%
    st_write(filepath, delete_layer = TRUE)
  
}

# colours for plotting
blue <- "steelblue3"
purple <- "#C3A0E8"
green <- brewer.pal(8, "Set2")[1]
yellow <- brewer.pal(8, "Set2")[6]
blue_green <- colorRampPalette(c("blue", green))(10)[8]
yellow_green <- colorRampPalette(c("yellow", green))(10)[8]
orange <- brewer.pal(8, "Set2")[2]
pink <- brewer.pal(8, "Set2")[4]