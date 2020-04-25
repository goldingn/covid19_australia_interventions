library(readr)
library(dplyr)
library(stringr)

# read in and tidy up Facebook movement data
facebook_mobility <- function() {
  
  file <- "data/fb_data/au_gadm_mobility_statistics.20200420.csv"
  data <- readr::read_csv(file) %>%
    dplyr::select(
      state = polygon_name,
      date = ds,
      "movement range" = all_day_bing_tiles_visited_relative_change,
      "staying still" = all_day_ratio_single_tile_users
    ) %>%
    tidyr::pivot_longer(
      cols = c("movement range", "staying still"),
      names_to = "metric",
      values_to = "trend"
    ) %>%
    mutate(date = lubridate::date(date))
  
  # set the staying home variable against a baseline of the first two weeks
  baseline <- data %>%
    filter(date < lubridate::date("2020-03-15")) %>%
    group_by(state, metric) %>%
    summarise(baseline = mean(trend))
  
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
  url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev14/v1/en-us/applemobilitytrends-2020-04-23.csv"
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
  url <- "https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_20200424.csv"
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
  
  facebook <- facebook_mobility() %>%
    mutate(
      datastream = str_c("Facebook: ", metric)
    ) %>%
    dplyr::select(
      -metric
    )
  
  citymapper <- citymapper_mobility() %>%
    mutate(
      datastream = str_c("Citymapper: directions")
    )
  
  # combine the datasets
  bind_rows(
    google,
    apple,
    facebook,
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

# a b-spline constrained to 0-1
latent_spline <- function (x, knots = 3) {
  bases <- splines::bs(x, df = knots)
  # for identifiability, make one weight positive. That way the rest can define
  # their sign relative to this, and the loading defines the overall sign
  weights1 <- normal(0, 1, truncation = c(0, Inf))
  weights2 <- normal(0, 1, dim = knots - 1)
  weights <- c(weights1, weights2)
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

# summarise the posterior for a vector greta array
summarise_vec_posterior <- function(vector,
                                    draws,
                                    quantiles = c(0.025, 0.975),
                                    nsim = 1000) {
  vector_sim <- calculate(vector, values = draws, nsim = nsim)[[1]][, , 1]
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
       ylab = "effect",
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
