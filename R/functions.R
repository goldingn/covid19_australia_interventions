source("R/lib.R")

Sys.setenv(RETICULATE_AUTOCONFIGURE = FALSE)
library(greta)
library(greta.gp)
library(tensorflow)

tfp <- reticulate::import("tensorflow_probability")

module <- greta::.internals$utils$misc$module
fl <- greta:::fl
tf_float <- greta:::tf_float


library(readr)
library(dplyr)
library(stringr)
library(rjson)
library(tidyr)
library(readxl)
library(RColorBrewer)
library(purrr)
library(ggplot2)
library(R6)
library(slider)
library(cowplot)
library(lubridate)
library(rvest)
library(magrittr)
library(conmat)
library(bayesplot)
library(gaussquad)
library(orthopolynom)

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
    )
  ) %>%
    filter(
      country_region == "Australia" & is.na(sub_region_2)
    ) %>%
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

# scrape out the URL to apple mobility data
apple_url <- function() {
  base_url <- "https://covid19-static.cdn-apple.com"
  json_data <- base_url %>%
    file.path("covid19-mobility-data/current/v3/index.json") %>%
    jsonlite::fromJSON()
  paste0(base_url, json_data$basePath, json_data$regions$`en-us`$csvPath)
}

# download and format Apple's mobility data
apple_mobility <- function() {
  data <- apple_url() %>%
    readr::read_csv(
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
      cols = !any_of(
        c(
          "geo_type",
          "region",
          "transportation_type",
          "alternative_name",
          "sub-region",
          "country"
        )
      ),
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

# try a bunch of previous days to find the most recent citymapper dataset
citymapper_url <- function(min_delay = 0, max_delay = 28) {
  date <- Sys.Date()
  delay <- min_delay
  while(delay < max_delay) {
    # try various dates to find the citymapper URL
    datestring <- format(date - delay, format = "%Y%m%d")
    
    url <- paste0(
      "https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_",
      datestring,
      ".csv"
    )
    
    # return if successful, or increment
    if (url_exists(url)) {
      return(url)
    } else {
      delay <- delay + 1
    }
    
  }
  
  # error if we hit the timeout
  if (delay == max_delay) {
    stop ("could not find a valid citymapper URL")
  }
  
}

# load the Citymapper index (urban direction requests, mostly public transport)
# for a couple of cities
citymapper_mobility <- function() {
  
  data <- citymapper_url() %>%
    readr::read_csv(
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
  
  # apple <- apple_mobility() %>%
  #   mutate(
  #     datastream = str_c("Apple: directions for ", transportation_type)
  #   ) %>%
  #   dplyr::select(
  #     -geo_type,
  #     -transportation_type
  #   )
  
  # facebook <- facebook_mobility() %>%
  #   mutate(
  #     datastream = str_c("Facebook: ", metric)
  #   ) %>%
  #   dplyr::select(
  #     -metric
  #   )
  
  # citymapper ended https://citymapper.com/news/2393/citymapper-mobility-index-comes-to-an-end
  # citymapper <- citymapper_mobility() %>%
  #   mutate(
  #     datastream = str_c("Citymapper: directions")
  #   )
  
  # combine the datasets
  bind_rows(
    google#,
    #apple#,
    # facebook,
    #citymapper
  )
  
}

# Google dropped a bunch of previous data from the latest file. Pull a cached
# version from the tidycovid package on GitHub and replace it.
tidycovid_url <- "https://github.com/goldingn/tidycovid19/raw/e4db3ab3007576f34dcb1e8c3299b235cff6198e/cached_data/google_cmr.RDS"

# append any missing google mobility data to mobility data (because Google
# dropped a bunch of data out this one time)
append_google_data <- function(mobility_data, url = tidycovid_url) {
  
  mobility_data <- mobility_data %>%
    filter(!is.na(trend)) %>%
    filter(!is.na(state)) %>%
    select(state, date, trend, datastream)
  
  
  file <- "data/google_cmr/tidycovid_cache.RDS"
  if (!file.exists(file)) {
    dowload_tidycovid()
  }
  tmp <- readRDS(file)
  
  
  # get the previous data in the same format
  previous_data <- tmp$country_region %>%
    filter(iso3c == "AUS") %>%
    rename(
      retail_and_recreation = retail_recreation,
      grocery_and_pharmacy = grocery_pharmacy,
      state = region
    ) %>%
    pivot_longer(
      cols = c(
        "retail_and_recreation",
        "grocery_and_pharmacy",
        "parks",
        "transit_stations",
        "workplaces",
        "residential"
      ),
      values_to = "trend",
      names_to = "category"
    ) %>%
    mutate(
      category = str_replace_all(category, "_", " ")
    ) %>%
    mutate(
      datastream = str_c("Google: time at ", category)
    ) %>%
    select(
      state,
      date,
      trend,
      datastream
    )
  
  # find any observations that are missing in the mobility data
  missing_data <- mobility_data %>%
    anti_join(previous_data, ., by = c("state", "date", "datastream"))
  
  # add them and return
  mobility_data %>%
    anti_join(missing_data, by = c("state", "date", "datastream")) %>%
    bind_rows(missing_data) %>%
    arrange(datastream, state, date)
  
}


abbreviate_states <- function(state_names) {
  case_when(
    state_names %in% c("Australian Capital Territory", "ACT") ~ "ACT",
    state_names %in% c("New South Wales", "NSW") ~ "NSW",
    state_names %in% c("Northern Territory", "NT") ~ "NT",
    state_names %in% c("Queensland", "QLD") ~ "QLD",
    state_names %in% c("South Australia", "SA") ~ "SA",
    state_names %in% c("Tasmania", "TAS") ~ "TAS",
    state_names %in% c("Victoria", "VIC") ~ "VIC",
    state_names %in% c("Western Australia", "WA") ~ "WA"
  )
}

unabbreviate_states <- function(state_names) {
  case_when(
    state_names %in% c("Australian Capital Territory", "ACT") ~ "Australian Capital Territory",
    state_names %in% c("New South Wales", "NSW") ~ "New South Wales",
    state_names %in% c("Northern Territory", "NT") ~ "Northern Territory",
    state_names %in% c("Queensland", "QLD") ~ "Queensland",
    state_names %in% c("South Australia", "SA") ~ "South Australia",
    state_names %in% c("Tasmania", "TAS") ~ "Tasmania",
    state_names %in% c("Victoria", "VIC") ~ "Victoria",
    state_names %in% c("Western Australia", "WA") ~ "Western Australia"
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

interventions <- function(
  which = c(
    "all",
    "national",
    "act",
    "nsw",
    "nt",
    "qld",
    "sa",
    "tas",
    "vic",
    "wa"
  ),
  end_dates = FALSE,
  exclude_after = NA
) {
  
  which <- match.arg(which)
  
  act_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-08-13", "ACT" # lockdown from 1700 12/08/2021 https://twitter.com/ACTHealth/status/1425652154526621700?s=20
  )
  
  nsw_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-06-25", "NSW", # stay-at-home order for 4 LGAs from 11.59 PM 24th, extended to all greater sydney +++ from 11.59 PM 25th. 
    "2021-07-18", "NSW", # increased restrictions from midnight 17th https://www.nsw.gov.au/media-releases/restrictions-to-further-limit-spread-of-covid-19-delta-strain
    "2021-08-15", "NSW" # increase to state-wide lockdown 5 PM 2021/08/14 https://twitter.com/NSWHealth/status/1426735650384998400?s=20
  )
  
  nt_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-06-27", "NT", # https://coronavirus.nt.gov.au/updates/items/2021-06-27-covid-19-update-lockdown-restrictions-in-place
    "2021-08-16", "NT" # in place from midday the 16th https://coronavirus.nt.gov.au/updates/items/2021-08-16-lockdown-restrictions-in-place
  )
  
  qld_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-01-09", "QLD",
    "2021-03-29", "QLD",
    "2021-06-29", "QLD", # starts 6 PM on 29th https://www.qld.gov.au/health/conditions/health-alerts/coronavirus-covid-19/current-status/public-health-directions/restrictions-in-qld-update
    "2021-08-01", "QLD" # starts 4 PM 31st July https://www.qld.gov.au/health/conditions/health-alerts/coronavirus-covid-19/current-status/public-health-directions/restrictions-impacted-areas
  )
  
  sa_interventions <- tibble::tribble(
    ~date, ~state,
    "2020-11-19", "SA",
    "2021-07-21", "SA" # lockdown, starts 6pm on the 20th, https://www.sahealth.sa.gov.au/wps/wcm/connect/public+content/sa+health+internet/about+us/news+and+media/all+media+releases/covid-19+update+20+july+2021
  )
  
  tas_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-10-16", "TAS" # https://www.premier.tas.gov.au/covid-19_updates/press_confernce_-_15_october_2021
  )
  
  vic_interventions <- tibble::tribble(
    ~date, ~state,
    "2020-07-01", "VIC",
    "2020-07-08", "VIC",
    "2020-08-02", "VIC",
    "2021-02-13", "VIC",
    "2021-05-28", "VIC",
    "2021-07-16", "VIC", # lockdown, 5 days from 11:59 the 15th, then extended https://www.dhhs.vic.gov.au/coronavirus-update-victoria-15-july-2021
    "2021-08-06", "VIC" # lockdown from 20:00 2021/08/05 
  )
  
  wa_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-01-31", "WA",
    "2021-04-24", "WA",
    "2021-06-29", "WA" # https://www.wa.gov.au/government/announcements/4-day-lockdown-introduced-perth-and-peel
  )
  
  
  national_interventions <- expand_grid(
    date = c("2020-03-16", "2020-03-24", "2020-03-29"),
    state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
  )
  
  
  if(end_dates){
    
    act_interventions <-  act_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-10-15", "ACT" # lockdown end 11:59 PM 2021/10/14 https://twitter.com/ACTHealth/status/1447778422755708933?s=20
        )
      )
    
    nsw_interventions <-  nsw_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-10-11", "NSW" # https://www.nsw.gov.au/media-releases/ready-set-go-nsw-prepares-to-re-open
        )
      )
    
    nt_interventions <-  nt_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-07-02", "NT", # https://coronavirus.nt.gov.au/updates/items/2021-06-28-covid-19-update-nt
          "2021-08-20", "NT" # lifted from 2021/08/19 1200 https://coronavirus.nt.gov.au/updates/items/2021-08-19-lockdown,-restrictions-and-border-entry-requirements
        )
      )
    
    qld_interventions <-  qld_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-01-12", "QLD",
          "2021-04-01", "QLD",
          "2021-07-04", "QLD", # Lifted 6 PM 3rd July https://www.qld.gov.au/health/conditions/health-alerts/coronavirus-covid-19/current-status/public-health-directions/restrictions-impacted-areas
          "2021-08-09", "QLD" # lifted 4 PM 8th Aug https://www.qld.gov.au/health/conditions/health-alerts/coronavirus-covid-19/current-status/public-health-directions/restrictions-in-qld-update
        )
      )
    
    sa_interventions <-  sa_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2020-11-22", "SA",
          "2021-07-28", "SA" # restrictions eased from 28th https://www.sahealth.sa.gov.au/wps/wcm/connect/public+content/sa+health+internet/about+us/news+and+media/all+media+releases/covid-19+update+28+july+2021
        )
      )
    
    tas_interventions <-  tas_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-10-19", "TAS" # https://www.premier.tas.gov.au/covid-19_updates/press_confernce_-_15_october_2021
        )
      )
    
    vic_interventions <-  vic_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-02-18", "VIC",
          "2021-06-11", "VIC",
          "2021-07-28", "VIC", # lockdown lifted 11.59 PM 2021/07/27 https://www.premier.vic.gov.au/lockdown-lifted-across-victoria
          "2021-10-22", "VIC" # lockdown lifted 11.59 PM 2021/10/21
        )
      )
    
    wa_interventions <-  wa_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-02-05", "WA",
          "2021-04-27", "WA",
          "2021-07-03", "WA" # https://www.wa.gov.au/government/announcements/end-of-lockdown-perth-and-peel-1201am-saturday-3-july
        )
      )
  }
  
  interventions <- switch(
    which,
    national = national_interventions %>%
      filter(state == "ACT") %>%
      mutate(state = "all"),
    act = act_interventions,
    nsw = nsw_interventions,
    nt  = nt_interventions,
    qld = qld_interventions,
    sa  = sa_interventions,
    tas = tas_interventions,
    vic = vic_interventions,
    wa =  wa_interventions,
    all = bind_rows(
      national_interventions,
      act_interventions,
      nsw_interventions,
      nt_interventions,
      qld_interventions,
      sa_interventions,
      tas_interventions,
      vic_interventions,
      wa_interventions,
    )
  ) %>%
    mutate(
      date = as.Date(date),
      state = factor(state)
    ) %>% 
    arrange(state, date)
  
  if(!is.na(exclude_after)){
    
    interventions <- interventions %>%
      filter(date <= as.Date(exclude_after))
  }
  
  return(interventions)
}

quarantine_dates <- function() {
  expand_grid(
    date = c("2020-03-15", "2020-03-28"),
    state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", 
              "VIC", "WA")
  ) %>%
    mutate(
      date = as.Date(date),
      state = factor(state)
    )
}

# dates of school holidays by state, from:
# 2020: https://info.australia.gov.au/about-australia/special-dates-and-events/school-term-dates
# 2021 onwards:
# https://www.education.act.gov.au/__data/assets/pdf_file/0007/801583/Term-Date-2019-2025.pdf
# https://education.nsw.gov.au/public-schools/going-to-a-public-school/calendars
# https://nt.gov.au/learning/primary-and-secondary-students/school-term-dates-in-nt
# https://education.qld.gov.au/about-us/calendar/term-dates
# https://www.education.sa.gov.au/teaching/south-australian-state-schools-term-dates
# https://www.education.tas.gov.au/about-us/term-dates/term-dates-2021/
# https://www.education.vic.gov.au/about/department/Pages/datesterm.aspx
# https://www.education.wa.edu.au/future-term-dates/
school_holiday_dates <- function() {
  dplyr::bind_rows(
    tibble::tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-26",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-19", "2021-01-28",
                    5, "2021-04-02", "2021-04-18",
                    6, "2021-06-26", "2021-07-11",
                    7, "2021-09-18", "2021-10-04",
                    8, "2021-12-18", "2022-01-27"
    ) %>%
      mutate(
        state = "Australian Capital Territory"
      ),
    tibble::tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-26",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-19", "2021-01-26",
                    5, "2021-04-05", "2021-04-16",
                    6, "2021-06-28", "2021-07-09",
                    7, "2021-09-20", "2021-10-01",
                    8, "2021-12-20", "2022-01-27"
    ) %>%
      mutate(
        state = "New South Wales"
      ),
    tibble::tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-19",
                    2, "2020-06-27", "2020-07-20",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-18", "2021-01-31",
                    5, "2021-04-10", "2021-04-18",
                    6, "2021-06-26", "2021-07-19",
                    7, "2021-09-25", "2021-10-10",
                    8, "2021-12-17", "2022-01-30"
    ) %>%
      mutate(
        state = "Northern Territory"
      ),
    tibble::tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-04", "2020-04-19",
                    2, "2020-06-27", "2020-07-12",
                    3, "2020-09-19", "2020-10-05",
                    4, "2020-12-12", "2021-01-26",
                    5, "2021-04-02", "2021-04-18",
                    6, "2021-06-26", "2021-07-11",
                    7, "2021-09-18", "2021-10-05",
                    8, "2021-12-11", "2022-01-26"
    ) %>%
      mutate(
        state = "Queensland"
      ),
    tibble::tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-26",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-12", "2021-01-26",
                    5, "2021-04-10", "2021-04-26",
                    6, "2021-07-03", "2021-07-18",
                    7, "2021-09-25", "2021-10-10",
                    8, "2021-12-11", "2022-01-30"
    ) %>%
      mutate(
        state = "South Australia"
      ),
    tibble::tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-26",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-18", "2021-02-02",
                    5, "2021-04-10", "2021-04-25",
                    6, "2021-07-03", "2021-07-19",
                    7, "2021-09-25", "2021-10-10",
                    8, "2021-12-17", "2022-02-02"
    ) %>%
      mutate(
        state = "Tasmania"
      ),
    tibble::tribble(~school_holiday, ~start, ~end,
                    # Vic extended each school holiday by a week during the
                    # pandemic
                    # https://www.education.vic.gov.au/about/department/Pages/datesterm.aspx
                    1, "2020-03-25", "2020-04-13",
                    2, "2020-06-27", "2020-07-19",
                    3, "2020-09-19", "2020-10-04",
                    4, "2020-12-19", "2021-01-26",
                    5, "2021-04-02", "2021-04-18",
                    6, "2021-06-26", "2021-07-11",
                    7, "2021-09-18", "2021-10-03",
                    8, "2021-12-18", "2022-01-27"
    ) %>%
      mutate(
        state = "Victoria"
      ),
    tibble::tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-27",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-18", "2021-01-31",
                    5, "2021-04-02", "2021-04-18",
                    6, "2021-07-03", "2021-07-18",
                    7, "2021-09-25", "2021-10-10",
                    8, "2021-12-17", "2022-01-30"
    ) %>%
      mutate(
        state = "Western Australia"
      )
  ) %>%
    mutate(
      start = lubridate::date(start),
      end = lubridate::date(end)
    ) %>%
    # loop through, expanding out into dates within term time
    mutate(id = row_number()) %>%
    group_by(id) %>%
    do(
      tibble(
        state = .$state,
        school_holiday = .$school_holiday, 
        date = seq(from = .$start, to =.$end, by = 1)
      )
    ) %>%
    ungroup() %>%
    select(-id)
}

download_holiday_dates <- function(destination) {
  
  # create the directory if needed
  directory <- dirname(destination)
  if (!file.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  list(
    "2020" = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/c4163dc4-4f5a-4cae-b787-43ef0fcf8d8b/download/australian_public_holidays_2020.csv",
    "2021" = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/2dee10ef-2d0c-44a0-a66b-eb8ce59d9110/download/australian_public_holidays_2021.csv",
    "2022" = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/d256f989-8f49-46eb-9770-1c6ee9bd2661/download/australian_public_holidays_2022.csv"
  ) %>%
    lapply(
      read_csv,
      col_types = 
        cols(
          Date = col_date(format = "%Y%m%d"),
          `Holiday Name` = col_character(),
          Information = col_character(),
          `More Information` = col_character(),
          Jurisdiction = col_character()
        )
    ) %>%
    do.call(
      bind_rows, .
    ) %>%
    mutate(
      state = toupper(Jurisdiction),
      state = unabbreviate_states(state),
      date = Date,
      name = `Holiday Name`
    ) %>%
    select(state, date, name) %>%
    write_csv(destination)
}

# dates of public holidays by state, from:
# https://data.gov.au/dataset/ds-dga-b1bc6077-dadd-4f61-9f8c-002ab2cdff10/details?q=
holiday_dates <- function(holiday_file = "data/holidays/public_holidays.csv") {
  
  if (!file.exists(holiday_file)) {
    download_holiday_dates(holiday_file)
  }
  
  read_csv(
    holiday_file,
    col_types = cols(
      state = col_character(),
      date = col_date(format = ""),
      name = col_character()
    )
  )
  
}

lockdown_lgas <- function() {
  c(
    "Banyule (C)",
    "Bayside (C)",
    "Boroondara (C)",
    "Brimbank (C)", 
    "Cardinia (S)",
    "Casey (C)",
    "Darebin (C)",
    "Frankston (C)", 
    "Glen Eira (C)",
    "Greater Dandenong (C)",
    "Hobsons Bay (C)", 
    "Hume (C)",
    "Kingston (C) (Vic.)",
    "Knox (C)",
    "Manningham (C)", 
    "Maribyrnong (C)",
    "Maroondah (C)",
    "Melbourne (C)",
    "Melton (C)", 
    "Mitchell (S)",
    "Monash (C)",
    "Moonee Valley (C)",
    "Moreland (C)", 
    "Mornington Peninsula (S)",
    "Nillumbik (S)",
    "Port Phillip (C)", 
    "Stonnington (C)",
    "Whitehorse (C)",
    "Whittlesea (C)",
    "Wyndham (C)", 
    "Yarra (C)",
    "Yarra Ranges (S)"
  )
}

nsw_lga_active_cases <- function() {
  # need to copy from here: https://www.health.nsw.gov.au/Infectious/covid-19/Pages/stats-local.aspx
  # paste in using datapasta addin
  # and remove the annoying asterisk and parentheses in title
  tibble::tribble(
    ~Local.Government.Area,  ~Cases, ~Cases.with.unknown.source, ~Total.tests,   ~Test.rate.per.1000,
    "Albury",       2,                          0,         3068,                    56,
    "Armidale Regional",       0,                          0,         1109,                    36,
    "Ballina",       0,                          0,         1986,                    45,
    "Balranald",       0,                          0,           87,                    37,
    "Bathurst Regional",       0,                          0,         2228,                    51,
    "Bayside",       2,                          0,         7368,                    41,
    "Bega Valley",       0,                          0,         2267,                    66,
    "Bellingen",       0,                          0,          484,                    37,
    "Berrigan",       0,                          0,          376,                    43,
    "Blacktown",       8,                          1,        22713,                    61,
    "Bland",       0,                          0,          269,                    45,
    "Blayney",       0,                          0,          309,                    42,
    "Blue Mountains",       5,                          0,         5687,                    72,
    "Bogan",       0,                          0,          102,                    40,
    "Bourke",       0,                          0,           92,                    36,
    "Brewarrina",       0,                          0,           56,                    35,
    "Broken Hill",       0,                          0,          611,                    35,
    "Burwood",       0,                          0,         1434,                    35,
    "Byron",       2,                          0,         1841,                    52,
    "Cabonne",       0,                          0,          342,                    25,
    "Camden",       2,                          1,        12092,                   119,
    "Campbelltown",       9,                          1,        16784,                    98,
    "Canada Bay",       0,                          0,         5867,                    61,
    "Canterbury-Bankstown",      14,                          1,        20189,                    53,
    "Carrathool",       0,                          0,           43,                    15,
    "Central Coast",       0,                          0,        16536,                    48,
    "Central Darling",       0,                          0,           64,                    35,
    "Cessnock",       0,                          0,         2158,                    36,
    "Clarence Valley",       0,                          0,         1448,                    28,
    "Cobar",       0,                          0,          176,                    38,
    "Coffs Harbour",       0,                          0,         2785,                    36,
    "Coolamon",       0,                          0,          188,                    43,
    "Coonamble",       0,                          0,          120,                    30,
    "Cootamundra-Gundagai Regional",       0,                          0,          468,                    42,
    "Cowra",       0,                          0,          450,                    35,
    "Cumberland",      13,                          0,        13060,                    54,
    "Dubbo Regional",       0,                          0,         2418,                    45,
    "Dungog",       0,                          0,          342,                    36,
    "Edward River",       0,                          0,          451,                    50,
    "Eurobodalla",       3,                          0,         4314,                   112,
    "Fairfield",      25,                          0,        16440,                    78,
    "Federation",       0,                          0,          490,                    39,
    "Forbes",       0,                          0,          308,                    31,
    "Georges River",       4,                          0,         7083,                    44,
    "Gilgandra",       0,                          0,          170,                    40,
    "Glen Innes Severn",       0,                          0,          287,                    32,
    "Goulburn Mulwaree",       0,                          0,         1419,                    46,
    "Greater Hume Shire",       0,                          0,          584,                    54,
    "Griffith",       0,                          0,         1244,                    46,
    "Gunnedah",       0,                          0,          435,                    34,
    "Gwydir",       0,                          0,           98,                    18,
    "Hawkesbury",       0,                          0,         4158,                    62,
    "Hay",       0,                          0,           69,                    23,
    "Hilltops",       0,                          0,          731,                    39,
    "Hornsby",       0,                          0,         6419,                    42,
    "Hunters Hill",       0,                          0,         1620,                   108,
    "Inner West",       0,                          0,        16140,                    80,
    "Inverell",       0,                          0,          618,                    37,
    "Junee",       0,                          0,          200,                    30,
    "Kempsey",       0,                          0,         1114,                    37,
    "Kiama",       0,                          0,         1439,                    62,
    "Ku-ring-gai",       0,                          0,         8177,                    64,
    "Kyogle",       0,                          0,          291,                    33,
    "Lachlan",       0,                          0,          170,                    28,
    "Lake Macquarie",       2,                          0,        10389,                    50,
    "Lane Cove",       0,                          0,         4544,                   113,
    "Leeton",       0,                          0,          311,                    27,
    "Lismore",       0,                          0,         2008,                    46,
    "Lithgow",       0,                          0,          854,                    40,
    "Liverpool",      32,                          2,        22937,                   101,
    "Liverpool Plains",       0,                          0,          259,                    33,
    "Lockhart",       0,                          0,          109,                    33,
    "Maitland",       0,                          0,         5846,                    69,
    "Mid-Coast",       0,                          0,         2956,                    32,
    "Mid-Western Regional",       0,                          0,         1150,                    46,
    "Moree Plains",       0,                          0,          337,                    25,
    "Mosman",       0,                          0,         1646,                    53,
    "Murray River",       0,                          0,          185,                    15,
    "Murrumbidgee",       0,                          0,          146,                    37,
    "Muswellbrook",       0,                          0,          750,                    46,
    "Nambucca",       0,                          0,          618,                    31,
    "Narrabri",       0,                          0,          379,                    29,
    "Narrandera",       0,                          0,          171,                    29,
    "Narromine",       0,                          0,          242,                    37,
    "Newcastle",       0,                          0,        11168,                    67,
    "North Sydney",       0,                          0,         3434,                    46,
    "Northern Beaches",       0,                          0,        13685,                    50,
    "Oberon",       0,                          0,          175,                    32,
    "Orange",       0,                          0,         2002,                    47,
    "Parkes",       0,                          0,          423,                    29,
    "Parramatta",      11,                          0,        11404,                    44,
    "Penrith",       2,                          1,        15811,                    74,
    "Port Macquarie-Hastings",       0,                          0,         3466,                    41,
    "Port Stephens",       3,                          0,         6845,                    93,
    "Queanbeyan-Palerang Regional",       0,                          0,         1942,                    32,
    "Randwick",       0,                          0,         9269,                    60,
    "Richmond Valley",       0,                          0,         1016,                    43,
    "Ryde",       0,                          0,         6019,                    46,
    "Shellharbour",       1,                          0,         4538,                    62,
    "Shoalhaven",       0,                          0,         4764,                    45,
    "Singleton",       0,                          0,         1517,                    65,
    "Snowy Monaro Regional",       0,                          0,         1033,                    50,
    "Snowy Valleys",       0,                          0,          642,                    44,
    "Strathfield",       0,                          0,         2665,                    57,
    "Sutherland Shire",       5,                          0,        15905,                    69,
    "Sydney",       3,                          1,        15135,                    61,
    "Tamworth Regional",       0,                          0,         2413,                    39,
    "Temora",       0,                          0,          178,                    28,
    "Tenterfield",       0,                          0,          144,                    22,
    "The Hills Shire",       6,                          0,        11766,                    66,
    "Tweed",       0,                          0,         3217,                    33,
    "Unincorporated NSW",       0,                          0,            0,                     0,
    "Upper Hunter Shire",       0,                          0,          604,                    43,
    "Upper Lachlan Shire",       0,                          0,          309,                    38,
    "Uralla",       0,                          0,          146,                    24,
    "Wagga Wagga",       0,                          0,         3385,                    52,
    "Walcha",       0,                          0,           82,                    26,
    "Walgett",       0,                          0,          305,                    51,
    "Warren",       0,                          0,          172,                    64,
    "Warrumbungle Shire",       0,                          0,          439,                    47,
    "Waverley",       1,                          1,         4832,                    65,
    "Weddin",       0,                          0,          172,                    48,
    "Wentworth",       0,                          0,          352,                    50,
    "Willoughby",       0,                          0,         3255,                    40,
    "Wingecarribee",       0,                          0,         4698,                    92,
    "Wollondilly",       5,                          0,         4349,                    82,
    "Wollongong",       4,                          1,        11981,                    55,
    "Woollahra",       0,                          0,         4161,                    70,
    "Yass Valley",       0,                          0,          466,                    27
  )
  
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
  
  op("negative_binomial_pmf",
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

# the pre-intervention serial interval of Taslim Ali et al.
taslim_ali_cdf <- function() {
  
  params <- lognormal_prior(mean = 7.8, sd = 5.2)
  
  gi_cdf <- function(days) {
    plnorm(days, meanlog = params$mean, sdlog = params$sd)
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

# given the mean and standard deviation of a lognormal distribution, compute the
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

plot_trend <- function(
  simulations,
  data,
  base_colour = grey(0.4),
  multistate = FALSE,
  hline_at = 1,
  ylim = c(0, 6),
  ybreaks = NULL,
  intervention_at = interventions(),
  projection_at = NA,
  keep_only_rows = NULL,
  max_date = data$dates$latest_mobility,
  min_date = as.Date("2020-03-01"),
  plot_voc = FALSE,
  plot_vax = FALSE
) {
  
  if(is.na(min_date)){
    min_date <- max_date - months(6)
  }
  
  
  mean <- colMeans(simulations)
  ci_90 <- apply(simulations, 2, quantile, c(0.05, 0.95)) 
  ci_50 <- apply(simulations, 2, quantile, c(0.25, 0.75))
  
  if (multistate) {
    states <- rep(data$states, each = data$n_dates_project)
    dates <- rep(data$dates$infection_project, data$n_states)
  } else {
    dates <- data$dates$infection_project
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
    filter(
      date >= min_date,
      date <= max_date
    ) %>%
    mutate(type = "Nowcast")
  
  
  if (length(unique(df$date)) >= 200){
    date_breaks <- "3 month"
    date_minor_breaks <- "1 month"
    date_labels <- "%b%y"
    x_text_angle <- 0
    x_text_size <- 9
    x_text_hjust <- 0.5
    x_text_vjust <- 0.5
  } else if(length(unique(df$date)) < 50){
    date_breaks <- "1 week"
    date_minor_breaks <- "1 day"
    date_labels <- "%e-%m"
    x_text_angle <- 0
    x_text_size <- 9
    x_text_hjust <- 0.5
    x_text_vjust <- 0.5
  } else {
    date_breaks <- "1 month"
    date_minor_breaks <- "2 weeks"
    date_labels <- "%b%y"
    x_text_angle <- 0
    x_text_size <- 9
    x_text_hjust <- 0.5
    x_text_vjust <- 0.5
  }
  
  
  
  # date_breaks <- ifelse(
  #   length(unique(df$date)) < 200,
  #   "1 month",
  #   "2 month"
  # )
  # 
  # date_minor_breaks <- ifelse(
  #   length(unique(df$date)) < 200,
  #   "2 weeks",
  #   "1 month"
  # )
  
  #range(ylim)[2] - range(ylim)[1]
  
  #x_text_size <- ifelse(length(unique(df$date)) < 200, 10, 9)
  
  
  if (is.null(ylim)) {
    ylim <- c(min(df$ci_90_lo), max(df$ci_90_hi)) 
  }
  
  if (is.null(ybreaks)){
    if(range(ylim)[2] - range(ylim)[1] >= 4 & range(ylim)[2] - range(ylim)[1] <= 10){
      y_scale <- scale_y_continuous(position = "right", breaks = seq(from = ylim[1], to = ylim[2], by = 1))
      
    } else(
      y_scale <- scale_y_continuous(position = "right")
    )
  } else {
    y_scale <- scale_y_continuous(position = "right", breaks = seq(from = ybreaks[1], to = ybreaks[2], by = 1))
  }
  
  p <- ggplot(df) + 
    
    aes(date, mean, fill = type) +
    
    xlab(element_blank()) +
    
    coord_cartesian(ylim = ylim) +
    y_scale +
    scale_x_date(date_breaks = date_breaks, date_minor_breaks = date_minor_breaks, date_labels = date_labels) +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    
    geom_vline(
      aes(xintercept = date),
      data = intervention_at,
      colour = "grey75"
    ) +
    
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
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = x_text_size, angle = x_text_angle, hjust = x_text_hjust, vjust = x_text_vjust))
  
  if(plot_voc){
    p <- p + 
      geom_vline(
        data = prop_variant_dates(),
        aes(xintercept = date),
        colour = "firebrick1",
        linetype = 5
      )
  }
  
  if(plot_vax){
    p <- p + 
      geom_vline(
        data = vaccination_dates(),
        aes(xintercept = date),
        colour = "steelblue3",
        linetype = 5
      ) 
  }
  
  if (multistate) {
    p <- p + facet_wrap(~ state, ncol = 2, scales = "free")
  }
  
  if (!is.na(projection_at)) {
    p <- p +
      geom_vline(xintercept = projection_at, linetype = "dashed", colour = "grey60") +
      annotate("rect",
               xmin = projection_at,
               xmax = max(df$date),
               ymin = -Inf,
               ymax = Inf,
               fill = grey(0.5), alpha = 0.1)
  }
  
  p    
  
}


# plot_trend_long is outdated at this point as plot_trend has been updated
# was intended as alternative to plot_trend with wider x-axis for longer timeseries
# still may be useful and need updating in that case to include voc
plot_trend_long <- function(simulations,
                            data,
                            base_colour = grey(0.4),
                            multistate = FALSE,
                            hline_at = 1,
                            ylim = c(0, 4),
                            intervention_at = interventions(),
                            projection_at = NA,
                            keep_only_rows = NULL,
                            max_date = data$dates$latest_mobility,
                            min_date = as.Date("2020-03-01")) {
  
  mean <- colMeans(simulations)
  ci_90 <- apply(simulations, 2, quantile, c(0.05, 0.95))
  ci_50 <- apply(simulations, 2, quantile, c(0.25, 0.75))
  
  if (multistate) {
    states <- rep(data$states, each = data$n_dates_project)
    dates <- rep(data$dates$infection_project, data$n_states)
  } else {
    dates <- data$dates$infection_project
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
    filter(
      date >= min_date,
      date <= max_date
    ) %>%
    mutate(type = "Nowcast")
  
  if (is.null(ylim)) {
    ylim <- c(min(df$ci_90_lo), max(df$ci_90_hi)) 
  }
  
  p <- ggplot(df) + 
    
    aes(date, mean, fill = type) +
    
    xlab(element_blank()) +
    
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_breaks = "1 month", date_labels = "%e/%m") +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    geom_vline(
      aes(xintercept = date),
      data = intervention_at,
      colour = "grey80"
    ) +
    
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
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = 8))
  
  if (multistate) {
    p <- p + facet_wrap(
      facets = ~ state,
      ncol = 2,
      scales = "free",
      strip.position = "left"
    ) +
      theme(
        strip.text.y.left = element_text(
          hjust = 0,
          vjust = 1,
          face = "bold",
          angle = 0
        )
      )
  }
  
  if (!is.na(projection_at)) {
    p <- p +
      geom_vline(xintercept = projection_at, linetype = "dashed", colour = "grey60") +
      annotate("rect",
               xmin = projection_at,
               xmax = max(df$date),
               ymin = -Inf,
               ymax = Inf,
               fill = grey(0.5), alpha = 0.1)
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
                (f <- tempfile()), method='wget')
  
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
    tidyr::complete(participant_id, hh_member, location, fill = list(contacts = 0)) %>%
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
  # increase the uncertainty on the baseline contact rate, since the survey
  # design of Polymod is not the same as those we have fielded, and the
  # demographic conversion in Prem may also make it less comparable
  baseline_contact_params$se_contacts[2] <- OC_0_prior$se * 5
  
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
      old_right_join(
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
trends_date_state <- function (file, dates = NULL, states = NULL) {
  
  trends <- readRDS(file)
  
  date_seq <- seq(min(trends$date), max(trends$date), by = 1)
  
  if (is.null(states)) {
    states <- unique(trends$state)
  }
  
  index <- trends %>%
    # expand to all required dates and states
    select(state, date, mean) %>%
    old_right_join(
      expand_grid(
        state = states,
        date = date_seq
      )
    ) %>%
    # turn into a date-by-state matrix
    pivot_wider(
      names_from = state,
      values_from = mean
    ) %>%
    select(-date) %>%
    as.matrix()
  
  # crop to specified dates, extending either end out flat if missing
  if (!is.null(dates)) {
    idx <- dates - min(date_seq) + 1
    idx <- pmax(idx, 1)
    idx <- pmin(idx, nrow(index))
    index <- index[idx, ]
  }
  
  index
  
}

# get the overall index of distancing (no waning) on the current dates and
# optionally add extra 1s at the end
social_distancing_national <- function(dates, n_extra = 0) {
  
  distancing_file <- "outputs/social_distancing_latent.RDS"
  distancing_index <- distancing_file %>%
    readRDS() %>%
    select(mean, date) %>%
    old_right_join(tibble(date = dates)) %>%
    replace_na(list(mean = 0)) %>%
    pull(mean)
  
  distancing_index <- c(distancing_index, rep(1, n_extra))
  distancing_index
  
}


# prop_voc_date_state <- function() {
#   tibble::tribble(
#     ~state,        ~date, ~prop_voc,
#      "ACT", "2021-01-27",         1,
#      "NSW", "2021-01-27",         1,
#       "NT", "2021-01-27",         1,
#      "QLD", "2021-01-27",         1,
#       "SA", "2021-01-27",         1,
#      "TAS", "2021-01-27",         1,
#      "VIC", "2021-01-27",         1,
#       "WA", "2021-01-27",         1
#   ) %>%
#     mutate(
#       date = as.Date(date)
#     )
# }
# 
# prop_voc_date_state_long <- function(dates) {
#   
#   df <- expand_grid(
#     date = dates,
#     state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
#   ) %>%
#     full_join(
#       prop_voc_date_state()
#     )%>%
#     pivot_wider(
#       names_from = state,
#       values_from = prop_voc
#     ) %>%
#     dplyr::select(-date) %>%
#     as.matrix
#   
#   
#   df[1,] <- apply(
#     X = df[1,] %>% as.matrix,
#     MARGIN = 2,
#     FUN = function(x){
#       ifelse(is.na(x), 0, x)
#     }
#   ) %>%
#     t
#   
#   df <- df %>%
#     as_tibble %>%
#     fill(everything()) %>%
#     as.matrix
#   
#   
#   return(df)
# }

prop_variant_dates <- function(){
  tibble::tribble(
    ~state,        ~date, ~prop_wt, ~prop_alpha, ~prop_delta, ~prop_omicron,
    "ACT",  "2020-01-01",        1,           0,           0,             0,
    "NSW",  "2020-01-01",        1,           0,           0,             0,
    "NT",   "2020-01-01",        1,           0,           0,             0,
    "QLD",  "2020-01-01",        1,           0,           0,             0,
    "SA",   "2020-01-01",        1,           0,           0,             0,
    "TAS",  "2020-01-01",        1,           0,           0,             0,
    "VIC",  "2020-01-01",        1,           0,           0,             0,
    "WA",   "2020-01-01",        1,           0,           0,             0,
    
    "ACT",  "2021-01-27",        0,           1,           0,             0,
    "NSW",  "2021-01-27",        0,           1,           0,             0,
    "NT",   "2021-01-27",        0,           1,           0,             0,
    "QLD",  "2021-01-27",        0,           1,           0,             0,
    "SA",   "2021-01-27",        0,           1,           0,             0,
    "TAS",  "2021-01-27",        0,           1,           0,             0,
    "VIC",  "2021-01-27",        0,           1,           0,             0,
    "WA",   "2021-01-27",        0,           1,           0,             0,
    
    "ACT",  "2021-06-07",        0,           0,           1,             0,
    "NSW",  "2021-06-07",        0,           0,           1,             0,
    "NT",   "2021-06-07",        0,           0,           1,             0,
    "QLD",  "2021-06-07",        0,           0,           1,             0,
    "SA",   "2021-06-07",        0,           0,           1,             0,
    "TAS",  "2021-06-07",        0,           0,           1,             0,
    "VIC",  "2021-06-07",        0,           0,           1,             0,
    "WA",   "2021-06-07",        0,           0,           1,             0,
    
    "ACT",  "2021-12-01",        0,           0,           0,             1,
    "NSW",  "2021-12-01",        0,           0,           0,             1,
    "NT",   "2021-12-01",        0,           0,           0,             1,
    "QLD",  "2021-12-01",        0,           0,           0,             1,
    "SA",   "2021-12-01",        0,           0,           0,             1,
    "TAS",  "2021-12-01",        0,           0,           0,             1,
    "VIC",  "2021-12-01",        0,           0,           0,             1,
    "WA",   "2021-12-01",        0,           0,           0,             1
  ) %>%
    mutate(
      date = as.Date(date)
    )
}

prop_variant <- function(dates){
  
  df <-  prop_variant_dates() %>%
    full_join(
      y = expand_grid(
        date = dates,
        state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
      )
    ) %>%
    arrange(state, date) %>%
    tidyr::fill(everything()) %>%
    filter(date %in% dates) # account for "2020-01-01" start date may not be in dates
  
  prop_wt <- df %>%
    dplyr::select(state, date, prop_wt) %>%
    pivot_wider(
      names_from = state,
      values_from = prop_wt
    ) %>%
    arrange(date) %>%
    dplyr::select(-date)%>%
    as.matrix
  
  prop_alpha <- df %>%
    dplyr::select(state, date, prop_alpha) %>%
    pivot_wider(
      names_from = state,
      values_from = prop_alpha
    ) %>%
    arrange(date) %>%
    dplyr::select(-date)%>%
    as.matrix
  
  prop_delta <- df %>%
    dplyr::select(state, date, prop_delta) %>%
    pivot_wider(
      names_from = state,
      values_from = prop_delta
    ) %>%
    arrange(date) %>%
    dplyr::select(-date)%>%
    as.matrix
  
  prop_omicron <- df %>%
    dplyr::select(state, date, prop_omicron) %>%
    pivot_wider(
      names_from = state,
      values_from = prop_omicron
    ) %>%
    arrange(date) %>%
    dplyr::select(-date)%>%
    as.matrix
  
  prop_variant <- list(
    "prop_wt"    = prop_wt,
    "prop_alpha" = prop_alpha,
    "prop_delta" = prop_delta,
    "prop_omicron" = prop_omicron
  )
  
  return(prop_variant)
}


# greta sub-model for the component R_eff due to macro- and micro-distancing
distancing_effect_model <- function(
  dates,
  gi_cdf,
  voc_mixture = c("all", "alpha", "delta", "omicron", "wt")
) {
  
  voc_mixture <- match.arg(voc_mixture)
  
  # informative priors on variables for contacts at t = 0 (Hx = household, Ox =
  # non-household, Tx = total, xC = contacts. xD = duration)
  baseline_contact_params <- baseline_contact_parameters(gi_cdf)
  
  
  prop_var <- prop_variant(dates = dates)
  prop_alpha <- prop_var$prop_alpha
  prop_delta <- prop_var$prop_delta
  prop_omicron <- prop_var$prop_omicron
  prop_wt    <- prop_var$prop_wt
  
  if(voc_mixture == "alpha") {
    prop_wt <- prop_wt * 0
    prop_alpha <- prop_alpha * 0 + 1
    prop_delta <- prop_delta * 0
    prop_omicron <- prop_omicron * 0
  }
  
  if(voc_mixture == "delta") {
    prop_wt <- prop_wt * 0
    prop_alpha <- prop_alpha * 0 
    prop_delta <- prop_delta * 0 + 1
    prop_omicron <- prop_omicron * 0
  }
  
  if(voc_mixture == "omicron") {
    prop_wt <- prop_wt * 0
    prop_alpha <- prop_alpha * 0 
    prop_delta <- prop_delta * 0
    prop_omicron <- prop_omicron * 0 + 1
  }
  
  if(voc_mixture == "wt") {
    prop_wt <- prop_wt * 0 + 1
    prop_alpha <- prop_alpha * 0 
    prop_delta <- prop_delta * 0
    prop_omicron <- prop_omicron * 0
  }
  
  
  # prior on the probability of *not* transmitting, per hour of contact
  # (define to match moments of R0 prior)
  logit_p_params <- logit_p_prior(baseline_contact_params, gi_cdf)
  logit_p <- normal(logit_p_params$meanlogit, logit_p_params$sdlogit)
  p <- ilogit(logit_p)
  
  phi_alpha       <- normal(1.454, 0.055, truncation = c(0, Inf))
  phi_delta_alpha <- normal(1.421, 0.033, truncation = c(0, Inf))
  phi_omicron <- 2.347662
  
  phi_delta <- phi_alpha * phi_delta_alpha
  
  phi_star <- prop_wt * 1 + prop_alpha * phi_alpha + prop_delta * phi_delta + prop_omicron * phi_omicron
  
  p_star <- p ^ phi_star
  
  
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
  
  # trends in non-household contacts in each state over time
  OC_t_state <- trends_date_state(
    "outputs/macrodistancing_trends.RDS",
    dates
  )
  OC_0 <- OC_t_state[1, 1]
  
  # model gamma_t: reduction in duration and transmission probability of
  # non-household contacts over time, per state
  
  # load probability of microdistancing and divide by the maximum value to get
  # an index of per-contact transmission probability
  microdistancing_prob <- trends_date_state(
    "outputs/microdistancing_trends.RDS",
    dates
  )
  d_t_state <- microdistancing_prob / max(microdistancing_prob)
  
  beta <- uniform(0, 1)
  gamma_t_state <- 1 - beta * d_t_state
  
  # compute component of R_eff for local cases
  household_infections <- HC_0 * (1 - p_star ^ HD_t)
  non_household_infections <- OC_t_state * gamma_t_state *
    infectious_days * (1 - p_star ^ OD_0)
  R_t <- household_infections + non_household_infections
  
  # return greta arrays
  list(R_t = R_t, 
       gamma_t_state = gamma_t_state,
       OC_t_state = OC_t_state,
       p = p,
       p_star = p_star,
       beta = beta,
       HC_0 = HC_0,
       HD_0 = HD_0,
       OC_0 = OC_0,
       OD_0 = OD_0,
       dates = dates,
       phi_alpha = phi_alpha,
       phi_delta_alpha = phi_delta_alpha,
       phi_delta = phi_delta,
       phi_star = phi_star)
  
}

#distancing_effect_model <- function(dates, gi_cdf) {
distancing_effect_model_old <- function(dates, gi_cdf) {
  
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
  
  # trends in non-household contacts in each state over time
  OC_t_state <- trends_date_state(
    "outputs/macrodistancing_trends.RDS",
    dates
  )
  OC_0 <- OC_t_state[1, 1]
  
  # model gamma_t: reduction in duration and transmission probability of
  # non-household contacts over time, per state
  
  # load probability of microdistancing and divide by the maximum value to get
  # an index of per-contact transmission probability
  microdistancing_prob <- trends_date_state(
    "outputs/microdistancing_trends.RDS",
    dates
  )
  d_t_state <- microdistancing_prob / max(microdistancing_prob)
  
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
       beta = beta,
       HC_0 = HC_0,
       HD_0 = HD_0,
       OC_0 = OC_0,
       OD_0 = OD_0,
       dates = dates)
  
}


plot_fit <- function(observed_cases, cases_sim, data) {
  
  valid <- which(data$valid_mat, arr.ind = TRUE)
  dates <- data$dates$infection
  states <- data$data$states
  
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

# read in the national survey raw data and create contact and microdistancing
# data in the same firmat as originally shared. I decided I can have a weird
# recursive function, as a treat.
format_raw_survey_data <- function(file = NULL, wave = NULL) {
  
  # in default mode, loop through all extra raw files and try to get them in order
  if (is.null(file)) {
    files <- list.files("data/survey_raw/", pattern = ".csv$", full.names = TRUE)
    lengths <- nchar(files)
    dates <- files %>%
      substr(lengths - 9, lengths - 4) %>%
      as.Date("%d%m%y")
    files <- files[order(dates)]
    waves <- seq_along(files) + 14
    mapply(format_raw_survey_data, files, waves)
    return(invisible(NULL))
  }
  
  raw <- read_csv(file)
  
  if ("Q222" %in% names(raw)) {
    mask <- raw %>%
      select(
        state = S3,
        date = StartDate,
        face_covering = Q222
      )%>%
      mutate(
        state = case_when(state == "ACT" ~ "Australian Capital Territory",
                          TRUE ~ state),
        state = abbreviate_states(state)
      ) %>%
      filter(!is.na(state)) %>%
      mutate(
        date = as.character(date),
        date = as.Date(date, format = "%Y%m%d"),
        date = min(date)
      ) %>%
      group_by(state, date) %>%
      mutate(respondents = n()) %>%
      group_by(state, date, face_covering, respondents) %>%
      summarise(
        count = n(),
        .groups = "drop"
      ) %>%
      select(date,
             state,
             face_covering,
             count,
             respondents) %>%
      arrange(state, date, face_covering)
    
  } else {
    date_survey <- raw$StartDate %>%
      as.character %>%
      as.Date(format = "%Y%m%d") %>%
      min
    
    mask <- expand_grid(
      date = date_survey,
      state = states,
      face_covering = c("Always", "No", "Often", "Rarely", "Sometimes"),
      count = 0,
      respondents = 0
    )
  }
  
  
  write_csv(
    mask,
    paste0(
      "data/face_covering/face_covering_",
      wave,
      "_.csv"
    )
  )
  
  
  micro <- raw %>%
    select(
      state = S3,
      date = StartDate,
      "1.5m compliance" = Q65,
      "non-household contact" = Q109,
      "hand hygine" = Q110,
      "cough" = Q111
    ) %>%
    mutate(
      state = case_when(state == "ACT" ~ "Australian Capital Territory",
                        TRUE ~ state),
      state = abbreviate_states(state)
    ) %>%
    filter(!is.na(state)) %>%
    mutate(
      date = as.character(date),
      date = as.Date(date, format = "%Y%m%d"),
      date = min(date)
    ) %>%
    pivot_longer(
      cols = c(
        "1.5m compliance",
        "non-household contact",
        "hand hygine",
        "cough"
      ),
      names_to = "question",
      values_to = "response"
    ) %>%
    group_by(state, date, question) %>%
    mutate(respondents = n()) %>%
    group_by(state, date, question, respondents) %>%
    count(response) %>%
    select(date,
           state,
           question,
           response,
           count = n,
           respondents) %>%
    arrange(state, date, question, response)
  
  write_csv(micro,
            paste0(
              "data/microdistancing/Barometer wave ",
              wave,
              " compliance.csv"
            ))
  
  contacts <- raw %>%
    select(state = S3,
           num_contacts = Q138) %>%
    mutate(state = replace_na(state, "Other")) %>%
    group_by(num_contacts, state) %>%
    summarise(n = n()) %>%
    pivot_wider(
      names_from = state,
      values_from = n,
      values_fill = list(n = 0)
    )
  
  
  write_csv(contacts,
            paste0("data/contacts/barometer/contact numbers wave ", wave, ".csv"))
  
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

# move columns of this matrix one to the left, and replace the right-most column
# with 1s. Used to define a weights matrix for a piecewise linear curve.
next_column <- function(x) {
  cbind(x[, -1], rep(1, nrow(x)))
}

# model for the trend in microdistancing
# compared to the macrodistancing effect, this has a linear transform of the
# distancing coefficient on the logit scale - corresponding to a different
# spread in adoption of microdistancing behaviour - and different dates of peak
# microdistancing and subsequent inflections
microdistancing_model <- function(data, parameters) {
  
  # shapes of inflection effects
  # timing of inflections in each state
  n_states <- nrow(parameters$inflections)
  segment_ends <- cbind(parameters$inflections, ones(n_states))
  denoms <- next_column(parameters$inflections) - parameters$inflections
  
  inflections_long <- parameters$inflections[data$state_id, ]
  denominators <- next_column(inflections_long) - inflections_long
  numerators <- sweep(-inflections_long, 1, data$time, FUN = "+")
  inflection_shape <- cbind(ones(length(data$state)), numerators / denominators)
  
  # masking columns to create weights matrix
  null1 <- (sign(inflection_shape) + 1) / 2
  null2 <- next_column(1 - null1)
  null3 <- next_column(null2)
  
  # piecewise linear weights matrix
  weights_up <- inflection_shape * null1 * null2
  anti_weights <- next_column(weights_up)
  weights_down <- (1 - anti_weights) * (1 - null2) * null3
  weights <- weights_up + weights_down 
  
  # apply weights to get inflections, and apply to initial distancing effect
  heights_long <- parameters$heights[data$state_id, ]
  inflection <- rowSums(weights * heights_long)
  
  # apply the initial distancing period (shrunk to meet the first height)
  initial <- (1 - data$distancing) * heights_long[, 1] 
  inflection - initial
  
}

log_imultilogit <- function(x) {
  
  dim <- dim(x)
  
  # check it's a matrix
  if (length(dim) != 2) {
    stop("log_imultilogit expects a 2D greta array",
         call. = FALSE)
  }
  
  op("log_imultilogit", x,
     dim = dim + c(0, 1),
     tf_operation = "tf_log_imultilogit")
}

tf_log_imultilogit <- function(x) {
  batch_size <- tf$shape(x)[[0]]
  shape <- c(batch_size, dim(x)[[2]], 1L)
  zeros <- tf$zeros(shape, tf_float())
  latent <- tf$concat(list(x, zeros), 2L)
  tf$nn$log_softmax(latent)
}


# model for the trend in macrodistancing a weighted sum of time at location
# types, and an overall scaling coefficient, multiplied by a scalar baseline
# contact rate
macrodistancing_model <- function(data, parameters) {
  
  # format data into a date/state by location greta array of log ratios of
  # mobility on baseline
  log_location_change <- data$location_change_trends %>%
    mutate_at(
      vars(public, home, retail, transit, work),
      log
    ) %>%
    # flip log ratio for at home (regression coefficients are all positive)
    mutate(home = -home) %>%
    # turn into a matrix
    select(-state, -date) %>%
    as.matrix() %>%
    # and into a greta array
    as_data()
  
  # expected log change in contacts
  log_change_contacts <- log_location_change %*% parameters$mobility_coefs
  
  # average daily number of contacts
  log_mean_daily_contacts <- log(parameters$OC_0) + log_change_contacts
  mean_daily_contacts <- exp(log_mean_daily_contacts)
  
  # model the fraction of contacts falling on each day of the week via a multilogit model
  n <- nrow(log_mean_daily_contacts)
  X <- cbind(ones(n), log_mean_daily_contacts)
  eta <- X %*% parameters$weekday_coefs
  
  # imultilogit gives the value for every weekday against each date, so pull out only the
  # actual weekday for that date
  idx <- cbind(
    seq_len(n),
    lubridate::wday(data$location_change_trends$date)
  )
  
  log_fraction_weekly_contacts <- log_imultilogit(eta)[idx]
  
  # this equivalent to:
  #   log_fraction_weekly_contacts_wday <- eta[idx] - log_sum_exp(eta)
  # but shouldn't be much different speed-wise
  
  # use this to apply the weekday effect
  log_mean_weekly_contacts <- log_mean_daily_contacts + log(7)
  log_mean_daily_contacts_wday <- log_mean_weekly_contacts + log_fraction_weekly_contacts
  
  # convert both to wide (date by state) format for lookups
  mean_daily_contacts_wide <- greta_long_to_date_state(
    mean_daily_contacts,
    data$location_change_trends$date,
    data$location_change_trends$state
  )
  
  log_mean_daily_contacts_wday_wide <- greta_long_to_date_state(
    log_mean_daily_contacts_wday,
    data$location_change_trends$date,
    data$location_change_trends$state
  )
  
  log_fraction_weekly_contacts_wide <- greta_long_to_date_state(
    log_fraction_weekly_contacts,
    data$location_change_trends$date,
    data$location_change_trends$state
  )
  
  # return wide version of all these
  list(
    mean_daily_contacts = mean_daily_contacts_wide,
    log_mean_daily_contacts_wday = log_mean_daily_contacts_wday_wide,
    log_fraction_weekly_contacts = log_fraction_weekly_contacts_wide
  )
  
}

# a sort of null model (assuming a different rate of contacts per survey/state) for plotting the data 
macrodistancing_null <- function(data, log_fraction_weekly_contacts_mean) {
  
  # extract indices to the survey waves and states for each observation
  
  # add numeric ids for wave dates and states
  wave_dates <- sort(unique(data$contacts$wave_date))
  states <- sort(unique(data$contacts$state))
  dates <- sort(unique(data$location_change_trends$date))
  
  idx <- data$contacts %>%
    mutate(
      wave_id = match(wave_date, wave_dates),
      state_id = match(state, states)
    ) %>%
    select(wave_id, state_id) %>%
    as.matrix()
  
  n_waves <- length(wave_dates)
  n_states <- length(states)
  
  # hierarchical model for the average number of contacts per survey/state
  log_contacts_mean <- normal(0, 10)
  log_contacts_sd <- normal(0, 1, truncation = c(0, Inf))
  log_contacts_raw <- normal(0, 1, dim = c(n_waves, n_states))
  log_contacts_wide <- log_contacts_mean + log_contacts_raw * log_contacts_raw
  
  # expand these out to match the data
  log_mean_daily_contacts <- log_contacts_wide[idx]
  
  # this has the fractions for each date and state, so pull out the relevant
  # entries
  idx <- data$contacts %>%
    mutate(
      date_id = match(date, dates),
      state_id = match(state, states)
    ) %>%
    select(date_id, state_id) %>%
    as.matrix()
  
  # use this to apply the weekday effect
  log_mean_weekly_contacts <- log_mean_daily_contacts + log(7)
  log_predicted_contacts <- log_mean_weekly_contacts + log_fraction_weekly_contacts_mean[idx]
  
  # get lognormal parameters from mean and standard deviation
  sdlog <- normal(0, 5, truncation = c(0, Inf))
  
  # because mean = exp(meanlog + (sdlog ^ 2) / 2)
  meanlog <- log_predicted_contacts - (sdlog ^ 2) / 2
  
  distribution(data$contacts$contact_num) <- discrete_lognormal(
    meanlog = meanlog,
    sdlog = sdlog,
    breaks = data$breaks
  )
  
  # return greta arrays to fit model  
  module(
    avg_daily_contacts_wide = exp(log_contacts_wide),
    sdlog,
    wave_dates,
    states
  )
  
}

# take a vector greta array correpsonding to dates and states and convert to
# date-by-state wide format
greta_long_to_date_state <- function(long, dates, states) {
  wide_dim <- c(n_distinct(dates), n_distinct(states))
  greta_array(long, dim = wide_dim)
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
check_projection <- function(fitted_model, start_date = as.Date("2020-02-28")) {
  
  R_eff_local <- fitted_model$greta_arrays$R_eff_loc_12
  R_eff_imported <- fitted_model$greta_arrays$R_eff_imp_12
  gi_vec <- gi_vector(gi_cdf, fitted_model$data$dates$latest, state = "ACT")
  local_infectiousness <- fitted_model$data$local$infectiousness
  imported_infectiousness <- fitted_model$data$imported$infectiousnes
  local_cases <- fitted_model$data$local$cases
  dates <- fitted_model$data$dates$infection
  n_states <- fitted_model$data$n_states
  n_dates <- fitted_model$data$n_dates
  
  # national-level Reff - no clusters and weighted by state populations
  local_weights <- sweep(
    local_infectiousness,
    1,
    rowSums(local_infectiousness),
    FUN = "/"
  )
  local_weights[is.na(local_weights)] <- 1 / n_states
  
  import_weights <- sweep(
    imported_infectiousness,
    1,
    rowSums(imported_infectiousness),
    FUN = "/"
  )
  import_weights[is.na(import_weights)] <- 1 / n_states
  
  R_eff_loc_ntnl <- rowSums(R_eff_local[seq_len(n_dates), ] * local_weights)
  R_eff_imp_ntnl <- rowSums(R_eff_imported[seq_len(n_dates), ] * import_weights)
  
  # subset to from the first of March, when transmission became established (the
  # model is not designed to work with the stochastic extinctions we saw at the beginning of the outbreak)
  start <- which(dates == start_date)
  sub_idx <- start:n_dates
  
  # simulate local-local transmission dynamics, at national level; forced using
  # (observed) case importation and local cases prior to the start of the
  # simulation
  
  # locally-acquired infections present prior to the start of the simulation
  previous_local_cases <- local_cases
  previous_local_cases[sub_idx, ] <- 0
  previous_local_infectiousness <- gi_convolution(
    cases = previous_local_cases,
    dates = dates,
    states = data$states,
    gi_cdf = gi_cdf
  )
  # previous_local_infectiousness <- rowSums(previous_local_infectiousness)
  
  # compute infectious forcing from local cases emerging during this period that
  # were directly infected by imported cases (can't just include the import
  # infectiousness, since they are subject to a different Reff). Get expected
  # number of new local cases from imports, then disaggregate according to their
  # infectiousness profile to get force of local infection
  
  # expected number of new locally-acquired cases during the simulation period due
  # to infection from imports
  import_local_cases <- sweep(imported_infectiousness, 1, R_eff_imp_ntnl[seq_len(n_dates)], FUN = "*")
  import_local_cases_ntnl <- rowSums(import_local_cases)
  import_local_infectiousness <- gi_convolution(
    cases = import_local_cases,
    dates = dates,
    states = data$states,
    gi_cdf = gi_cdf
  )
  
  # combine these to get forcing from existing and import-associated local cases,
  # and disaggregate to get infectiousness of these
  local_infectiousness <- previous_local_infectiousness + import_local_infectiousness
  
  # Given this basic force of infection, R for locally-acquired cases (mean trend,
  # no clusters), and the infectiousness profile, iterate the dynamics to compute
  # the numbers of local cases
  secondary_locals <- project_local_cases(
    infectiousness = rowSums(local_infectiousness)[sub_idx],
    R_local = R_eff_loc_ntnl[sub_idx],
    disaggregation_probs = gi_vec
  )
  
  # compute locally-acquired cases
  local_cases_project_ntnl <- import_local_cases_ntnl[sub_idx] + secondary_locals
  local_cases_project_ntnl_sim <- calculate(local_cases_project_ntnl,
                                            values = fitted_model$draws,
                                            nsim = 1000)[[1]]
  
  data <- fitted_model$data
  data$dates$infection_project <- dates[sub_idx]
  data$n_dates_project <- length(sub_idx)
  
  local_cases_ntnl <- rowSums(local_cases[sub_idx, ])
  plot_trend(local_cases_project_ntnl_sim,
             data = data,
             multistate = FALSE,
             ylim = c(0, 2 * max(local_cases_ntnl)),
             hline_at = NULL,
             min_date = start_date,
             base_colour = green) +
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
  
  # load the first set of awkward datasets and their dates
  contact_data_list <- list(
    
    # Freya's survey waves
    load_contacts_by_state(
      "data/contacts/freya_survey/contact_counts_NG_wave1.csv",
      as.Date("2020-04-04")
    ),
    
    load_contacts_by_state(
      "data/contacts/freya_survey/contact_counts_NG_wave2.csv",
      as.Date("2020-05-02")
    ),
    
    # first barometer wave
    load_contacts_by_state(
      "data/contacts/barometer/contacts_by_state.csv",
      as.Date("2020-05-27")
    )
    
  )
  
  # add on barometer survey data that comes in waves
  contact_wave_files <- list.files("data/contacts/barometer",
                                   pattern = "\\d.csv",
                                   full.names = TRUE)
  
  # find survey dates from corresponding microdistancing data
  wave_dates <- contact_wave_files %>%
    # pull out wave number
    basename() %>%
    strsplit(" ") %>%
    vapply(`[`, 4, FUN.VALUE = character(1)) %>%
    gsub(".csv", "", x = .) %>%
    # pull in file
    paste("Barometer wave", ., "compliance.csv") %>%
    file.path("data/microdistancing/", .) %>%
    lapply(read_csv) %>%
    lapply(pull, "date") %>%
    lapply(first) %>%
    do.call(c, .)
  
  barometer_data_list <- mapply(
    load_contacts_by_state,
    csv = contact_wave_files,
    date = wave_dates,
    SIMPLIFY = FALSE
  )
  
  # combine these
  do.call(
    bind_rows,
    c(contact_data_list, barometer_data_list)
  )
  
}

# load the data needed for the macrodistancing model
macrodistancing_data <- function(dates = NULL, breaks = c(0:10, 20, 50, Inf)) {
  
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
  contact_data <- parse_all_surveys() %>%
    filter(
      !is.na(contact_num),
      !is.na(state)
    ) %>%
    select(
      state,
      date,
      wave,
      wave_date,
      wave_duration,
      weekend_fraction,
      age_groups,
      city,
      employment,
      contact_num,
      starts_with("contacts_")
    ) %>%
    filter(
      contact_num <= 999,
      date %in% location_change_trends$date
    )
  
  list(
    breaks = breaks,
    contacts = contact_data,
    location_change_trends = location_change_trends
  )
  
}

macrodistancing_params <- function(baseline_contact_params) {
  
  # baseline number of non-household contacts, from Prem and Rolls
  OC_0 <- normal(baseline_contact_params$mean_contacts[2],
                 baseline_contact_params$se_contacts[2],
                 truncation = c(0, Inf))
  
  # coefficients for change in average contacts as a function of mobility
  # indices (all set to be negative in first lockdown)
  mobility_coefs <- normal(0, 1, dim = 5, truncation = c(0, Inf))
  
  # coefficients for the fraction of weekly contacts that are on weekends, as a
  # function of the log diffference in contacts
  weekday_coefs <- normal(0, 1, dim = c(2, 6))
  
  list(
    OC_0 = OC_0,
    mobility_coefs = mobility_coefs,
    weekday_coefs = weekday_coefs
  )
  
}

# greta distribution object for the grouped negative binomial distribution
right_aggregated_negative_binomial_distribution <- R6Class(
  "right_aggregated_negative_binomial_distribution",
  inherit = greta:::distribution_node,
  public = list(
    
    max_count = NA,
    
    initialize = function(size, prob, max_count, dim) {
      
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)
      
      self$max_count <- max_count
      
      # add the nodes as parents and parameters
      dim <- greta:::check_dims(size, prob, target_dim = dim)
      super$initialize(
        "right_aggregated_negative_binomial",
        dim,
        discrete = TRUE
      )
      self$add_parameter(size, "size")
      self$add_parameter(prob, "prob")
      
    },
    
    tf_distrib = function(parameters, dag) {
      
      size <- parameters$size
      prob <- parameters$prob
      
      log_prob <- function(x) {
        
        # define the unadjusted density
        distribution <- tfp$distributions$NegativeBinomial(
          total_count = size,
          probs = fl(1) - prob
        )
        
        
        # Get the adjustment to account for truncation need the integral from 0
        # to max_count. We can't use the CDF as there is no gradient w.r.t. the
        # size parameter, so we sum over the PMF in the uncensored range instead
        integers <- tf$range(fl(0), fl(self$max_count + 1))
        integer_log_densities <- distribution$log_prob(integers)
        
        # compute the log-probability of *not* exceeding max_count
        # do reduce_log_sum_exp to get log of sum over PMFs
        log_p_not_excess <- tf$reduce_logsumexp(
          integer_log_densities,
          axis = 2L,
          keepdims = TRUE
        )
        
        # compute the log-probability of being in excess
        # log(1 - exp(log_p_not_excess))
        log_p_excess <- log(-tf$math$expm1(log_p_not_excess))
        
        # get the point densities of all counts        
        log_density_integers <- distribution$log_prob(x)
        
        # bonus: extract the log probs from integer_log_densities instead of
        # recomputing
        # set the excess counts to 0 for now
        # x_clean <- tf$where(excess_count, fl(0), x)
        
        # find the counts exceeding max_count
        excess_count <- x > fl(self$max_count)
        
        # for these, replace the log prob with the probabiity of being in excess
        # of max_count
        log_density <- tf$where(excess_count, log_p_excess, log_density_integers)
        
        log_density
        
      }
      
      sample <- function(seed) {
        
        d <- tfp$distributions$NegativeBinomial(
          total_count = size,
          probs = fl(1) - prob
        )
        d$sample(seed = seed)
        
      }
      
      list(log_prob = log_prob, sample = sample)
      
    }
    
  )
)

right_aggregated_negative_binomial <- function(size, prob, max_count, dim = NULL) {
  greta:::distrib("right_aggregated_negative_binomial", size, prob, max_count, dim)
}

# CDF of the provided distribution, handling 0s and Infs
tf_safe_cdf <- function(x, distribution) {
  
  # prepare to handle values outside the supported range
  too_low <- tf$less(x, greta:::fl(0))
  too_high <- tf$equal(x, greta:::fl(Inf))
  supported <- !too_low & !too_high
  ones <- tf$ones_like(x)
  zeros <- tf$zeros_like(x)
  
  # run cdf on supported values, and fill others in with the appropriate value
  x_clean <- tf$where(supported, x, ones)
  cdf_clean <- distribution$cdf(x_clean)
  mask <- tf$where(supported, ones, zeros)
  add <- tf$where(too_high, ones, zeros)
  cdf_clean * mask + add
  
}

# greta distribution object for the grouped negative binomial distribution
discrete_lognormal_distribution <- R6Class(
  "discrete_lognormal_distribution",
  inherit = greta:::distribution_node,
  public = list(
    
    breaks = NA,
    lower_bounds = NA,
    upper_bounds = NA,
    
    initialize = function(meanlog, sdlog, breaks, dim) {
      
      meanlog <- as.greta_array(meanlog)
      sdlog <- as.greta_array(sdlog)
      
      # handle gradient issue between sdlog and 0s
      breaks <- pmax(breaks, .Machine$double.eps)
      self$breaks <- breaks
      self$lower_bounds <- breaks[-length(breaks)]
      self$upper_bounds <- breaks[-1]
      
      # add the nodes as parents and parameters
      dim <- greta:::check_dims(meanlog, sdlog, target_dim = dim)
      super$initialize("discrete_lognormal", dim, discrete = TRUE)
      self$add_parameter(meanlog, "meanlog")
      self$add_parameter(sdlog, "sdlog")
      
    },
    
    tf_distrib = function(parameters, dag) {
      
      meanlog <- parameters$meanlog
      sdlog <- parameters$sdlog
      
      tf_breaks <- fl(self$breaks)
      tf_lower_bounds <- fl(self$lower_bounds)
      tf_upper_bounds <- fl(self$upper_bounds)
      
      log_prob <- function(x) {
        
        # build distribution object
        d <- tfp$distributions$LogNormal(
          loc = meanlog,
          scale = sdlog
        )
        
        # for those lumped into groups, compute the bounds of the observed groups and get tensors for the
        # bounds in the format expected by TFP
        x_safe <- tf$math$maximum(x, fl(.Machine$double.eps))
        tf_idx <- tfp$stats$find_bins(x_safe, tf_breaks)
        tf_idx_int <- greta:::tf_as_integer(tf_idx)
        tf_lower_vec <- tf$gather(tf_lower_bounds, tf_idx_int)
        tf_upper_vec <- tf$gather(tf_upper_bounds, tf_idx_int)
        
        # compute the density over the observed groups
        low <- tf_safe_cdf(tf_lower_vec, d)
        up <- tf_safe_cdf(tf_upper_vec, d)
        log_density <- log(up - low)
        
      }
      
      sample <- function(seed) {
        
        d <- tfp$distributions$LogNormal(
          loc = meanlog,
          scale = sdlog
        )
        continuous <- d$sample(seed = seed)
        tf$floor(continuous)
        
      }
      
      list(log_prob = log_prob, sample = sample)
      
    }
    
  )
)

# a discretised lognormal distribution (i.e. samplesd by applying the floor
# operation to samples from a lognormal). Due to the numerical instability of
# integrating across the distribution, a vector of breaks must be defined and
# the observations will be treated as censored within those breaks
discrete_lognormal <- function(meanlog, sdlog, breaks, dim = NULL) {
  greta:::distrib("discrete_lognormal", meanlog, sdlog, breaks, dim)
}

lognormal_params <- function(mean, sd) {
  var <- sd ^ 2
  list(
    meanlog = log((mean ^ 2) / sqrt(var + mean ^ 2)),
    sdlog = sqrt(log(1 + var / (mean ^ 2)))
  )
}

# define the likelihood for the macrodistancing model
macrodistancing_likelihood <- function(predictions, data) {
  
  # pull out the expected number of non-household contacts by state and date
  all_dates <- unique(data$location_change_trends$date)
  all_states <- unique(data$location_change_trends$state)
  date_idx <- match(data$contacts$date, all_dates)
  state_idx <- match(data$contacts$state, all_states)
  idx <- cbind(date_idx, state_idx)
  
  # get expected number of contacts per respondent based on their date and state
  # (accounting for day of the week effects)
  log_predicted_contacts <- predictions$log_mean_daily_contacts_wday[idx]
  
  # get lognormal parameters from mean and standard deviation
  sdlog <- normal(0, 5, truncation = c(0, Inf))
  
  # because mean = exp(meanlog + (sdlog ^ 2) / 2)
  meanlog <- log_predicted_contacts - (sdlog ^ 2) / 2
  
  distribution(data$contacts$contact_num) <- discrete_lognormal(
    meanlog = meanlog,
    sdlog = sdlog,
    breaks = data$breaks
  )
  
  result <- list(
    predictions = exp(log_predicted_contacts),
    sdlog = sdlog
  )
  
  invisible(result)
  
}

# check convergence
convergence <- function(draws) {
  
  r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
  n_eff <- coda::effectiveSize(draws)
  
  # sometimes n_eff underflows to 0 simply because the values beinng traced are
  # very small, so remove these (exactly 0 is not possible)
  n_eff <- n_eff[n_eff != 0]
  
  cat(sprintf("maximum R-hat: %.2f\nminimum n effective: %.2f",
              max(r_hats),
              min(n_eff)))
  
  result <- list(r_hats = r_hats,
                 n_eff = n_eff)
  
  invisible(result)
  
}

# has the sampler converged to our standards?
converged <- function(draws, max_r_hat = 1.1, min_n_eff = 1000) {
  stats <- convergence(draws)
  all(stats$r_hats < max_r_hat) &
    all(stats$n_eff >= min_n_eff)
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

microdistancing_params <- function(n_locations = 8, n_inflections = 1, inflection_max = 1) {
  
  # share information between peaks on both timing and amplitude
  inflection_means <- normal(0, 10, dim = n_inflections)
  inflection_sds <- normal(0, 0.5, truncation = c(0, Inf), dim = n_inflections)
  inflection_raw <- normal(0, 1, dim = c(n_locations, n_inflections))
  inflection_scaled <- sweep(inflection_raw, 2, inflection_sds, FUN = "*")
  inflection_centred <- sweep(inflection_raw, 2, inflection_means, FUN = "+")
  inflections <- ilogit(inflection_centred)
  
  height_means <- normal(0, 10, dim = n_inflections + 1)
  height_sds <- normal(0, 0.5, truncation = c(0, Inf), dim = n_inflections + 1)
  height_raw <- normal(0, 1, dim = c(n_locations, n_inflections + 1))
  height_scaled <- sweep(height_raw, 2, height_sds, FUN = "*")
  height_centred <- sweep(height_raw, 2, height_means, FUN = "+")
  heights <- ilogit(height_centred)
  
  # order inflections between 0 and 1 and constrain to earlier than a maximum value
  inflections <- apply(inflections, 1, "cumprod")
  inflections <- 1 - t(inflections)
  inflections <- inflections * inflection_max
  
  list(
    inflections = inflections,
    heights = heights
  )
  
}

# load  all hygiene/microdistancing survey data
hygiene_data <- function () {
  parse_all_surveys()  %>%
    select(
      -starts_with("contact")
    ) %>%
    mutate(
      phys_contact = ifelse(phys_contact, "Yes", "No")
    ) %>%
    pivot_longer(
      cols = c(phys_contact, phys_distance, wash_hands, cough, face_covering),
      names_to = "question",
      values_to = "response"
    ) %>%
    mutate(
      question = recode(
        question,
        `phys_distance` = "1.5m compliance",
        `wash_hands` = "Hand washing",
        `cough` = "Cough etiquette",
        `face_covering` = "Face covering",
        `phys_contact` = "Physical contact"
      )
    ) %>%
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
    # recode face covering as Always or not
    mutate(
      response = case_when(
        question == "Face covering" &
          response %in% c("Always") ~ "yes",
        question == "Face covering" &
          response %in% c("Often", "Sometimes", "Rarely", "No") ~ "no",
        TRUE ~ response) 
    ) %>%
    # recode physical contact to the opposite, to reflect avoidance
    mutate(
      response = case_when(
        question == "Physical contact" & response == "No" ~ "yes",
        question == "Physical contact" & response == "Yes" ~ "no",
        TRUE ~ response) 
    ) %>%
    filter(
      !is.na(response),
      !is.na(state)
    ) %>%
    # collate responses into respondents and yeses
    group_by(state, wave_date, date, question, response) %>%
    summarise(count = n())%>%
    ungroup() %>%
    pivot_wider(
      names_from = "response",
      values_from  = "count",
    ) %>%
    mutate(
      yes = replace_na(yes, 0),
      no = replace_na(no, 0),
      respondents = yes + no
    ) %>%
    rename(count = yes) %>%
    select(-no) %>%
    mutate(proportion = count / respondents) %>%
    arrange(state, question, date)
  
}

# get data for fitting and predicting from microdistancing model
microdistancing_data <- function(dates = NULL) {
  
  # assume adoption of microdistancing follows the same trend as macrodistancing,
  # and that waning starts at the same time, but don't assume it wanes at the same
  # rate
  distancing <- readRDS("outputs/social_distancing_latent.RDS")
  
  # use dates from start of distancing to present if no others are specified
  if (is.null(dates)) {
    dates <- seq(
      min(distancing$date),
      Sys.Date(),
      by = 1
    )
  }
  
  survey <- hygiene_data() %>%
    filter(
      date %in% dates
    )
  
  # clip distancing to non-degenerate values
  range <- range(distancing$mean[!distancing$mean %in% c(0,  1)])
  
  # get data to predict to
  pred_data <- distancing %>%
    rename(distancing = mean) %>%
    mutate(
      distancing = pmax(distancing, range[1]),
      distancing = pmin(distancing, range[2])
    ) %>%
    select(date, distancing) %>%
    old_right_join(
      expand_grid(
        date = dates,
        state = unique(survey$state)
      )
    ) %>%
    mutate(
      distancing = case_when(
        is.na(distancing) & date < min(dates) ~ 0,
        is.na(distancing) & date >= min(dates) ~ 1,
        TRUE ~ distancing
      )
    ) %>%
    mutate(
      state_id = match(state, unique(state)),
      time = as.numeric(date - interventions("national")$date[3]),
      time = time / max(time)
    ) %>%
    arrange(state, date)
  
  # subset to 1.5m question and add data for modelling
  survey_distance <- survey %>%
    filter(question == "1.5m compliance") %>%
    left_join(pred_data)
  
  result <- list(survey_distance = survey_distance,
                 prediction_data = pred_data)
  
  result
  
}

# given vectors of dates and numbers of days post infection, and a single state,
# return the fraction of cases *not* being detected by that point
ttd_survival <- function(days, dates, target_state, cdfs = NULL) {
  
  # filter to this state,
  # loop through dates running ecdfs on days (accounting for change of dates from onset to infection!)
  
  # will need to line up dates, but shouldn't need to line up days_idx (ecdf()
  # will take care of it)
  
  # load empirical CDFs of delay from onset to notificiation (aggregated from
  # date of onset) over time
  if (is.null(cdfs)) {
    cdfs <- readRDS("outputs/delay_from_onset_cdfs.RDS")
  }
  
  # subset to this state
  cdfs <- cdfs %>%
    filter(state == target_state)
  
  # line up dates
  dates <- pmin(dates, max(cdfs$date))
  dates <- pmax(dates, min(cdfs$date))
  dates_idx <- match(dates, cdfs$date)
  
  # convert from days post infection to days post onset (can be up to 5 days
  # negative)
  days_onset <- days - 5
  days_onset_list <- lapply(days_onset, list)
  
  # apply relevant CDF to each number of days
  ecdfs <- cdfs$ecdf[dates_idx]
  cdf_vec <- mapply(do.call, ecdfs, days_onset_list)
  
  # return probability of not being detected by this point
  1 - cdf_vec
  
}

# returna date-by-state matrix of reduction in R due to faster detection of cases
surveillance_effect <- function(dates, states, cdf,
                                gi_bounds = c(0, 20),
                                ttd_cdfs = NULL) {
  
  n_dates <- length(dates)
  n_states <- length(states)
  gi_range <- diff(gi_bounds) + 1
  day_vec <- seq_len(gi_range) - 1 + gi_bounds[1]
  day_mat <- col(matrix(0, n_dates, gi_range)) - 1
  
  # generation interval probability on each day post-infection
  gi_days <- gi_probability(cdf, day_vec, bounds = gi_bounds)
  
  date_state_mat <- matrix(1, n_dates, n_states)
  
  for (i in seq_along(states)) {
    
    # times to detection for each date in this states
    ttd_days <- day_mat
    ttd_days[] <- ttd_survival(
      c(day_mat),
      rep(dates, gi_range),
      target_state = states[i],
      cdfs = ttd_cdfs
    )
    
    # weighted sum to get reduction due to impeded transmission
    date_state_mat[, i] <- c(ttd_days %*% gi_days)
    
  }
  
  date_state_mat
  
}

# compute the extra effect of early isolation of cases, on top of the effect
# of detection of cases. I.e. the multiplicative extra benefit you get from
# putting cases into isolation before they test positive
extra_isolation_effect <- function(
  dates,
  states,
  cdf,
  gi_bounds = c(0, 20),
  ttd_cdfs = NULL,
  tti_cdfs = NULL
) {
  
  # compute the surveillance effect
  surveillance <- surveillance_effect(
    dates = dates,
    states = states,
    cdf = cdf,
    gi_bounds = gi_bounds,
    ttd_cdfs = ttd_cdfs
  )
  
  # load the time to isolation CDFs (unless the user provided one)
  if (is.null(tti_cdfs)) {
    tti_cdfs <- readRDS("outputs/isolation_cdfs.RDS")
  }
  
  # compute the isolation effect
  isolation <- surveillance_effect(
    dates = dates,
    states = states,
    cdf = cdf,
    gi_bounds = gi_bounds,
    ttd_cdfs = tti_cdfs
  )
  
  # compute the ratio of these too, to get the extra multiplicative effect
  # of isolation
  isolation / surveillance
  
}


# get the mean date of symptom onset give a date of detection (using the
# time-varying time to detection distribution)
impute_one_onset <- function(confirmation_date,
                             state,
                             notification_delay_cdf,
                             method = c("expected", "random"),
                             min_days = -10,
                             max_days = 40) {
  
  method <- match.arg(method)
  
  # get possible dates of onset
  delays <- seq(min_days, max_days) 
  possible_onset_dates <- confirmation_date - delays
  
  # probability of being detected this many days later (probability of detection
  # by this day, minus probability of detection by the previous day)
  surv_from <- notification_delay_cdf(delays - 1, possible_onset_dates, state)
  surv_to <- notification_delay_cdf(delays, possible_onset_dates, state)
  prob <- surv_from - surv_to
  
  # normalise to get probabilities of different delays
  prob <- prob / sum(prob)
  
  # compute either the expected time since onset, or draw a random one
  delay <- switch(method,
                  expected = round(sum(delays * prob)),
                  random = sample(delays, 1, prob = prob))
  
  # subtract to get expected date of symptom onset
  onset_date <- confirmation_date - delay
  onset_date
  
}

impute_onsets_old <- function(confirmation_dates,
                          states,
                          notification_delay_cdf,
                          method = c("expected", "random"),
                          min_days = -10,
                          max_days = 40) {
  
  method <- match.arg(method)
  onset_dates <- mapply(
    impute_one_onset,
    confirmation_date = confirmation_dates,
    state = states,
    MoreArgs = list(
      notification_delay_cdf = notification_delay_cdf,
      method = method,
      min_days = min_days,
      max_days = max_days
    ),
    SIMPLIFY = FALSE
  )
  do.call(c, onset_dates)
  
}

# clean up some weird date encoding in the linelist
clean_date <- function (date, min_date = as.Date("2019-12-01"), max_date = Sys.Date()) {
  # don't use ifelse as it converts to a numeric
  # date <- original_date
  # date[weird] <- corrected_date[weird]
  
  # remove any that are out of bounds
  early <- as.Date(date) < min_date
  late <- as.Date(date) > max_date
  date[early | late] <- NA
  
  date
}

# convert a date-by-region matrix into long format 
lengthen <- function(matrix, dates, region_name, value_name) {
  matrix %>%
    as.data.frame() %>%
    bind_cols(date = dates) %>%
    pivot_longer(-date,
                 names_to = region_name,
                 values_to = value_name)
  
}

# fetch the state corresponding to each postcode
postcode_to_state <- function(postcode) {
  
  state <- case_when(
    #grepl("^26", postcode) ~ "ACT",
    # For ACT postcodes see https://en.wikipedia.org/wiki/Postcodes_in_Australia
    is_act_postcode(postcode) ~ "ACT",
    grepl("^2", postcode) ~ "NSW",
    grepl("^3", postcode) ~ "VIC",
    grepl("^4", postcode) ~ "QLD",
    grepl("^5", postcode) ~ "SA",
    grepl("^6", postcode) ~ "WA",
    grepl("^7", postcode) ~ "TAS",
    grepl("^08", postcode) ~ "NT",
    # QLD seems to be recording some locations as 93xx (QLD po-box or LVR?) 9399
    # is in NIR list as QLD, but 9301 is not)
    grepl("^93", postcode) ~ "QLD",
    TRUE ~ "NA"
  )
  
  state[state == "NA"] <- NA
  state
  
}


is_act_postcode <- function(x){
  any(as.numeric(x) == c(
    200:299,
    2600:2618,
    2620,
    2699,#this seems to be used as ACT unknown
    2900:2920
  ))
}

lga_to_state <- function (lga) {
  
  "data/spatial/abs/LGA19_to_STATE16.csv" %>%
    read_csv(
      col_types = cols(
        LGA_CODE_2019 = col_double(),
        LGA_NAME_2019 = col_character(),
        STATE_CODE_2016 = col_double(),
        STATE_NAME_2016 = col_character()
      )
    ) %>%
    select(
      lga = LGA_NAME_2019,
      state = STATE_NAME_2016
    ) %>%
    mutate(
      state = abbreviate_states(state)
    ) %>%
    old_right_join(
      tibble(lga = lga)
    ) %>%
    pull(state)
  
}

linelist_date_times <- function(
  dir,
  name_pattern = "^COVID-19 UoM "
) {
  # find the files
  files <- list.files(dir, pattern = c(".xlsx*$|.csv$"), full.names = TRUE)
  # pull out the date time stamp
  date_time_text <- gsub(name_pattern, "", basename(files)) 
  date_time_text <- gsub(c(".xlsx*$|.csv$"), "", date_time_text)
  #date_times_hm <- as.POSIXct(date_time_text, format = "%d%b%Y %H%M")
  date_times <- as.POSIXct(date_time_text, format = "%d%b%Y")
  # return as a dataframe
  tibble::tibble(
    file = files,
    date_time = date_times
  )
}

# copy over all new NNDSS linelist files from the shared drive to an unsynced local store
sync_nndss <- function(mount_dir = "~/Mounts/nndss", storage_dir = "~/not_synced/nndss") {
  
  # mount the drive
  system("mount_nndss", ignore.stderr = TRUE)
  Sys.sleep(5)
  
  from_files <- list.files(mount_dir, full.names = TRUE)
  existing_files <- list.files(storage_dir)
  new <- !(basename(from_files) %in% existing_files)
  files_to_read <- from_files[new]
  for (new_file in files_to_read) {
    file.copy(new_file, file.path(storage_dir, basename(new_file)), )
  }
}

# process the read-in nndss linelist 
get_nndss_linelist <- function(
  date = NULL,
  dir = "~/not_synced/nndss",
  strict = TRUE,
  #missing_location_assumption = "imported"
  missing_location_assumption = "local",
  #missing_location_assumption = "missing",
  location_conflict_assumption = "local",
  preprocessed = NULL
) {
  
  if (is.null(preprocessed)) {
    
    preprocessed <- preprocess_nndss_linelist(
      date = date,
      dir = dir,
      strict = strict,
      #missing_location_assumption = "imported"
      missing_location_assumption = missing_location_assumption,
      #missing_location_assumption = "missing",
      location_conflict_assumption = location_conflict_assumption)
    
    dat <- preprocessed$dat
    if (is.null(date)) {
      date <- preprocessed$data$date_time
    }
    
  } else {
    if (!is.null(date)) {
      #when both preprocessed and date are supplied, check if it is correct
      if (preprocessed$data$date_time != date) {
        stop ("proprocessed data date and argument date don't match", call. = FALSE)
      }
    } else { #when preprocessed data is read in but date not supplied, read in date
      date <- preprocessed$data$date_time
    }
    dat <- preprocessed$dat
  }
  
  # record state of acquisition, and residence
  dat <- dat %>%
    # fill in missing places of acquisition with correct code
    mutate(
      PLACE_OF_ACQUISITION = ifelse(
        is.na(PLACE_OF_ACQUISITION),
        "00038888",
        PLACE_OF_ACQUISITION)
    ) %>%
    mutate(
      postcode_of_acquisition = substr(PLACE_OF_ACQUISITION, 5, 8),
      postcode_of_residence = replace_na(POSTCODE, "8888"),
      state_of_residence = postcode_to_state(postcode_of_residence)
    ) %>%
    rowwise %>%
    mutate(
      state_of_acquisition = postcode_to_state(postcode_of_acquisition),
      .after = postcode_of_residence
    ) %>% 
    ungroup %>%
    mutate(
      interstate_import_cvsi = case_when(
        CV_SOURCE_INFECTION == 4 ~ TRUE,
        CV_SOURCE_INFECTION == 7 ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  
  
  # Generate linelist data
  linelist <- dat %>%
    # notification receive date seems buggy, and is sometimes before the
    # notification date and specimen collection date
    mutate(
      date_confirmation = pmax(NOTIFICATION_RECEIVE_DATE,
                               NOTIFICATION_DATE,
                               na.rm = TRUE),
    ) %>%
    select(
      date_onset = TRUE_ONSET_DATE,
      date_detection = SPECIMEN_DATE,
      date_confirmation,
      date_quarantine = CV_DATE_ENTERED_QUARANTINE,
      state = STATE,
      import_status,
      postcode_of_acquisition,
      postcode_of_residence,
      state_of_acquisition,
      state_of_residence,
      interstate_import_cvsi
    ) %>%
    mutate(
      report_delay = as.numeric(date_confirmation - date_onset),
      date_linelist = as.Date(date, tz = "Australia/Canberra"),
      state = as.factor(state)
    ) %>%
    
    # Remove those with onset date after confirmation date
    # only keep individuals with date of confirmation after onset date if less than 2 days (inclusive) because
    # we assume some individuals tested via contact tracing will test positive before symptom onset and therefore plausible
    # (noting that reporting delay distribution only calculated from positive differences)
    # also remove any individuals with NA for both notification and symptom onset dates
    # filter(
    #   date_confirmation >= (date_onset - 2) | is.na(date_confirmation) | is.na(date_onset)
    # ) %>%
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
  
  linelist
  
}



# read in the latest linelist and format for analysis
preprocess_nndss_linelist <- function(
  date = NULL,
  dir = "~/not_synced/nndss",
  strict = TRUE,
  #missing_location_assumption = "imported"
  missing_location_assumption = "local",
  #missing_location_assumption = "missing",
  location_conflict_assumption = "local"
) {
  
  data <- linelist_date_times(dir)
  
  # subset to this date
  if (!is.null(date)) {
    data <- data %>%
      filter(as.Date(date_time) == date)
  }
  
  # get the latest linelist
  data <- data %>%
    filter(date_time == max(date_time, na.rm = TRUE))
  
  col_types <- NULL
  if (strict) {
    col_types_1 <- c(
      STATE = "text",
      POSTCODE = "numeric",
      CONFIRMATION_STATUS = "text",
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
      #CV_EXPOSURE_SETTING = "numeric",
      #CV_SOURCE_INFECTION = "numeric"
    )
    
    
    col_types_2 <- c(
      STATE = "text",
      POSTCODE = "numeric",
      CONFIRMATION_STATUS = "text",
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
      #CV_CLOSE_CONTACT = "numeric"
      CV_EXPOSURE_SETTING = "numeric",
      CV_SOURCE_INFECTION = "numeric"
    )
    
    col_types_3 <- c(
      STATE = "text",
      POSTCODE = "numeric",
      CONFIRMATION_STATUS = "text",
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
      #CV_CLOSE_CONTACT = "numeric"
      CV_EXPOSURE_SETTING = "numeric",
      CV_SOURCE_INFECTION = "numeric",
      CV_SYMPTOMS_REPORTED = "numeric",
      CV_QUARANTINE_STATUS = "numeric",
      CV_DATE_ENTERED_QUARANTINE = "date"
    )
    
    col_types_4 <- c(
      STATE = "text",
      POSTCODE = "numeric",
      CONFIRMATION_STATUS = "text",
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
      #CV_CLOSE_CONTACT = "numeric"
      CV_EXPOSURE_SETTING = "numeric",
      CV_SOURCE_INFECTION = "numeric",
      CV_SYMPTOMS_REPORTED = "numeric",
      CV_QUARANTINE_STATUS = "numeric",
      CV_DATE_ENTERED_QUARANTINE = "date",
      "numeric",
      "text",
      "date",
      "numeric",
      "text",
      "date",
      "numeric",
      "text",
      "date",
      "numeric",
      "text",
      "date",
      "numeric",
      "text",
      "date"
    )
    
    col_types_5 <- c( #same as 4 but postcode in lower case
      STATE = "text",
      CONFIRMATION_STATUS = "text",
      Postcode = "numeric",
      TRUE_ONSET_DATE = "date",
      SPECIMEN_DATE = "date",
      NOTIFICATION_DATE = "date",
      NOTIFICATION_RECEIVE_DATE = "date",
      `DIAGNOSIS DATE` = "date",
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
      #CV_CLOSE_CONTACT = "numeric"
      CV_EXPOSURE_SETTING = "numeric",
      CV_SOURCE_INFECTION = "numeric",
      CV_SYMPTOMS_REPORTED = "numeric",
      CV_QUARANTINE_STATUS = "numeric",
      CV_DATE_ENTERED_QUARANTINE = "date",
      "numeric",
      "text",
      "date",
      "numeric",
      "text",
      "date",
      "numeric",
      "text",
      "date",
      "numeric",
      "text",
      "date",
      "numeric",
      "text",
      "date"
    )
  }
  
  
  ll_date <- data$date_time[[1]]
  
  
  if (ll_date < "2021-03-08") {
    col_types <- col_types_1
  } else if (ll_date < "2021-11-08") {
    col_types <- col_types_2
  } else if (ll_date < "2021-12-02") {
    col_types <- col_types_3
  } else if (ll_date < "2022-01-06") {
    col_types <- col_types_4
  } else {
    col_types <- col_types_5
  }
  
  
  if (ll_date >= "2022-01-25") {
    #try csv
    # sheets <- readxl::excel_sheets(data$file)
    # dat <- lapply(sheets, 
    #               function(X) readxl::read_xlsx(data$file,
    #                                             col_types = col_types,
    #                                             #na = "NULL", 
    #                                             sheet = X)) 
    # dat <- dat %>% reduce(full_join)
    
    dat <- readr::read_csv(
      data$file,
      col_types = cols_only(
        STATE = col_character(),
        POSTCODE = col_double(),
        CONFIRMATION_STATUS = col_character(),
        TRUE_ONSET_DATE = col_date(format = "%d/%m/%Y"),
        SPECIMEN_DATE = col_date(format = "%d/%m/%Y"),
        NOTIFICATION_DATE = col_date(format = "%d/%m/%Y"),
        NOTIFICATION_RECEIVE_DATE = col_date(format = "%d/%m/%Y"),
        'DIAGNOSIS DATE' = col_date(format = "%d/%m/%Y"),
        AGE_AT_ONSET = col_double(),
        SEX = col_double(),
        DIED = col_double(),
        PLACE_OF_ACQUISITION = col_character(),
        HOSPITALISED = col_double(),
        CV_ICU = col_double(),
        CV_VENTILATED = col_double(),
        OUTBREAK_REF = col_character(),
        CASE_FOUND_BY = col_double(),
        CV_SYMPTOMS = col_character(),
        CV_OTHER_SYMPTOMS = col_character(),
        CV_COMORBIDITIES = col_character(),
        CV_OTHER_COMORBIDITIES = col_character(),
        CV_GESTATION = col_double(),
        #CV_CLOSE_CONTACT = "numeric"
        CV_EXPOSURE_SETTING = col_double(),
        CV_SOURCE_INFECTION = col_double(),
        CV_SYMPTOMS_REPORTED = col_double(),
        CV_QUARANTINE_STATUS = col_double(),
        CV_DATE_ENTERED_QUARANTINE = col_date(format = "%d/%m/%Y"),
        LOADED_DATE = col_date(format = "%Y-%m-%d %H:%M:%S")),
      na = c("", "NULL") # usually turn this off
    ) #%>% rename(POSTCODE = Postcode)
    
    # dat <- dat %>%
    #   mutate(
    #     Postcode = as.numeric(Postcode),
    #     TRUE_ONSET_DATE = as.Date(TRUE_ONSET_DATE, format = "%d/%m/%Y"),
    #     SPECIMEN_DATE  = as.Date(SPECIMEN_DATE, format = "%d/%m/%Y"),
    #     NOTIFICATION_DATE  = as.Date(NOTIFICATION_DATE, format = "%d/%m/%Y"),
    #     NOTIFICATION_RECEIVE_DATE = as.Date(NOTIFICATION_DATE, format = "%d/%m/%Y"),
    #     CV_DATE_ENTERED_QUARANTINE  = as.Date(CV_DATE_ENTERED_QUARANTINE, format = "%d/%m/%Y"),
    #     CV_SOURCE_INFECTION = as.numeric(CV_SOURCE_INFECTION)
    #   )
    
  } else if (ll_date < "2022-01-06"|ll_date > "2022-01-07" & ll_date < "2022-01-25") {
    if (length(readxl::excel_sheets(data$file)) == 1) { #handle multiple sheets
      dat <- readxl::read_xlsx(
        data$file,
        col_types = col_types#,
        #na = "NULL" # usually turn this off
      )
    } else { #deal with multiple sheets
      sheets <- readxl::excel_sheets(data$file)
      dat <- lapply(sheets, 
                    function(X) readxl::read_xlsx(data$file,
                                                  col_types = col_types,
                                                  #na = "NULL", 
                                                  sheet = X)) 
      dat <- dat %>% reduce(full_join)
    }
  }
  # } else { #read the xls format starting from 06-01-2022
  #   dat <- readr::read_csv(
  #     data$file,
  #     col_types = cols_only(
  #       STATE = col_character(),
  #       Postcode = col_double(),
  #       CONFIRMATION_STATUS = col_character(),
  #       TRUE_ONSET_DATE = col_date(format = "%d/%m/%Y"),
  #       SPECIMEN_DATE = col_date(format = "%d/%m/%Y"),
  #       NOTIFICATION_DATE = col_date(format = "%d/%m/%Y"),
  #       NOTIFICATION_RECEIVE_DATE = col_date(format = "%d/%m/%Y"),
  #       'DIAGNOSIS DATE' = col_date(format = "%d/%m/%Y"),
  #       AGE_AT_ONSET = col_double(),
  #       SEX = col_double(),
  #       DIED = col_double(),
  #       PLACE_OF_ACQUISITION = col_character(),
  #       HOSPITALISED = col_double(),
  #       CV_ICU = col_double(),
  #       CV_VENTILATED = col_double(),
  #       OUTBREAK_REF = col_character(),
  #       CASE_FOUND_BY = col_double(),
  #       CV_SYMPTOMS = col_character(),
  #       CV_OTHER_SYMPTOMS = col_character(),
  #       CV_COMORBIDITIES = col_character(),
  #       CV_OTHER_COMORBIDITIES = col_character(),
  #       CV_GESTATION = col_double(),
  #       #CV_CLOSE_CONTACT = "numeric"
  #       CV_EXPOSURE_SETTING = col_double(),
  #       CV_SOURCE_INFECTION = col_double(),
  #       CV_SYMPTOMS_REPORTED = col_double(),
  #       CV_QUARANTINE_STATUS = col_double(),
  #       CV_DATE_ENTERED_QUARANTINE = col_date(format = "%d/%m/%Y")),
  #     na = "NULL" # usually turn this off
  #   ) %>% rename(POSTCODE = Postcode)
  # }
  
  if(ll_date < "2021-03-08"){
    dat <- dat %>%
      mutate(
        CV_SOURCE_INFECTION = NA_real_
      )
  }
  
  # if(ll_date > "2022-01-07"){ #fix changed postcode colname
  #   dat <- dat %>%
  #     rename(POSTCODE = Postcode) %>% 
  #     mutate(POSTCODE = as.numeric(POSTCODE)) #not sure why this breaks down
  # }
  
  if (is.numeric(dat$POSTCODE)) {
    dat <- dat %>%
      mutate(
        POSTCODE = sprintf("%04d", dat$POSTCODE),
        POSTCODE = ifelse(POSTCODE == "00NA", NA, POSTCODE) 
      )
  } else {
    dat <- dat %>%
      mutate(POSTCODE = NA)
  }
  
  # Remove cases without a state
  dat <- dat %>%
    filter(!is.na(STATE))
  
  # tidy up dates and parse place of acquisition to local (Australia) vs. overseas
  dat <- dat %>%
    mutate(
      TRUE_ONSET_DATE = clean_date(TRUE_ONSET_DATE),
      NOTIFICATION_RECEIVE_DATE = clean_date(NOTIFICATION_RECEIVE_DATE),
      SPECIMEN_DATE = clean_date(SPECIMEN_DATE),
      CV_DATE_ENTERED_QUARANTINE = clean_date(CV_DATE_ENTERED_QUARANTINE)
    ) %>% 
    mutate( #tidy up PoA codes - maybe fixed at some point
      PLACE_OF_ACQUISITION = ifelse(nchar(PLACE_OF_ACQUISITION) == 5, 
                                    paste0("000",PLACE_OF_ACQUISITION),
                                    PLACE_OF_ACQUISITION)
    ) %>% 
    mutate(
      import_status = case_when(
        # return "ERROR" if place of acquisition and cv_source_infection
        # indicate opposite import statuses
        grepl("^1101", PLACE_OF_ACQUISITION) & CV_SOURCE_INFECTION == 1 ~ "ERROR",
        !is.na(PLACE_OF_ACQUISITION) & 
          !grepl("^1101|^00038888", PLACE_OF_ACQUISITION) &
          CV_SOURCE_INFECTION == 2 ~ "ERROR",
        !is.na(PLACE_OF_ACQUISITION) & 
          !grepl("^1101|^00038888", PLACE_OF_ACQUISITION) &
          CV_SOURCE_INFECTION == 3 ~ "ERROR",
        !is.na(PLACE_OF_ACQUISITION) & 
          !grepl("^1101|^00038888", PLACE_OF_ACQUISITION) &
          CV_SOURCE_INFECTION == 4 ~ "ERROR",
        # where source known us that
        grepl("^1101", PLACE_OF_ACQUISITION) ~ "local",
        CV_SOURCE_INFECTION == 1 ~ "imported",
        CV_SOURCE_INFECTION == 2 ~ "local",
        CV_SOURCE_INFECTION == 3 ~ "local",
        CV_SOURCE_INFECTION == 4 ~ "local",
        CV_SOURCE_INFECTION == 6 ~ "local",
        CV_SOURCE_INFECTION == 7 ~ "local",
        # otherwise impute it
        CV_SOURCE_INFECTION == 5 ~ missing_location_assumption,
        grepl("^00038888", PLACE_OF_ACQUISITION) ~ missing_location_assumption,
        !is.na(PLACE_OF_ACQUISITION) ~ "imported",
        is.na(PLACE_OF_ACQUISITION) ~ missing_location_assumption,
        is.na(CV_SOURCE_INFECTION) ~ missing_location_assumption
      )#,
      # import_status = case_when(
      #   import_status == "missing" & STATE == "WA" ~ "local",
      #   import_status == "missing" & STATE != "WA" ~ "imported",
      #   TRUE ~ import_status
      # )
    ) %>%
    mutate(
      import_status = ifelse(import_status == "ERROR", location_conflict_assumption, import_status)
    )
  
  return(list(data = data,
              dat = dat))
  
}

# replace VIC elements with VIC linelist
get_vic_linelist <- function(file) {
  
  linelist_date <- file %>%
    basename() %>%
    substr(1, 8) %>%
    as.Date(format = "%Y%m%d")
  
  
  file %>%
    read_csv(
      col_types = cols(
        CaseNumber = col_double(),
        DiagnosisDate = col_date(format = ""),
        #DiagnosisDate = col_date(format = "%d/%m/%Y"),
        SymptomsOnsetDate = col_date(format = ""),
        #SymptomsOnsetDate = col_date(format = "%d/%m/%Y"),
        LGA = col_character(),
        Acquired = col_character(),
        FirstSpecimenPositiveDate = col_date(format = "")#,
        #Classification = col_character()# not backwards compatible for the moment, will do a proper fix later
        #FirstSpecimenPositiveDate = col_date(format = "%d/%m/%Y")
      ),
      na = "NA"
    ) %>% #filter(Classification == "Confirmed"|FirstSpecimenPositiveDate > "2021-12-25") %>%
    # read_excel(
    #   col_types = c("numeric", "date", "date", "text", "text", "date"),
    #   na = "NA"
    # ) %>%
    mutate(
      date_onset = clean_date(as.Date(SymptomsOnsetDate)),#deal with future onset dates
      date_confirmation = as.Date(DiagnosisDate),
      date_detection = clean_date(as.Date(FirstSpecimenPositiveDate)),
      state = "VIC",
      import_status = case_when(
        Acquired == "Travel overseas" ~ "imported",
        TRUE ~ "local"
      ),
      postcode_of_acquisition = "8888",
      postcode_of_residence = "8888",
      state_of_acquisition = NA,
      state_of_residence = NA,
      report_delay = NA,
      date_linelist = linelist_date
    ) %>%
    mutate(
      date_confirmation = case_when(
        is.na(date_confirmation) ~ date_detection + 1,
        TRUE ~ date_confirmation
      ),
      date_detection = case_when(
        is.na(date_detection) ~ date_confirmation - 1,
        TRUE ~ date_detection)
    ) %>%
    select(
      date_onset,
      date_detection,
      date_confirmation,
      state,
      import_status,
      postcode_of_acquisition,
      postcode_of_residence,
      state_of_acquisition,
      state_of_residence,
      report_delay,
      date_linelist
    )
  
  
  # 
  # file %>%
  #   read_csv(
  #     col_types = cols(
  #       PHESSID = col_double(),
  #       diagnosis_date = col_datetime(format = ""),
  #       ss_onset = col_datetime(format = ""),
  #       Localgovernmentarea = col_character(),
  #       acquired = col_character(),
  #       SPECIMEN_DATE = col_date(format = "%d/%m/%Y")
  #     ),
  #     na = "NULL"
  #   ) %>%
  #   mutate(
  #     date_onset = as.Date(ss_onset),
  #     date_confirmation = as.Date(diagnosis_date),
  #     date_detection = clean_date(SPECIMEN_DATE),
  #     state = "VIC",
  #     import_status = case_when(
  #       acquired == "Travel overseas" ~ "imported",
  #       TRUE ~ "local"
  #     ),
  #     postcode_of_acquisition = "8888",
  #     postcode_of_residence = "8888",
  #     state_of_acquisition = NA,
  #     state_of_residence = NA,
  #     report_delay = NA,
  #     date_linelist = linelist_date
  #   ) %>%
  #   # the mode and mean of the delay from testing to confirmation in VIC is around 3 days at the moment
  #   mutate(
  #     date_detection = case_when(
  #       is.na(date_detection) ~ date_confirmation - 3,
  #       TRUE ~ date_detection
  #     )
  #   ) %>%
  #   select(
  #     date_onset,
  #     date_detection,
  #     date_confirmation,
  #     state,
  #     import_status,
  #     postcode_of_acquisition,
  #     postcode_of_residence,
  #     state_of_acquisition,
  #     state_of_residence,
  #     report_delay,
  #     date_linelist
  #   )
  
  
}



get_sa_linelist <- function(file = "~/not_synced/sa/SA_linelist_25July2021.csv") {
  
  # We need to time travel as SA health has given us an outdated linelist (by 1 day)
  # We'll undo this on the EpiFX end.
  t_offset <- lubridate::days(1)
  
  file %>%
    read_csv() %>%
    mutate(
      symptom_onset = lubridate::dmy(symptom_onset) + t_offset,
      notification_date = lubridate::dmy(notification_date) + t_offset,
      likely_infection_date = lubridate::dmy(likely_infection_date) + t_offset) %>%
    
    mutate(
      date_onset = case_when(!is.na(likely_infection_date) ~ likely_infection_date + 5,
                             T                             ~ symptom_onset),
      date_detection = NA,
      date_confirmation = notification_date,
      state = "SA",
      postcode_of_acquisition = "8888",
      postcode_of_residence = NA,
      state_of_acquisition = ifelse(
        import_status == "local",
        "SA",
        NA
      ),
      state_of_residence = NA,
      report_delay = NA,
      date_linelist = as.Date("2021-07-25"),
      interstate_import = FALSE
    ) %>%
    select(
      date_onset,
      date_detection,
      date_confirmation,
      state,
      import_status,
      postcode_of_acquisition,
      postcode_of_residence,
      state_of_acquisition,
      state_of_residence,
      report_delay,
      date_linelist,
      interstate_import
    ) %>%
    filter(
      import_status == "local"
    )
}


col_nsw_date <- function(type = c("short", "long")) {
  type <- match.arg(type)
  switch(
    type,
    short = col_date(format = "%Y-%m-%d"),
    #short = col_date(format = "%d/%m/%y"),
    long = col_date(format = "%d/%m/%Y %H:%M:%S AM")
  )
}

# get latest NSW linelist
get_nsw_linelist <- function (
  file = NULL,
  nindss_compatible = TRUE
) {
  
  if(is.null(file)){
    files <- list.files(
      "~/not_synced/nsw",
      pattern = ".csv",
      full.names = TRUE
    )
    
    dates <- files %>%
      basename() %>%
      substr(1, 8) %>%
      as.Date(format = "%Y%m%d")
    
    latest <- which.max(dates)
    file <- files[latest]
    date <- dates[latest]
  } else {
    date <- file %>%
      basename() %>%
      substr(1, 8) %>%
      as.Date(format = "%Y%m%d")
  }
  
  nsw_linelist <- file %>%
    read_csv(
      col_types = cols(
        .default = col_character(),
        CASE_ID = col_double(),
        EARLIEST_CONFIRMED_OR_PROBABLE = col_nsw_date(),
        SYMPTOM_ONSET_DATE = col_nsw_date(),
        CALCULATED_ONSET_DATE = col_nsw_date(),
        AGE_AT_EVENT_YEARS = col_double(),
        DATE_ISOLATION_BEGAN = col_nsw_date(),
        #SETTING_OF_TRANSMISSION_DATE = col_date(format = "%Y-%m-%d"),
        SETTING_OF_TRANSMISSION_DATE = col_nsw_date(),
        INTERVIEWED_DATE = col_nsw_date(),
        S_gene_result_date = col_nsw_date(),
        Omicron_Category = col_factor(),
        TEST_TYPE = col_character()
      )
    ) %>%
    # remove some bogus dates
    mutate(across(
      all_of(c(
        "EARLIEST_CONFIRMED_OR_PROBABLE",
        "SYMPTOM_ONSET_DATE",
        "SETTING_OF_TRANSMISSION_DATE",
        "CALCULATED_ONSET_DATE",
        "DATE_ISOLATION_BEGAN",
        "SETTING_OF_TRANSMISSION_DATE",
        "INTERVIEWED_DATE",
        "S_gene_result_date"
      )),
      clean_date
    )
    ) %>%
    # if any infection dates are after onset, or on/after confirmation, set the infection date to NA
    mutate(
      SETTING_OF_TRANSMISSION_DATE = case_when(
        SETTING_OF_TRANSMISSION_DATE > SYMPTOM_ONSET_DATE ~ as.Date(NA),
        SETTING_OF_TRANSMISSION_DATE >= EARLIEST_CONFIRMED_OR_PROBABLE ~ as.Date(NA),
        TRUE ~ SETTING_OF_TRANSMISSION_DATE
      )
    ) %>%
    mutate(
      date_onset = case_when(
        !is.na(SETTING_OF_TRANSMISSION_DATE) ~ SETTING_OF_TRANSMISSION_DATE + 5,
        #TRUE ~ CALCULATED_ONSET_DATE
        TRUE ~ SYMPTOM_ONSET_DATE
      ),
      #date_onset = NA,
      date_detection = NA,
      date_confirmation = EARLIEST_CONFIRMED_OR_PROBABLE,
      date_quarantine = DATE_ISOLATION_BEGAN,
      state = "NSW",
      import_status = ifelse(
        PLACE_ACQUISITION %in% c("Acquired in NSW",NA),
        "local",
        "imported"
      ),
      postcode_of_acquisition = NA,
      postcode_of_residence = NA,
      state_of_acquisition = "NSW",
      state_of_residence = NA,
      report_delay = NA,
      date_linelist = date,
      interstate_import = FALSE,
      test_type = TEST_TYPE
    )
  
  if(nindss_compatible){
    nsw_linelist <- nsw_linelist %>%
      select(
        date_onset,
        date_detection,
        date_confirmation,
        date_quarantine,
        state,
        import_status,
        postcode_of_acquisition,
        postcode_of_residence,
        state_of_acquisition,
        state_of_residence,
        report_delay,
        date_linelist,
        interstate_import,
        test_type
      ) %>%
      arrange(
        desc(date_onset)
      )
  }
  
  return(nsw_linelist)
  
}

# given a date-by-state matrix of case counts by date of infection,
# corresponding vectors of dates and states, and a function got the CDF of the
# continous version of a generation interval distribution, adjust the GI
# distribution by surveillance effectiveness (fraction of cases detected and
# isolated by each day post infection) and convolve the cases to get the
# combined infectiousness in each date and state.
gi_convolution <- function(
  cases,
  dates,
  states,
  gi_cdf,
  # ttd_cdfs,
  gi_bounds = c(0, 20)
) {
  
  n_dates <- length(dates)
  n_states <- length(states)
  if (!identical(dim(cases), c(n_dates, n_states))) {
    stop ("cases does not match dates and states", call. = FALSE)
  }
  
  convolved <- cases * 0
  for (i in seq_len(n_states)) {
    # Circulant matrices of generation interval discrete probabilities
    # use Nishiura's serial interval as a generation interval
    # gi_cdf <- nishiura_cdf()
    gi_mat <- gi_matrix(
      gi_cdf = gi_cdf,
      dates = dates,
      state = states[i],
      gi_bounds = gi_bounds#,
      # ttd_cdfs = ttd_cdfs
    )
    
    convolved[, i] <- gi_mat %*% cases[, i]
    
  }
  
  convolved
  
  
}

# cases of spillover (import-local transmission) in during mandatory hotel quarantine
hotel_quarantine_spillover_data <- function() {
  
  tibble::tribble(
    ~earliest_date, ~latest_date, ~state, ~infectee, ~information_source,
    "2020-05-01", "2020-05-14", "VIC", "quarantine security guard (Rydges Hotel)",
    "https://www.dhhs.vic.gov.au/tracking-coronavirus-victoria",
    "2020-08-03", "2020-08-08", "NSW", "quarantine security guard (Sydney Harbour Marriott Hotel)",
    "https://www.health.nsw.gov.au/news/Pages/20200818_01.aspx",
    "2020-11-01", "2020-11-14", "SA", "quarantine security guard (Peppers Waymouth Hotel)",
    "https://www.sahealth.sa.gov.au/wps/wcm/connect/public+content/sa+health+internet/about+us/news+and+media/all+media+releases/covid-19+update+15+november",
    "2020-11-27", "2020-11-30", "NSW", "quarantine domestic worker (Novotel Darling Harbour)",
    "https://www.health.nsw.gov.au/news/Pages/20201204_01.aspx",
    "2020-12-07", "2020-12-12", "NSW", "quarantine airport driver (Sydney Ground Transport, Alexandria)",
    "https://www.abc.net.au/news/2020-12-16/nsw-confirms-new-locally-acquired-coronavirus-case/12988866",
    "2020-12-01", "2020-12-13", "NSW", "unknown (link from quarantine hotel to Northern Beaches)",
    "https://www.health.nsw.gov.au/news/Pages/20201216_03.aspx",
    "2020-12-31", "2021-01-02", "QLD", "quarantine cleaner (Hotel Grand Chancellor)",
    "https://www.health.qld.gov.au/news-events/doh-media-releases/releases/public-health-alert-brisbane",
  ) %>%
    mutate_at(
      c("earliest_date", "latest_date"),
      as.Date
    )
  
}

# given a raw (unimputed) linelist, prepare all the data needed for modelling
reff_model_data <- function(
  linelist_raw = load_linelist(),
  n_weeks_ahead = 6,
  inducing_gap = 3,
  detection_cutoff = 0.95,
  notification_delay_cdf = NULL
) {
  
  linelist_date <- max(linelist_raw$date_linelist)
  
  # load modelled google mobility data 
  mobility_data <- readRDS("outputs/google_change_trends.RDS")
  
  # compute delays from symptom onset to detection for each state over time if null
  if (is.null(notification_delay_cdf)) {
    notification_delay_cdf <- get_notification_delay_cdf(linelist_raw)
  }

  
  # impute onset dates and infection dates using this
  linelist <- linelist_raw %>%
    impute_linelist(notification_delay_cdf = notification_delay_cdf)
  # truncate mobility data to no later than the day before the linelist (needed
  # for modelling on historic linelists) and then get the most recent date
  latest_mobility_date <- mobility_data %>%
    filter(date < linelist_date) %>%
    pull(date) %>%
    max()
  
  # get linelist date and state information
  earliest_date <- min(linelist$date)
  latest_date <- max(linelist$date)
  
  states <- sort(unique(linelist$state))
  dates <- seq(earliest_date, latest_date, by = 1)
  mobility_dates <- seq(earliest_date, latest_mobility_date, by = 1)
  
  n_states <- length(states)
  n_dates <- length(dates)
  n_extra <- as.numeric(Sys.Date() - max(dates)) + 7 * n_weeks_ahead
  date_nums <- seq_len(n_dates + n_extra)
  dates_project <- earliest_date + date_nums - 1
  n_dates_project <- n_date_nums <- length(date_nums)
  
  # build a vector of inducing points, regularly spaced over time but with one on
  # the most recent date
  inducing_date_nums <- rev(seq(n_date_nums, 1, by = -inducing_gap))
  n_inducing <- length(inducing_date_nums)
  
  # get detection probabilities for these dates and states
  detection_prob_mat <- detection_probability_matrix(
    latest_date = linelist_date - 1,
    infection_dates = dates,
    states = states,
    notification_delay_cdf = notification_delay_cdf
  )
  
  # subset to dates with reasonably high detection probabilities in some states
  detectable <- detection_prob_mat >= detection_cutoff
  
  # the last date with infection data we include
  last_detectable_idx <- which(!apply(detectable, 1, any))[1]
  latest_infection_date <- dates[ifelse(is.na(last_detectable_idx), length(dates), last_detectable_idx)]
  

  # those infected in the state
  local_cases <- linelist %>%
    filter(!interstate_import) %>%
    infections_by_region(
      region_type = "state",
      case_type = "local"
    )
  
  # include day of the week glm to smooth weekly report artefact
  
  #subset to omicron period
  week_count <- 1 + 1:length(seq(earliest_date, latest_date, by = 1)) %/% 7
  
  dow <- lubridate::wday(seq(earliest_date, latest_date, by = 1))
  
  dow_effect <- local_cases
  dow_effect[] <- 1
  
  dow_effect[dates>=earliest_date,] <- apply(local_cases[dates>=earliest_date,],
                                                     2,
                                                     FUN = function(x){
                                                       m <- glm(
                                                         x ~ factor(week_count) + factor(dow),
                                                         family = stats::poisson
                                                       )
                                                       trend_estimate <- tibble(
                                                         week_count = 1,
                                                         dow = dow
                                                       ) %>%
                                                         mutate(
                                                           effect = predict(
                                                             m,
                                                             newdata = .,
                                                             type = "response"
                                                           ),
                                                           effect = effect / mean(effect[1:7])
                                                         )
                                                       trend_estimate$effect}
  )
  
  
  # and those infected in any state, but infectious in this one
  local_cases_infectious <- linelist %>%
    infections_by_region(
      region_type = "state",
      case_type = "local"
    )
  
  # de-oscillate local infectious numbers
  local_cases_infectious <- local_cases_infectious / dow_effect
  
  # those imported (only considered infectious, but with a different Reff)
  imported_cases <- linelist %>%
    infections_by_region(
      region_type = "state",
      case_type = "imported"
    )
  
  # correct Reff denominator for right-truncation (infectors not yet detected) by
  # expectation (resolving divide-by-zero error)
  detection_prob_mat[] <- pmax(detection_prob_mat, 1e-6)
  local_cases_infectious_corrected <- local_cases_infectious /  detection_prob_mat
  imported_cases_corrected <- imported_cases / detection_prob_mat
  
  # disaggregate imported and local cases according to the generation interval
  # probabilities to get the expected number of infectious people in each state
  # and time
  #tti_cdfs <- readRDS("outputs/isolation_cdfs.RDS")
  
  local_infectiousness <- gi_convolution(
    local_cases_infectious_corrected,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf#,
    #ttd_cdfs = tti_cdfs
  )
  
  imported_infectiousness <- gi_convolution(
    imported_cases_corrected,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf#,
  )
  
  # elements to exclude due to a lack of infectiousness
  local_valid <- is.finite(local_infectiousness) & local_infectiousness > 0
  import_valid <- is.finite(imported_infectiousness) & imported_infectiousness > 0
  valid_mat <- (local_valid | import_valid) & detectable
  
  # data on quarantine spillovers (import-local infections) and imported cases
  # since mandatory hotel quarantine was implemented hotel quarantine
  n_hotel_spillovers <- nrow(hotel_quarantine_spillover_data())
  hotel_quarantine_start_date <- max(quarantine_dates()$date)
  n_hotel_cases <- sum(imported_cases[dates >= hotel_quarantine_start_date, ])
  
 
  vaccine_effect_timeseries <- readRDS("outputs/vaccination_effect.RDS")
  
  ve_omicron_ba2 <- vaccine_effect_timeseries %>%
    filter(variant == "Omicron BA2") %>%
    select(date, state, effect)
  
  ve_delta <- vaccine_effect_timeseries %>%
    filter(variant == "Delta") %>%
    select(date, state, effect)
  
  vaccine_effect_matrix_delta <- ve_delta %>%
    pivot_wider(
      names_from = state,
      values_from = effect
    ) %>%
    right_join(
      y = tibble(date = dates_project)
    ) %>%
    arrange(date) %>%
    tidyr::fill(
      everything(),
      .direction = "updown"
    ) %>%
    dplyr::select(-date) %>%
    as.matrix
  
  vaccine_effect_matrix_omicron_ba2 <- ve_omicron_ba2 %>%
    pivot_wider(
      names_from = state,
      values_from = effect
    ) %>%
    right_join(
      y = tibble(date = dates_project)
    ) %>%
    arrange(date) %>%
    tidyr::fill(
      everything(),
      .direction = "updown"
    ) %>%
    dplyr::select(-date) %>%
    as.matrix
  
  
  omicron_ba2_matrix <- prop_variant(dates_project)$prop_omicron
  
  omicron_ba2_index <- which(omicron_ba2_matrix == 1)
  
  vaccine_effect_matrix <- vaccine_effect_matrix_delta
  
  vaccine_effect_matrix[omicron_ba2_index] <- vaccine_effect_matrix_omicron_ba2[omicron_ba2_index]
  
  vaccine_dates <- unique(vaccine_effect_timeseries$date)
  
  # #case ascertainment assumptions
  # date_seq_asc <- seq.Date(
  #   from = as.Date("2021-12-01"),
  #   to = Sys.Date() + weeks(16),
  #   by = "1 week"
  # )
  # case_ascertainment_tables <- tibble(date = date_seq_asc)
  # 
  # # NSW, VIC, ACT and QLD
  # # Case ascertainment at 75% from 1 December 2021, drop to 33.3% from
  # # 12 December 2021 to 22 January 2022, and return to 75% by 23 January
  # # 2022.
  # 
  # # WA, SA, NT, and TAS
  # # Assume constant 75% case ascertainment from 1 December 2021
  # 
  # date_state_ascertainment <- expand_grid(date = seq.Date(
  #   from = min(case_ascertainment_tables$date),
  #   to = max(case_ascertainment_tables$date),
  #   by = 1
  # ),
  # state = states) %>% # states = sort(unique(linelist$state)
  #   mutate(
  #     ascertainment = case_when(
  #       state %in% c("NSW", "ACT", "VIC", "QLD") &
  #         (date >= "2021-12-01") & (date < "2021-12-12") ~ 0.75,
  #       state %in% c("NSW", "ACT", "VIC", "QLD") &
  #         (date >= "2021-12-20") & (date < "2022-01-16") ~ 0.33,
  #       state %in% c("NSW", "ACT", "VIC", "QLD") &
  #         (date >= "2022-01-23") ~ 0.75,
  #       state %in% c("WA", "NT", "SA", "TAS") ~ 0.75,
  #       TRUE ~ NA_real_
  #     )
  #   ) %>%
  #   arrange(state) %>%
  #   mutate(ascertainment = na.approx(ascertainment))
  # 
  # ascertainment_matrix <- date_state_ascertainment %>%
  #   pivot_wider(names_from = state, values_from = ascertainment) %>%
  #   right_join(y = tibble(date = dates_project)) %>%
  #   arrange(date) %>%
  #   mutate(across(-date, ~ replace_na(.x, 1))) %>% # 100% FOR PRE DEC 2021
  #   dplyr::select(-date) %>%
  #   as.matrix
  # 
  # return a named, nested list of these objects
  list(
    local = list(
      cases = local_cases,
      cases_infectious = local_cases_infectious,
      infectiousness = local_infectiousness
    ),
    imported = list(
      cases = imported_cases,
      infectiousness = imported_infectiousness,
      total_hotel_cases = n_hotel_cases,
      total_hotel_spillovers = n_hotel_spillovers
    ),
    detection_prob_mat = detection_prob_mat,
    valid_mat = valid_mat,
    states = states,
    dates = list(
      infection = dates,
      infection_project = dates_project,
      onset = dates + 5,
      date_nums = date_nums,
      inducing_date_nums = inducing_date_nums,
      mobility = mobility_dates,
      earliest = earliest_date,
      latest = latest_date,
      latest_mobility = latest_mobility_date,
      latest_infection = latest_infection_date,
      latest_project = max(dates_project),
      linelist = linelist_date,
      vaccine_dates = vaccine_dates,
      dow = dow
    ),
    n_dates = n_dates,
    n_states = n_states,
    n_date_nums = n_date_nums,
    n_dates_project = n_dates_project,
    n_inducing =  n_inducing,
    vaccine_effect_matrix = vaccine_effect_matrix,
   # ascertainment_matrix = ascertainment_matrix,
    dow_effect = dow_effect
  )
  
}

reff_model <- function(data) {
  
  # reduction in R due to surveillance detecting and isolating infectious people
  surveillance_reff_local_reduction <- surveillance_effect(
    dates = data$dates$infection_project,
    cdf = gi_cdf,
    states = data$states
  )
  
  # ascertainment_rate <- data$ascertainment_matrix
  # surveillance_effect_local_reduction_with_ascertainment <- 1 - ((1 - surveillance_reff_local_reduction) * ascertainment_rate)
  # 
  # extra_isolation_local_reduction <- extra_isolation_effect(
  #   dates = data$dates$infection_project,
  #   cdf = gi_cdf,
  #   states = data$states
  # )
  
  
  # the reduction from R0 down to R_eff for imported cases due to different
  # quarantine measures each measure applied during a different period. Q_t is
  # R_eff_t / R0 for each time t, modelled as a monotone decreasing step function
  # over three periods with increasingly strict policies
  q_index <- case_when(
    data$dates$infection < quarantine_dates()$date[1] ~ 1,
    data$dates$infection < quarantine_dates()$date[2] ~ 2,
    TRUE ~ 3,
  )
  q_index <- c(q_index, rep(3, data$n_date_nums - data$n_dates))
  
  # q_raw <- uniform(0, 1, dim = 3)
  log_q_raw <- -exponential(1, dim = 3)
  log_q <- cumsum(log_q_raw)
  log_Qt <- log_q[q_index]
  
  # add likelihood for hotel quarantine spillovers - assume Poisson since
  # there's no reason to expect clustering with these rare events, and we'd
  # never be able to determine the number infected in each incident anyway
  expected_hotel_spillovers <- exp(log_q[3] + log(data$imported$total_hotel_cases))
  distribution(data$imported$total_hotel_spillovers) <- poisson(expected_hotel_spillovers)
  
  # The change in R_t for locally-acquired cases due to social distancing
  # behaviour, modelled as a sum of household R_t and non-household R_t
  # Non-household Reff is modelled as a function of the number of non-household
  # contacts per 24h (itself modelled from mobility data, calibrated against
  # contact surveys) and the relative transmission probability per contact,
  # inferred from surveys on micro-distancing behaviour.
  distancing_effect <- distancing_effect_model(data$dates$mobility, gi_cdf)
  
  # pull out R_t component due to distancing for locally-acquired cases, and
  # extend to correct length
  R_eff_loc_1_no_surv <- extend(distancing_effect$R_t, data$n_dates_project)
  
  # pull out vaccination effect
  vax_effect <- data$vaccine_effect_matrix
  
  # multiply by the surveillance and vaccination effects
  R_eff_loc_1 <- R_eff_loc_1_no_surv * surveillance_reff_local_reduction * vax_effect #*
  # R_eff_loc_1 <- R_eff_loc_1_no_surv *  surveillance_effect_local_reduction_with_ascertainment * vax_effect #*
  #extra_isolation_local_reduction
  
  
  log_R_eff_loc_1 <- log(R_eff_loc_1)
  
  # extract R0 from this model and estimate R_t component due to quarantine for
  # overseas-acquired cases
  log_R0 <- log_R_eff_loc_1[1, 1]
  log_R_eff_imp_1 <- log_R0 + log_Qt
  R_eff_imp_1 <- exp(log_R_eff_imp_1)
  
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
  kernel_L <- mat52(
    lengthscales = lognormal(3, 1),
    variance = 1
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
  
  # expand out the Reff for locals
  log_R_eff_imp <- sweep(
    zeros(data$n_date_nums, data$n_states),
    1,
    log_R_eff_imp_1,
    FUN = "+"
  )
  
  R_eff_loc_12 <- exp(log_R_eff_loc)
  R_eff_imp_12 <- exp(log_R_eff_imp)
  
  # work out which elements to exclude (because there were no infectious people)
  valid <- which(data$valid_mat, arr.ind = TRUE)
  
  #vectorise dow effect and remove invalid
  dow_effect <- data$dow_effect[valid]
  # combine everything as vectors, excluding invalid datapoints (remove invalid
  # elements here, otherwise it causes a gradient issue)
  R_eff_loc <- exp(log_R_eff_loc[1:data$n_dates, ])
  R_eff_imp <- exp(log_R_eff_imp[1:data$n_dates, ])
  new_from_loc_vec <- data$local$infectiousness[valid] * R_eff_loc[valid]
  new_from_imp_vec <- data$imported$infectiousness[valid] * R_eff_imp[valid]
  expected_infections_vec <- (new_from_loc_vec + new_from_imp_vec) * dow_effect
  
  # negative binomial likelihood for number of cases
  sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf), dim = data$n_states)
  size <- 1 / sqrt(sqrt_inv_size[valid[, 2]])
  prob <- 1 / (1 + expected_infections_vec / size)
  
  # Account for right truncation; underreporting of recent infections which have
  # had less time to be detected. Given the number of cases N_t infected on day t
  # (that will ever be detected), the number of cases N^*_t infected on that day
  # that are known about so far is drawn from a binomial sample with probability
  # p, from the time-to-detection distribution. Since N_t is drawn from a negative
  # binomial,  N^*_t is drawn from a compound binomial/negative binomial mixture
  # distribution. Fortunately that turns out to be a negative binomial with
  # modified probability parameter (NB is poisson-gamma, so binomial-NB is
  # binomial-poisson-gamma, but binomial-poisson is poisson with rate lambda * p and gamma times a constant is gamma,
  # so it's a poisson-gamma, which is NB).
  
  # There is an average of one day from specimen collection to confirmation, and
  # the linelist covers the previous day, so the date by which they need to have
  # been detected two days prior to the linelist date.
  detection_prob_vec <- data$detection_prob_mat[valid]
  
  # Modify the probability to account for truncation. When detection_prob_vec = 1,
  # this collapses to prob
  prob_trunc <- 1 / (1 + detection_prob_vec * (1 - prob) / prob)
  
  distribution(data$local$cases[valid]) <- negative_binomial(size, prob_trunc)
  
  m <- model(expected_infections_vec)
  
  list(
    greta_model = m,
    greta_arrays = module(
      expected_infections_vec,
      size,
      prob_trunc,
      R_eff_loc_1,
      R_eff_imp_1,
      R_eff_loc_12,
      R_eff_imp_12,
      log_R0,
      log_q,
      distancing_effect,
      surveillance_reff_local_reduction,
      #extra_isolation_local_reduction,
      log_R_eff_loc,
      log_R_eff_imp,
      epsilon_L
    )
  )
  
  # p <- distancing_effect$p
  # phi <- distancing_effect$phi
  # phi_wt <- distancing_effect$phi_wt
  # 
  # 
  # m <- model(expected_infections_vec,
  #            p)
  # 
  # list(
  #   greta_model = m,
  #   greta_arrays = module(
  #     expected_infections_vec,
  #     size,
  #     prob_trunc,
  #     R_eff_loc_1,
  #     R_eff_imp_1,
  #     R_eff_loc_12,
  #     R_eff_imp_12,
  #     log_R0,
  #     log_q,
  #     distancing_effect,
  #     surveillance_reff_local_reduction,
  #     log_R_eff_loc,
  #     log_R_eff_imp,
  #     epsilon_L
  #   ),
  #   phi,
  #   phi_wt
  # )
  
  
}

# extend (or truncate, or optionally clamp) values in rows of a matrix
# greta_array. I.e. given a matrix `x`, return another matrix with `n_rows` rows
# (by default the same as `x`), and with rows after `clamp_from` taking the
# value of row `clamp_from`. This can be used to extend a matrix, propagating
# the last value (if `n_rows` is increased), shorten a matrix (if n_rows is
# decreased), and simultaneously clamp subsequent values in the matrix at a
# fixed value.
extend <- function(x, n_rows = nrow(x), clamp_from = nrow(x)) {
  if (clamp_from > nrow(x)) {
    stop ("clamp_from must not be higher than the umber of rows in the matrix",
          call. = FALSE)
  }
  index <- seq_len(n_rows)
  clamped_index <- pmin(index, clamp_from)
  x[clamped_index, ]
}

# reff component 1 under only surveillance changes
reff_1_only_surveillance <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  log_R0 <- ga$log_R0
  reduction <- ga$surveillance_reff_local_reduction
  exp(log_R0 + log(reduction))
}

# reff component 1 under only extra isolation effect
reff_1_only_extra_isolation <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  log_R0 <- ga$log_R0
  # reduction <- ga$extra_isolation_local_reduction
  reduction <- extra_isolation_effect(
    dates = fitted_model$data$dates$infection_project,
    cdf = gi_cdf,
    states = fitted_model$data$states
  )
  
  exp(log_R0 + log(reduction))
}

reff_1_only_ttiq <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  log_R0 <- ga$log_R0
  # extra_iso <- ga$extra_isolation_local_reduction
  extra_iso <- extra_isolation_effect(
    dates = fitted_model$data$dates$infection_project,
    cdf = gi_cdf,
    states = fitted_model$data$states
  )
  
  surv <- ga$surveillance_reff_local_reduction
  
  reduction <- extra_iso*surv
  
  exp(log_R0 + log(reduction))
}


# reff component 1 if only macrodistancing had changed
reff_1_only_macro <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  h_t <- h_t_state(fitted_model$data$dates$mobility)
  HD_t <- de$HD_0 * h_t
  household_infections_macro <- de$HC_0 * (1 - de$p_star ^ HD_t)
  non_household_infections_macro <- de$OC_t_state * infectious_days * (1 - de$p_star ^ de$OD_0)
  hourly_infections_macro <- household_infections_macro + non_household_infections_macro
  hourly_infections_macro_extended <- extend(
    hourly_infections_macro,
    fitted_model$data$n_dates_project
  )
  hourly_infections_macro_extended * baseline_surveillance_effect
}

# reff component 1 if only macrodistancing had changed
reff_1_only_micro <- function(fitted_model) {
  ga <- fitted_model$greta_arrays
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  household_infections_micro <- de$HC_0 * (1 - de$p_star ^ de$HD_0)
  non_household_infections_micro <- de$OC_0 * infectious_days *
    (1 - de$p_star ^ de$OD_0) * de$gamma_t_state
  hourly_infections_micro <- household_infections_micro +
    non_household_infections_micro
  hourly_infections_micro_extended <- extend(
    hourly_infections_micro,
    fitted_model$data$n_dates_project
  )
  hourly_infections_micro_extended * baseline_surveillance_effect
}


# reff component 1 if only vaccination
reff_1_vaccine_only <- function(fitted_model, vaccine_effect){
  
  ga <- fitted_model$greta_arrays
  
  dates <- fitted_model$data$dates$infection_project
  
  df <- full_join(
    expand_grid(
      date = dates,
      state = unique(vaccine_effect$state)
    ),
    vaccine_effect
  ) %>%
    arrange(state, date) %>% 
    group_by(state) %>%
    tidyr::fill(
      effect,
      .direction = "updown"
    ) %>%
    pivot_wider(
      names_from = state,
      values_from = effect
    )
  
  ote <- df %>%
    dplyr::select(-date) %>%
    as.matrix
  
  
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  household_infections_vacc <- de$HC_0 * (1 - de$p_star ^ de$HD_0)
  non_household_infections_vacc <- de$OC_0 * infectious_days *
    (1 - de$p_star ^ de$OD_0)
  hourly_infections_vacc <- household_infections_vacc +
    non_household_infections_vacc
  hourly_infections_vacc_extended <- extend(
    hourly_infections_vacc,
    fitted_model$data$n_dates_project
  )
  
  reff_non_vac <- hourly_infections_vacc_extended * baseline_surveillance_effect 
  
  #sweep(reff_non_vac, 1, ote, "*")
  
  reff_non_vac*ote
  
}

# reff component 1 incorporating vaccine
reff_1_with_vaccine <- function(fitted_model, vaccine_effect){
  
  ga <- fitted_model$greta_arrays
  
  dates <- fitted_model$data$dates$infection_project
  
  df <- full_join(
    expand_grid(
      date = dates,
      state = unique(vaccine_effect$state)
    ),
    vaccine_effect
  ) %>%
    arrange(state, date) %>% 
    group_by(state) %>%
    tidyr::fill(
      effect,
      .direction = "updown"
    ) %>%
    pivot_wider(
      names_from = state,
      values_from = effect
    )
  
  ote <- df %>%
    dplyr::select(-date) %>%
    as.matrix
  
  
  reff1 <- ga$R_eff_loc_1 
  
  reff1*ote
  
}

# remove vaccine effect from C1 if fitted with vaccine
reff_1_without_vaccine <- function(fitted_model, vaccine_effect){
  
  ga <- fitted_model$greta_arrays
  
  dates <- fitted_model$data$dates$infection_project
  
  df <- full_join(
    expand_grid(
      date = dates,
      state = unique(vaccine_effect$state)
    ),
    vaccine_effect
  ) %>%
    arrange(state, date) %>% 
    group_by(state) %>%
    tidyr::fill(
      effect,
      .direction = "updown"
    ) %>%
    pivot_wider(
      names_from = state,
      values_from = effect
    )
  
  ote <- df %>%
    dplyr::select(-date) %>%
    as.matrix
  
  
  reff1 <- ga$R_eff_loc_1 
  
  reff1/ote
  
}



vaccination_dates <- function() {
  expand_grid(
    date = c("2021-02-22"),
    state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", 
              "VIC", "WA")
  ) %>%
    mutate(
      date = as.Date(date),
      state = factor(state)
    )
}


reff_C12_C1_ratio <- function(fitted_model){
  ga <- fitted_model$greta_arrays
  
  reff_1  <- ga$R_eff_loc_1
  reff_12 <- ga$R_eff_loc_12
  
  log(reff_12/reff_1)
}

# outputting Reff trajectories for Rob M
reff_sims <- function(fitted_model, nsim = 2000, which = "R_eff_loc_12") {
  
  ga <- fitted_model$greta_arrays[[which]]
  ga_vec <- c(ga)
  sim <- calculate(ga_vec, values = fitted_model$draws, nsim = nsim)
  
  samples <- t(sim[[1]][, , 1])
  colnames(samples) <- paste0("sim", 1:2000)
  
  tibble(
    date = rep(fitted_model$data$dates$infection_project, fitted_model$data$n_states),
    state = rep(fitted_model$data$states, each = fitted_model$data$n_dates_project),
  ) %>%
    mutate(date_onset = date + 5) %>%
    cbind(samples)
  
}

# given a vector of discrete values (e.g. logicals) representing switches
# between states, constrain those switches so that the state may not switch to
# another state unless thge run of the same value must have been at least
# 'min_run_length' long.
constrain_run_length <- function(x, min_run_length = 7) {
  
  # create an empty vector to fill and then iterate (this must be recursive, so
  # can't use e.g. slider::slide())
  result <- rep(FALSE, length(x))
  for (end in seq_along(result)) {
    
    # check whether there was a change recently
    start <- max(1, end - min_run_length) 
    idx <-  seq(start, end - 1)
    previous <- result[idx]
    recently_flipped <- !all(previous == previous[1])
    
    # if there was, then keep the same state as the last iteration. If there
    # wasn't, use the ideal value (the value in x)
    if (recently_flipped) {
      most_recent <- previous[length(previous)]
      result[end] <- most_recent
    } else {
      result[end] <- x[end]
    }
    
  }
  
  result
  
}

reff_plotting_sims <- function(
  fitted_model,
  #vaccine_timeseries = vaccine_effect_timeseries,
  nsim = 10000
){
  # add counterfactuals to the model object:
  # add fitted_model_extended obect because fitted_model is modified
  fitted_model_extended <- fitted_model
  
  vaccine_timeseries <- as_tibble(fitted_model$data$vaccine_effect_matrix) %>%
    mutate(date = fitted_model$data$dates$infection_project) %>%
    pivot_longer(cols = -date, names_to = "state", values_to = "effect")
  
  # Reff for locals component 1 under
  # only micro/macro/surveillance improvements
  fitted_model_extended$greta_arrays <- c(
    fitted_model$greta_arrays,
    list(
      R_eff_loc_1_macro = reff_1_only_macro(fitted_model_extended),
      R_eff_loc_1_micro = reff_1_only_micro(fitted_model_extended),
      R_eff_loc_1_surv = reff_1_only_surveillance(fitted_model_extended),
      R_eff_loc_1_iso = reff_1_only_extra_isolation(fitted_model_extended),
      R_eff_loc_1_ttiq = reff_1_only_ttiq(fitted_model_extended),
      R_eff_loc_1_vaccine_only = reff_1_vaccine_only(fitted_model_extended, vaccine_timeseries),
      R_eff_loc_1_without_vaccine = reff_1_without_vaccine(fitted_model_extended, vaccine_timeseries),
      R_eff_12_1_ratio = reff_C12_C1_ratio(fitted_model_extended)
    ) 
  )
  
  # flatten all relevant greta array matrices to vectors before calculating
  trajectory_types <- c(
    "R_eff_loc_1",
    "R_eff_imp_1",
    "R_eff_loc_12",
    "R_eff_imp_12",
    "epsilon_L",
    "R_eff_loc_1_micro",
    "R_eff_loc_1_macro",
    "R_eff_loc_1_surv",
    "R_eff_loc_1_iso",
    "R_eff_loc_1_ttiq",
    "R_eff_loc_1_vaccine_only",
    "R_eff_loc_1_without_vaccine",
    "R_eff_12_1_ratio"
  )
  vector_list <- lapply(fitted_model_extended$greta_arrays[trajectory_types], c)
  
  # simulate from posterior for these quantities of interest
  args <- c(vector_list, list(values = fitted_model_extended$draws, nsim = nsim))
  sims <- do.call(calculate, args)
  
  return(sims)
}

# function to calculate, plot, and save all the outputs (with flags for plot
# types) - pass in an optional maximum date argument
reff_plotting <- function(
  fitted_model,
  dir = "outputs",
  subdir = "figures",
  min_date = as.Date("2020-03-01"),
  #min_date = NA,
  max_date = fitted_model$data$dates$latest_mobility,
  mobility_extrapolation_rectangle = TRUE,
  projection_date = NA,
  washout_cutoff = 0,
  vaccine_timeseries = vaccine_effect_timeseries,
  sims = NULL
) {
  
  if(is.null(sims)){
    # add counterfactuals to the model object:
    # add fitted_model_extended obect because fitted_model is modified
    fitted_model_extended <- fitted_model
    # Reff for locals component 1 under
    # only micro/macro/surveillance improvements
    fitted_model_extended$greta_arrays <- c(
      fitted_model$greta_arrays,
      list(
        R_eff_loc_1_macro = reff_1_only_macro(fitted_model_extended),
        R_eff_loc_1_micro = reff_1_only_micro(fitted_model_extended),
        R_eff_loc_1_surv = reff_1_only_surveillance(fitted_model_extended),
        R_eff_loc_1_iso = reff_1_only_extra_isolation(fitted_model_extended),
        R_eff_loc_1_ttiq = reff_1_only_ttiq(fitted_model_extended),
        R_eff_loc_1_vaccine_only = reff_1_vaccine_only(fitted_model_extended, vaccine_timeseries),
        R_eff_loc_1_without_vaccine = reff_1_without_vaccine(fitted_model_extended, vaccine_timeseries),
        R_eff_12_1_ratio = reff_C12_C1_ratio(fitted_model_extended)
      ) 
    )
    
    # flatten all relevant greta array matrices to vectors before calculating
    trajectory_types <- c(
      "R_eff_loc_1",
      "R_eff_imp_1",
      "R_eff_loc_12",
      "R_eff_imp_12",
      "epsilon_L",
      "R_eff_loc_1_micro",
      "R_eff_loc_1_macro",
      "R_eff_loc_1_surv",
      "R_eff_loc_1_iso",
      "R_eff_loc_1_ttiq",
      "R_eff_loc_1_vaccine_only",
      "R_eff_loc_1_without_vaccine",
      "R_eff_12_1_ratio"
    )
    vector_list <- lapply(fitted_model_extended$greta_arrays[trajectory_types], c)
    
    # simulate from posterior for these quantities of interest
    args <- c(vector_list, list(values = fitted_model_extended$draws, nsim = 10000))
    sims <- do.call(calculate, args)
  }
  
  if(is.na(min_date)){
    min_date <- max_date %m-% months(6)
  }
  
  # reformat case data for plotting (C1 and C12)
  local_cases_long <- fitted_model$data$local$cases %>%
    as_tibble() %>%
    mutate(
      date = fitted_model$data$dates$infection,
    ) %>%
    pivot_longer(
      cols = -date,
      names_to = "state",
      values_to = "cases"
    )
  
  # rugplot of case counts
  case_data <- local_cases_long %>%
    filter(date >= min_date) %>%
    mutate(
      type = "Nowcast",
      height = 1
    ) %>%
    uncount(cases)
  
  case_rug <- geom_rug(
    aes(date, height),
    data = case_data,
    sides = "b",
    alpha = 0.5,
    size = 0.5,
    colour = grey(0.7)
  )
  
  # a washout for whether there are few cases (<20 in past 5 days) with a run of
  # at least 7 days (to prevent rapidly flip-flopping)
  few_case_data <- local_cases_long %>%
    full_join(
      expand_grid(
        state = fitted_model$data$states,
        date = seq(min_date, max_date, by = 1),
      )
    ) %>%
    mutate(
      cases = replace_na(cases, 0)
    ) %>%
    group_by(state)  %>%
    mutate(
      recent_count = slider::slide_int(cases, sum, .before = 13),
    ) %>%
    ungroup() %>%
    filter(date >= min_date) %>%
    mutate(
      few_cases = recent_count < washout_cutoff,
      type = "Nowcast",
      state = factor(state),
      mean = 1
    ) %>%
    # don't let the washout state change until there have been at least the
    # specified number of days in the same state
    arrange(state, date) %>%
    group_by(state) %>%
    mutate(
      washout = constrain_run_length(few_cases, 7)
    ) %>%
    ungroup()
  
  few_case_washout <- geom_ribbon(
    aes(ymin = -10, ymax = washout * 100 - 10),
    data = few_case_data,
    fill = grey(1),
    alpha = 0.5,
    colour = grey(0.9),
    linetype = 3
  )
  
  # ratio of C12 and C1
  p <- plot_trend(sims$R_eff_12_1_ratio,
                  data = fitted_model$data,
                  min_date = min_date,
                  max_date = max_date,
                  multistate = TRUE,
                  base_colour = pink,
                  hline_at = 0,
                  projection_at = projection_date,
                  ylim = NULL,
                  ybreaks = c(-5, 5),
                  plot_voc = TRUE) + 
    ggtitle(label = "Log ratio of local-to-local transmission and transmission potential",
            subtitle = expression(Log~ratio~of~R["eff"]~and~transmission~potential)) +
    ylab("Deviation")
  
  if (mobility_extrapolation_rectangle) {
    p <- p + annotate("rect",
                      xmin = fitted_model$data$dates$latest_infection,
                      xmax = fitted_model$data$dates$latest_mobility,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  # add case rug plot and washout
  p <- p + case_rug + few_case_washout
  
  p
  
  save_ggplot("R_eff_12_1_ratio.png", dir, subdir)
  
  
  # vaccine effect only
  plot_trend(sims$R_eff_loc_1_vaccine_only, 
             data = fitted_model$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = fifo,
             projection_at = projection_date,
             ylim = c(0, 8),
             intervention_at = interventions(),
             plot_voc = TRUE,
             plot_vax = TRUE
  ) + 
    ggtitle(label = "Impact of vaccination",
            subtitle = expression(R["eff"]~"if"~only~vaccination~had~occurred)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_vaccine_only.png", dir, subdir, multi = TRUE)
  
  # vaccine effect out of C1
  plot_trend(
    sims$R_eff_loc_1_without_vaccine,
    data = fitted_model$data,
    min_date = min_date,
    max_date = max_date,
    multistate = TRUE,
    #ylim = c(0, 6),
    intervention_at = interventions(),
    base_colour = green,
    projection_at = projection_date,
    plot_voc = TRUE,
    plot_vax = TRUE
  ) + 
    ggtitle(label = "Impact of social distancing only",
            subtitle = expression(Component~of~R["eff"]~due~to~social~distancing~only)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_without_vaccine.png", dir, subdir, multi = TRUE)
  
  # microdistancing only
  plot_trend(sims$R_eff_loc_1_micro,
             data = fitted_model$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = purple,
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of micro-distancing",
            subtitle = expression(R["eff"]~"if"~only~"micro-distancing"~behaviour~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_micro.png", dir, subdir)
  
  # macrodistancing only
  plot_trend(sims$R_eff_loc_1_macro,
             data = fitted_model$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = blue,
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of macro-distancing",
            subtitle = expression(R["eff"]~"if"~only~"macro-distancing"~behaviour~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_macro.png", dir, subdir)
  
  # improved surveilance only
  plot_trend(sims$R_eff_loc_1_surv,
             data = fitted_model$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = yellow,
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of improved surveillance",
            subtitle = expression(R["eff"]~"if"~only~surveillance~effectiveness~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_surv.png", dir, subdir)
  
  plot_trend(sims$R_eff_loc_1_iso,
             data = fitted_model$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = purple,
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact contract tracing isolation",
            subtitle = expression(R["eff"]~"if"~only~extra~isolation~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_iso.png", dir, subdir)
  
  plot_trend(sims$R_eff_loc_1_ttiq,
             data = fitted_model$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = "Coral",
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of TTIQ",
            subtitle = expression(R["eff"]~"if"~only~TTIQ~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_ttiq.png", dir, subdir)
  
  # Component 1 for national / state populations
  plot_trend(sims$R_eff_loc_1,
             data = fitted_model$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = green,
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of social distancing & vaccination",
            subtitle = expression(Component~of~R["eff"]~due~to~social~distancing~and~vaccination)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local.png", dir, subdir)
  
  plot_trend(sims$R_eff_imp_1,
             data = fitted_model$data,
             min_date = min_date,
             max_date = max_date,
             multistate = FALSE,
             base_colour = orange,
             ylim = c(0, 0.4),
             intervention_at = quarantine_dates(),
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of quarantine of overseas arrivals",
            subtitle = expression(Component~of~R["eff"]~due~to~quarantine~of~overseas~arrivals)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_import.png", dir, subdir, multi = FALSE)
  
  # Reff for active cases
  p <- plot_trend(sims$R_eff_loc_12,
                  data = fitted_model$data,
                  min_date = min_date,
                  max_date = max_date,
                  multistate = TRUE,
                  base_colour = green,
                  ylim = c(0, 4),
                  projection_at = projection_date,
                  plot_voc = TRUE) +
    ggtitle(label = "Local to local transmission potential",
            subtitle = "Average across active cases") +
    ylab(expression(R["eff"]~from~"locally-acquired"~cases))
  
  if (mobility_extrapolation_rectangle) {
    p <- p + annotate("rect",
                      xmin = fitted_model$data$dates$latest_infection,
                      xmax = fitted_model$data$dates$latest_mobility,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  # add case rug plot and washout
  p <- p + few_case_washout + case_rug
  p
  
  save_ggplot("R_eff_12_local.png", dir, subdir)
  
  # Reff for active cases
  p <- plot_trend(sims$R_eff_loc_12,
                  data = fitted_model$data,
                  min_date = min_date,
                  max_date = max_date,
                  multistate = TRUE,
                  base_colour = green,
                  ylim = c(0, 2),
                  projection_at = projection_date,
                  plot_voc = TRUE) +
    ggtitle(label = "Local to local transmission potential",
            subtitle = "Average across active cases") +
    ylab(expression(R["eff"]~from~"locally-acquired"~cases))
  
  if (mobility_extrapolation_rectangle) {
    p <- p + annotate("rect",
                      xmin = fitted_model$data$dates$latest_infection,
                      xmax = fitted_model$data$dates$latest_mobility,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  # add case rug plot and washout
  p <- p + few_case_washout + case_rug
  p
  
  save_ggplot("R_eff_12_local_zoom.png", dir, subdir)
  
  # component 2 (noisy error trends)
  p <- plot_trend(sims$epsilon_L,
                  data = fitted_model$data,
                  min_date = min_date,
                  max_date = max_date,
                  multistate = TRUE,
                  base_colour = pink,
                  hline_at = 0,
                  projection_at = projection_date,
                  ylim = NULL,
                  ybreaks = c(-2, 1),
                  plot_voc = TRUE) + 
    ggtitle(label = "Short-term variation in local to local transmission rates",
            subtitle = expression(Deviation~from~log(R["eff"])~of~"local-local"~transmission)) +
    ylab("Deviation")
  
  if (mobility_extrapolation_rectangle) {
    p <- p + annotate("rect",
                      xmin = fitted_model$data$dates$latest_infection,
                      xmax = fitted_model$data$dates$latest_mobility,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  # add case rug plot and washout
  p <- p + case_rug + few_case_washout
  
  p
  
  save_ggplot("R_eff_2_local.png", dir, subdir)
  
  
}


# given a dataframe of Reff trajectory samples for Rob M, 'soft-clamp' the Reff
# trajectories so that the log-mean of Reff is constant into the future from the target date, but the
# trajectories still vary over time (rather than hard-clamping them so the
# trajectory for each Reff is flat into the future).
soft_clamp <- function(local_samples, target_date) {
  local_samples %>%
    pivot_longer(
      cols = starts_with("sim"),
      names_to = "sim",
      values_to = "reff"
    ) %>%
    group_by(
      date,
      state
    ) %>%
    mutate(
      log_reff = log(reff),
      log_mean = mean(log_reff)
    ) %>%
    group_by(
      state
    ) %>%
    mutate(
      latest_log_mean = mean(log_reff[date == target_date]),
      adjust = ifelse(date > target_date, latest_log_mean - log_mean, 0),
      log_reff = log_reff + adjust,
      reff = exp(log_reff)
    ) %>%
    ungroup() %>%
    select(-log_reff, -log_mean, -latest_log_mean, -adjust) %>%
    pivot_wider(
      names_from = sim,
      values_from = reff
    ) 
}

# given a dataframe of Reff trajectory samples for Rob M, 'hard-clamp' the Reff
# trajectories so that the trajectory for each Reff is costant into the future
# from the target date.
hard_clamp <- function(local_samples, target_date) {
  local_samples %>%
    pivot_longer(
      cols = starts_with("sim"),
      names_to = "sim",
      values_to = "reff"
    ) %>%
    group_by(
      state,
      sim
    ) %>%
    mutate(
      target_reff = reff[date == target_date],
      reff = ifelse(date > target_date, reff[date == target_date], reff),
    ) %>%
    ungroup() %>%
    select(-target_reff) %>%
    pivot_wider(
      names_from = sim,
      values_from = reff
    )
}

# output simulations
write_reff_sims <- function(
  fitted_model,
  dir = "outputs/projection",
  write_reff_1 = TRUE,
  write_reff_12 = TRUE,
  write_reff_2 = FALSE
) {
  
  if (write_reff_1) {
    
    reff_1 <- reff_sims(fitted_model, which = "R_eff_loc_1")
    
    reff_1 %>%
      write_csv(
        file.path(dir, "r_eff_1_local_samples.csv")
      )
    
  }
  
  if (write_reff_2) {
    
    reff_2 <- reff_sims(fitted_model, which = "epsilon_L")
    
    reff_2 %>%
      write_csv(
        file.path(dir, "r_eff_2_samples.csv")
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
  
}

fit_reff_model <- function(data, max_tries = 1, iterations_per_step = 2000,
                           warmup = 1000) {
  
  # build the greta model
  model_output <- reff_model(data)
  greta_arrays <- model_output$greta_arrays
  greta_model <- model_output$greta_model
  
  # first pass at model fitting  
  draws <- mcmc(
    greta_model,
    sampler = hmc(Lmin = 25, Lmax = 30),
    chains = 10,
    warmup = warmup,
    n_samples = 2000,
    one_by_one = TRUE
  )
  
  # if it did not converge, try extending it a bunch more times
  finished <- converged(draws)
  tries <- 0
  while(!finished & tries < max_tries) {
    draws <- extra_samples(
      draws,
      iterations_per_step,
      one_by_one = TRUE
    )
    tries <- tries + 1
    finished <- converged(draws)
  }
  
  # warn if we timed out before converging successfully
  if (tries == max_tries) {
    warning("sampling did not converge according to benchmarks")
  }
  
  # return a fitted model object
  module(greta_model, greta_arrays, data, draws)
  
}

write_reff_key_dates <- function(model_data, dir = "outputs/") {
  # save these dates for Freya and Rob to check
  tibble(
    linelist_date = model_data$dates$linelist,
    latest_infection_date = model_data$dates$latest_infection,
    latest_reff_date = model_data$dates$latest_mobility,
    forecast_reff_change_date = model_data$dates$latest_mobility + 1
  ) %>%
    write_csv(
      file.path(dir, "output_dates.csv")
    )
}

# save local case data, dates, and detection probabilities for Robs
write_local_cases <- function(model_data, dir = "outputs") {
  
  tibble::tibble(
    date_onset = rep(model_data$dates$onset, model_data$n_states),
    detection_probability = as.vector(model_data$detection_prob_mat),
    state = rep(model_data$states, each = model_data$n_dates),
    count = as.vector(model_data$local$cases_infectious)*as.vector(model_data$dow_effect),
    acquired_in_state = as.vector(model_data$local$cases),
    dow_effect = as.vector(model_data$dow_effect)
  ) %>%
    write.csv(
      file.path(dir, "local_cases_input.csv"),
      row.names = FALSE
    )
  
}

# convert any new linelist files into formatted case data in past_cases
update_past_cases <- function(past_cases_dir = "outputs/past_cases") {
  
  # linelists already processed
  past_cases_files <- list.files(past_cases_dir, pattern = ".csv$") %>%
    tibble(file = .) %>%
    mutate(
      date = gsub("local_cases_input_", "", file),
      date = gsub(".csv", "", date),
      date = as.Date(date)
    )
  
  # find all nndss linelists that haven't yet been processed (using only the latest on each date)
  linelist_files <- linelist_date_times("~/not_synced/nndss") %>%
    mutate(
      date = as.Date(date_time),
      file = basename(file)
    ) %>%
    group_by(date) %>%
    filter(
      date_time == max(date_time)
    ) %>%
    ungroup() %>%
    anti_join(
      past_cases_files,
      by = "date"
    )
  
  # subset to only Wednesdays
  # mutate(wday = lubridate::wday(date, label = TRUE)) %>%
  # filter(wday == "Wed")
  
  linelists <- linelist_files %>%
    pull(date) %>%
    lapply(load_linelist)
  
  # format and write these out
  for (linelist in linelists) {
    
    model_data <- reff_model_data(linelist)
    
    linelist_date <- model_data$dates$linelist
    
    message("processing linelist: ", linelist_date)
    
    tibble::tibble(
      date_onset = rep(model_data$dates$onset, model_data$n_states),
      detection_probability = as.vector(model_data$detection_prob_mat),
      state = rep(model_data$states, each = model_data$n_dates),
      count = as.vector(model_data$local$cases_infectious),
      acquired_in_state = as.vector(model_data$local$cases)
    ) %>%
      write.csv(
        paste0(past_cases_dir, "/local_cases_input_",
               format(linelist_date, format = "%Y-%m-%d"), ".csv"),
        row.names = FALSE)
    
  }
  
}

# plot visual checks of model posterior calibration against observed data
plot_reff_checks <- function(fitted_model, nsim = 10000) {
  
  cases <- negative_binomial(
    fitted_model$greta_arrays$size,
    fitted_model$greta_arrays$prob_trunc
  )
  cases_sim <- calculate(cases, values = fitted_model$draws, nsim = nsim)[[1]][, , 1]
  
  valid <- which(fitted_model$data$valid_mat, arr.ind = TRUE)
  observed <- fitted_model$data$local$cases[valid]
  
  # overall PPC check
  bayesplot::ppc_ecdf_overlay(
    observed,
    cases_sim[1:1000, ],
    discrete = TRUE
  )
  
  # check by state and time
  plot_fit(observed, cases_sim, fitted_model$data)
  
  # check simulation fit
  check_projection(fitted_model)
  
}

# split the dates and states into periods  with similar notification delay distributions
notification_delay_group <- function(date_confirmation, state) {
  
  stage <- case_when(
    date_confirmation < as.Date("2020-06-14") ~ 1,
    date_confirmation < as.Date("2020-08-01") ~ 2,
    date_confirmation < as.Date("2020-08-21") ~ 3,
    TRUE ~ 4,
  )
  
  group <- case_when(
    stage == 1 ~ "all states (start-Jun13)",
    stage == 2 & state == "VIC" ~ "VIC 1 (Jun14-Jul31)",
    stage == 3 & state == "VIC" ~ "VIC 2 (Aug1-Aug20)",
    stage == 4 & state == "VIC" ~ "VIC 3 (Aug21-now)",
    TRUE ~ "other states Jun14-now"
  )
  
  group
  
}

# return a function to get the CDf of the notification delay distribution for a
# given date and state
get_notification_delay_cdf <- function(linelist) {
  
  delay_data <- linelist %>%
    filter(
      !is.na(date_onset),
      date_confirmation <= (date_linelist - 3)
    ) %>%
    select(
      date_onset,
      date_confirmation,
      state,
      import_status
    ) %>%
    mutate(
      delay = as.numeric(date_confirmation - date_onset),
      group = notification_delay_group(date_confirmation, state)
    ) %>%
    filter(
      delay <= 6 * 7
    ) %>%
    group_by(group) %>%
    mutate(
      lower = quantile(delay, 0.005),
      upper = quantile(delay, 0.995)
    ) %>%
    filter(
      delay >= lower,
      delay <= upper
    )
  
  # get an ecdf for each group
  ecdfs <- delay_data %>%
    mutate(
      ecdf = list(ecdf(delay)),
      id = row_number()
    ) %>%
    filter(id == 1) %>%
    select(group, ecdf)
  
  # return a function to compute the CDF of the delay distribution for that
  # state and those delays and dates
  function(delays, possible_onset_dates, states) {
    
    group <- notification_delay_group(possible_onset_dates, states)
    idx <- match(group, ecdfs$group)
    idx <- replace_na(idx, 1)
    
    cdfs <- ecdfs$ecdf[idx]
    probs <- rep(0, length(group))
    
    for(i in seq_along(group)) {
      probs[i] <- cdfs[[i]](delays[i])
    }
    
    probs
    
  }
  
}

# return a date-by-state matrix of detection probabilities
detection_probability_matrix <- function(latest_date, infection_dates, states, notification_delay_cdf) {
  
  n_dates <- length(infection_dates)
  n_states <- length(states)
  onset_dates <- infection_dates + 5
  delays <- latest_date - onset_dates
  
  onset_dates_mat <- matrix(
    onset_dates, 
    nrow = n_dates,
    ncol = n_states
  )
  
  delays_mat <- matrix(
    delays, 
    nrow = n_dates,
    ncol = n_states
  )
  
  states_mat <- matrix(
    states,
    nrow = n_dates,
    ncol = n_states,
    byrow = TRUE
  )
  
  # get the detection probability matrix
  detection_prob_mat <- delays_mat * 0
  detection_prob_mat[] <- notification_delay_cdf(
    delays = delays_mat,
    possible_onset_dates = onset_dates_mat,
    states = states_mat
  )
  
  detection_prob_mat
  
}

impute_linelist_old <- function(linelist, notification_delay_cdf) {
  
  # impute the onset dates (only 0.6% of cases) using expected value from time to
  # detection distribution. Do this outside dplyr to avoid duplicating slow computations
  missing_onset <- is.na(linelist$date_onset)
  imputed_onsets <- impute_onsets(
    linelist$date_confirmation[missing_onset],
    linelist$state[missing_onset],
    notification_delay_cdf,
    method = "random"
  )
  linelist$date_onset[missing_onset] <- imputed_onsets
  
  linelist %>%
    mutate(date = date_onset - 5)
  
}

load_nndss <- function () {
  get_nndss_linelist() %>%
    impute_linelist()
} # possibly deprecated?

load_vic <- function (file) {
  get_vic_linelist(file) %>%
    impute_linelist()
} # possibly deprecated?

load_linelist <- function(date = NULL,
                          use_vic = TRUE,
                          use_sa = FALSE,
                          use_nsw = TRUE) {
  
  # load the latest NNDSS linelist (either the latest or specified file)
  linelist <- get_nndss_linelist(date = date)
  
  # optionally replace VIC data with DHHS direct upload
  if (use_vic) {
    
    vic_linelist <- linelist$date_linelist[1] %>%
      format(format = "%Y%m%d") %>%
      paste0("~/not_synced/vic/", ., "_linelist_reff.csv") %>%
      get_vic_linelist() %>%
      filter(!is.na(date_confirmation))
    
    vic_ll_date <- vic_linelist$date_linelist[1]
    vic_ll_start <- min(vic_linelist$date_confirmation)
    
    linelist <- linelist %>%
      filter(
        !(state == "VIC" &
            date_confirmation >= vic_ll_start & 
            date_confirmation <= vic_ll_date
        )
      ) %>%
      bind_rows(vic_linelist)
    
  }
  
  if (use_sa) {
    
    linelist <- linelist %>%
      filter(
        !(state == "SA" &
            import_status == "local" &
            date_detection >= as.Date("2021-07-19") & 
            date_detection < as.Date("2021-07-26")
        )
      ) %>%
      
      bind_rows(
        get_sa_linelist()
      )
    
  }
  
  if (use_nsw) {
    
    nsw_ll <- get_nsw_linelist()
    nsw_ll_date <- nsw_ll$date_linelist[1]
    nsw_ll_start <- min(nsw_ll$date_confirmation)
    
    
    linelist <- linelist %>%
      filter(
        !(state == "NSW" &
            import_status == "local" &
            date_detection >= nsw_ll_start #& 
          #grey this out to follow NCIMs delay pattern  
          # date_detection <= nsw_ll_date
        )
      ) %>%
      bind_rows(
        nsw_ll
      )
  }
  
  # flag whether each case is an interstate import
  linelist <- linelist %>%
    mutate(
      interstate_import = case_when(
        state == "ACT" ~ interstate_import_cvsi,
        # ACT have indicated that CV_SOURCE_INFECTION is a reliable indicator for their territory
        # whereas postcodes in ACT may also cover NSW so postcodes are less reliable
        state != state_of_acquisition ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    select(-interstate_import_cvsi)
  
  linelist
  
}

write_linelist <- function(linelist = linelist,
                           dir = "outputs"){
  
  if (is.null(linelist)) {
    linelist <- load_linelist()
  }
  
  ll_date <- linelist$date_linelist[1]
  
  write.csv(
    x = linelist,
    file = sprintf(
      "%s/interim_linelist_%s.csv",
      dir,
      ll_date
    ),
    row.names = FALSE
  )
  
}

# convert imputed linelist into matrix of new infections by date and state
infections_by_region <- function(linelist,
                                 region_type = c("state", "postcode_of_residence"),
                                 case_type = c("local", "imported", "both"),
                                 from = min(linelist$date),
                                 to = max(linelist$date)) {
  
  region_type <- match.arg(region_type)
  case_type <- match.arg(case_type)
  
  # get full range of dates (do this before dropping rows)
  dates <- seq(from, to, by = 1)
  
  # drop unneeded rows (and regions)
  if (case_type != "both") {
    linelist <- linelist %>%
      filter(import_status == case_type)
  }
  
  regions <- unique(linelist[[region_type]])
  
  # pad this with full set of dates, states, and import statuses
  grid <- expand_grid(
    date = dates,
    region = regions
  )
  
  # widen into matrices of date by state
  new_infections <- linelist %>%
    mutate(cases = 1) %>%
    rename(region = !!region_type) %>%
    old_right_join(grid) %>%
    group_by(region, date) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = region, values_from = cases) %>%
    select(-date) %>%
    as.matrix()
  
  new_infections
  
}

# aggregate infections and infectiousness for a given case type at lga level
lga_infections <- function(linelist, dates, gi_mat, case_type = c("local", "imported")) {
  
  case_type <- match.arg(case_type)
  
  # aggregate locally-acquired cases by postcode and date
  postcode_matrix <- linelist %>%
    infections_by_region(
      region_type = "postcode",
      case_type = case_type,
      from = min(dates),
      to = max(dates)
    )
  
  postcodes <- colnames(postcode_matrix)
  
  # read in postcode-lga lookup and weights
  weights_tbl <- read_xlsx(
    "data/spatial/abs/CA_POSTCODE_2018_LGA_2018.xlsx",
    sheet = 4,
    skip = 5
  ) %>%
    filter(
      row_number() > 1,
      !is.na(RATIO)
    ) %>%
    select(
      postcode = POSTCODE_2018...1,
      lga = LGA_NAME_2018,
      lga_code = LGA_CODE_2018,
      weight = RATIO
    ) %>%
    # subset to observed postcodes
    old_right_join(
      tibble(
        postcode = postcodes,
      )
    ) %>%
    # assign unrecognised/unknown/overseas postcodes to a separate class
    mutate(
      lga = replace_na(lga, "other"),
      weight = replace_na(weight, 1)
    )
  
  # convert to matrix for weighting
  weights_matrix <- weights_tbl %>%
    select(-lga_code) %>%
    pivot_wider(
      names_from = lga,
      values_from = "weight",
      values_fill = list(weight = 0)
    ) %>%
    select(-postcode) %>%
    as.matrix() %>%
    `rownames<-`(postcodes)
  
  # normalise so we don't lose any cases
  weights_matrix <- sweep(weights_matrix, 1, rowSums(weights_matrix), FUN = "/")
  
  # aggregate cases to lga level
  lga_matrix <- postcode_matrix %*% weights_matrix
  lga <- lengthen(lga_matrix,
                  dates,
                  "lga",
                  "infections")
  
  # get infectiousness of these cases
  postcode_infectious_matrix <- gi_mat %*% postcode_matrix
  lga_infectious_matrix <- postcode_infectious_matrix %*% weights_matrix
  lga_infectious <- lengthen(lga_infectious_matrix,
                             dates,
                             "lga",
                             "infectiousness")
  
  # convert both to long form and combine
  lga_long <- lga %>%
    left_join(
      lga_infectious
    ) %>%
    # remove empty entries
    filter(
      infections > 0 | infectiousness > 0
    ) %>%
    # add LGA codes
    left_join(
      weights_tbl %>%
        select(lga, lga_code) %>%
        filter(!duplicated(.))
    ) %>%
    mutate(
      state = lga_to_state(lga)
    ) %>%
    arrange(state, lga, date)
  
  lga_long
  
}


# function to get a greta array forecasting numbers of locally-acquired cases
# in each state into the future.

# local cases and imported cases should be matrices containing integer (or
# fractional) numbers of observed or assumed cases infected on each date.
# Reff_locals and Reff_imports should be either matrices or 2D greta arrays of
# transmission potential for locally-acquired and imported cases. All four of
# these arguments must have the same number of columns. Reff_locals and
# Reff_imports must have the same number of rows, which should be greater than
# or equal to the numbers of rows in local_cases and imported_cases. Where
# local_cases and imported_cases have fewer rows than the other matrices, they
# will be padded with zeros to represent an assumption of no imported cases or
# other local cases injected into the local population. dates must be a vector
# of dates with as many elements as rows in the Reff matrices, and gi_cdf must
# be a function returning the continuous version of the generation interval
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
  states <- c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
  
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
  # gi_mat <- gi_matrix(gi_cdf, dates, gi_bounds = gi_bounds)
  gi_vec <- gi_vector(gi_cdf, max(dates), gi_bounds = gi_bounds)
  
  # infectiousness of imported cases over time
  imported_infectious <- gi_convolution(
    cases = imported_cases,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  
  # expected number of primary (import-local) locally-acquired cases
  primary_local_cases <- imported_infectious * Reff_imports
  
  # infectiousness of primary locally-acquired cases
  primary_local_infectiousness <- gi_convolution(
    cases = primary_local_cases,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  
  # infectiousness of observed (or assumed) locally-acquired cases
  existing_local_infectiousness <- gi_convolution(
    cases = local_cases,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  
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
  forecast_local_infectious <- gi_convolution(
    cases = forecast_local_cases,
    dates = dates,
    states = states,
    gi_cdf = gi_cdf,
    gi_bounds = gi_bounds
  )
  
  expected_transmission <- forecast_local_infectious * Reff_locals +
    primary_local_cases
  p_cases <- 1 - exp(-expected_transmission)
  
  list(
    local_cases = forecast_local_cases,
    secondary_local_cases = secondary_local_cases,
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

# build a convolution matrix for the discrete generation interval for a single
# state, applying the effect of improving surveillance and normalising to
# integrate to 1
gi_matrix <- function(gi_cdf, dates, state,
                      gi_bounds = c(0, 20),
                      ttd_cdfs = NULL) {
  
  n_dates <- length(dates)
  
  # baseline GI matrix, without effects of improved surveillance
  day_diff <- time_difference_matrix(n_dates)
  gi_mat_naive <- gi_probability(gi_cdf, day_diff)
  
  # compute fraction surviving without detection in circulant matrix format,
  # multiply by GI matrix and rescale to get new GI distribution on each day
  ttd_mat <- day_diff
  ttd_mat[] <- ttd_survival(
    days = c(day_diff),
    dates = rep(dates, each = n_dates),
    target_state = state,
    cdfs = ttd_cdfs
  )
  scaling <- surveillance_effect(
    dates = dates,
    cdf = gi_cdf,
    state = state,
    gi_bounds = gi_bounds,
    ttd_cdfs = ttd_cdfs
  )
  rel_gi_mat <- gi_mat_naive * ttd_mat
  gi_mat <- sweep(rel_gi_mat, 2, scaling, FUN = "/")
  
  gi_mat
  
}

# build a vector of discrete generation interval probability masses for a given
# date, applying the effect of improving surveillance and normalising to
# integrate to 1
gi_vector <- function(gi_cdf, date, state,
                      gi_bounds = c(0, 20),
                      ttd_cdfs = NULL) {
  
  # baseline GI vector, without effects of improved surveillance
  days <- seq(gi_bounds[1], gi_bounds[2])
  gi_vec_naive <- gi_probability(gi_cdf, days = days, bounds = gi_bounds)
  
  # compute fraction surviving without detection in circulant matrix format,
  # multiply by GI matrix and rescale to get new GI distribution on each day
  ttd_vec <- ttd_survival(
    days = days,
    dates = rep(date, each = length(days)),
    target_state = state,
    cdfs = ttd_cdfs
  )
  scaling <- surveillance_effect(
    dates = date,
    cdf = gi_cdf,
    state = state,
    gi_bounds = gi_bounds,
    ttd_cdfs = ttd_cdfs
  )
  rel_gi_vec <- gi_vec_naive * ttd_vec
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
    scale <- 0.95 * scale
    
  } else {
    
    height <- height * 1.25
    
  }
  
  path <- file.path(dir, subdir, filename)
  ggsave(path,
         width = width,
         height = height,
         scale = scale,
         dpi = dpi,
         bg = 'white')
  
}

# prep a spatial layer with Victorian LGAs and their populations
prep_state_lgas <- function(
  state = "Victoria",
  out_dir = "data/spatial"
) {
  
  state_short <- abbreviate_states(state)
  filepath <- file.path(out_dir,
                        paste0(
                          tolower(state_short),
                          "_lga.RDS"
                        ))
  library(sf)
  
  # load populations of all meshblocks
  mesh_pop <- read_csv(
    "data/spatial/abs/2016 census mesh block counts.csv",
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
  state_mesh <- paste0(
    "data/spatial/abs/MB_2016_",
    state_short,
    ".shp"
  ) %>%
    st_read(
      stringsAsFactors = FALSE
    ) %>%
    left_join(mesh_pop)
  
  # get LGAs in VIC, join with mesh blocks, and sum populations
  st_read("data/spatial/abs/LGA_2016_AUST.shp",
          stringsAsFactors = FALSE) %>%
    filter(STE_NAME16 == state) %>%
    select(lga_code = LGA_CODE16,
           lga = LGA_NAME16,
           area = AREASQKM16) %>%
    st_join(state_mesh) %>%
    group_by(lga_code, lga, area) %>%
    summarise(pop = sum(Person)) %>%
    filter(area > 0) %>%
    mutate(
      pop_dens = pop / area
    ) %>%
    saveRDS(filepath)
  
}

# dplyr introduced a breaking change whereby the order of entries returned by a
# right_join was changed from the order of y to the order of x. This may work to
# reverse it.
old_right_join <- function(x, y, ...) {
  left_join(y, x, ...)
}

# get the survey wave number from a raw survey filename
wave_from_file <- function(filename, previous_waves = 14) {
  
  files <- list.files("data/survey_raw/", pattern = ".csv$", full.names = TRUE)
  lengths <- nchar(files)
  dates <- files %>%
    substr(lengths - 9, lengths - 4) %>%
    as.Date("%d%m%y")
  files <- files[order(dates)]
  waves <- seq_along(files) + previous_waves
  idx <- match(
    basename(filename),
    basename(files)
  )
  
  waves[idx]
  
}

# which household types correspond to the respondent being a parent (guessing at
# BETA's 'parent' field)
parenty_households <- c(
  "Couple with non-dependent child(ren)",
  "Couple with dependent child(ren)",
  "Couple with dependent and non-dependent children",
  "Single parent with non-dependent child(ren)",
  "Single parent with dependent child(ren)",
  "Single parent with dependent and non-dependent children"
)

parse_all_doh_surveys <- function(dir = "data/survey_raw") {
  
  dir %>%
    list.files(
      pattern = ".csv$",
      full.names = TRUE
    ) %>%
    lapply(parse_doh_survey) %>%
    bind_rows() %>%
    remove_doh_duplicates()
  
}

# read in and parse raw DoH survey files in a similar way to BETA
parse_doh_survey <- function(filename) {
  full <- filename %>%
    read_csv(
      col_types = cols(
        .default = col_character(),
        S1 = col_double(),
        Q138 = col_double(),
        Q166_1 = col_double(),
        Q166_4 = col_double(),
        Q167_1 = col_double(),
        Q168_2 = col_double(),
        Q169_1 = col_double()
      )
    ) %>%
    mutate(
      wave = wave_from_file(filename),
      state = abbreviate_states(S3),
      city = Location == "Major city",
      parent = Q39 %in% parenty_households,
      distancing_any = NA,
      cough_any = NA,
      mask = NA,
      survey = "doh",
      date = as.Date(StartDate, format = "%Y%m%d"),
    )
  
  # add face covering data if it is there
  if ("Q222" %in% names(full)) {
    full <- full %>%
      mutate(face_covering = Q222)
  } else {
    full <- full %>%
      mutate(face_covering = NA)
  }
  
  full %>%
    select(
      wave,
      state,
      gender = S2,
      age = S1,
      vulnerable = Q75,
      age_groups = AgeBracket,
      city,
      location = Location,
      postcode = Q37,
      household = Q39,
      income = Q42,
      parent,
      employment = Q38,
      phys_contact = Q109,
      phys_distance = Q65,
      wash_hands = Q110,
      cough_any,
      cough = Q111,
      mask,
      face_covering,
      contact_num = Q138,
      contacts_under18 = Q166_1,
      contacts_18to39 = Q166_2,
      contacts_40to59 = Q166_3,
      contacts_60plus = Q166_4,
      contacts_ageunsure = Q166_5,
      contacts_phys = Q167_1,
      contacts_close = Q167_2,
      contacts_notclose = Q167_3,
      contacts_physdontknow = Q167_4,
      contacts_home = Q168_1,
      contacts_work = Q168_2,
      contacts_worship = Q168_3,
      contacts_school = Q168_4,
      contacts_shop = Q168_5,
      contacts_cafe = Q168_6,
      contacts_sport = Q168_7,
      contacts_public = Q168_8,
      contacts_other = Q168_9,
      contacts_cantremember = Q168_10,
      contacts_5min = Q169_1,
      contacts_5to30min = Q169_2,
      contacts_30to90min = Q169_3,
      contacts_90minto3hrs = Q169_4,
      contacts_3hrsplus = Q169_5,
      contacts_timedontknow = Q169_6,
      date,
      survey
    ) %>%
    mutate_at(
      vars(vulnerable, phys_contact),
      ~yesno_to_logical(.)
    ) %>%
    mutate_at(
      vars(starts_with("contacts_")),
      ~as.numeric(.)
    )
  
}

# plot apparent duplication of records in doh surveys by wave and state to spot
# bot entries
plot_age_duplication <- function(doh_surveys, max_fraction = 0.12) {
  doh_surveys %>%
    # remove any state with fewer than 50 respondents per week, on average
    group_by(state, wave) %>%
    mutate(respondents = n()) %>%
    group_by(state) %>%
    mutate(mean_respondents = mean(respondents)) %>%
    filter(mean_respondents > 50) %>%
    # count fraction of respondents by age in each wave/state
    group_by(wave, state, age) %>%
    count() %>%
    group_by(state, wave) %>%
    mutate(
      fraction = n / sum(n)
    ) %>%
    ungroup() %>%
    select(-n) %>%
    complete(
      wave, state, age,
      fill = list(fraction = 0)
    ) %>%
    ggplot() +
    aes(state, age, fill = fraction) +
    geom_tile() +
    facet_wrap(~wave) +
    scale_fill_viridis_c(
      na.value = grey(0.6),
      limits = c(0, max_fraction)
    ) +
    theme_minimal()
}

# remove duplicated data in DoH surveys caused by bots
remove_doh_duplicates <- function(doh_surveys) {
  
  # find clusters of more than responses of the same age gender and postcode in a given wave
  duplicates <-
    doh_surveys %>%
    filter(!is.na(postcode) & postcode != -99 & wave >= 22) %>%
    group_by(wave, age, gender, postcode) %>%
    summarise(count = n()) %>%
    filter(count > 3) %>%
    arrange(wave, postcode, gender, age)
  
  # print out the detected duplicates
  message("duplicates detected:")
  print(duplicates, n = Inf)
  
  # return the data with them removed
  doh_surveys %>%
    anti_join(duplicates)
  
}

# read in and parse a uom survey in a similar way to the Barometer data
parse_uom_survey <- function(filename, wave = NA) {
  
  major_cities <- c(
    "Sydney",
    "Melbourne",
    "Brisbane",
    "Perth",
    "Adelaide",
    "Hobart",
    "ACT"
  )
  
  # read the file and skip the second row (full questions)
  read_xlsx(filename, col_types = "text")[-1, ] %>%
    map_df(~parse_guess(.)) %>%
    mutate(
      wave = wave,
      state = abbreviate_states(state),
      city = location %in% major_cities,
      parent = case_when(
        parent_yn_1 == "Yes" | parent_yn_2 == "Yes" ~ TRUE,
        parent_yn_3 == "Yes" ~ FALSE,
        TRUE ~ NA
      ),
      ATSI = case_when(
        Q2 == "No" ~ FALSE,
        Q2 == "Prefer not to say" ~ NA,
        TRUE ~ TRUE
      ),
      poor_health = Q3 == "poor",
      vulnerable = age > 70 | ATSI | poor_health,
      age_groups = case_when(
        age >= 75 ~ "75+",
        age > 64 ~ "65-74",
        age > 54 ~ "55-64",
        age > 44 ~ "45-54",
        age > 34 ~ "35-44",
        age > 24 ~ "25-34",
        age >= 18 ~ "18-24",
        TRUE ~ "NA"
      ),
      location = ifelse(city, "Major city", "Regional"),
      postcode = as.character(postcode),
      household = NA,
      income = NA,
      employment = NA,
      phys_contact = NA,
      phys_distance = NA,
      distancing_any = yesno_to_logical(Q9_7),
      wash_hands = NA,
      cough_any = yesno_to_logical(Q9_2),
      cough = NA,
      mask = yesno_to_logical(Q9_20),
      face_covering = NA,
      contacts_under18 = NA,
      contacts_18to39 = NA,
      contacts_40to59 = NA,
      contacts_60plus = NA,
      contacts_ageunsure = NA,
      contacts_phys = NA,
      contacts_close = NA,
      contacts_notclose = NA,
      contacts_physdontknow = NA,
      contacts_home = NA,
      contacts_work = NA,
      contacts_worship = NA,
      contacts_school = NA,
      contacts_shop = NA,
      contacts_cafe = NA,
      contacts_sport = NA,
      contacts_public = NA,
      contacts_other = NA,
      contacts_cantremember = NA,
      contacts_5min = NA,
      contacts_5to30min = NA,
      contacts_30to90min = NA,
      contacts_90minto3hrs = NA,
      contacts_3hrsplus = NA,
      contacts_timedontknow = NA,
      date = as.Date(`Date of`, origin = "1900-01-01"),
      survey = "uom"
    ) %>%
    select(
      wave,
      state,
      gender = Q1,
      vulnerable,
      age_groups,
      city,
      location,
      postcode,
      household,
      income,
      parent,
      employment,
      phys_contact,
      distancing_any,
      phys_distance,
      wash_hands,
      cough_any,
      cough,
      mask,
      face_covering,
      contact_num = Q11,
      contacts_under18,
      contacts_18to39,
      contacts_40to59,
      contacts_60plus,
      contacts_ageunsure,
      contacts_phys,
      contacts_close,
      contacts_notclose,
      contacts_physdontknow,
      contacts_home,
      contacts_work,
      contacts_worship,
      contacts_school,
      contacts_shop,
      contacts_cafe,
      contacts_sport,
      contacts_public,
      contacts_other,
      contacts_cantremember,
      contacts_5min,
      contacts_5to30min,
      contacts_30to90min,
      contacts_90minto3hrs,
      contacts_3hrsplus,
      contacts_timedontknow,
      date,
      survey
    )
}

yesno_to_logical <- function(x) {
  case_when(
    x == "Yes" ~ TRUE,
    x == "No" ~ FALSE,
    TRUE ~ NA
  )
}

# read in and parse the BETA barometer data dump
parse_barometer <- function(filename = "data/barometer_all/Barometer full contact_dates.csv") {
  df <- read_csv(
    filename,
    col_types = cols(
      .default = col_double(),
      postcode = col_character(),
      state = col_character(),
      gender = col_character(),
      vulnerable = col_character(),
      age_groups_pd = col_character(),
      city = col_character(),
      location = col_character(),
      household = col_character(),
      income = col_character(),
      parent = col_character(),
      employment = col_character(),
      phys_contact = col_character(),
      phys_distance = col_character(),
      wash_hands = col_character(),
      cough = col_character(),
      wave_date = col_date(format = ""),
      date = col_date(format = "%Y%m%d")
    )
  ) %>%
    # filter out leading numbers in responses
    mutate_at(
      vars(income, phys_distance, wash_hands, cough),
      ~substr(., 3, 100)
    ) %>%
    mutate_at(
      vars(vulnerable, city, phys_contact, parent),
      ~yesno_to_logical(.)
    ) %>%
    mutate_at(
      vars(starts_with("contacts_")),
      ~ifelse(. == -66, NA, .)
    ) %>%
    mutate(
      survey = "barometer",
      state = abbreviate_states(state),
      cough_any = NA,
      mask = NA,
      face_covering = NA,
      distancing_any = NA
    ) %>%
    rename(
      wave = barometer_week
    ) %>%
    select(-age_groups_pd)
  
  # fill in missing dates withthe wave date - the only information we have
  missing_date <- is.na(df$date)
  df$date[missing_date] <- df$wave_date[missing_date]
  df %>%
    select(-wave_date)
  
}

parse_all_uom_surveys <- function(dir = "~/not_synced/uom_surveys/unlocked") {
  
  dir %>%
    file.path(
      c(
        "SPSS Updated Data Covid19 Attitudes and Practices labels April2020.xlsx",
        "SPSS Wave 2 Data Covid19 Attitudes and Practices labels May2020.xlsx"
      )
    ) %>%
    mapply(
      parse_uom_survey,
      .,
      wave = c(-1, 0),
      SIMPLIFY = FALSE
    ) %>%
    bind_rows()
  
}

is_weekend <- function(date) {
  lubridate::wday(date, label = TRUE) %in% c("Sat", "Sun")
}

# get the expected fraction of the previous 24h that was on a weekend
weekend_weight <- function(date) {
  weekend_today <- is_weekend(date)
  weekend_yesterday <- is_weekend(date - 1)
  (weekend_today + weekend_yesterday) / 2
}

# read in and parse all the respondent-level survey data
parse_all_surveys <- function() {
  bind_rows(
    parse_all_uom_surveys(),
    parse_barometer(),
    parse_all_doh_surveys()
  ) %>%
    mutate(
      weekend_fraction = weekend_weight(date)
    ) %>%
    group_by(wave) %>%
    mutate(
      wave_date = median(date),
      wave_duration = as.numeric(max(date) - min(date))
    ) %>%
    ungroup()
}

# the vector of dates to use for each scenario
scenario_dates <- function(scenario) {
  
  switch(
    scenario$phase,
    importation = seq(as.Date("2020-03-01"),
                      as.Date("2020-04-30"),
                      by = 1),
    suppression = seq(as.Date("2020-05-01"),
                      as.Date("2020-06-30"),
                      by = 1),
    community = seq(as.Date("2020-07-01"),
                    as.Date("2020-08-31"),
                    by = 1)
  )
  
}

# reff C1 locals (except surveillance effect) with macro/microdistancing either
# at optimal effect (TRUE) or turned off (FALSE)
reff_distancing <- function(fitted_model, macro_effect = TRUE, micro_effect = TRUE) {
  
  de <- fitted_model$greta_arrays$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  
  # duration in the household
  HD <- de$HD_0
  
  # non-household contacts
  OC <- de$OC_0
  
  # reduction in transmission probability due to hygiene measures
  gamma <- 1
  
  if (macro_effect) {
    
    macro_trends <- trends_date_state(
      "outputs/macrodistancing_trends.RDS",
      fitted_model$data$dates$infection
    )
    
    # optimal date and state for reduction in contacts
    macro_optimum <- which(macro_trends == min(macro_trends), arr.ind = TRUE)[1, , drop = FALSE]
    
    # duration in the household increases at optimum
    h_optimal <- h_t_state(fitted_model$data$dates$infection)[macro_optimum]
    HD <- HD * h_optimal
    
    # number of non-household contacts decreases at optimum
    OC <- macro_trends[macro_optimum]
  }
  
  if (micro_effect) {
    
    # optimal date and state for reduction in contacts
    micro_trends <- trends_date_state(
      "outputs/microdistancing_trends.RDS",
      fitted_model$data$dates$infection
    )
    micro_optimum <- which(micro_trends == max(micro_trends), arr.ind = TRUE)[1, , drop = FALSE]
    
    # transmission probability is reduced at optimum
    gamma <- de$gamma_t_state[micro_optimum]
    
  }
  
  household_infections <- de$HC_0 * (1 - de$p ^ HD)
  non_household_infections <- OC * infectious_days *
    (1 - de$p ^ de$OD_0) * gamma
  
  household_infections + non_household_infections
  
}

# scalar reffs for locally- and overseas-acquired cases under different policy scenarios
counterfactual_reffs <- function(scenario, fitted_model) {
  
  # baseline reff due to household/non-household model
  baseline_local_reff <- reff_distancing(
    fitted_model,
    macro_effect = scenario$mobility_restrictions,
    micro_effect = scenario$physical_distancing
  )
  
  # reduction in reff due to contact tracing
  contact_tracing_effect <- switch(
    scenario$contact_tracing,
    none = 1,
    suboptimal = surveillance_effect(as.Date("2020-01-01"), gi_cdf),
    optimal = surveillance_effect(as.Date("2020-09-01"), gi_cdf)
  )
  
  # overall reff for locally-acquired cases
  local_reff <- baseline_local_reff * contact_tracing_effect
  
  # overall reff for overseas-acquired cases  
  import_reff <- local_reff
  if (scenario$overseas_quarantine) {
    log_q3 <- fitted_model$greta_arrays$log_q[3]
    import_reff <- exp(log_q3)
  }
  
  # return these scalar greta arrays
  module(local_reff, import_reff)
  
}

# things to do for each:
# 1. compute Reff C1s under each scenario

# 2. project case counts under Reff trajectory
#  - function to create greta array for locally-acquired case trajectories based on two reff trajectories, imported case counts, and initial local case counts
#  - function to set up imported case counts and initial case counts for each phase
#  - wrapper function to take in scenario config, do calculation of posteriors, and save outputs 


# the vectors of case counts to use for each scenario
scenario_cases <- function(scenario, fitted_model) {
  
  all_dates <- fitted_model$data$dates$infection
  all_imported <- rowSums(fitted_model$data$imported$cases)
  all_local <- rowSums(fitted_model$data$local$cases)
  
  scenario_dates <- scenario_dates(scenario)
  scenario_start <- min(scenario_dates)
  n_scenario_dates <- length(scenario_dates)
  
  during <- all_dates %in% scenario_dates
  before <- all_dates < scenario_start & all_dates >= (scenario_start - 21)
  
  list(
    local_cases = c(all_local[before], rep(0, n_scenario_dates)),
    imported_cases = all_imported[before | during],
    dates = all_dates[before | during],
    simulation_start = scenario_start,
    n_dates = n_scenario_dates
  )  
  
}

# given a fitted reff model and a scenario, simulate Reffs and the numbers of
# locally-acquired cases nationally
simulate_scenario <- function (index, scenarios, fitted_model, nsim = 5000) {
  
  scenario <- scenarios[index, ]
  
  reffs <- counterfactual_reffs(scenario, fitted_model)
  case_data <- scenario_cases(scenario, fitted_model)
  one <- ones(length(case_data$local_cases))
  
  # need to include 3 weeks of pre-simulation local and imported cases, so pad
  # dates. pass in a single column matrix of these things to do national simulations
  simulation <- forecast_locals(
    local_cases = as.matrix(case_data$local_cases),
    imported_cases = as.matrix(case_data$imported_cases),
    Reff_locals = reffs$local_reff * one,
    Reff_imports = reffs$import_reff * one,
    dates = case_data$dates,
    gi_cdf = gi_cdf,
    simulation_start = case_data$simulation_start
  )
  
  keep <- case_data$dates >= case_data$simulation_start
  local_cases <- simulation$local_cases[keep]
  imported_cases <- case_data$imported_cases[keep]
  dates <- case_data$dates[keep]
  
  reff_local <- reffs$local_reff
  reff_imported <- reffs$import_reff
  
  # get posterior samples
  sims <- calculate(
    local_cases, reff_local, reff_imported,
    values = fitted_model$draws,
    nsim = nsim
  )
  
  # handle the outputting
  local_cases <- tibble(
    sim = rep(seq_len(nsim), each = length(dates)),
    date = rep(dates, nsim),
    cases = as.vector(t(sims$local_cases[, , 1]))
  )
  
  reffs <- tibble(
    sim = seq_len(nsim),
    local = as.vector(sims$reff_local),
    imported = as.vector(sims$reff_imported),
  )
  
  # save these samples
  module(scenario, local_cases, reffs) %>%
    saveRDS(paste0("outputs/counterfactuals/scenario", index, ".RDS"))
  
}


mean.ecdf <- function(x, ...) {
  mean(evalq(rep.int(x, diff(c(0, round(nobs * y)))), environment(x)), ...)
}

count_in_window <- function(target_date, states, delay_data, window, date_tabulation) {
  dates <- delay_data %>%
    filter(state %in% states) %>%
    pull(!!date_tabulation)
  diff <- abs(dates - target_date)
  in_window <- diff <= window
  sum(in_window)
}

get_window_size <- function(
  target_date,
  states,
  delay_data,
  date_tabulation,
  n_min = 500,
  window_min = 7,
  window_max = 42
) {
  
  dates <- delay_data %>%
    filter(state %in% states) %>%
    pull(!!date_tabulation)
  
  # find the smallest window that yields the required number of counts
  for (window in window_min:window_max) {
    
    diff <- abs(dates - target_date)
    in_window <- diff <= window
    
    if (sum(in_window) >= n_min) {
      break()
    }
    
  }
  
  window
  
}

delay_ecdf <- function(target_date, states, window, delay_data, date_tabulation) {
  
  data <- delay_data %>%
    filter(state %in% states)
  dates <- pull(data, !!date_tabulation)
  delays <- pull(data, delay)
  
  diff <- abs(dates - target_date)
  in_window <- diff <= window
  
  valid_delays <- delays[in_window]
  
  if (length(valid_delays) > 0) {
    distribution <- ecdf(valid_delays)
  } else {
    distribution <- NULL
  }
  
  list(distribution)
  
}

ci_ribbon <- function(ci) {
  
  lo <- paste0("ci_", ci, "_lo")
  hi <- paste0("ci_", ci, "_hi")
  
  geom_ribbon(
    aes_string(ymin = lo,
               ymax = hi),
    alpha = 1/9
  )
}

# calculate a weighted average ecdf out of two (weight is the probability of the first)
weight_ecdf <- function(ecdf_1, ecdf_2, weight) {
  
  if (is.null(ecdf_1) | weight == 0) {
    return(ecdf_2)
  }
  if (is.null(ecdf_2) | weight == 1) {
    return(ecdf_1)
  }
  
  e1 <- environment(ecdf_1)
  e2 <- environment(ecdf_2)
  
  # reconcile the xs
  x_1 <- e1$x
  x_2 <- e2$x
  
  x <- sort(unique(c(x_1, x_2)))
  
  # get the two CDFs
  y_1 <- ecdf_1(x)
  y_2 <- ecdf_2(x)
  
  # get the two pdfs
  pdf_1 <- diff(c(0, y_1))
  pdf_2 <- diff(c(0, y_2))
  
  # get a weighted average of them
  pdf <- pdf_1 * weight + pdf_2 * (1 - weight)
  
  # convert back to a CDF
  y <- cumsum(pdf)
  
  # rebuild an ecdf object, the slow way
  method <- 2L
  yleft <- 0
  yright <- 1
  f <- e1$f
  n <- e1$nobs
  rval <- function (v) {
    stats:::.approxfun(x, y, v, method, yleft, yright, f)
  }
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- attr(ecdf_1, "call")
  rval
  
}

# convert a vector fo cumulative probabilities into an ecdf object
make_ecdf <- function(y, x) {
  
  sims <- sample(x,
                 100,
                 prob = y,
                 replace = TRUE)
  
  ecdf_null <- ecdf(sims)
  envir <- environment(ecdf_null)
  
  # rebuild an ecdf object, the slow way
  method <- 2L
  yleft <- 0
  yright <- 1
  f <- envir$f
  n <- envir$nobs
  rval <- function (v) {
    stats:::.approxfun(x, y, v, method, yleft, yright, f)
  }
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- attr(ecdf_null, "call")
  rval  
}




get_cis <- function(date, state, ecdf, weight, use_national) {
  
  deciles_lower <- seq(0.05, 0.45, by = 0.05)
  deciles_upper <- 1 - deciles_lower
  deciles <- c(deciles_lower, deciles_upper)
  decile_names <- paste0("ci_", (1 - 2 * deciles_lower) * 100)
  decile_names <- c(paste0(decile_names, "_lo"),
                    paste0(decile_names, "_hi"))
  
  cis <- quantile(ecdf, deciles)
  names(cis) <- decile_names
  cis
}

# Calculate time-varying delay distributions for each state, smoothly reverting
# to the national average when and where there are insufficient records to
# compute state-level distributions. Return a tibble of empirical CDFs of the
# distribution by state and date.
# Parameters:

# state, date, delay: vectors of equal length giving the data on observed delays
#   by date and state
# all_dates, all_states: optional vectors of dates and states for which to compute
#   delays (taken from linelist if not specified)
# direction: whether to tabulate delays by date in a 'forward' ('date' is at the
#   start of the delay) or 'backward' ('date' is at the end of the delay) manner
# delay_plausible_bounds: a vector of length 2 of plausible delays. Records with
#   values outside these bounds are assumed to be erroneous and removed.
# min_records: the minimum number of records required to reliably estimate the delay
#   distribution within a window
# absolute_min_records: the absolute minimum number of records to estimate a
#   state-level delay distribution within a window. If the number of records is
#   below this (even with the maximum window size), the national estimate is used
#   instead. If it is between this and 'min_records', the distribution is
#   estimated as a weighted average of the state and national distributions.
# min_window: the minimum window size (number of days wide) in which to estimate
#   the delay distribution
# max_window: the maximum window size in which to estimate the delay
#   distribution for each date at the state level. At the national level there is
#   no maximum applied
# national_exclusions: a tibble of states, start dates, and end dates denoting
#   times and places that should not contribute to the national estimate. If
#   either of the dates are NA, the earliest (or latest) dates in the linelist
#   are used
estimate_delays <- function(
  state,
  date,
  delay,
  all_dates = NULL,
  all_states = NULL,
  direction = c("forward", "backward"),
  min_records = 500,
  absolute_min_records = 100,
  min_window = 7,
  max_window = 56,
  national_exclusions = tibble(
    state = "VIC",
    start = as.Date("2020-06-14"),
    end   = as.Date("2020-12-01")
  ),
  revert_to_national = TRUE
) {
  
  direction <- match.arg(direction)
  
  # account for right-truncation when tabulating
  # which date to tabulate by
  if (direction == "forward") {
    date_from <- date
    date_to <- date + delay
    date_tabulation <- "date_from"
  } else {
    date_from <- date - delay
    date_to <- date
    date_tabulation <- "date_to"
  }
  
  delay_data <- tibble(
    state = state,
    date_from = date_from,
    date_to = date_to,
    delay = delay
  )
  
  if (is.null(all_dates)) {
    all_dates <- seq(
      min(delay_data$date_from),
      max(delay_data$date_to),
      by = 1
    )
  }
  
  if (is.null(all_states)) {
    all_states <- unique(delay_data$state)
  }
  
  date_state <- expand_grid(
    date = all_dates,
    state = all_states
  )
  
  # get the half-window size (number of days on either side of the target)
  absolute_max_window <- as.numeric(diff(range(all_dates)))
  min_window <- ceiling((min_window - 1) / 2)
  max_window <- floor((max_window - 1) / 2)
  
  # for each confirmation date, run the algorithm on each date
  statewide <- date_state %>%
    group_by(date, state) %>%
    mutate(
      window = get_window_size(
        date,
        state,
        delay_data = delay_data,
        date_tabulation = date_tabulation,
        n_min = min_records,
        window_min = min_window,
        window_max = max_window 
      ),
      count = count_in_window(
        date,
        state,
        delay_data = delay_data,
        date_tabulation = date_tabulation,
        window = window
      ),
      state_ecdf = delay_ecdf(
        date,
        state,
        window = window,
        delay_data = delay_data,
        date_tabulation = date_tabulation
      )
    )
  
  if (revert_to_national) {
    
    
    if(!is.null(national_exclusions)){
      # fill in exclusion periods
      national_exclusions <- national_exclusions %>%
        mutate(
          start = as.Date(start),
          end = as.Date(end),
          start = replace_na(start, min(all_dates)),
          end = replace_na(end, max(all_dates))
        )
      
      # remove the specified data for estimating the national background distribution
      for (i in seq_len(nrow(national_exclusions))) {
        delay_data <- delay_data %>%
          filter(
            !(
              state == national_exclusions$state[i] &
                date_from >= national_exclusions$start[i] &
                date_to <= national_exclusions$end[i]
            )
          )
      }
    }
    
    nationwide <- date_state %>%
      # arbitrarily pick one set of dates
      filter(state == "ACT") %>%
      select(-state) %>%
      group_by(date) %>%
      mutate(
        window = get_window_size(
          date,
          all_states,
          delay_data = delay_data,
          date_tabulation = date_tabulation,
          n_min = min_records,
          window_min = min_window,
          window_max = absolute_max_window
        ),
        national_ecdf = delay_ecdf(
          date,
          all_states,
          window = window,
          delay_data = delay_data,
          date_tabulation = date_tabulation
        )
      )
    
    # for statewide, replace any invalid ecdfs with the national one
    state_ecdfs <- statewide %>%
      right_join(
        nationwide %>%
          select(-window)
      ) %>%
      mutate(
        use_national = count < absolute_min_records,
        weight = pmin(1, count / min_records),
        weight = ifelse(use_national, 0, weight),
        ecdf = mapply(
          FUN = weight_ecdf,
          state_ecdf,
          national_ecdf,
          weight,
          SIMPLIFY = FALSE
        )
      ) %>%
      select(
        date, state, ecdf, weight, use_national
      )
    
  } else {
    
    state_ecdfs <- statewide %>%
      mutate(
        use_national = count < absolute_min_records,
        weight = pmin(1, count / min_records),
        weight = ifelse(use_national, 0, weight),
        ecdf = state_ecdf
      ) %>%
      select(
        date, state, ecdf, weight, use_national
      )
    
  }
  
  state_ecdfs
  
}

# plot changing delay distributions by state over time
plot_delays <- function(
  delay_distributions,
  date,
  state,
  delay,
  ylim = c(0, 20),
  hline_at = 0,
  intervention_at = interventions(), 
  base_colour = yellow
) {
  
  
  # mutate to output quantiles and then plot them
  quantiles <- delay_distributions %>%
    # plot it as the date of infection, not date of onset!
    mutate(date = date - 5) %>%
    pmap_dfr(get_cis) %>%
    bind_cols(delay_distributions, .) %>%
    mutate(
      median = vapply(
        ecdf,
        quantile,
        0.5,
        FUN.VALUE = numeric(1)
      ),
      mean = vapply(
        ecdf,
        mean,
        FUN.VALUE = numeric(1)
      )
    )
  
  # observed delays
  df_obs <- tibble(
    date = date,
    state = state,
    delay = delay,
    type = "Nowcast"
  )
  
  p <- quantiles %>%
    mutate(type = "Nowcast") %>%
    ggplot() + 
    
    aes(date, mean, fill = type) +
    
    facet_wrap(~state, ncol = 2) +
    
    xlab(element_blank()) +
    
    coord_cartesian(
      ylim = ylim,
      xlim = c(as.Date("2020-03-01"), max(delay_distributions$date))
    ) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_breaks = "2 month", date_labels = "%e/%m") +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    ci_ribbon("90") +
    ci_ribbon("80") +
    ci_ribbon("70") +
    ci_ribbon("60") +
    ci_ribbon("50") +
    ci_ribbon("40") +
    ci_ribbon("30") +
    ci_ribbon("20") +
    ci_ribbon("10") +
    
    geom_line(aes(y = ci_90_lo),
              colour = base_colour,
              alpha = 0.8) + 
    geom_line(aes(y = ci_90_hi),
              colour = base_colour,
              alpha = 0.8) + 
    
    geom_line(aes(y = mean),
              colour = grey(0.4),
              alpha = 1,
              size = 1) +
    
    # add shading for regions where the national distribution is used
    geom_ribbon(
      aes(ymin = -100, ymax = use_national * 100 - 10),
      fill = grey(1),
      alpha = 0.5,
      colour = grey(0.9),
      linetype = 3
    ) +
    
    # add horizontal and vertical lines for context
    geom_vline(
      aes(xintercept = date),
      data = intervention_at,
      colour = "grey80"
    ) +
    
    geom_hline(
      yintercept = hline_at,
      colour = "grey80"
    ) +
    
    # add points for true delays
    geom_point(
      aes(date, delay),
      data = df_obs,
      pch = 16,
      size = 0.2,
      alpha = 0.1
    ) +
    
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = 8)
    )
  
  p
  
}

gaussian_smooth <- function (values, sd = 1, ...) {
  id <- seq_along(values)
  middle <- round(mean(id))
  diff <- id - middle
  weights <- exp(-0.5 * (diff / sd) ^ 2)
  weights <- weights / sum(weights)
  weighted_mean(values, weights, ...)
}

# given a dataframe of mobility data subsetted to a particular mobility metric,
# fit a generalised additive model for the trend and return a dataframe with the
# modelled mobility trend for all dates between min_date and max_date
predict_mobility_trend <- function(
  mobility,
  min_date = min(mobility$date),
  max_date = max(mobility$date)
) {
  
  print(mobility$state[[1]])
  print(mobility$datastream[[1]])
  
  all_dates <- seq(min_date, max_date, by = 1)
  
  min_data_date = min(mobility$date)
  max_data_date = max(mobility$date)
  
  public_holidays <- holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    ) %>%
    rename(
      holiday = name
    )
  
  school_holidays <- school_holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    )
  
  # create intervention step-change covariates
  intervention_steps <- interventions(end_dates = TRUE) %>%
    # add events for SA and QLD ending short lockdowns, to enable effects to be
    # reversed
    # bind_rows(
    #   tibble(
    #     date = as.Date("2020-11-22"),
    #     state = "SA"
    #   ),
    #   tibble(
    #     date = as.Date("2021-01-12"),
    #     state = "QLD"
    #   ),
  #   tibble(
  #     date = as.Date("2021-02-05"),
  #     state = "WA"
  #   ),
  #   tibble(
  #     date = as.Date("2021-02-18"),
  #     state = "VIC"
  #   )
  # ) %>% # this code now superceded by end_dates = TRUE
  filter(date <= max_data_date) %>%
    mutate(
      intervention_id = paste0(
        "intervention_",
        match(date, unique(date))
      )
    ) %>%
    group_by(intervention_id, state) %>%
    do(
      tibble(
        date = all_dates,
        intervention_effect = as.numeric(all_dates >= .$date)
      )
    ) %>%
    group_by(state, date) %>%
    summarise(
      intervention_stage = sum(intervention_effect),
      .groups = "drop"
    ) %>%
    mutate(
      intervention_stage = factor(intervention_stage)
    )
  
  df <- mobility %>%
    left_join(
      public_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      school_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      intervention_steps,
      by = c("state", "date")
    ) %>%
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      is_a_school_holiday = !is.na(school_holiday),
      holiday = factor(holiday),
      date_num = as.numeric(date - min_date),
      dow = lubridate::wday(date, label = TRUE),
      dow = as.character(dow)
    ) %>%
    filter(!is.na(trend))
  
  library(mgcv)
  
  m <- gam(trend ~
             
             # smooth variations in mobility
             s(date_num, k = 50) +
             
             # step changes around intervention impositions
             intervention_stage +
             
             # random effect on holidays (different for each holiday, but shrunk
             # to an average holiday effect which used to predict into future)
             is_a_holiday +
             s(holiday, bs = "re") +
             
             # constant effect for school holidays
             is_a_school_holiday +
             
             # day of the week effect
             dow,
           
           select = TRUE,
           gamma = 2,
           data = df)
  
  # compute mean and standard deviation of Gaussian observation model for fitted data
  fit <- predict(m, se.fit = TRUE)
  fit$sd <- sqrt(var(residuals(m)) + fit$se.fit ^ 2)
  
  df_fitted <- df %>%
    # predict with fitted model (and get 90% CIs)
    mutate(
      fitted_trend = fit$fit,
      fitted_trend_upper = fit$fit + fit$sd * qnorm(0.025),
      fitted_trend_lower = fit$fit + fit$sd * qnorm(0.975),
    )
  
  # predict each date, *averaging over the weekday effect for each date*
  pred_df <- expand_grid(
    state_long = unique(df$state_long),
    dow = unique(df$dow),
    date = all_dates,
  ) %>%
    mutate(
      state = abbreviate_states(state_long)
    ) %>% 
    left_join(
      public_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      school_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      intervention_steps,
      by = c("state", "date")
    ) %>%
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      # remove any named holidays not in the training data
      holiday = case_when(
        holiday %in% unique(df$holiday) ~ holiday,
        TRUE ~ "none"
      ),
      holiday = factor(holiday),
      is_a_school_holiday = !is.na(school_holiday),
      date_num = as.numeric(date - min_date),
      # clamp the smooth part of the prediction at both ends
      date_num = pmax(date_num, min_data_date - min_date),
      date_num = pmin(date_num, max_data_date - min_date)
    )
  
  # predict trends under these conditions, and average over day of the week
  pred_df <- pred_df %>%
    mutate(
      predicted_trend = predict(m, newdata = pred_df)
    ) %>%
    group_by(
      state_long, state, date
    ) %>%
    summarise(
      predicted_trend = mean(predicted_trend),
      .groups = "drop"
    ) %>%
    group_by(
      state
    ) %>%
    # smooth fitted curve over days of the week and holidays
    mutate(
      predicted_trend = slider::slide_dbl(
        predicted_trend,
        gaussian_smooth,
        na.rm = TRUE,
        sd = 2.8,
        .before = 5,
        .after = 5
      )
    ) %>%
    ungroup() %>%
    left_join(
      df_fitted %>%
        select(
          state, state_long, date,
          trend,
          fitted_trend,
          fitted_trend_lower,
          fitted_trend_upper
        ),
      by = c("state", "state_long", "date")
    )
  
  pred_df
  
}

# modify Reff of a fitted model by multiplying by multiplicative effect, drawn from a distribution
multiply_reff <- function(fitted_model, prior_mean, prior_range, quantile = 0.95) {
  prob <- 1 - (1 - quantile) / 2
  prior_sd <- abs(diff(prior_range)) / (2 * qnorm(prob))
  effect <- normal(prior_mean, prior_sd, truncation = c(0, Inf))
  fitted_model$greta_arrays$R_eff_loc_12 <- fitted_model$greta_arrays$R_eff_loc_12 * effect 
  fitted_model$greta_arrays$R_eff_loc_1 <- fitted_model$greta_arrays$R_eff_loc_1 * effect 
  fitted_model
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
fifo <- "#A8EB12"

# default cdf
gi_cdf <- nishiura_cdf()


url_exists <- function(address) {  
  tryCatch(
    {  
      con <- url(address)  
      a  <- capture.output(suppressWarnings(readLines(con)))  
      close(con)  
      TRUE;  
    },  
    error = function(err) {  
      occur <- grep("cannot open the connection", capture.output(err));  
      if(length(occur) > 0) FALSE;  
    }  
  )  
}

download_tidycovid <- function() {
  f <- tempfile(fileext = ".RDS")
  download.file(tidycovid_url, destfile = f, method='wget')
  tmp <- readRDS(f)
  saveRDS(tmp, file = "data/google_cmr/tidycovid_cache.RDS")  
}


fit_survey_gam <- function(
  fit_dat,
  pred_dat
){
  
  respondents <- fit_dat$respondents
  count <- fit_dat$count
  date <- fit_dat$date
  intervention_stage <- fit_dat$intervention_stage
  
  date_num <- as.numeric(date - min(date))
  
  m <- mgcv::gam(
    cbind(count, I(respondents - count)) ~ s(date_num) + intervention_stage,
    select = TRUE,
    family = stats::binomial,
    optimizer = c("outer","optim")
  )
  
  
  pred_dat$date_num <- as.numeric(pred_dat$date - min(date))
  
  #pred <- predict(m, se.fit = TRUE, type = "link")
  pred <- predict(
    object = m,
    newdata = pred_dat,
    se.fit = TRUE,
    type = "link"
  )
  
  quantile95 <- qnorm(0.95)
  quantile75 <- qnorm(0.75)
  ci_90_hi <- pred$fit + (quantile95 * pred$se.fit)
  ci_90_lo <- pred$fit - (quantile95 * pred$se.fit)
  ci_50_hi <- pred$fit + (quantile75 * pred$se.fit)
  ci_50_lo <- pred$fit - (quantile75 * pred$se.fit)
  
  fitted <- m$family$linkinv(pred$fit) * pred_dat$distancing
  ci_90_hi <- m$family$linkinv(ci_90_hi) * pred_dat$distancing
  ci_90_lo <- m$family$linkinv(ci_90_lo) * pred_dat$distancing
  ci_50_hi <- m$family$linkinv(ci_50_hi) * pred_dat$distancing
  ci_50_lo <- m$family$linkinv(ci_50_lo) * pred_dat$distancing
  
  
  
  tibble(
    date = pred_dat$date,
    mean = fitted ,
    ci_90_lo,
    ci_50_lo,
    ci_50_hi,
    ci_90_hi
  )
  
}



get_ifr <- function(voc = TRUE) {
  
  # Age-structured infection fatality ratio (%) estimates from O'Driscoll et al.
  # 2020 https://doi.org/10.1038/s41586-020-2918-0 (Table S3 in supplement) and
  # Brazeau et al. 2020 (Imperial report 34)
  # https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-34-ifr/
  # (Table 2 - estimate *with* seroreversion)
  
  # Brazeau et al has separate estimates for ages 80-84, 85-89, 90+, so we
  # recombine these based on ABS 2020 population age fractions into the 80+
  # category (50% ofover 80s are in 80-84, 30% are in 85-89, and 20% are 90 or
  # older)
  
  ifr <- tibble::tribble(
    ~age,      ~odriscoll,     ~brazeau,
    "0-4",          0.003,         0.00,
    "5-9",          0.001,         0.01,
    "10-14",        0.001,         0.01,
    "15-19",        0.003,         0.02,
    "20-24",        0.006,         0.02,
    "25-29",        0.013,         0.04,
    "30-34",        0.024,         0.06,
    "35-39",        0.040,         0.09,
    "40-44",        0.075,         0.15,
    "45-49",        0.121,         0.23,
    "50-54",        0.207,         0.36,
    "55-59",        0.323,         0.57,
    "60-64",        0.456,         0.89,
    "65-69",        1.075,         1.39,
    "70-74",        1.674,         2.17,
    "75-79",        3.203,         3.39,
    "80+",          8.292,         5.3*0.5 + 8.28*0.3 + 16.19*0.2
  )
  
  
  if(voc){
    # multiplier from Davis et al.
    ifr <- ifr %>%
      mutate(
        odriscoll = odriscoll*1.61,
        brazeau = brazeau*1.61
      )
  }
  
  return(ifr)
  
}


scrape_doses_timeseries <- function() {
  
  url <- "https://covidlive.com.au/report/daily-vaccinations/aus"
  
  # scrape the cumulative number of doses and convert to daily new doses
  scraped <- url %>%
    read_html() %>%
    html_nodes(
      "table"
    ) %>%
    .[[2]] %>%
    html_table(
      fill = TRUE
    ) %>%
    mutate(
      date = as.Date(DATE, format = "%d %B %y"),
      cumulative_doses = DOSES,
      cumulative_doses = gsub(",", "", cumulative_doses),
      cumulative_doses = as.numeric(cumulative_doses)
    ) %>%
    # compute new doses per day
    arrange(
      date
    ) %>%
    mutate(
      new_doses = diff(c(0, cumulative_doses))
    ) %>%
    select(
      date,
      new_doses
    )
  
  # fill in zeroes for any missing datess
  missing_dates <- expand_grid(
    date = seq(
      min(scraped$date),
      max(scraped$date),
      by = 1
    ),
    new_doses = 0
  ) %>%
    anti_join(
      scraped,
      by = "date"
    )
  
  # combine, reorder, compute cumulative number, drop leading 0s
  scraped %>%
    bind_rows(missing_dates) %>%
    arrange(date) %>%
    mutate(
      doses = cumsum(new_doses)
    ) %>%
    filter(
      doses > 0
    )
  
}


get_R <- function (transition_matrix, stable_age = NULL, tolerance = 0.001, max_iter = 1000) {
  #Re(eigen(x)$value[1])
  # function from STEPS
  # https://github.com/steps-dev/steps/blob/74c5359dd4470c4056cd799c53ef56d503ba69da/R/growth_transition_functions-class.R#L211
  # compute R from a transition (next generation) matrix
  
  if (is.null(stable_age)) {
    stable_age <- rep(1, ncol(transition_matrix))
  }
  old_stages <- stable_age
  converged <- FALSE
  iter <- 0
  old_Rs <- rep(.Machine$double.eps, ncol(transition_matrix))
  
  while (!converged & iter < max_iter) {
    new_stages <- transition_matrix %*% old_stages
    Rs <- new_stages / old_stages
    errors <- abs(1 - (Rs / old_Rs))
    converged <- all(errors < tolerance)
    old_Rs <- Rs
    old_stages <- new_stages
    iter <- iter + 1
  }
  
  if (!converged) {
    warning(
      "estimation of growth rate did not converge in ",
      max_iter,
      " iterations"
    )
  }
  
  # return the intrinsic growth rate
  Rs[1]
  
}

find_m <- function(R_target, transition_matrix, stable_age = NULL) {
  # this function from STEPS
  #https://github.com/steps-dev/steps/blob/74c5359dd4470c4056cd799c53ef56d503ba69da/R/growth_transition_functions-class.R#L266
  #
  # compute the m that calibrates a next generation matrix to R0
  
  obj <- function (m, R_target, transition_matrix, stable_age = NULL) {
    new_transition_matrix <- m*transition_matrix
    R_current <- get_R(new_transition_matrix, stable_age = stable_age)
    (R_current - R_target) ^ 2
  } 
  
  out <- stats::optimise(f = obj,
                         interval = c(0, 1),
                         R_target,
                         transition_matrix,
                         stable_age)
  out$minimum
  
  return(out$minimum)
}


disaggregate <- function(population, mask) {
  # disaggregate a population across age groups, based on the age groups in that
  # population and the national age distribution
  
  masked_age_distribution <- age_distribution * mask
  disaggregation <- masked_age_distribution / sum(masked_age_distribution)
  population * disaggregation
}

disaggregation_vec <- function(mask, distribution) {
  masked_distribution <- mask * distribution
  masked_distribution / sum(masked_distribution)
}

get_unscaled_ngm <- function(contact_matrices, transmission_matrices) {
  
  next_generation_matrices <- mapply(
    FUN = `*`,
    contact_matrices,
    transmission_matrices,
    SIMPLIFY = FALSE
  )
  
  ngm_overall <- Reduce("+", next_generation_matrices)
  
}

get_australia_ngm_unscaled <- function(model, age_breaks) {
  
  # unscaled next generation matrix for all Australia
  
  transmission_matrices <- get_setting_transmission_matrices(
    age_breaks = age_breaks
  )
  
  abs_pop_age_lga_2020 %>%
    group_by(age_group) %>%
    summarise(
      population = sum(population)
    ) %>%
    mutate(
      lower.age.limit = readr::parse_number(as.character(age_group))
    ) %>%
    mutate(
      country = "Australia"
    ) %>%
    nest(
      population = -country
    ) %>%
    rowwise() %>%
    mutate(
      per_capita_household_size = get_per_capita_household_size(),
      setting_matrices = list(
        predict_setting_contacts(
          contact_model = model,
          population = population,
          per_capita_household_size = per_capita_household_size,
          age_breaks = age_breaks
        )
      ),
      contact_matrices = list(
        setting_matrices[c("home", "school", "work", "other")]
      ),
      ngm_unscaled = list(
        all = get_unscaled_ngm(
          contact_matrices = contact_matrices,
          transmission_matrices = transmission_matrices
        )
      )
    ) %>%
    pull(ngm_unscaled) %>%
    `[[`(1)
  
  
}

polymod_model <- function(){
  
  file <- "outputs/polymod_model.RDS"
  
  library(conmat)
  
  if(file.exists(file)){
    
    model <- readRDS(file = file)
    
  } else {
    
    print("The polymod model is not stored. Don't you fret my love, I'm fitting the model now, just chill.")
    
    model <- fit_setting_contacts(
      contact_data_list = get_polymod_setting_data(),
      population = get_polymod_population()
    )
    
    saveRDS(
      object = model,
      file = file
    )
    
  }
  
  return(model)
  
}

australia_ngm_unscaled <- function(age_breaks = NULL){
  
  file <- "outputs/aus_ngm_unscaled.RDS"
  
  library(conmat)
  
  if(file.exists(file)){
    
    ngm_unscaled <- readRDS(file = file)
    
  } else {
    
    print("The Australia Nex. Gen. Mat. is not stored. Don't you fret my love, I'm fetching it now, just chill.")
    
    library("conmat")
    
    model <- polymod_model()
    
    if(is.null(age_breaks)){
      age_breaks <- c(seq(0, 80, by = 5), Inf)
    }
    
    
    ngm_unscaled <- get_australia_ngm_unscaled(
      model = model,
      age_breaks = age_breaks
    )
    
    saveRDS(
      object = ngm_unscaled,
      file = file
    )
    
  }
  
  return(ngm_unscaled)
}

baseline_matrix <- function(R0 = 3, final_age_bin = 80, age_breaks = NULL) {
  # # construct a next generation matrix for Australia from Prem matrix
  # 
  # # Prem 2017 contact matrix
  # contact_matrix_raw <- readxl::read_xlsx(
  #   path = "data/vaccinatinon/MUestimates_all_locations_1.xlsx",
  #   sheet = "Australia",
  #   col_types = rep("numeric", 16)
  # ) %>%
  #   as.matrix
  # 
  # # expand out to add an 80+ category the same as the 75-80 category
  # contact_matrix <- matrix(NA, 17, 17)
  # contact_matrix[17, 17] <- contact_matrix_raw[16, 16]
  # contact_matrix[17, 1:16] <- contact_matrix_raw[16, ]
  # contact_matrix[1:16, 17] <- contact_matrix_raw[, 16]
  # contact_matrix[1:16, 1:16] <- contact_matrix_raw
  # 
  # # set names
  # bin_names <- age_classes(80)$classes
  # dimnames(contact_matrix) <- list(
  #   bin_names,
  #   bin_names
  # )
  # 
  # # relative infectiousness data from Trauer et al 2021
  # age_susceptability <- readr::read_csv(
  #   file = "data/vaccinatinon/trauer_2021_supp_table5.csv",
  #   col_names = c(
  #     "age_group",
  #     "clinical_fraction",
  #     "relative_susceptability",
  #     "infection_fatality_rate",
  #     "proportion_symtomatic_hospitalised"
  #   ),
  #   col_types = cols(
  #     age_group = col_character(),
  #     clinical_fraction = col_double(),
  #     relative_susceptability = col_double(),
  #     infection_fatality_rate = col_double(),
  #     proportion_symtomatic_hospitalised = col_double()
  #   ),
  #   skip = 1
  # ) %>%
  #   # duplicate the final row, and re-use for the 74-79 and 80+ classes
  #   add_row(
  #     .[16, ]
  #   ) %>%
  #   mutate(
  #     age_class = bin_names
  #   ) %>%
  #   dplyr::select(
  #     age_class,
  #     everything(),
  #     -age_group
  #   )
  # 
  # # calculate relative infectiousness - assume asymptomatics are 50% less
  # # infectious, and use age-stratified symptomaticity
  # relative_infectiousness <- age_susceptability$clinical_fraction*1 + 0.5*(1 - age_susceptability$clinical_fraction)
  # q <- relative_infectiousness
  # q_scaled <- q/max(q)
  # 
  # # apply the q scaling before computing m
  # contact_matrix_scaled <- sweep(contact_matrix, 2, q_scaled, FUN = "*")
  # 
  # # calculate m - number of onward infections per relative contact
  # m <- find_m(
  #   R_target = R0,
  #   transition_matrix = contact_matrix_scaled
  # )
  # 
  # contact_matrix_scaled * m
  
  ######
  
  # construct ngm for australia using conmat and davies estimates
  
  ngm_unscaled <- australia_ngm_unscaled(age_breaks)
  
  m <- find_m(
    R_target = R0,
    transition_matrix = ngm_unscaled
  )
  
  
  ngm_unscaled*m
  
}


age_classes <- function(final_age_bin = 80, by = 5) {
  
  # compute age classes based on this spec
  ages_lower = seq(0, final_age_bin, by = by)
  n_ages <- length(ages_lower)
  ages_upper = ages_lower + by - 1
  ages_upper[n_ages] <- Inf
  
  age_classes <- c(
    paste(
      ages_lower[-n_ages],
      ages_upper[-n_ages],
      sep = "-"
    ),
    paste0(
      final_age_bin,
      "+"
    )
  )
  
  tibble::tibble(
    classes = age_classes,
    lower = ages_lower,
    upper = ages_upper
  )
  
}

get_age_distribution <- function(
  final_age_bin = 85,
  by = 5,
  population_total = 25693000
) {
  
  # check the final age bin in sensible
  if (final_age_bin > 85) {
    stop(
      "No age-specific population data for ages greater than 85",
      call. = TRUE
    )
  }
  
  ages <- age_classes(
    final_age_bin = final_age_bin,
    by = by
  )
  
  # Age structure of the Australian population by year of age, up to 100+
  # This is a "standard" distribution data frame but old population size data
  # from 2001 hence is adjusted later
  # aust_population_standard <- readxl::read_xls(
  #   path = "data/vaccinatinon/abs_standard_age_31010DO003_200106.xls",
  #   sheet = "Table_1",
  #   skip = 6,
  #   col_names = c(
  #     "age",
  #     "pop"
  #   )
  # ) %>%
  #   filter(age != "Total", !is.na(age), age != "© Commonwealth of Australia 2013") %>%
  #   mutate(
  #     age = case_when(
  #       age == "100 and over" ~ "100",
  #       TRUE ~ age
  #     ) %>%
  #       as.integer,
  #   )
  
  # use 2020 population, as this better matches proportion 80+
  aust_population_2020 <- readxl::read_xls(
    path = "data/vaccinatinon/abs_population_2020.xls",
    sheet = "Table_8",
    range = cell_rows(c(223:328)),
    col_names = as.character(1:10)
  ) %>%
    select(1, 10) %>%
    rename(
      age = "1",
      pop = "10"
    ) %>%
    select(
      age,
      pop
    ) %>%
    mutate(
      age = case_when(
        age == "85-89" ~ "85",
        age == "90-94" ~ "85",
        age == "95-99" ~ "85",
        age == "100 and over" ~ "85",
        TRUE ~ age
      )
    ) %>%
    filter(
      !grepl("-", age)
    ) %>%
    group_by(
      age
    ) %>%
    summarise(
      pop = sum(pop),
      .groups = "drop"
    ) %>%
    mutate(
      age = as.integer(age)
    ) %>%
    arrange(age)
  
  # aggregate into age classes and return
  age_class_fractions <- aust_population_2020 %>%
    mutate(
      age_class = cut(
        age,
        breaks = c(ages$lower - 1, Inf),
        labels = ages$classes
      ),
      age_class = as.character(age_class),
      age_class = factor(age_class, levels= unique(age_class))
    ) %>%
    group_by(
      age_class
    ) %>%
    summarise(
      pop = sum(pop),
      .groups = "drop"
    ) %>%
    mutate(
      fraction = pop / sum(pop),
      pop = fraction * population_total
    )
  
  age_class_fractions
  
}


phase_age_populations <- function() {
  
  # allocation leads to over vaccination in some groups - try to fix this!
  # also cap coverage (of single doses) at 2?
  # disaggregation vectors for different vaccination groups
  
  
  # get the age classes and age distributions in national population
  ages <- age_classes(final_age_bin = 80)
  national_distribution <- get_age_distribution(final_age_bin = 80)$fraction
  
  # remove under-15s from all vaccination (vaccines not approved for these ages)
  distribution <- disaggregation_vec(ages$lower >= 15, national_distribution)
  
  # disaggregation vectors (summing to 1) into age classes for different
  # population groups
  under_50 <- disaggregation_vec(ages$lower < 50, distribution)
  over_50 <- disaggregation_vec(ages$lower >= 50, distribution)
  working_under_50 <- disaggregation_vec(ages$lower >= 16 & ages$lower < 50,
                                         distribution)
  working_over_50 <- disaggregation_vec(ages$lower >= 50 & ages$lower < 65,
                                        distribution)
  over_50_under_65 <- disaggregation_vec(ages$lower >= 50 & ages$lower < 65, distribution)
  over_50_under_70 <- disaggregation_vec(ages$lower >= 50 & ages$lower < 70, distribution)
  over_65 <- disaggregation_vec(ages$lower >= 65, distribution)
  over_70_under_80 <- disaggregation_vec(ages$lower >= 70 & ages$lower < 80, distribution)
  over_80 <- disaggregation_vec(ages$lower >= 80, distribution)
  
  # population sizes in vaccination roll-out groups
  populations_1A <- list(
    aged_care_residents = 183000 * over_65,
    disability_residents_u50 = 5000 * under_50,
    disability_residents_o50 = 21000 * over_50_under_65,
    border_workers_u50 = 16000 * working_under_50,
    border_workers_o50 = 11000 * working_over_50,
    care_staff_u50 = 143000 * working_under_50,
    care_staff_o50 = 107000 * working_over_50,
    health_staff_priority_u50 = 222000 * working_under_50,
    health_staff_priority_o50 = 90000 * working_over_50
  )
  
  populations_1B <- list(
    elderly_o80 = 915000 * over_80,
    elderly_70_79 = 1857000 * over_70_under_80,
    health_staff_other_u50 = 267000 * working_under_50,
    health_staff_other_o50 = 126000 * working_over_50,
    atsi_o50 = 91000 * over_50_under_70,
    medical_condition_u50 = 896000 * under_50,
    medical_condition_o50 = 1167000 * over_50_under_70,
    priority_workers_u50 = 201000 * working_under_50,
    priority_workers_o50 = 67000 * working_over_50
  )
  
  # combine these for each phase
  list(
    phase_1A = Reduce(`+`, populations_1A),
    phase_1B = Reduce(`+`, populations_1B)
  )
  
}



doses_by_age <- function(n_doses, age_populations) {
  
  # assuming a single dose per person, preferential allocation to group 1A
  # and then subsequent partial coverage in 1B, guesstimate the number of doses
  # given out in each age group coverage in each age group
  population_1A <- sum(age_populations$phase_1A)
  population_1B <- sum(age_populations$phase_1B)
  n_doses_1A <- min(n_doses, population_1A)
  age_doses_1A <- n_doses_1A * age_populations$phase_1A / population_1A
  n_doses_1B <- n_doses - n_doses_1A
  age_doses_1B <- n_doses_1B * age_populations$phase_1B / population_1B
  
  # combine into total doses so far
  age_doses_1A + age_doses_1B
  
}


combine_efficacy <- function(infection, transmission) {
  1 - ((1 - infection) * (1 - transmission)) 
}


# proportion_pf and proportion_2_dose are only used if the other proportion
# arguments are not specified
average_efficacy <- function(
  efficacy_pf_2_dose = 0.9685,
  efficacy_az_2_dose = 0.93,
  efficacy_pf_1_dose = 0.8317,
  efficacy_az_1_dose = 0.892,
  proportion_pf = 0.5,
  proportion_2_dose = 0.2,
  proportion_pf_2_dose = proportion_pf * proportion_2_dose,
  proportion_az_2_dose = (1 - proportion_pf) * proportion_2_dose,
  proportion_pf_1_dose = proportion_pf * (1 - proportion_2_dose),
  proportion_az_1_dose = (1 - proportion_pf) * (1 - proportion_2_dose)
) {
  
  # based on Pritchard MedRXiv / Harris via ATAGI advice paper
  # single dose calculations
  # AZ: 1 - (1 - 0.64) * (1 - 0.47) = 0.809
  # PF: 1 - (1 - 0.67) * (1 - 0.49) = 0.832
  
  # AZ: 1 - (1 - 0.8) * (1 - 0.65) = 0.93
  # Pf: 1 - (1 - 0.91) * (1 - 0.65) = 0.9685
  
  # compute the average efficacy of doses, given the proportion of vaccines from
  # each provider, and the proportion people who are vaccined with 2 doses - these
  # are estiamtes for b.1.1.7, from James Wood's email
  
  # proportion of each vaccine
  proportion_az <- 1 - proportion_pf
  
  # proportion fully dosed
  proportion_1_dose <- 1 - proportion_2_dose # may need edit when 
  
  efficacy_mean <- proportion_pf_2_dose * efficacy_pf_2_dose +
    proportion_pf_1_dose * efficacy_pf_1_dose +
    proportion_az_2_dose * efficacy_az_2_dose +
    proportion_az_1_dose * efficacy_az_1_dose 
  
  efficacy_mean 
  
}

average_ifr_efficacy <- function() {
  average_efficacy(
    efficacy_pf_2_dose = 0.95,
    efficacy_az_2_dose = 0.95,
    efficacy_pf_1_dose = 0.8,
    efficacy_az_1_dose = 0.8
  )
}

average_transmission_efficacy <- function() {
  average_efficacy(
    efficacy_pf_2_dose = 0.9685,
    efficacy_az_2_dose = 0.93,
  )
}


vaccination_transmission_effect <- function(
  age_coverage,
  efficacy_mean,
  next_generation_matrix
) {
  # given vaccination coverage in each age group, the average vaccine efficacy (by
  # age or overall), and the baseline next generation matrix, compute the
  # reduction in transmission for each age and overall
  
  age_transmission_reduction <- 1 - age_coverage * efficacy_mean
  vc_next_gen_matrix <- sweep(
    next_generation_matrix,
    2,
    age_transmission_reduction,
    FUN = "*"
  )
  
  overall <- get_R(vc_next_gen_matrix) / get_R(next_generation_matrix)
  
  list(
    by_age = age_transmission_reduction,
    overall = overall
  )
  
}

vaccination_ifr_effect <- function(
  age_coverage,
  efficacy_mean,
  ifr
) {
  
  age_structure <- get_age_distribution(80)
  
  # compute age-specific IFRs, post vaccination
  age_reduction <- 1 - (age_coverage * efficacy_mean)
  age_odriscoll <- ifr$odriscoll * age_reduction
  age_brazeau <- ifr$brazeau * age_reduction
  
  overall_odriscoll <- sum(age_structure$fraction * age_odriscoll)
  overall_brazeau <- sum(age_structure$fraction * age_brazeau)
  
  list(
    age_reduction = age_reduction,
    age = list(
      odriscoll = age_odriscoll,
      brazeau = age_brazeau
    ),
    overall = list(
      odriscoll = overall_odriscoll,
      brazeau = overall_brazeau
    )
  )
  
}


summarise_effect <- function(
  n_doses,
  age_populations,
  age_distribution,
  next_generation_matrix,
  ifr
) {
  
  # Given a number of doses, compute vaccine coverage in each age group, the
  # effect on reducing transmission among the whole population, and the efect on
  # reducing transmission in the most-vaccinated age group
  
  # compute coverage
  age_doses <- doses_by_age(n_doses, age_populations)
  age_coverage <- age_doses / age_distribution$pop
  
  # compute effect on transmission  
  transmission_effect <- vaccination_transmission_effect(
    age_coverage = age_coverage,
    efficacy_mean = average_transmission_efficacy(),
    next_generation_matrix = next_generation_matrix
  )
  
  # compute effect on IFR
  ifr_effect <- vaccination_ifr_effect(
    age_coverage = age_coverage,
    efficacy_mean = average_ifr_efficacy(),
    ifr = ifr
  )
  
  list(
    coverage_by_age = age_coverage,
    transmission = transmission_effect,
    ifr = ifr_effect
  )
  
}



overall_transmission_effect <- function (
  n_doses,
  age_populations,
  age_distribution,
  next_generation_matrix,
  ifr
) {
  all_effects <- summarise_effect(
    n_doses,
    age_populations,
    age_distribution,
    next_generation_matrix,
    ifr
  )
  all_effects$transmission$overall
}

overall_ifr_effect <- function (
  n_doses,
  age_populations,
  age_distribution,
  next_generation_matrix,
  ifr
) {
  all_effects <- summarise_effect(
    n_doses,
    age_populations,
    age_distribution,
    next_generation_matrix,
    ifr
  )
  all_effects$overall_ifr_reduction
}


extract_overall_transmission_effect <- function(x) {
  # extract results for the overall transmission effect from a list of outputs
  # from summarise_effect
  x$transmission$overall
}


extract_overall_ifr_effect <- function(x, which = c("odriscoll", "brazeau")) {
  # extract results for the population-wide IFR from a list of outputs from
  # summarise_effect
  which <- match.arg(which)
  x$ifr$overall[[which]]
}

extract_over_70_ifr_effect <- function(x, which = c("odriscoll", "brazeau")) {
  # extract age-specific IFRs after vaccination  from a list of outputs from
  # summarise_effect, then population weight them to represent the IFR for the
  # population over 70
  
  which <- match.arg(which)
  age_effects <- x$ifr$age[[which]]
  weighting <- get_age_distribution(80) %>%
    mutate(
      mask = case_when(
        age_class %in% c("70-74", "75-79", "80+") ~ 1,
        TRUE ~ 0
      ),
      weights = mask * fraction,
      weights = weights / sum(weights)
    ) %>%
    pull(weights)
  
  sum(age_effects * weighting)
}


load_air_data <- function(
  data_dir = "~/not_synced/vaccination/vaccination_data_with_booster/"
){
  
  do_dir <- file.path(data_dir, "dose_ordering") 
  un_dir <- file.path(data_dir, "unknowns")
  
  do_dates <- list.files(
    path = do_dir
  ) %>%
    sub(
      pattern = "DohertyTimeseriesExport_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "DoseOrdering_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "_.*",
      replacement = "",
      x = .
    ) %>%
    as.Date
  
  un_dates <- list.files(
    path = un_dir
  ) %>%
    sub(
      pattern = "DohertyTimeseriesExport_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "Unknowns_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "_.*",
      replacement = "",
      x = .
    ) %>%
    as.Date
  
  if(max(do_dates) != max(un_dates)){
    stop("Most recent dose ordering and unknowns files have different dates")
  }
  
  do_index <- which.max(do_dates)
  un_index <- which.max(un_dates)
  
  extraction_date <- do_dates[do_index]
  
  do_path <- list.files(
    path = do_dir,
    full.names = TRUE
  )[do_index]
  
  un_path <-list.files(
    path = un_dir,
    full.names = TRUE
  )[un_index]
  
  do_raw <- read_csv(
    file = do_path
  ) %>%
    rename(
      "state" = PATIENT_MEDICARE_STATE
    )
  
  un_raw <- read_csv(
    file = un_path
  ) %>%
    rename(
      "state" = PROVIDER_STATE
    )
  
  over_80 <- c(
    "80+",
    "80-84",
    "85+",
    "85-89",
    "90+",
    "90-94",
    "95+",
    "95-99",
    "100+"
  )
  
  df <- bind_rows(
    do_raw %>%
      filter(
        state != "Unknown",
        state != "UNK"
      ),
    un_raw
  ) %>%
    rename(
      "age_class" = CURRENT_AGE_GROUP,
      "week" = AS_OF_ENCOUNTER_WEEK,
      "date" = AS_OF_WEEK_COMMENCING,
      "dose1" = FIRST_DOSE,
      "dose2" = SECOND_DOSE,
      "dose3" = THIRD_DOSE,
      "count" = CUMULATIVE_UNIQUE_INDIVIDUALS_VACCINATED
    ) %>%
    select(-week) %>%
    mutate( #deal with the half week special case dates
      date = case_when(
        date == "2022-01-01" ~ as.Date("2021-12-27"),
        TRUE ~ as.Date(
          date,
          format = "%d/%m/%Y"
        )),
      
      age_class = case_when(
        age_class %in% over_80 ~ "80+",
        TRUE ~ age_class
      )
    )
  
  df_59 <- df %>%
    filter(age_class == "5-11") %>%
    mutate(
      count = 5/7 * count,
      age_class = "5-9"
    )
  
  df_1014 <- df %>%
    filter(age_class == "12-15" | age_class == "5-11") %>%
    mutate(
      count = case_when(
        age_class == "12-15" ~ 0.75 * count,
        TRUE ~ 2/7 * count
      ),
      age_class = "10-14"
    )
  
  df_1519 <- df %>%
    filter(age_class == "12-15" | age_class == "16-19") %>%
    mutate(
      count = case_when(
        age_class == "12-15" ~ 0.25 * count,
        TRUE ~ count
      ),
      age_class = "15-19"
    ) 
  
  df2 <- bind_rows(
    df %>%
      filter(age_class != "12-15", age_class != "16-19", age_class != "5-11"),
    df_59,
    df_1014,
    df_1519
  ) %>%
    group_by(
      state,
      age_class,
      date,
      dose1,
      dose2,
      dose3
    ) %>%
    summarise(
      count = sum(count),
      .groups = "drop"
    )
  
  
  if(
    any(
      any(!is.na(df2$dose3) & is.na(df2$dose2)),
      any(!is.na(df2$dose2) & is.na(df2$dose1))
    )
  ){
    stop("Dodgy schedules: at least one entry has had a subsequent dose without previous dose, e.g. dose 2 without dose 1")
  }
  
  age_distribution_state <- get_age_distribution_by_state()
  
  dose_dates <- unique(df2$date)
  
  df2 %$%
    expand_grid(
      age_distribution_state %>%
        dplyr::select(state, age_class),
      date = dose_dates,
      dose1 = unique(dose1),
      dose2 = unique(dose2),
      dose3 = unique(dose3)
    ) %>%
    mutate(
      legitimate_schedule = case_when(
        !is.na(dose3) & is.na(dose2) ~ FALSE,
        !is.na(dose2) & is.na(dose1) ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    filter(legitimate_schedule) %>%
    dplyr::select(-legitimate_schedule) %>%
    full_join(
      df2,
      by = c("state", "age_class", "date", "dose1", "dose2", "dose3")
    ) %>%
    mutate(
      count = ifelse(
        is.na(count),
        yes = 0,
        no = count
      )
    ) %>%
    arrange(
      state,
      age_class,
      date,
      dose1,
      dose2,
      dose3
    )
  
}

simulate_variant <- function(
  .fitted_model = fitted_model,
  dir = "outputs/projection/",
  subdir = NA,
  ratio_samples = FALSE,
  variant = c("wt", "alpha", "delta", "omicron"),
  vax_effect = NULL
){
  
  variant <- match.arg(variant)
  
  if(is.na(subdir)){
    subdir <- variant
  }
  
  data <- .fitted_model$data
  dates <- .fitted_model$data$dates$mobility
  
  de <- .fitted_model$greta_arrays$distancing_effect
  
  p <- de$p
  
  
  prop_var <- prop_variant(dates = dates)
  prop_alpha <- prop_var$prop_alpha
  prop_delta <- prop_var$prop_delta
  prop_omicron <- prop_var$prop_omicron
  prop_wt    <- prop_var$prop_wt
  
  
  phi_alpha       <- normal(1.454, 0.055, truncation = c(0, Inf))
  phi_delta_alpha <- normal(1.421, 0.033, truncation = c(0, Inf))
  phi_omicron <- 2.347662
  
  phi_delta <- phi_alpha * phi_delta_alpha
  
  if(variant == "wt") {
    prop_wt_hat    <- prop_wt    * 0 + 1
    prop_alpha_hat <- prop_alpha * 0 
    prop_delta_hat <- prop_delta * 0
    prop_omicron_hat <- prop_omicron * 0
  } else if(variant == "alpha") {
    prop_wt_hat    <- prop_wt    * 0
    prop_alpha_hat <- prop_alpha * 0 + 1
    prop_delta_hat <- prop_delta * 0
    prop_omicron_hat <- prop_omicron * 0
  } else if(variant == "delta") {
    prop_wt_hat    <- prop_wt    * 0
    prop_alpha_hat <- prop_alpha * 0
    prop_delta_hat <- prop_delta * 0 + 1
    prop_omicron_hat <- prop_omicron * 0
  }  else if(variant == "omicron") {
    prop_wt_hat    <- prop_wt    * 0
    prop_alpha_hat <- prop_alpha * 0
    prop_delta_hat <- prop_delta * 0
    prop_omicron_hat <- prop_omicron * 0 + 1
  } else if(variant == "omicron.BA2") {
    prop_wt_hat    <- prop_wt    * 0
    prop_alpha_hat <- prop_alpha * 0
    prop_delta_hat <- prop_delta * 0
    prop_omicron_hat <- prop_omicron * 0
    prop_omicron.BA2_hat <- prop_omicron * 0 + 1
  } 
  
  phi_hat <- prop_wt_hat * 1 + prop_alpha_hat * phi_alpha + prop_delta_hat * phi_delta + prop_omicron_hat * phi_omicron
  
  p_hat <- p ^ phi_hat
  
  infectious_days <- infectious_period(gi_cdf)
  
  h_t <- h_t_state(dates)
  HD_t <- de$HD_0 * h_t
  
  household_infections <- de$HC_0 * (1 - p_hat ^ HD_t)
  non_household_infections <- de$OC_t_state * de$gamma_t_state *
    infectious_days * (1 - p_hat ^ de$OD_0)
  R_t <- household_infections + non_household_infections
  R_eff_loc_1_no_surv <- extend(R_t, data$n_dates_project)
  
  
  # multiply by the surveillance effect to get component 1
  surveillance_reff_local_reduction <- surveillance_effect(
    dates = data$dates$infection_project,
    cdf = gi_cdf,
    states = data$states
  )
  # 
  
  wt_fitted_model <- .fitted_model
  
  if(is.null(vax_effect)){
    wt_fitted_model$greta_arrays$R_eff_loc_1 <- R_eff_loc_1_no_surv * surveillance_reff_local_reduction
  } else {
    vax_effect <- vax_effect %>%
      pivot_wider(
        names_from = state,
        values_from = effect
      ) %>%
      full_join(
        y = tibble(date = data$dates$infection_project)
      ) %>%
      arrange(date) %>%
      tidyr::fill(
        everything(),
        .direction = "updown"
      ) %>%
      dplyr::select(-date) %>%
      as.matrix
    
    wt_fitted_model$greta_arrays$R_eff_loc_1 <- R_eff_loc_1_no_surv * surveillance_reff_local_reduction * vax_effect
  }
  
  
  
  dir.create(dir, showWarnings = FALSE)
  write_reff_sims(wt_fitted_model, paste(dir, subdir, sep = "/"), write_reff_12 = FALSE)
  
  if(ratio_samples) {
    ratio <- wt_fitted_model$greta_arrays$R_eff_loc_1 / .fitted_model$greta_arrays$R_eff_loc_1
    ratio_vec <- c(ratio)
    ratio_sims <- calculate(ratio_vec, values = .fitted_model$draws, nsim = 2000)
    ratio_samples <- t(ratio_sims[[1]][, , 1])
    colnames(ratio_samples) <- paste0("sim", 1:2000)
    
    tibble(
      date = rep(.fitted_model$data$dates$infection_project, .fitted_model$data$n_states),
      state = rep(.fitted_model$data$states, each = .fitted_model$data$n_dates_project),
    ) %>%
      mutate(date_onset = date + 5) %>%
      cbind(ratio_samples) %>%
      write_csv(
        file.path(dir, "r_eff_1_ratio_samples.csv")
      )
    
    # read_csv("outputs/projection/wild_type/r_eff_1_local_samples.csv") %>%
    #   filter(date == as.Date("2020-04-11")) %>%
    #   pivot_longer(
    #     cols = starts_with("sim"),
    #     names_to = "sim"
    #   ) %>%
    #   group_by(state, date) %>%
    #   summarise(
    #     mean = mean(value),
    #     median = median(value),
    #     lower = quantile(value, 0.05),
    #     upper = quantile(value, 0.95),
    #     p_exceedance = mean(value > 1)
    #   )
  }
  
}

# given a timeseries of cumulative doses (for contiguous consecutive dates), and
# an assumed ideal inter-dose period, return the timeseries of the cumulative
# number of people being fully-vaccinated (having received both doses), assuming
# that second-doses are prioritised for vaccination and are always given at the
# earliest availability after the inter-dose period has elapsed.
model_vaccination_coverage <- function (daily_new_doses, inter_dose_days = 28) {
  
  # empty vectors for the number of first and second doses distributed
  first_doses <- second_doses <- daily_new_doses * 0
  
  # empty scalar for the queue of people due to receive their second dose but
  # who could not yet receive it due to insufficient doses availables
  queueing_for_second_dose <- 0
  
  # iterate through days, 
  for (day in seq_along(daily_new_doses)) {
    
    # the number of new second doses eligible today
    lagged_day <- day - inter_dose_days
    if (lagged_day >= 1) {
      target_second_doses <- first_doses[lagged_day]
    } else {
      target_second_doses <- 0
    }
    
    # add on any waiting from previous days
    target_second_doses <- target_second_doses + queueing_for_second_dose
    
    # calculate the maximum number of second doses that can be given out
    # (assuming these are the priority)
    second_doses[day] <- min(daily_new_doses[day], target_second_doses)
    
    # tally up any second doses in waiting that were not vaccinated today, to
    # wait for the next day
    queueing_for_second_dose <- max(0, target_second_doses - second_doses[day])
    
    # assign any remaining doses as first doses
    first_doses[day] <- daily_new_doses[day] - second_doses[day]
    
  }
  
  # cumulative number of fully-vaccinated people
  cumsum(second_doses)
  
}

# get a dataframe of vaccine coverage estimates
vaccination_coverage <- function() {
  
  # scrape a timeseries of the cumulative number of doses given out
  scrape_doses_timeseries() %>%
    # apply an allocation model to compute the cumulative number of people
    # receiving their second dose (fully vaccinated)
    mutate(
      fully_vaccinated = model_vaccination_coverage(new_doses)
    ) %>%
    # get the fraction of vaccinated people who are fully vaccinated
    mutate(
      partially_vaccinated = doses - (fully_vaccinated * 2),
      vaccinated = fully_vaccinated + partially_vaccinated,
      proportion_2_dose = fully_vaccinated / vaccinated
    )
  
}


hist_prior_posterior <- function(greta_array, draws, nsim = 1000, ...)  {
  
  prior_sim <- calculate(greta_array, nsim = nsim)[[1]]
  posterior_sim <- calculate(greta_array, values = draws, nsim = nsim)[[1]]
  
  prior_sim <- c(prior_sim)
  posterior_sim <- c(posterior_sim)
  xlim <- range(c(prior_sim, posterior_sim))
  
  op <- par()
  on.exit(par(op))
  par(mfrow = c(1, 2))
  
  hist(c(prior_sim), xlim = xlim, main = "Prior", ...)
  hist(c(posterior_sim), xlim = xlim, main = "Posterior", ...)
  
}

read_reff_samples <- function(
  sample.file
){
  
  read.csv(sample.file) %>%
    as_tibble %>%
    pivot_longer(
      cols = starts_with("sim"),
      values_to = "value",
      names_to = "name"
    )  %>%
    group_by(date, state) %>%
    summarise(
      med = median(value),
      lw5 = quantile(value, 0.05),
      up95 = quantile(value, 0.95),
      lw25 = quantile(value, 0.25),
      up75 = quantile(value, 0.75)
    ) %>%
    ungroup %>%
    mutate(date = as.Date(as.character(date)))
}


write_mobility_dates <- function(mobility, dir = "outputs/"){
  mobility %>%
    group_by(datastream) %>%
    summarise(
      latest = max(date),
      earliest = min(date)
    ) %>%
    write_csv(
      file = file.path(dir, 'mobility_dates.csv')
    )
}


states <- c(
  "ACT",
  "NT",
  "NSW",
  "QLD",
  "SA",
  "TAS",
  "VIC",
  "WA"
)

vax_files_dates <- function(dir){
  list.files(path = dir) %>%
    sub(
      pattern = "DohertyTimeseriesExport_",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = "_.*",
      replacement = "",
      x = .
    ) %>%
    as.Date
}

load_cumulative_doses_old <- function(dir = "~/not_synced/vaccination/vaccination_timeseries_medicare_and_provider/"){
  
  file_index <- vax_files_dates(dir) %>%
    which.max()
  
  extraction_date <- vax_files_dates(dir)[file_index]
  
  file <- list.files(
    path = dir,
    full.names = TRUE
  )[file_index]
  
  
  over_80 <- c(
    "80+",
    "80-84",
    "85+",
    "85-89",
    "90+",
    "90-94",
    "95+",
    "95-99",
    "100+"
  )
  
  df <- read_csv(
    file = file
  ) %>%
    rename(
      "state_provider" = PROVIDER_STATE,
      "state_medicare" = PATIENT_MEDICARE_STATE,
      "vaccine" = VACCINE_CODE,
      "dose_number" = DOSE_NUMBER,
      "age_class" = CURRENT_AGE_GROUP,
      "week" = AS_OF_ENCOUNTER_WEEK,
      "date" = AS_OF_WEEK_COMMENCING,
      "doses" = CUMULATIVE_UNIQUE_INDIVIDUALS_VACCINATED
    ) %>%
    mutate(
      date = as.Date(
        date,
        format = "%d/%m/%Y"
      ),
      age_class = case_when(
        age_class %in% over_80 ~ "80+",
        TRUE ~ age_class
      ),
      state = case_when(
        state_medicare == "Unknown" ~ state_provider,
        state_medicare == "UNK" ~ state_provider,
        TRUE ~ state_medicare
      )
    ) %>%
    dplyr::select(-state_provider, -state_medicare) %>%
    group_by(
      state,
      vaccine,
      dose_number,
      age_class,
      date
    ) %>%
    summarise(
      doses = sum(doses)
    ) %>% 
    ungroup
  
  df_1014 <- df %>%
    filter(age_class == "12-15") %>%
    mutate(
      doses = 0.75 * doses,
      age_class = "10-14"
    )
  
  df_1519 <- df %>%
    filter(age_class == "12-15" | age_class == "16-19") %>%
    mutate(
      doses = case_when(
        age_class == "12-15" ~ 0.25 * doses,
        TRUE ~ doses
      ),
      age_class = "15-19"
    ) %>%
    group_by(state, age_class, vaccine, dose_number, date) %>%
    summarise(doses = sum(doses)) %>%
    ungroup %>%
    dplyr::select(state, vaccine, dose_number, age_class, date, doses)
  
  df2 <- bind_rows(
    df_1014,
    df_1519,
    df %>%
      filter(age_class != "12-15", age_class != "16-19")
  ) %>%
    mutate(
      vaccine = case_when(
        vaccine == "COMIRN" ~ "pf",
        vaccine == "COVAST" ~ "az",
        vaccine == "MODERN" ~ "mo",
      )
    )
  
  
  df2 %$%
    expand_grid(
      state = unique(state),
      vaccine = unique(vaccine),
      dose_number = unique(dose_number),
      age_class = unique(age_class),
      date = unique(date)
    ) %>%
    full_join(
      df2,
      by = c("state", "vaccine", "dose_number", "age_class", "date")
    ) %>%
    mutate(
      doses = ifelse(
        is.na(doses),
        yes = 0,
        no = doses
      )
    ) %>%
    arrange(
      state,
      vaccine,
      age_class,
      dose_number,
      date
    )
  
  
}

immunity_lag_correction <- function(
  date,
  doses,
  dose_number
) {
  
  if (dose_number[1] == 1) {
    weeks_increase <- 2
    weeks_wait <- 1
  } else if (dose_number[1] == 2) {
    weeks_increase <- 2
    weeks_wait <- 0
  }
  
  # compute the current doses (by dose/vaccine)
  max_date <- which.max(date)
  latest_doses <- doses[max_date]
  
  # compute the diff of dosess for all dates to get the proportion of the
  # population added on/by that date
  new_doses <- diff(c(0, doses))
  
  # compute the ratio of the proportion added on each previous date to the
  # current doses (should sum to 1)
  date_weights <- new_doses / latest_doses
  
  # multiply each of those ratios by the relative effect based on the date difference¿
  week_diff <- as.numeric(date[max_date] - date) / 7
  relative_effect <- pmax(0, pmin(1, (week_diff - weeks_wait) / weeks_increase))
  
  # sum the ratios to get the correction multiplier
  correction <- sum(relative_effect * date_weights)
  
  if (is.na(correction)) {
    correction <- 0
  }
  
  correction
  
  
}



# lookup to disaggregate coverages to 5y age groups
age_lookup <- tibble::tribble(
  ~age_5y, ~age, ~proportion_of_group,
  "0-4", "0-14", 5/15,
  "5-9", "0-14", 5/15,   
  "10-14", "0-14", 5/15,
  "15-19", "15-29", 5/15,
  "20-24", "15-29", 5/15,
  "25-29", "15-29", 5/15,
  "30-34", "30-39", 5/10,
  "35-39", "30-39", 5/10,
  "40-44", "40-49", 5/10,
  "45-49", "40-49", 5/10,
  "50-54", "50-59", 5/10,
  "55-59", "50-59", 5/10,
  "60-64", "60-69", 5/10,
  "65-69", "60-69", 5/10,
  "70-74", "70-79", 5/10,
  "75-79", "70-79", 5/10,
  "80+", "80+", 1/1
)

get_age_distribution_by_state <- function(
  final_age_bin = 80,
  by = 5,
  population_total = 25693000,
  ages = NULL
) {
  
  if(is.null(ages)){
    # check the final age bin in sensible
    if (final_age_bin > 85) {
      stop(
        "No age-specific population data for ages greater than 85",
        call. = TRUE
      )
    }
    
    ages <- age_classes(
      final_age_bin = final_age_bin,
      by = by
    )
  }
  
  # Age structure of the Australian population by year of age, up to 100+
  # This is a "standard" distribution data frame but old population size data
  # from 2001 hence is adjusted later
  # aust_population_standard <- readxl::read_xls(
  #   path = "data/vaccinatinon/abs_standard_age_31010DO003_200106.xls",
  #   sheet = "Table_1",
  #   skip = 6,
  #   col_names = c(
  #     "age",
  #     "pop"
  #   )
  # ) %>%
  #   filter(age != "Total", !is.na(age), age != "© Commonwealth of Australia 2013") %>%
  #   mutate(
  #     age = case_when(
  #       age == "100 and over" ~ "100",
  #       TRUE ~ age
  #     ) %>%
  #       as.integer,
  #   )
  
  # use 2020 population, as this better matches proportion 80+
  aust_population_2020 <- readxl::read_xls(
    path = "data/vaccinatinon/abs_population_2020.xls",
    sheet = "Table_8",
    range = cell_rows(c(223:328)),
    col_names = c(
      "age",
      "NSW",
      "VIC",
      "QLD",
      "SA",
      "WA",
      "TAS",
      "NT",
      "ACT",
      "Aus"
    )
  ) %>%
    dplyr::select(-Aus) %>%
    mutate(
      age = case_when(
        age == "85-89" ~ "85",
        age == "90-94" ~ "85",
        age == "95-99" ~ "85",
        age == "100 and over" ~ "85",
        TRUE ~ age
      )
    ) %>%
    filter(
      !grepl("-", age)
    ) %>%
    pivot_longer(
      cols = -age,
      names_to = "state",
      values_to = "pop"
    ) %>%
    group_by(
      state,
      age
    ) %>%
    summarise(
      pop = sum(pop),
      .groups = "drop"
    ) %>%
    mutate(
      age = as.integer(age)
    ) %>%
    arrange(age)
  
  
  state_pops <- aust_population_2020 %>%
    group_by(state) %>%
    summarise(pop = sum(pop)) %>%
    mutate(state_pop = pop*population_total/sum(pop)) %>%
    dplyr::select(-pop)
  
  # aggregate into age classes and return
  age_class_fractions <- aust_population_2020 %>%
    mutate(
      age_class = cut(
        age,
        breaks = c(ages$lower - 1, Inf),
        labels = ages$classes
      ),
      age_class = as.character(age_class),
      age_class = factor(age_class, levels= unique(age_class))
    ) %>%
    group_by(
      state,
      age_class
    ) %>%
    summarise(
      pop = sum(pop)
    ) %>%
    left_join(
      y = state_pops,
      by = "state"
    ) %>%
    group_by(state) %>%
    mutate(
      fraction = pop / sum(pop),
      pop = fraction * state_pop
    ) %>%
    dplyr::select(-state_pop)
  
  return(age_class_fractions)
  
}



write_reff_sims_vax <- function(
  fitted_model,
  vaccine_timeseries
){
  
  # ESSENTIALLY DEPRECATED
  # adds vaccination effect to C1 if fitted without it
  
  fitted_model_extended <- fitted_model
  
  fitted_model_extended$greta_arrays <- c(
    fitted_model$greta_arrays,
    list(
      R_eff_loc_1_with_vaccine = reff_1_with_vaccine(fitted_model_extended, vaccine_timeseries)
    ) 
  )
  
  ga <- fitted_model_extended$greta_arrays[["R_eff_loc_1_with_vaccine"]]
  ga_vec <- c(ga)
  sim <- calculate(ga_vec, values = fitted_model_extended$draws, nsim = 2000)
  
  samples <- t(sim[[1]][1:2000, , 1])
  colnames(samples) <- paste0("sim", 1:2000)
  
  tibble(
    date = rep(fitted_model$data$dates$infection_project, fitted_model$data$n_states),
    state = rep(fitted_model$data$states, each = fitted_model$data$n_dates_project),
  ) %>%
    mutate(date_onset = date + 5) %>%
    cbind(samples) %>%
    write_csv(
      file.path("outputs/projection/", "r_eff_1_local_with_vaccine_samples.csv")
    )
  
  
}


write_reff_sims_novax <- function(
  fitted_model#,
  #vaccine_timeseries
){
  
  # removes vaccination effect from C1 if fitted with it
  
  fitted_model_extended <- fitted_model
  
  vaccine_timeseries <- as_tibble(fitted_model$data$vaccine_effect_matrix) %>%
    mutate(date = fitted_model$data$dates$infection_project) %>%
    pivot_longer(cols = -date, names_to = "state", values_to = "effect")
  
  fitted_model_extended$greta_arrays <- c(
    fitted_model$greta_arrays,
    list(
      R_eff_loc_1_without_vaccine = reff_1_without_vaccine(fitted_model_extended, vaccine_timeseries)
    ) 
  )
  
  ga <- fitted_model_extended$greta_arrays[["R_eff_loc_1_without_vaccine"]]
  ga_vec <- c(ga)
  sim <- calculate(ga_vec, values = fitted_model_extended$draws, nsim = 2000)
  
  samples <- t(sim[[1]][1:2000, , 1])
  colnames(samples) <- paste0("sim", 1:2000)
  
  tibble(
    date = rep(fitted_model$data$dates$infection_project, fitted_model$data$n_states),
    state = rep(fitted_model$data$states, each = fitted_model$data$n_dates_project),
  ) %>%
    mutate(date_onset = date + 5) %>%
    cbind(samples) %>%
    write_csv(
      file.path("outputs/projection/", "r_eff_1_local_without_vaccine_samples.csv")
    )
  
  
}

state_short_long_table <- tibble::tribble(
  ~state_short, ~state_long,
  "ACT", "Australian Capital Territory",
  "NSW", "New South Wales",
  "NT", "Northern Territory",
  "QLD", "Queensland",
  "SA", "South Australia",
  "TAS", "Tasmania",
  "VIC", "Victoria",
  "WA", "Western Australia"
)


get_quantium_lookups <- function(dir) {
  
  lookups <- list(
    age = read_csv(
      sprintf(
        "%s/dim_age_band.csv",
        dir
      ),
      col_types = cols(
        age_band_id = col_double(),
        age_band = col_character()
      )
    ),
    product = read_csv(
      sprintf(
        "%s/dim_vaccine.csv",
        dir
      ),
      col_types = cols(
        vaccine = col_double(),
        name = col_character(),
        short_name = col_character()
      )
    ),
    date = read_csv(
      sprintf(
        "%s/dim_time.csv",
        dir
      ),
      col_types = cols(
        time = col_double(),
        week_starting = col_date("%d/%m/%Y")
      )
    ),
    scenario = read_csv(
      sprintf(
        "%s/dim_scenario.csv",
        dir
      ),
      col_types = cols(
        scenario = col_double(),
        `5-11 uptake curve` = col_character(),
        booster_shape = col_character(),
        booster_scale_by_age = col_character(),
        booster_uptake_terminal = col_double(),
        booster_uptake_months = col_double()
      )
    ),
    sa4 = read_csv(
      sprintf(
        "%s/dim_sa4.csv",
        dir
      )
    )
  )
  
  return(lookups)
  
}

sort_age_groups <- function(age_groups) {
  start <- str_split_fixed(age_groups, "-", 2)[, 1]
  order <- str_order(start, numeric = TRUE)
  age_groups[order]
}

get_quantium_data_dates <- function(){
  list.dirs(
    path = "~/not_synced/vaccination/quantium_forecasts/",
    full.names = FALSE,
    recursive = FALSE
  ) %>%
    as.Date
}


get_quantium_data_dir <- function(
  date = NULL
){
  # get most recent forecast
  dir_dates <- get_quantium_data_dates()
  
  if (is.null(date)) {
    dir_index <- which.max(dir_dates)
  } else {
    dir_index <- which(dir_dates == date)
    if (length(dir_index) != 1){
      stop("Either no directory or too many directories match this date")
    }
  }
  
  dir <- list.dirs(
    path = "~/not_synced/vaccination/quantium_forecasts/",
    full.names = TRUE,
    recursive = FALSE
  )[dir_index]
  
  return(dir)
  
}

read_quantium_vaccination_data <- function(
  date = NULL
){
  
  dir <- get_quantium_data_dir(date)
  
  # load all vaccine data
  vaccines <- read_csv(
    file = sprintf(
      "%s/vaccines.csv",
      dir
    )
  )
  
  # load quantium lookup tables
  lookups <- get_quantium_lookups(dir = dir)
  
  # add on times, age bands, and products, and return
  vaccine_data <- vaccines %>%
    # join on the dates
    ungroup() %>%
    left_join(
      lookups$date,
      by = c(
        "time_dose_1" = "time"
      )
    ) %>%
    rename(
      date_dose_1 = week_starting
    ) %>%
    left_join(
      lookups$date,
      by = c(
        "time_dose_2" = "time"
      )
    ) %>%
    rename(
      date_dose_2 = week_starting
    ) %>%
    left_join(
      lookups$date,
      by = c(
        "time_dose_3" = "time"
      )
    ) %>%
    rename(
      date_dose_3 = week_starting
    ) %>%
    left_join(
      lookups$date,
      by = c(
        "time_dose_4" = "time"
      )
    ) %>%
    rename(
      date_dose_4 = week_starting
    ) %>%
    select(
      -starts_with("time")
    ) %>%
    # join on the products
    left_join(
      lookups$product,
      by = "vaccine"
    ) %>%
    select(
      -vaccine,
      -short_name
    ) %>%
    rename(
      vaccine = name
    ) %>%
    left_join(
      lookups$product,
      by = c("vaccine_dose_3" = "vaccine")
    ) %>%
    select(
      -vaccine_dose_3,
      -short_name
    ) %>%
    rename(
      vaccine_dose_3 = name
    ) %>%
    left_join(
      lookups$product,
      by = c("vaccine_dose_4" = "vaccine")
    ) %>%
    select(
      -vaccine_dose_4,
      -short_name
    ) %>%
    rename(
      vaccine_dose_4 = name
    ) %>%
    # join on the age bands and convert to a factor
    left_join(
      lookups$age,
      by = "age_band_id"
    ) %>%
    select(
      -age_band_id
    ) %>%
    mutate(
      age_band = factor(
        age_band,
        levels = sort_age_groups(unique(age_band))
      )
    ) %>%
    left_join(
      lookups$sa4,
      by = "sa4_code16"
    )
  
  return(vaccine_data)
  
}


aggregate_quantium_vaccination_data_to_state <- function(data){
  
  data %>%
    left_join(
      state_short_long_table,
      by = c("STE_NAME16" = "state_long")
    ) %>%
    group_by(
      scenario,
      date_dose_1,
      date_dose_2,
      date_dose_3,
      date_dose_4,
      vaccine,
      vaccine_dose_3,
      vaccine_dose_4,
      age_band,
      state_short
    ) %>%
    summarise(
      num_people = sum(num_people, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(state = state_short) %>%
    select(
      scenario,
      state,
      age_band,
      vaccine,
      vaccine_dose_3,
      vaccine_dose_4,
      date_dose_1,
      date_dose_2,
      date_dose_3,
      date_dose_4,
      num_people
    ) %>%
    arrange(
      scenario,
      state,
      age_band,
      vaccine,
      vaccine_dose_3,
      vaccine_dose_4,
      date_dose_1,
      date_dose_2,
      date_dose_3,
      date_dose_4,
    ) %>%
    # excluding vaccinations outside of the 6 states or ACT and NT
    filter(!is.na(state))
  
}


get_vaccine_cohorts_at_date <- function(vaccine_scenarios, target_date) {
  
  # set future times to NA and collapse to get contemporary data
  vaccine_scenarios %>%
    mutate(
      across(
        starts_with("date"),
        ~if_else(.x > target_date, as.Date(NA), .x)
      )
    ) %>%
    group_by(
      across(
        -num_people
      )
    ) %>%
    summarise(
      num_people = sum(num_people),
      .groups = "drop"
    ) %>%
    # compute most recent vaccines and how long ago they were for each cohort
    mutate(
      most_recent_dose = pmax(
        date_dose_1,
        date_dose_2,
        date_dose_3,
        date_dose_4,
        na.rm = TRUE
      )
    ) %>%
    pivot_longer(
      cols = starts_with("date"),
      names_to = "dose",
      values_to = "date",
      names_prefix = "date_"
    ) %>%
    # keep only one of these entries
    mutate(
      keep = case_when(
        is.na(date) ~ dose == "dose_1",
        TRUE ~ date == most_recent_dose
      )
    ) %>%
    filter(
      keep
    ) %>%
    select(
      -most_recent_dose,
      -keep
    ) %>%
    mutate(
      dose = if_else(is.na(date), NA_character_, dose),
      # labelling all booster as boosters instead of by brand, thus assuming all boosters are mRNA
      vaccine_dose_3 = if_else(
        !is.na(vaccine_dose_3),
        "Booster",
        NA_character_
      ),
      vaccine_dose_4 = if_else(
        !is.na(vaccine_dose_4),
        "Booster",
        NA_character_
      ),
      product = case_when(
        dose == "dose_3" ~ vaccine_dose_3,
        dose == "dose_4" ~ vaccine_dose_4,
        TRUE ~ vaccine
      ),
      # rename products to match VE model, recoding Moderna as Pfizer for now
      # since there is not enough evidence on efficacy to disinguish it from
      # Pfizer. Ditto Novavax
      product = case_when(
        product == "Pfizer" ~ "Pf",
        product == "AstraZeneca" ~ "AZ",
        product == "Moderna" ~ "Pf",
        product == "Booster" ~ "mRNA",
        product == "Pfizer (5-11)" ~ "Pf",
        product == "Novavax" ~ "Pf"
      ),
      # dose = case_when(
      #   dose == "dose_3" ~ "booster",
      #   dose == "dose_4" ~ "booster",
      #   TRUE ~ dose
      # ),
      immunity = case_when(
        !is.na(date) ~ paste(product, dose, sep = "_"),
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      days_ago = as.numeric(target_date - date)
    ) %>%
    select(
      -starts_with("vaccine"),
      -product,
      -dose,
      -date
    )
  
}



add_missing_age_cohorts <- function(vaccine_cohorts){
  dir <- get_quantium_data_dir(date = NULL)
  lookups <- get_quantium_lookups(dir = dir)
  
  state_age_fractions <- lookups$age %>%
    select(age_band) %>%
    mutate(
      lower = sub(
        pattern = "\\-.*",
        replacement = "",
        x = age_band
      ) %>%
        sub(
          pattern = "\\+",
          replacement = "",
          x = .
        ) %>%
        as.numeric,
      upper = sub(
        pattern = ".*\\-",
        replacement = "",
        x = age_band
      ) %>%
        sub(
          pattern = ".*\\+",
          replacement = "Inf",
          x = .
        ) %>%
        as.numeric
    ) %>%
    rename(classes = age_band) %>%
    get_age_distribution_by_state(ages = .)
  
  # add in 0-4 yo cohort which is missing
  age_class_state_scenario <- expand_grid(
    state_age_fractions %>%
      select(state, age_class),
    scenario = unique(vaccine_cohorts$scenario)
  )
  
  vaccine_cohorts_all <- age_class_state_scenario %>%
    full_join(
      y = vaccine_cohorts,
      by = c("scenario", "state", "age_class" = "age_band")
    ) %>%
    left_join(
      state_age_fractions,
      by = c("state", "age_class")
    ) %>%
    group_by(
      state, scenario
    ) %>%
    mutate(
      qspop = sum(num_people, na.rm = TRUE),
      qpop = qspop / (1 - fraction),
      num_people = if_else(
        age_class == "0-4",
        fraction * qpop,
        num_people
      )
    ) %>%
    rename(age_band = age_class) %>%
    select(
      scenario, state, age_band, num_people, immunity, days_ago
    )
  
  return(vaccine_cohorts_all)
}

add_missing_age_cohorts_nsw_sa4 <- function(vaccine_cohorts){
  dir <- get_quantium_data_dir(date = NULL)
  lookups <- get_quantium_lookups(dir = dir)
  
  state_age_fractions <- lookups$age %>%
    select(age_band) %>%
    mutate(
      lower = sub(
        pattern = "\\-.*",
        replacement = "",
        x = age_band
      ) %>%
        sub(
          pattern = "\\+",
          replacement = "",
          x = .
        ) %>%
        as.numeric,
      upper = sub(
        pattern = ".*\\-",
        replacement = "",
        x = age_band
      ) %>%
        sub(
          pattern = ".*\\+",
          replacement = "Inf",
          x = .
        ) %>%
        as.numeric
    ) %>%
    rename(classes = age_band) %>%
    get_age_distribution_by_state(ages = .) %>%
    ungroup %>%
    filter(state == "NSW") %>%
    select(-state)
  
  # add in 0-4 yo cohort which is missing
  age_class_state_scenario <- expand_grid(
    state_age_fractions %>%
      select(age_class),
    state = unique(vaccine_cohorts$state),
    scenario = unique(vaccine_cohorts$scenario)
  )
  
  vaccine_cohorts_all <- age_class_state_scenario %>%
    full_join(
      y = vaccine_cohorts,
      by = c("scenario", "state", "age_class" = "age_band")
    ) %>%
    left_join(
      state_age_fractions,
      by = c("age_class")
    ) %>%
    group_by(
      state, scenario
    ) %>%
    mutate(
      qspop = sum(num_people, na.rm = TRUE),
      qpop = qspop / (1 - fraction),
      num_people = if_else(
        age_class == "0-4",
        fraction * qpop,
        num_people
      )
    ) %>%
    rename(age_band = age_class) %>%
    select(
      scenario, state, age_band, num_people, immunity, days_ago, qpop, qspop, fraction
    )
  
  return(vaccine_cohorts_all)
}


get_coverage <- function(vaccine_cohorts) {
  
  # get current coverage with any dose in each age band, for each scenario
  coverage <- vaccine_cohorts %>%
    mutate(
      immune = !is.na(immunity)
    ) %>%
    group_by(
      scenario, state, age_band
    ) %>%
    summarise(
      coverage = weighted.mean(immune, num_people),
      .groups = "drop"
    )
  
}

get_omicron_params_wide <- function(
  # param_file = NULL
) {
  
  # if(is.null(param_file)){
    param_file <- "outputs/scenario_parameters_omicron.csv"
  # } else if (param_file == "infection") {
  #   param_file <- "outputs/scenario_parameters_omicron_infection_assumption.csv"
  # }
  
  read_csv(
    param_file,
    col_types = cols(
      parameter = col_character(),
      intermediate = col_double(),
      optimistic = col_double(),
      estimate = col_double(),
      pessimistic = col_double()
    )
  ) %>%
    pivot_longer(
      cols = c(-parameter),
      names_to = "omicron_scenario",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = parameter,
      values_from = value
    )
}

log10_neut_over_time <- function (time, maximum_log10_neut, decay){
  # equivalent to: log10(10 ^ maximum_log10_neut * exp(-decay * time))
  maximum_log10_neut - decay * time / log(10)
}

ve_from_mean_log10_neut <- function(
  mean_log10_neut_vec,
  sd_log10_neut,
  log_k,
  c50_vec,
  method = c("adaptive", "gaussian"),
  lower = -10,
  upper = 10
) {
  
  # choose the method and dispatch to the appropriate integration function
  method <- match.arg(method)
  
  integrator <- switch(
    method,
    adaptive = adaptive_ve_integrator,
    gaussian = gaussian_ve_integrator
  )
  
  integrals <- integrator(
    c50_vec = c50_vec,
    mean_log10_neut_vec = mean_log10_neut_vec,
    sd_log10_neut = sd_log10_neut,
    log_k = log_k,
    lower = lower,
    upper = upper
  )
  
  integrals
  
}

gaussian_ve_integrator <- function(
  c50_vec,
  mean_log10_neut_vec,
  sd_log10_neut,
  log_k,
  lower,
  upper
) {
  
  # dimensions and quadrature rules
  n_obs <- length(mean_log10_neut_vec)
  quads <- get_quad_rules(n_obs, lower = lower, upper = upper)
  n_quads <- length(quads$values)
  
  # expand out the vector parameters of the logit-normal density to matrices
  if (is.vector(c50_vec) & is.vector(mean_log10_neut_vec)) {
    c50_vec <- as.matrix(c50_vec)
    mean_log10_neut_vec <- as.matrix(mean_log10_neut_vec)
    vector_input <- TRUE
  } else {
    vector_input <- FALSE
  }
  
  repeater <- rep(1, n_quads)
  c50_mat <- c50_vec[, repeater]
  mean_log10_neut_mat <- mean_log10_neut_vec[, repeater]
  
  # and expand out the integration points to match
  values_matrix <- t(replicate(n_obs, quads$values))
  
  # get function values in matrix
  function_values <- logit_normal_density(
    x = values_matrix,
    c50 = c50_mat,
    mean_log10_neut = mean_log10_neut_mat,
    sd_log10_neut = sd_log10_neut,
    log_k = log_k
  )
  
  weights <- quads$weights
  
  # if we're doing this with greta arrays, we need to work around an issue with
  # greta checking matrix multiplly dimensions too early
  if (inherits(function_values, "greta_array")) {
    weights <- as_data(quads$weights)
  }
  
  ves <- function_values %*% weights
  
  if(vector_input) {
    dim(ves) <- NULL
  }
  
  ves
  
}

adaptive_ve_integrator <- function(
  c50_vec,
  mean_log10_neut_vec,
  sd_log10_neut,
  log_k,
  lower,
  upper
) {
  
  
  if (
    inherits(c50_vec, "greta_array") |
    inherits(mean_log10_neut_vec, "greta_array") |
    inherits(sd_log10_neut, "greta_array") |
    inherits(log_k, "greta_array")
  ) {
    stop ("adaptive integration can not be used with greta models")
  }
  
  integrate_once <- function(c50, mean_log10_neut) {
    integral <- integrate(
      f = logit_normal_density,
      c50 = c50,
      mean_log10_neut = mean_log10_neut,
      sd_log10_neut = sd_log10_neut,
      log_k = log_k,
      lower = lower,
      upper = upper
    )$value
  }
  
  logit_normal_density(0, c50_vec[1], mean_log10_neut = mean_log10_neut_vec[1], sd_log10_neut = sd_log10_neut, log_k = log_k)
  
  integrals <- mapply(
    integrate_once,
    c50_vec,
    mean_log10_neut_vec
  )
  
  integrals
  
}

get_quad_rules <- function(n_observations, lower = -3, upper = 3) {
  
  n_quads <- round(5 * (upper - lower))
  
  # get quadrature rules on (-1, 1)
  quads <- gaussquad::legendre.quadrature.rules(n_quads)[[n_quads]]
  
  # transform them to (lower, upper) and return
  lambda <- (upper - lower) / 2
  mu <- (lower + upper) / 2
  
  list(
    values = lambda * quads$x + mu,
    weights = lambda * quads$w
  )
  
}

logit_normal_density <- function(x, c50, mean_log10_neut, sd_log10_neut, log_k) {
  prob <- prob_avoid_outcome(log10_neut = x, log_k = log_k, c50 = c50)
  dens <- log10_neut_density(x, mean_log10_neut, sd_log10_neut)
  prob * dens
}

prob_avoid_outcome <- function(log10_neut, log_k, c50) {
  1 / (1 + exp(-exp(log_k) * (log10_neut - c50)))
}

log10_neut_density <- function(x, mean, sd) {
  
  with_greta <- inherits(x, "greta_array") |
    inherits(mean, "greta_array") |
    inherits(sd, "greta_array")
  
  if (with_greta) {
    normal_density(x, mean, sd)
  } else {
    dnorm(x, mean, sd)
  }
}


get_vaccine_efficacies <- function(vaccine_cohorts, 
                                   variants = c("Delta", "Omicron BA2", "Omicron BA4/5"),
                                   neut_immune_escape = 0.44) {
  
  # load omicron parameters in wide format and subset to different parameter sets
  params_wide <- get_omicron_params_wide()
  
  neut_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      log10_mean_neut_AZ_dose_1,
      log10_mean_neut_AZ_dose_2,
      log10_mean_neut_Pfizer_dose_1,
      log10_mean_neut_Pfizer_dose_2,
      log10_mean_neut_mRNA_booster,
      neut_decay
    )
  
  ve_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      starts_with("c50"),
      log_k,
      sd_log10_neut_titres,
      omicron_log10_neut_fold
    )
  
  # compute the average neutralisation level (mean log10 neut fold of WT
  # convalescent) in each age group, scenario, and omicron scenario
  mean_neuts <- vaccine_cohorts %>%
    filter(
      !is.na(immunity)
    ) %>%
    left_join(
      tibble(
        omicron_scenario = c(
          #"intermediate",
          #"optimistic",
          "estimate"#,
          #"pessimistic"
        )
      ),
      by = character()
    ) %>%
    left_join(
      neut_params_wide,
      by = "omicron_scenario"
    ) %>%
    # compute the peak and waned log10 mean neuts for each cohort
    mutate(
      peak_neuts = case_when(
        immunity == "AZ_dose_1" ~ log10_mean_neut_AZ_dose_1,
        immunity == "AZ_dose_2" ~ log10_mean_neut_AZ_dose_2,
        immunity == "Pf_dose_1" ~ log10_mean_neut_Pfizer_dose_1,
        immunity == "Pf_dose_2" ~ log10_mean_neut_Pfizer_dose_2,
        #immunity == "mRNA_booster" ~ log10_mean_neut_mRNA_booster
        immunity == "mRNA_dose_3" ~ log10_mean_neut_mRNA_booster,
        immunity == "mRNA_dose_4" ~ log10_mean_neut_mRNA_booster + log10(1.33)
      )
    ) %>%
    mutate(
      neuts = log10_neut_over_time(
        time = days_ago,
        maximum_log10_neut = peak_neuts,
        decay = neut_decay
      )
    ) %>%
    select(
      -starts_with("log10_mean_neut"),
      -peak_neuts,
      -neut_decay
    ) %>%
    # average the mean neuts over cohorts and scenarios
    group_by(
      scenario, state, omicron_scenario, age_band
    ) %>%
    summarise(
      neuts = weighted.mean(neuts, num_people),
      .groups = "drop"
    )
  
  # now compute VEs against each outcome, for Omicron and Delta
  ves <- mean_neuts %>%
    left_join(
      ve_params_wide,
      by = "omicron_scenario"
    ) %>%
    # for omicron, adjust down the neuts
    full_join(
      tibble(
        variant = variants
      ),
      by = character()
    ) %>%
    mutate(
      neuts = case_when(
        variant == "Omicron BA2" ~ neuts + omicron_log10_neut_fold,
        variant == "Omicron BA4/5" ~ neuts + omicron_log10_neut_fold + log10(neut_immune_escape),
        TRUE ~ neuts
      )
    ) %>%
    # compute all the VEs in one shot with Gaussian integration
    pivot_longer(
      cols = starts_with("c50"),
      names_to = "outcome",
      values_to = "c50",
      names_prefix = "c50_"
    ) %>%
    mutate(
      ve = ve_from_mean_log10_neut(
        mean_log10_neut_vec = neuts,
        sd_log10_neut = sd_log10_neut_titres,
        log_k = log_k,
        c50_vec = c50,
        method = "gaussian"
      )
    ) %>%
    select(
      -neuts,
      -log_k,
      -sd_log10_neut_titres,
      -omicron_log10_neut_fold,
      -c50
    )
  
  ves
  
}


get_vaccine_transmission_effects <- function(ves, coverage) {
  
  
  # load quantium lookup tables
  dir <- get_quantium_data_dir()
  lookups <- get_quantium_lookups(dir = dir)
  
  age_breaks_quantium <-lookups$age %>%
    mutate(
      brk = sub(
        pattern = "\\-.*",
        replacement = "",
        x = age_band
      ) %>%
        sub(
          pattern = "\\+",
          replacement = "",
          x = .
        ) %>%
        as.numeric
    ) %>%
    pull(brk)
  
  labq <- length(age_breaks_quantium)
  
  age_breaks_quantium[labq + 1] <- Inf
  
  # get a conmat NGM for Australia
  australia_ngm <- baseline_matrix(age_breaks = age_breaks_quantium)
  
  age_band_factor <- levels(ves$age_band)
  
  # combine coverage and VEs to get transmission reduction for each rollout
  # scenario, and omicron scenario
  ves %>%
    # add back in the younger age_groups
    complete(
      scenario,
      state,
      omicron_scenario,
      variant,
      outcome,
      age_band = unique(lookups$age$age_band),
      fill = list(
        ve = 0
      )
    ) %>%
    # get the two transmission VEs as columns
    filter(
      outcome %in% c("acquisition", "transmission")
    ) %>%
    pivot_wider(
      names_from = outcome,
      values_from = ve
    ) %>%
    group_by(
      scenario,
      state,
      omicron_scenario,
      variant
    ) %>%
    mutate(
      age_band = factor(age_band, levels = age_band_factor)
    ) %>%
    arrange(
      age_band,
      .by_group = TRUE
    ) %>%
    # join on coverages
    left_join(
      coverage,
      by = c("scenario", "age_band", "state")
    ) %>%
    # compute percentage reduction in acquisition and transmission in each age group
    mutate(
      acquisition_multiplier = 1 - acquisition * coverage,
      transmission_multiplier = 1 - transmission * coverage,
    ) %>%
    select(
      -acquisition,
      -transmission,
      -coverage
    ) %>%
    # transform these into matrices of reduction in transmission, matching the NGM
    summarise(
      transmission_reduction_matrix =
        list(
          outer(
            # 'to' groups are on the rows in conmat, and first element in outer is rows,
            # so acquisition first
            acquisition_multiplier,
            transmission_multiplier,
            FUN = "*"
          )
        ),
      .groups = "drop"
    ) %>%
    group_by(
      scenario,
      state,
      omicron_scenario,
      variant
    ) %>%
    mutate(
      ngm_unvaccinated = list(australia_ngm),
      ngm_vaccinated = list(ngm_unvaccinated[[1]] * transmission_reduction_matrix[[1]]),
      vaccination_effect = 1 - get_R(ngm_vaccinated[[1]]) / get_R(ngm_unvaccinated[[1]])
    ) %>%
    select(
      -transmission_reduction_matrix,
      -ngm_unvaccinated,
      -ngm_vaccinated
    )
  
}


split_ticks_and_labels <- function(
  # data can be vector of dates or dataframe/tibble with date column
  data,
  tick_freq = "1 month",
  label_freq = "2 months",
  label_format = "%b %y",
  label_last = TRUE
){
  
  if(is.tbl(data)){
    dates <- data$date
  } else if (is.Date(data)){
    dates <- data
  } else {
    stop ("Data must be data frame with `date` column or vector of dates")
  }
  
  start_date <- min(dates)
  end_date <- max(dates)
  
  # Create date objects for ticks/labels (e.g., show ticks every n.week.ticks, but label every n.week.labels.panel)
  ticks <- seq.Date(
    from = start_date,
    to = end_date,
    by = tick_freq
  )
  
  labs_short <- seq.Date(
    from = start_date,
    to = end_date,
    by = label_freq
  ) %>%
    format(label_format) %>%
    as.character
  
  labs <- ticks %>%
    format(label_format) %>%
    as.character
  
  label_shift <- ifelse(label_last, 1, 0)
  
  labs[!(labs %in% labs_short) - label_shift] <- ""
  
  tick.cols <- ifelse(labs == "", "grey70", "black")
  
  return(
    list(
      ticks = ticks,
      labels = labs,
      tick.cols = tick.cols
    )
  )
}

get_omicron_infections <- function(
  local_cases,
  ascertainment_rates,
  state_population
){
  
  omicron_infections_only <- local_cases %>%
    mutate(
      year = year(date) %>% as.integer,
      week = isoweek(date) %>% as.integer,
      month = month(date) %>% as.integer,
      yearweek = if_else(
        week == 52 & month == 1,
        sprintf("%s%02d", year - 1, week),
        sprintf("%s%02d", year, week)
      )
    ) %>%
    group_by(state, yearweek) %>%
    mutate(
      num_people = sum(cases)
    ) %>%
    filter(date == min(date)) %>%
    ungroup %>%
    mutate(
      num_people = if_else(
        date < "2021-12-01",
        0,
        num_people
      )
    ) %>%
    select(date, state, num_people) %>%
    expand_grid(
      ascertainment = ascertainment_rates
    ) %>%
    mutate(
      num_people = num_people/ascertainment
    )
  
  
  not_infected <- omicron_infections_only %>% 
    group_by(state, ascertainment) %>%
    summarise(
      num_people = sum(num_people)
    ) %>%
    left_join(
      state_population,
      by = "state"
    ) %>%
    mutate(
      num_people = population - num_people
    ) %>%
    mutate(
      date = NA_Date_
    ) %>%
    select(date, state, num_people, ascertainment)
  
  
  omicron_infections <- bind_rows(
    omicron_infections_only,
    not_infected
  ) %>%
    nest(
      "omicron_infections" = -ascertainment
    )
  
  return(omicron_infections)
}

get_infection_cohorts_at_date <- function(infection_series, target_date) {
  
  # set future times to NA and collapse to get contemporary data
  infection_series %>%
    mutate(
      across(
        starts_with("date"),
        ~if_else(.x > target_date, as.Date(NA), .x)
      )
    ) %>%
    group_by(
      across(
        -num_people
      )
    ) %>%
    summarise(
      num_people = sum(num_people),
      .groups = "drop"
    ) %>%
    mutate(
      immunity = case_when(
        !is.na(date) ~ "omicron_infection",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      days_ago = as.numeric(target_date - date)
    )
  
}


get_coverage_infection <- function(infection_cohort) {
  
  # get current coverage with any dose in each age band, for each scenario
  coverage <- infection_cohort %>%
    mutate(
      immune = !is.na(immunity)
    ) %>%
    group_by(
      state
    ) %>%
    summarise(
      coverage = weighted.mean(immune, num_people),
      .groups = "drop"
    )
  
}




get_infection_efficacies_vax <- function(
  vaccine_cohorts,
  infection_cohorts, 
  variants = c("Omicron BA2", "Omicron BA4/5"),
  neut_immune_escape = 0.44
) {
  
  # load omicron parameters in wide format and subset to different parameter sets
  params_wide <- get_omicron_params_wide()
  
  neut_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      log10_mean_neut_AZ_dose_1,
      log10_mean_neut_AZ_dose_2,
      log10_mean_neut_Pfizer_dose_1,
      log10_mean_neut_Pfizer_dose_2,
      log10_mean_neut_mRNA_booster,
      #log10_mean_neut_omicron_infection,
      #additional_log10_mean_neut_omicron_infection,
      neut_decay
    )
  
  ve_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      starts_with("c50"),
      log_k,
      sd_log10_neut_titres,
      omicron_log10_neut_fold
    )
  
  
  combined_cohorts <- left_join(
    x = vaccine_cohorts %>%
      filter(!is.na(days_ago)) %>%
      rename(
        num_people_v = num_people,
        days_v = days_ago
      ) %>%
      group_by(
        scenario,
        state,
        age_band
      ) %>%
      mutate(
        weight_v = num_people_v / sum(num_people_v)
      ) %>%
      ungroup,
    y = infection_cohorts %>%
      filter(date >= "2021-12-06") %>%
      rename(
        num_people_i = num_people,
        days_i = days_ago
      ) %>%
      select(-date, -immunity) %>%
      group_by(state) %>%
      mutate(
        weight_i = num_people_i / sum(num_people_i)
      ) %>%
      ungroup,
    by = "state"
  ) %>%
    select(
      scenario,
      state,
      age_band,
      num_people_v,
      num_people_i,
      immunity,
      days_v,
      days_i,
      weight_v,
      weight_i
    ) %>%
    mutate(
      days_ago = pmin(days_v, days_i),
      weight = weight_v * weight_i
    ) %>%
    select(-days_v, -days_i, -weight_v, -weight_i)
  
  
  # compute the average neutralisation level (mean log10 neut fold of WT
  # convalescent) in each age group, scenario, and omicron scenario
  mean_neuts <- combined_cohorts %>%
    mutate(
      omicron_scenario = "estimate"
    ) %>%
    left_join(
      neut_params_wide,
      by = "omicron_scenario"
    ) %>%
    # compute the peak and waned log10 mean neuts for each cohort
    # where each cohort is that dose plus Omicron infection
    mutate(
      peak_neuts = case_when(
        immunity == "AZ_dose_1" ~ log10_mean_neut_AZ_dose_2,
        immunity == "AZ_dose_2" ~ log10_mean_neut_mRNA_booster,
        immunity == "Pf_dose_1" ~ log10_mean_neut_Pfizer_dose_2,
        immunity == "Pf_dose_2" ~ log10_mean_neut_mRNA_booster,
        immunity == "mRNA_dose_3" ~ log10_mean_neut_mRNA_booster,
        immunity == "mRNA_dose_4" ~ log10_mean_neut_mRNA_booster
      )
    ) %>%
    mutate(
      neuts = log10_neut_over_time(
        time = days_ago,
        maximum_log10_neut = peak_neuts,
        decay = neut_decay
      )
    ) %>%
    select(
      -starts_with("log10_mean_neut"),
      -peak_neuts,
      -neut_decay
    ) %>%
    mutate(
      weighted_neuts = weight * neuts
    ) %>%
    # average the mean neuts over cohorts and scenarios
    group_by(
      scenario, state, omicron_scenario, age_band
    ) %>%
    summarise(
      neuts = sum(weighted_neuts),
      .groups = "drop"
    )
  
  # now compute VEs against each outcome, for Omicron and Delta
  ves <- mean_neuts %>%
    left_join(
      ve_params_wide,
      by = "omicron_scenario"
    ) %>%
    # for omicron, adjust down the neuts
    full_join(
      tibble(
        variant = variants
      ),
      by = character()
    ) %>%    
    mutate(
      neuts = case_when(
        variant == "Omicron BA4/5" ~ neuts + log10(neut_immune_escape),
        TRUE ~ neuts
      )
    ) %>%
    # compute all the VEs in one shot with Gaussian integration
    pivot_longer(
      cols = starts_with("c50"),
      names_to = "outcome",
      values_to = "c50",
      names_prefix = "c50_"
    ) %>%
    mutate(
      ve = ve_from_mean_log10_neut(
        mean_log10_neut_vec = neuts,
        sd_log10_neut = sd_log10_neut_titres,
        log_k = log_k,
        c50_vec = c50,
        method = "gaussian"
      ),
      ve = if_else(is.nan(ve), 1, ve)
    ) %>%
    select(
      -neuts,
      -log_k,
      -sd_log10_neut_titres,
      -omicron_log10_neut_fold,
      -c50
    )
  
  ves
  
}


get_infection_efficacies_infection_only <- function(vaccine_cohorts, 
                                                    variants = c("Omicron BA2", "Omicron BA4/5"),
                                                    neut_immune_escape = 0.44) {
  
  # load omicron parameters in wide format and subset to different parameter sets
  params_wide <- get_omicron_params_wide()
  
  neut_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      # log10_mean_neut_AZ_dose_1,
      # log10_mean_neut_AZ_dose_2,
      # log10_mean_neut_Pfizer_dose_1,
      # log10_mean_neut_Pfizer_dose_2,
      # log10_mean_neut_mRNA_booster,
      # omicron_log10_neut_fold,
      log10_mean_neut_infection,
      #log10_mean_neut_omicron_infection,
      #additional_log10_mean_neut_omicron_infection,
      neut_decay
    ) %>%
    filter(omicron_scenario == "estimate")
  
  ve_params_wide <- params_wide %>%
    select(
      omicron_scenario,
      starts_with("c50"),
      log_k,
      sd_log10_neut_titres,
      omicron_log10_neut_fold
    ) %>%
    filter(omicron_scenario == "estimate")
  
  # compute the average neutralisation level (mean log10 neut fold of WT
  # convalescent) in each age group, scenario, and omicron scenario
  mean_neuts <- vaccine_cohorts %>%
    filter(
      !is.na(immunity)
    ) %>%
    full_join(
      tibble(
        omicron_scenario = c(
          # "intermediate",
          # "optimistic",
          "estimate"#,
          #"pessimistic"
        )
      ),
      by = character()
    ) %>%
    left_join(
      neut_params_wide,
      by = "omicron_scenario"
    ) %>%
    # compute the peak and waned log10 mean neuts for each cohort
    mutate(
      peak_neuts = case_when(
        # immunity == "AZ_dose_1" ~ log10_mean_neut_AZ_dose_1,
        # immunity == "AZ_dose_2" ~ log10_mean_neut_AZ_dose_2,
        # immunity == "Pf_dose_1" ~ log10_mean_neut_Pfizer_dose_1,
        # immunity == "Pf_dose_2" ~ log10_mean_neut_Pfizer_dose_2,
        # immunity == "mRNA_booster" ~ log10_mean_neut_mRNA_booster,
        immunity == "omicron_infection" ~ log10_mean_neut_infection
      )
    ) %>%
    mutate(
      neuts = log10_neut_over_time(
        time = days_ago,
        maximum_log10_neut = peak_neuts,
        decay = neut_decay
      )
    ) %>%
    select(
      -starts_with("log10_mean_neut"),
      -peak_neuts,
      -neut_decay
    ) %>%
    # average the mean neuts over cohorts and scenarios
    group_by(
      state, omicron_scenario,
    ) %>%
    summarise(
      neuts = weighted.mean(neuts, num_people),
      .groups = "drop"
    )
  
  # now compute VEs against each outcome, for Omicron and Delta
  ves <- mean_neuts %>%
    left_join(
      ve_params_wide,
      by = "omicron_scenario"
    ) %>%
    # for omicron, adjust down the neuts
    full_join(
      tibble(
        variant = variants
      ),
      by = character()
    ) %>%
    mutate(
      neuts = case_when(
        variant == "Omicron BA4/5" ~ neuts + log10(neut_immune_escape),
        TRUE ~ neuts
      )
    ) %>%
    # compute all the VEs in one shot with Gaussian integration
    pivot_longer(
      cols = starts_with("c50"),
      names_to = "outcome",
      values_to = "c50",
      names_prefix = "c50_"
    ) %>%
    mutate(
      ve = ve_from_mean_log10_neut(
        mean_log10_neut_vec = neuts,
        sd_log10_neut = sd_log10_neut_titres,
        log_k = log_k,
        c50_vec = c50,
        method = "gaussian"
      ),
      ve = if_else(is.nan(ve), 1, ve)
    ) %>%
    select(
      -neuts,
      -log_k,
      -sd_log10_neut_titres,
      -omicron_log10_neut_fold,
      -c50
    )
  
  
  ves
  
}



get_infection_transmission_effects <- function(vies, coverage) {
  
  
  # load quantium lookup tables
  dir <- get_quantium_data_dir()
  lookups <- get_quantium_lookups(dir = dir)
  
  age_breaks_quantium <-lookups$age %>%
    mutate(
      brk = sub(
        pattern = "\\-.*",
        replacement = "",
        x = age_band
      ) %>%
        sub(
          pattern = "\\+",
          replacement = "",
          x = .
        ) %>%
        as.numeric
    ) %>%
    pull(brk)
  
  labq <- length(age_breaks_quantium)
  
  age_breaks_quantium[labq + 1] <- Inf
  
  # get a conmat NGM for Australia
  australia_ngm <- baseline_matrix(age_breaks = age_breaks_quantium)
  
  age_band_factor <- levels(vies$age_band)
  
  # combine coverage and VEs to get transmission reduction for each rollout
  # scenario, and omicron scenario
  vies %>%
    ungroup %>%
    # add back in the younger age_groups
    complete(
      scenario,
      state,
      omicron_scenario,
      variant,
      outcome,
      age_band = unique(lookups$age$age_band),
      fill = list(
        ve = 0
      )
    ) %>%
    # get the two transmission VEs as columns
    filter(
      outcome %in% c("acquisition", "transmission")
    ) %>%
    pivot_wider(
      names_from = outcome,
      values_from = ve
    ) %>%
    group_by(
      scenario,
      state,
      omicron_scenario,
      variant
    ) %>%
    mutate(
      age_band = factor(age_band, levels = age_band_factor)
    ) %>%
    arrange(
      age_band,
      .by_group = TRUE
    ) %>%
    # join on coverages
    left_join(
      coverage %>%
        expand_grid(
          age_band = lookups$age$age_band
        ),
      by = c("age_band", "state")
    ) %>%
    # compute percentage reduction in acquisition and transmission in each age group
    mutate(
      acquisition_multiplier = 1 - acquisition * coverage,
      transmission_multiplier = 1 - transmission * coverage,
    ) %>%
    select(
      -acquisition,
      -transmission,
      -coverage
    ) %>%
    # transform these into matrices of reduction in transmission, matching the NGM
    summarise(
      transmission_reduction_matrix =
        list(
          outer(
            # 'to' groups are on the rows in conmat, and first element in outer is rows,
            # so acquisition first
            acquisition_multiplier,
            transmission_multiplier,
            FUN = "*"
          )
        ),
      .groups = "drop"
    ) %>%
    group_by(
      state,
      omicron_scenario,
      variant
    ) %>%
    mutate(
      ngm_unvaccinated = list(australia_ngm),
      ngm_vaccinated = list(ngm_unvaccinated[[1]] * transmission_reduction_matrix[[1]]),
      infection_effect = 1 - get_R(ngm_vaccinated[[1]]) / get_R(ngm_unvaccinated[[1]])
    ) %>%
    select(
      -transmission_reduction_matrix,
      -ngm_unvaccinated,
      -ngm_vaccinated
    ) %>%
    ungroup
  
}



combine_transmission_effects <- function(
  ves,
  coverage_vaccination,
  ies,
  coverage_infection,
  vies
) {
  
  # load quantium lookup tables
  dir <- get_quantium_data_dir()
  lookups <- get_quantium_lookups(dir = dir)
  
  age_breaks_quantium <-lookups$age %>%
    mutate(
      brk = sub(
        pattern = "\\-.*",
        replacement = "",
        x = age_band
      ) %>%
        sub(
          pattern = "\\+",
          replacement = "",
          x = .
        ) %>%
        as.numeric
    ) %>%
    pull(brk)
  
  labq <- length(age_breaks_quantium)
  
  age_breaks_quantium[labq + 1] <- Inf
  
  # get a conmat NGM for Australia
  australia_ngm <- baseline_matrix(age_breaks = age_breaks_quantium)
  
  age_band_factor <- levels(ves$age_band)
  
  # combine coverage and VEs to get transmission reduction for each rollout
  # scenario, and omicron scenario
  ves %>%
    filter(variant %in% c("Omicron BA2","Omicron BA4/5")) %>%
    # add back in the younger age_groups
    complete(
      scenario,
      state,
      omicron_scenario,
      variant,
      outcome,
      age_band = unique(lookups$age$age_band),
      fill = list(
        ve = 0
      )
    ) %>%
    rename(effect_vaccination = ve) %>%
    left_join(
      ies %>%
        rename(effect_infection = ve)
    ) %>%
    left_join(
      vies %>%
        rename(effect_infandvax = ve)
    ) %>%
    # correct missing young age VEs and re-factor age_bands
    mutate(
      effect_infandvax = ifelse(is.na(effect_infandvax), 0, effect_infandvax),
      age_band = factor(age_band, levels = age_band_factor)
    ) %>%
    # get the two transmission VEs as columns
    filter(
      outcome %in% c("acquisition", "transmission")
    ) %>%
    pivot_longer(
      cols = starts_with("effect_"),
      names_prefix = "effect_",
      names_to = "effect_type",
      values_to = "ve"
    ) %>%
    pivot_wider(
      names_from = c(outcome, effect_type),
      values_from = ve
    ) %>%
    group_by(
      scenario,
      state,
      omicron_scenario,
      variant
    ) %>%
    arrange(
      age_band,
      .by_group = TRUE
    ) %>%
    # join on coverages
    left_join(
      coverage_vaccination %>%
        rename(coverage_vaccination = coverage),
      by = c("age_band", "state", "scenario")
    ) %>%
    left_join(
      coverage_infection %>%
        rename(coverage_infection = coverage) %>%
        expand_grid(
          age_band = lookups$age$age_band
        ),
      by = c("age_band", "state")
    ) %>%
    # compute percentage reduction in acquisition and transmission in each age group
    mutate(
      p_infected_only = coverage_infection * (1 - coverage_vaccination),
      p_vaccinated_only = coverage_vaccination * (1 - coverage_infection),
      p_infandvax = coverage_vaccination * coverage_infection,
      weighted_acquisition = p_infected_only * acquisition_infection + p_vaccinated_only * acquisition_vaccination + p_infandvax * acquisition_infandvax,
      weighted_transmission = p_infected_only * transmission_infection + p_vaccinated_only * transmission_vaccination + p_infandvax * transmission_infandvax,
      acquisition_multiplier = 1 - weighted_acquisition,
      transmission_multiplier = 1 - weighted_transmission
    ) %>%
    # select(
    #   -acquisition,
    #   -transmission,
    #   -coverage
    # ) %>%
    # transform these into matrices of reduction in transmission, matching the NGM
    summarise(
      transmission_reduction_matrix =
        list(
          outer(
            # 'to' groups are on the rows in conmat, and first element in outer is rows,
            # so acquisition first
            acquisition_multiplier,
            transmission_multiplier,
            FUN = "*"
          )
        ),
      .groups = "drop"
    ) %>%
    group_by(
      state,
      omicron_scenario,
      variant
    ) %>%
    mutate(
      ngm_unvaccinated = list(australia_ngm),
      ngm_vaccinated = list(ngm_unvaccinated[[1]] * transmission_reduction_matrix[[1]]),
      combined_effect = 1 - get_R(ngm_vaccinated[[1]]) / get_R(ngm_unvaccinated[[1]])
    ) %>%
    select(
      -transmission_reduction_matrix,
      -ngm_unvaccinated,
      -ngm_vaccinated
    ) %>%
    ungroup
  
}

get_hospitalisation_ve <- function(coverage, ves, ...) {
  ves %>%
    filter(outcome == "hospitalisation") %>%
    
    left_join(coverage, by = c("scenario", "state", "age_band")) %>%
    complete(scenario, omicron_scenario, state, age_band, variant, outcome) %>%
    
    mutate(ve = replace_na(ve, 0),
           coverage = replace_na(coverage, 0),
           m_hosp = 1 - ve * coverage)
}

get_hospitalisation_ve_pop_mean <- function(coverage, ves, state_population_by_age_band, ...) {
  ves %>%
    filter(outcome == "hospitalisation") %>%
    
    left_join(coverage, by = c("scenario", "state", "age_band")) %>%
    complete(scenario, omicron_scenario, state, age_band, variant, outcome) %>%
    
    mutate(ve = replace_na(ve, 0),
           coverage = replace_na(coverage, 0),
           m_hosp = 1 - ve * coverage) %>%
    
    left_join(state_population_by_age_band, by = c("age_band", "state")) %>%
    
    group_by(scenario, omicron_scenario, variant, state) %>%
    summarise(m_hosp = sum(m_hosp * prop_age), .groups = "drop")
}

get_hospitalisation_vie_pop_mean <- function(coverage, ves, coverage_infection, ies, vies, state_population_by_age_band, ...) {
  
  
  coverage %>%
    left_join(ves %>% filter(variant != "Delta"), by = c("scenario", "state", "age_band")) %>%
    rename(coverage_vacc = coverage,
           effect_vacc = ve) %>%
    filter(outcome == "hospitalisation") %>%
    complete(scenario, omicron_scenario, state, age_band, variant, outcome) %>%
    
    left_join(coverage_infection %>% rename(coverage_inf = coverage), by = "state") %>%
    left_join(ies %>% rename(effect_inf = ve), by = c("omicron_scenario", "state", "variant", "outcome")) %>%
    left_join(vies %>% rename(effect_both = ve), by = c("scenario", "omicron_scenario", "state", "age_band", "variant", "outcome")) %>%
    mutate(coverage_vacc = replace_na(coverage_vacc, 0),
           effect_vacc = replace_na(effect_vacc, 0),
           effect_both = replace_na(effect_both, 0)) %>%
    
    mutate(p_vacc_only = coverage_vacc * (1 - coverage_inf),
           p_inf_only = coverage_inf * (1 - coverage_vacc),
           p_both = coverage_inf * coverage_vacc) %>%
    
    mutate(m_hosp = 1 - (p_vacc_only * effect_vacc + p_inf_only * effect_inf + p_both * effect_both)) %>%
    
    left_join(state_population_by_age_band, by = c("age_band", "state")) %>%
    
    group_by(scenario, omicron_scenario, variant, state) %>%
    summarise(m_hosp = sum(m_hosp * prop_age), .groups = "drop")
}


vaccine_age_bands_to_wider <- function(age_group_vacc) {
  case_when(
    age_group_vacc %in% c("0-4") ~ "0-4",
    age_group_vacc %in% c("5-11") ~ "5-11",
    age_group_vacc %in% c("12-15", "16-19") ~ "12-19",
    age_group_vacc %in% c("20-24", "25-29") ~ "20-29",
    age_group_vacc %in% c("30-34", "35-39") ~ "30-39",
    age_group_vacc %in% c("40-44", "45-49") ~ "40-49",
    age_group_vacc %in% c("50-54", "55-59") ~ "50-59",
    age_group_vacc %in% c("60-64", "65-69") ~ "60-69",
    age_group_vacc %in% c("70-74", "75-79") ~ "70-79",
    age_group_vacc %in% c("80+") ~ "80+")
}

#deal with empty ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)




impute_linelist <- function(
  linelist,
  notification_delay_cdf
) {
  ll <- linelist %>%
    arrange(state, date_confirmation, date_onset)
  
  missing_onsets <- is.na(ll$date_onset)
  
  ll_missing <- ll %>%
    filter(is.na(date_onset))
  
  ll_missing_summary <- ll_missing %>%
    group_by(state, date_confirmation) %>%
    summarise(n_onsets = n())
  
  # impute the onset dates (only 0.6% of cases) using expected value from time to
  # detection distribution. Do this outside dplyr to avoid duplicating slow computations
  imputed_onsets <- impute_onsets(
    confirmation_dates = ll_missing_summary$date_confirmation,
    states = ll_missing_summary$state,
    n_onsets = ll_missing_summary$n_onsets,
    notification_delay_cdf,
    method = "random"
  )
  
  # add back 
  ll$date_onset[missing_onsets] <- imputed_onsets
  
  ll %>%
    mutate(date = date_onset - 5)
}

impute_onsets <- function(
  confirmation_dates,
  states,
  n_onsets,
  notification_delay_cdf,
  method = c("expected", "random"),
  min_days = -10,
  max_days = 40
){
  
  method <- match.arg(method)
  
  onset_dates <- mapply(
    impute_many_onsets,
    confirmation_date = confirmation_dates,
    state = states,
    n_onsets = n_onsets,
    MoreArgs = list(
      notification_delay_cdf = notification_delay_cdf,
      method = method,
      min_days = min_days,
      max_days = max_days
    ),
    SIMPLIFY = FALSE
  )
  
  do.call(c, onset_dates)
  
  
}

impute_many_onsets <- function(
  confirmation_date,
  state,
  notification_delay_cdf,
  method = c("expected", "random"),
  min_days = -10,
  max_days = 40,
  n_onsets = 1
) {
  
  method <- match.arg(method)
  
  # get possible dates of onset
  delays <- seq(min_days, max_days) 
  possible_onset_dates <- confirmation_date - delays
  
  # probability of being detected this many days later (probability of detection
  # by this day, minus probability of detection by the previous day)
  surv_from <- notification_delay_cdf(delays - 1, possible_onset_dates, state)
  surv_to <- notification_delay_cdf(delays, possible_onset_dates, state)
  prob <- surv_from - surv_to
  
  # normalise to get probabilities of different delays
  prob <- prob / sum(prob)
  
  # compute either the expected time since onset, or draw a random one
  sim_delays <- switch(method,
                       expected = round(sum(delays * prob)),
                       random = sample(delays, n_onsets, prob = prob, replace = TRUE))
  
  # subtract to get expected date of symptom onset
  onset_dates <- confirmation_date - sim_delays
  return(onset_dates)
  
}

