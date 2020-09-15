library(readr)
library(dplyr)
library(stringr)
library(rjson)
library(tidyr)
library(greta)
library(greta.gp)
library(readxl)
library(RColorBrewer)
library(tensorflow)
library(purrr)

tfp <- reticulate::import("tensorflow_probability")

module <- greta::.internals$utils$misc$module

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

# try a bunch of previous days to find the most recent citymapper dataset
citymapper_url <- function(min_delay = 0, max_delay = 14) {
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
    if (RCurl::url.exists(url)) {
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
    scale_x_date(date_breaks = "1 month", date_labels = "%d/%m") +
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

plot_fit <- function(observed_cases, cases_sim, model) {
  
  valid <- which(model$data$valid_mat, arr.ind = TRUE)
  dates <- model$data$dates$infection
  states <- model$data$states
  
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

# model for the trend in microdistancing
# compared to the macrodistancing effect, this has a linear transform of the
# distancing coefficient on the logit scale - corresponding to a different
# spread in adoption of microdistancing behaviour - and a different date of peak
# microdistancing and therefore different waning shape
microdistancing_model <- function(data,
                                  peak,
                                  inflections,
                                  distancing_effects,
                                  waning_effects,
                                  inflection_effects) {
  
  # shape of unscaled waning effect (0 at/before peak, to 1 at latest date)
  waning_shape <- (data$time - peak) / (1 - peak)
  nullify <- (sign(waning_shape) + 1) / 2
  waning_shape <- waning_shape * nullify
  
  # convert inflections to be after peak
  inflections <- peak + inflections * (1 - peak)
  
  # shapes of inflection effects
  inflections_vec <- inflections[data$state_id]
  inflection_shape <- (data$time - inflections_vec) / (1 - inflections_vec)
  
  nullify <- (sign(inflection_shape) + 1) / 2
  inflection_shape <- inflection_shape * nullify
  
  # multiply by coefficients to get trends for each state
  distancing <- data$distancing * distancing_effects[data$state_id]
  waning <- waning_shape * waning_effects[data$state_id]  
  inflection <- inflection_shape * inflection_effects[data$state_id]  
  
  # combine them all
  distancing + waning + inflection
  
}

# model for the trend in macrodistancing a weighted sum of time at location
# types, and an overall scaling coefficient, multiplied by a scalar baseline
# contact rate
macrodistancing_model <- function(data, parameters) {
  
  # format data into a date/state by location greta array
  log_location_changes <- data$location_change_trends %>%
    select(-state, -date) %>%
    as.matrix() %>%
    log() %>%
    as_data()
  
  # log of change in number of contacts compared to baseline
  log_diff_contacts <- log_location_changes %*% parameters$mobility_coefs
  
  # fraction of weekly contacts that are on weekends - as a function of change in numbers of contacts
  p_weekend <- ilogit(parameters$weekend_intercept + parameters$weekend_coef * log_diff_contacts)
  
  # average daily number of contacts
  log_avg_daily_contacts <- log(parameters$OC_0) + log_diff_contacts
  avg_daily_contacts <- exp(log_avg_daily_contacts)
  
  avg_daily_contacts_wide <- greta_long_to_date_state(
    avg_daily_contacts,
    data$location_change_trends$date,
    data$location_change_trends$state
  )
  
  p_weekend_wide <- greta_long_to_date_state(
    p_weekend,
    data$location_change_trends$date,
    data$location_change_trends$state
  )
  
  list(
    avg_daily_contacts = avg_daily_contacts_wide,
    p_weekend = p_weekend_wide
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
check_projection <- function(draws, 
                             model,
                             start_date = as.Date("2020-02-28")) {
  
  R_eff_local <- model$greta_arrays$R_eff_loc_12
  R_eff_imported <- model$greta_arrays$R_eff_imp_12
  gi_mat <- model$data$gi_mat
  gi_vec <- gi_vector(gi_cdf, model$data$dates$latest)
  local_infectiousness <- model$data$local$infectiousness
  imported_infectiousness <- model$data$imported$infectiousnes
  local_cases <- model$data$local$cases
  dates <- model$data$dates$infection
  n_states <- model$data$n_states
  n_dates <- model$data$n_dates
  
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
  import_local_cases <- rowSums(imported_infectiousness) * R_eff_imp_ntnl[seq_len(n_dates)]
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
macrodistancing_data <- function(dates = NULL) {
  
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
    contacts = contact_data,
    location_change_trends = location_change_trends
  )
  
}

macrodistancing_params <- function(baseline_contact_params) {
  
  # baseline number of non-household contacts, from Prem and Rolls
  OC_0 <- normal(baseline_contact_params$mean_contacts[2],
                 baseline_contact_params$se_contacts[2],
                 truncation = c(0, Inf))
  
  # coefficients for change in average contacts as a function of mobility indices
  mobility_coefs <- normal(0, 10, dim = 5)
  
  # coefficients for the fraction of weekly contacts that are on weekends, as a
  # function of the log diffference in contacts
  weekend_intercept <- normal(0, 1)
  weekend_coef <- normal(0, 10) 
  
  list(
    OC_0 = OC_0,
    mobility_coefs = mobility_coefs,
    weekend_intercept = weekend_intercept,
    weekend_coef = weekend_coef 
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
  
  # get expected number of contacts per respondent based on their date, state,
  # and the fraction of the contact survey period that was a weekend
  OC_t_state <- predictions$avg_daily_contacts
  p_weekend_t_state <- predictions$p_weekend
  avg_daily_contacts <- OC_t_state[idx]
  p_weekend <- p_weekend_t_state[idx]
  
  # weighting to account for the weekendiness of each survey response scale up
  # to weekly then down to avg on weekend/weekday, then weighted sum to get
  # expected count on the observed day:
  # week_total_contacts = avg_daily_contacts * 7
  # weekend_avg_daily_contacts = week_total_contacts * p_weekend / 2
  # weekday_avg_daily_contacts = week_total_contacts * (1 - p_weekend) / 5
  # expected_observed_contacts = weekend_avg_daily_contacts * weekend_fraction +
  #  weekday_avg_daily_contacts * (1  - weekend_fraction
  # factor out avg_daily contacts to get a weight for each observation
  weight <- p_weekend * data$contacts$weekend_fraction * 7 / 2 +
    (1 - p_weekend) * (1 - data$contacts$weekend_fraction) * 7 / 5
  predicted_contacts <- avg_daily_contacts * weight
  
  sqrt_inv_size <- normal(0, 0.5, truncation = c(0, Inf))
  size <- 1 / sqrt(sqrt_inv_size)
  prob <- 1 / (1 + predicted_contacts / size)
  distribution(data$contacts$contact_num) <- negative_binomial(size, prob)
  
  result <- list(size = size,
                 prob = prob,
                 weekend_weight = weight)
  
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

microdistancing_params <- function(n_locations = 8) {
  
  # timing of peak microdistancing between the date of the last intervention and
  # today's date
  peak <- normal(0, 1, truncation = c(0, 1))
  
  # timing of inflections between peak and now
  inflections <- normal(0, 1, truncation = c(0, 1), dim = n_locations)
  
  # hierarchical structure on state-level waning
  logit_waning_effects <- hierarchical_normal(n_locations)
  waning_effects <- -ilogit(logit_waning_effects)
  
  # hierarchical structure on state-level peak effect (proportion adhering) 
  logit_inflection_effects <- hierarchical_normal(n_locations)
  inflection_effects <- ilogit(logit_inflection_effects)
  
  # hierarchical structure on state-level peak effect (proportion adhering) 
  logit_distancing_effects <- hierarchical_normal(n_locations)
  distancing_effects <- ilogit(logit_distancing_effects)
  
  list(peak = peak,
       inflections = inflections,
       distancing_effects = distancing_effects,
       waning_effects = waning_effects,
       inflection_effects = inflection_effects)
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
  
  # use these dates if no others are specified
  if (is.null(dates)) {
    dates <- distancing$date
  }
  
  survey <- hygiene_data() %>%
    filter(
      date %in% dates
    )
  
  # get data to predict to
  pred_data <- distancing %>%
    rename(distancing = mean) %>%
    select(date, distancing) %>%
    old_right_join(
      expand_grid(
        date = dates,
        state = unique(survey$state)
      )
    ) %>%
    replace_na(list(distancing = 0)) %>%
    mutate(
      state_id = match(state, unique(state)),
      time = as.numeric(date - intervention_dates()$date[3]),
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

# given vectors of dates and numbers of days post infection, return the fraction
# of cases not being detected by that point
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

impute_onsets <- function(confirmation_dates,
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
clean_date <- function (original_date, min_date = as.Date("2020-01-01"), max_date = Sys.Date()) {
  weird <- !grepl("2020", original_date)
  corrected_date <- gsub("^\\d\\d\\d\\d", "2020", original_date)
  # don't use ifelse as it converts to a numeric
  date <- original_date
  date[weird] <- corrected_date[weird]
  
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
    grepl("^26", postcode) ~ "ACT",
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

linelist_date_times <- function(dir) {
  # find the files
  files <- list.files(dir, pattern = ".xlsx$", full.names = TRUE)
  # pull out the date time stamp
  date_time_text <- gsub("^COVID-19 UoM ", "", basename(files)) 
  date_time_text <- gsub(".xlsx$", "", date_time_text)
  date_times <- as.POSIXct(date_time_text, format = "%d%b%Y %H%M")
  # return as a dataframe
  tibble::tibble(
    file = files,
    date_time = date_times
  )
}

# copy over all new NNDSS linelist files from the shared drive to an unsynced local store
sync_nndss <- function(mount_dir = "~/Mounts/nndss", storage_dir = "~/not_synced/nndss") {
  from_files <- list.files(mount_dir, full.names = TRUE)
  existing_files <- list.files(storage_dir)
  new <- !(basename(from_files) %in% existing_files)
  files_to_read <- from_files[new]
  for (new_file in files_to_read) {
    file.copy(new_file, file.path(storage_dir, basename(new_file)), )
  }
}

# read in the latest linelist and format for analysis
get_nndss_linelist <- function(date = NULL, dir = "~/not_synced/nndss", strict = TRUE) {
  
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
    col_types <- c(
      STATE = "text",
      POSTCODE = "numeric",
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
    )
  }
  
  
  dat <- readxl::read_xlsx(
    data$file,
    col_types = col_types
  )
  
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
      SPECIMEN_DATE = clean_date(SPECIMEN_DATE)
    ) %>%
    mutate(
      import_status = ifelse(
        is.na(PLACE_OF_ACQUISITION) |
          grepl("^1101|^00038888", PLACE_OF_ACQUISITION),
        "local",
        "imported"
      )
    )
  
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
      state_of_acquisition = postcode_to_state(postcode_of_acquisition),
      state_of_residence = postcode_to_state(postcode_of_residence)
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
      state = STATE,
      import_status,
      postcode_of_acquisition,
      postcode_of_residence,
      state_of_acquisition,
      state_of_residence
    ) %>%
    mutate(
      report_delay = as.numeric(date_confirmation - date_onset),
      date_linelist = as.Date(data$date_time, tz = "Australia/Canberra"),
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

# replace VIC elements with VIC linelist
get_vic_linelist <- function(file) {
  
  linelist_date <- file %>%
    basename() %>%
    substr(1, 8) %>%
    as.Date(format = "%Y%m%d")
  
  file %>%
    read_csv(
      col_types = cols(
        PHESSID = col_double(),
        diagnosis_date = col_datetime(format = ""),
        ss_onset = col_datetime(format = ""),
        Localgovernmentarea = col_character(),
        acquired = col_character(),
        SPECIMEN_DATE = col_date(format = "%d/%m/%Y")
      ),
      na = "NULL"
    ) %>%
    mutate(
      date_onset = as.Date(ss_onset),
      date_confirmation = as.Date(diagnosis_date),
      date_detection = clean_date(SPECIMEN_DATE),
      state = "VIC",
      import_status = case_when(
        acquired == "Travel overseas" ~ "imported",
        TRUE ~ "local"
      ),
      postcode_of_acquisition = "8888",
      postcode_of_residence = "8888",
      state_of_acquisition = NA,
      state_of_residence = NA,
      report_delay = NA,
      date_linelist = linelist_date
    ) %>%
    # the mode and mean of the delay from testing to confirmation in VIC is around 3 days at the moment
    mutate(
      date_detection = case_when(
        is.na(date_detection) ~ date_confirmation - 3,
        TRUE ~ date_detection
      )
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
  
}

get_output_directories <- function(
  dirs = c("outputs", "outputs/projection", "outputs/fixed_projection"),
  staging = FALSE,
  staging_name = "staging"
) {
  
  # amend the directories for staging if needed
  if (staging) {
    dirs <- file.path(dirs, staging_name)
  }
  
  # make sure the output directories exist 
  . <- dirs %>%
    file.path("figures") %>%
    lapply(
      FUN = dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    )
  
  dirs
  
}


# given a raw (unimputed) linelist, prepare all the data needed for modelling
reff_model_data <- function(linelist_raw,
                            google_change_data,
                            n_weeks_ahead = 6,
                            inducing_gap = 3) {
  
  linelist_date <- linelist_raw$date_linelist[1]
  
  # truncate mobility data to no later than the day before the linelist
  google_change_data <- google_change_data %>%
    filter(date < linelist_date)
  
  # compute delays from symptom onset to detection for each state over time
  notification_delay_cdf <- get_notification_delay_cdf(linelist_raw)
  
  # impute onset dates and infection dates using this
  linelist <- linelist_raw %>%
    impute_linelist(notification_delay_cdf = notification_delay_cdf)
  
  # get linelist date and state information
  earliest_date <- min(linelist$date)
  latest_date <- max(linelist$date)
  latest_mobility_date <- max(google_change_data$date)
  
  states <- sort(unique(linelist$state))
  dates <- seq(earliest_date, latest_date, by = 1)
  mobility_dates <- seq(earliest_date, latest_mobility_date, by = 1)
  
  n_states <- length(states)
  n_dates <- length(dates)
  n_extra <- as.numeric(Sys.Date() - max(dates)) + 7 * n_weeks_ahead
  date_nums <- seq_len(n_dates + n_extra)
  n_date_nums <- length(date_nums)
  
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
  detectable <- detection_prob_mat >= 0.5
  
  # the last date with infection data we include
  last_detectable_idx <- which(!apply(detectable, 1, any))[1]
  latest_infection_date <- dates[last_detectable_idx]
  
  # those infected in the state
  local_cases <- linelist %>%
    filter(!interstate_import) %>%
    infections_by_region(
      region_type = "state",
      case_type = "local"
    )
  
  # and those infected in any state, but infectious in this one
  local_cases_infectious <- linelist %>%
    infections_by_region(
      region_type = "state",
      case_type = "local"
    )
  
  # those imported (only considered infectious, but with a different Reff)
  imported_cases <- linelist %>%
    infections_by_region(
      region_type = "state",
      case_type = "imported"
    )
  
  # Circulant matrix of generation interval discrete probabilities
  # use Nishiura's serial interval as a generation interval
  # gi_cdf <- nishiura_cdf()
  gi_mat <- gi_matrix(gi_cdf, dates, gi_bounds = c(0, 20))
  
  # correct Reff denominator for right-truncation (infectors not yet detected) by
  # expectation (resolving divide-by-zero error)
  detection_prob_mat[] <- pmax(detection_prob_mat, 1e-6)
  local_cases_infectious_corrected <- local_cases_infectious /  detection_prob_mat
  imported_cases_corrected <- imported_cases / detection_prob_mat
  
  # disaggregate imported and local cases according to the generation interval
  # probabilities to get the expected number of infectious people in each state
  # and time
  local_infectiousness <- gi_mat %*% local_cases_infectious_corrected
  imported_infectiousness <- gi_mat %*% imported_cases_corrected  
  
  # elements to exclude due to a lack of infectiousness
  local_valid <- is.finite(local_infectiousness) & local_infectiousness > 0
  import_valid <- is.finite(imported_infectiousness) & imported_infectiousness > 0
  valid_mat <- (local_valid | import_valid) & detectable
  
  # return a named, nested list of these objects
  list(
    local = list(
      cases = local_cases,
      cases_infectious = local_cases_infectious,
      infectiousness = local_infectiousness
    ),
    imported = list(
      cases = imported_cases,
      infectiousness = imported_infectiousness
    ),
    detection_prob_mat = detection_prob_mat,
    gi_mat = gi_mat,
    valid_mat = valid_mat,
    states = states,
    dates = list(
      infection = dates,
      onset = dates + 1,
      date_nums = date_nums,
      inducing_date_nums = inducing_date_nums,
      mobility = mobility_dates,
      earliest = earliest_date,
      latest = latest_date,
      latest_mobility = latest_mobility_date,
      latest_infection = latest_infection_date,
      linelist = linelist_date
    ),
    n_dates = n_dates,
    n_states = n_states,
    n_date_nums = n_date_nums,
    n_inducing =  n_inducing
  )
  
}

reff_model <- function(model_data) {
  
  # reduction in R due to surveillance detecting and isolating infectious people
  dates_long <- data$dates$earliest + seq_along(data$dates$date_nums) - 1
  surveillance_reff_local_reduction <- surveillance_effect(
    dates = dates_long,
    cdf = gi_cdf
  )
  
  # the reduction from R0 down to R_eff for imported cases due to different
  # quarantine measures each measure applied during a different period. Q_t is
  # R_eff_t / R0 for each time t, modelled as a monotone decreasing step function
  # over three periods with increasingly strict policies
  quarantine_dates <- as.Date(c("2020-03-15", "2020-03-28"))
  
  q_index <- case_when(
    data$dates$infection < quarantine_dates[1] ~ 1,
    data$dates$infection < quarantine_dates[2] ~ 2,
    TRUE ~ 3,
  )
  q_index <- c(q_index, rep(3, data$n_date_nums - data$n_dates))
  
  # q_raw <- uniform(0, 1, dim = 3)
  log_q_raw <- -exponential(1, dim = 3)
  log_q <- cumsum(log_q_raw)
  log_Qt <- log_q[q_index]
  
  # The change in R_t for locally-acquired cases due to social distancing
  # behaviour, modelled as a sum of household R_t and non-household R_t
  # Non-household Reff is modelled as a function of the number of non-household
  # contacts per 24h (itself modelled from mobility data, calibrated against
  # contact surveys) and the relative transmission probability per contact,
  # inferred from surveys on micro-distancing behaviour.
  distancing_effect <- distancing_effect_model(data$dates$mobility, gi_cdf)
  
  # pull out R_t component due to distancing for locally-acquired cases, and
  # extend to correct length
  R_eff_loc_1_no_surv <- extend(distancing_effect$R_t, data$n_date_nums)
  
  # multiply by the surveillance effect
  R_eff_loc_1 <- sweep(
    R_eff_loc_1_no_surv,
    1,
    surveillance_reff_local_reduction,
    FUN = "*"
  )
  
  log_R_eff_loc_1 <- log(R_eff_loc_1)
  
  # extract R0 from this model and estimate R_t component due to quarantine for
  # overseas-acquired cases
  log_R0 <- log_R_eff_loc_1[1, 1]
  log_R_eff_imp_1 <- log_R0 + log_Qt
  R_eff_imp_1 <- exp(log_R_eff_imp_1)
  
  # temporally correlated errors in R_eff for local and imported cases - representing all the
  # stochastic transmission dynamics in the community, such as outbreaks in
  # communities with higher or lower tranmission rates, and interstate and
  # temporal variation in quarantine effectiveness not captured by the step
  # function
  
  kernel_O <- rbf(
    lengthscales = lognormal(3, 1),
    variance = normal(0, 0.5, truncation = c(0, Inf)) ^ 2,
  )
  
  epsilon_O <- epsilon_gp(
    date_nums = data$dates$date_nums,
    n_states = data$n_states,
    kernel = kernel_O,
    inducing_date_nums = data$dates$inducing_date_nums
  )
  
  kernel_L <- rational_quadratic(
    lengthscales = lognormal(3, 1),
    variance = normal(0, 0.5, truncation = c(0, Inf)) ^ 2,
    alpha = lognormal(3, 1)
  )
  
  epsilon_L <- epsilon_gp(
    date_nums = data$dates$date_nums,
    n_states = data$n_states,
    kernel = kernel_L,
    inducing_date_nums = data$dates$inducing_date_nums
  )
  
  # work out which elements to exclude (because there were no infectious people)
  valid <- which(data$valid_mat, arr.ind = TRUE)
  
  # log Reff for locals and imports
  log_R_eff_loc <- log_R_eff_loc_1 + epsilon_L
  
  log_R_eff_imp <- sweep(
    epsilon_O,
    1,
    log_R_eff_imp_1,
    FUN = "+"
  )
  
  R_eff_loc_12 <- exp(log_R_eff_loc)
  R_eff_imp_12 <- exp(log_R_eff_imp)
  
  # combine everything as vectors, excluding invalid datapoints (remove invalid
  # elements here, otherwise it causes a gradient issue)
  R_eff_loc <- exp(log_R_eff_loc[1:data$n_dates, ])
  R_eff_imp <- exp(log_R_eff_imp[1:data$n_dates, ])
  new_from_loc_vec <- data$local$infectiousness[valid] * R_eff_loc[valid]
  new_from_imp_vec <- data$imported$infectiousness[valid] * R_eff_imp[valid]
  expected_infections_vec <- new_from_loc_vec + new_from_imp_vec
  
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
    model = m,
    data = model_data,
    greta_arrays = module(
      expected_infections_vec,
      size,
      prob_trunc,
      R_eff_loc_1,
      R_eff_imp_1,
      R_eff_loc_12,
      R_eff_imp_12,
      log_R0,
      distancing_effect,
      surveillance_reff_local_reduction,
      log_R_eff_loc,
      log_R_eff_imp,
      epsilon_L,
      epsilon_O
    )
  )
  
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
reff_1_only_surveillance <- function(reff_model) {
  log_R0 <- reff_model$greta_arrays$log_R0
  reduction <- reff_model$greta_arrays$surveillance_reff_local_reduction
  exp(log_R0 + log(reduction))
}

# reff component 1 if only macrodistancing had changed
reff_1_only_macro <- function(reff_model) {
  ga <- reff_model$greta_arrays
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  h_t <- h_t_state(reff_model$data$dates$mobility)
  HD_t <- de$HD_0 * h_t
  household_infections_macro <- de$HC_0 * (1 - de$p ^ HD_t)
  non_household_infections_macro <- de$OC_t_state * infectious_days * (1 - de$p ^ de$OD_0)
  hourly_infections_macro <- household_infections_macro + non_household_infections_macro
  hourly_infections_macro_extended <- extend(
    hourly_infections_macro,
    reff_model$data$n_date_nums
  )
  hourly_infections_macro_extended * baseline_surveillance_effect
}

# reff component 1 if only macrodistancing had changed
reff_1_only_micro <- function(reff_model) {
  ga <- reff_model$greta_arrays
  baseline_surveillance_effect <- ga$surveillance_reff_local_reduction[1]
  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  household_infections_micro <- de$HC_0 * (1 - de$p ^ de$HD_0)
  non_household_infections_micro <- de$OC_0 * infectious_days *
    (1 - de$p ^ de$OD_0) * de$gamma_t_state
  hourly_infections_micro <- household_infections_micro +
    non_household_infections_micro
  hourly_infections_micro_extended <- extend(
    hourly_infections_micro,
    reff_model$data$n_date_nums
  )
  hourly_infections_micro_extended * baseline_surveillance_effect
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

fit_reff_model <- function(model, max_tries = 3, iterations_per_step = 1000) {
  
  # first pass at model fitting  
  draws <- mcmc(
    model$model,
    sampler = hmc(Lmin = 25, Lmax = 30),
    chains = 10,
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
  
  draws
  
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
write_local_cases <- function(model_data, file = "outputs/local_cases_input.csv") {
  
  tibble::tibble(
    date_onset = rep(model_data$dates$onset, model_data$n_states),
    detection_probability = as.vector(model_data$detection_prob_mat),
    state = rep(model_data$states, each = model_data$n_dates),
    count = as.vector(model_data$local$cases_infectious),
    acquired_in_state = as.vector(model_data$local$cases)
  ) %>%
    write.csv(file, row.names = FALSE)
  
}

# save the whole fitted Reff model to disk
write_fitted_reff <- function(model, draws, file = "outputs/fitted_reff.RDS") {
  object <- list(
    model = model,
    draws = draws
  )
  saveRDS(object, file)
}

# plot visual checks of model posterior calibration against observed data
plot_reff_ppc_checks <- function(draws, model, nsim = 10000) {
  
  cases <- negative_binomial(
    model$greta_arrays$size,
    model$greta_arrays$prob_trunc
  )
  cases_sim <- calculate(cases, values = draws, nsim = nsim)[[1]][, , 1]
  
  valid <- which(model$data$valid_mat, arr.ind = TRUE)
  observed <- model$data$local$cases[valid]
  
  # overall PPC check
  bayesplot::ppc_ecdf_overlay(
    observed,
    cases_sim[1:1000, ],
    discrete = TRUE
  )
  
  # check by state and time
  plot_fit(observed, cases_sim, model)
  
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

impute_linelist <- function(linelist, notification_delay_cdf) {
  
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
}

load_vic <- function (file) {
  get_vic_linelist(file) %>%
    impute_linelist()
}

load_linelist <- function(date = NULL, use_vic = FALSE) {
  
  # load the latest NNDSS linelist (either the latest or specified file)
  linelist <- get_nndss_linelist(date = date)
  
  # optionally replace VIC data with DHHS direct upload
  if (use_vic) {
    
    vic_linelist <- linelist$date_linelist[1] %>%
      format(format = "%Y%m%d") %>%
      paste0("~/not_synced/vic/", ., "_linelist_reff.csv") %>%
      get_vic_linelist()
    
    linelist <- linelist %>%
      filter(state != "VIC") %>%
      bind_rows(vic_linelist)
    
  }
  
  # flag whether each case is an interstate import
  linelist <- linelist %>%
    mutate(
      interstate_import = case_when(
        state != state_of_acquisition ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  linelist
  
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
    scale <- 0.95 * scale
    
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
    bind_rows()
  
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

# colours for plotting
blue <- "steelblue3"
purple <- "#C3A0E8"
green <- brewer.pal(8, "Set2")[1]
yellow <- brewer.pal(8, "Set2")[6]
blue_green <- colorRampPalette(c("blue", green))(10)[8]
yellow_green <- colorRampPalette(c("yellow", green))(10)[8]
orange <- brewer.pal(8, "Set2")[2]
pink <- brewer.pal(8, "Set2")[4]

# default cdf
gi_cdf <- nishiura_cdf()
