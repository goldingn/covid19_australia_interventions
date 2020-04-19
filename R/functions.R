library(readr)
library(fs)
library(purrr)
library(dplyr)
library(stringr)
library(sf)
library(raster)

# read in and tidy up Facebook movement data
load_fb_data <- function() {
  
  # load all datasets
  data <- load_fb_csvs() 
  
  # fill in centroid coordinates for LGAs known to be missing them
  #   Meander Valley (M) 146.4871 -41.62647
  #   Hobart (C) 147.2873 -42.89699
  #   Tasman (M) 147.8344 -43.05465
  # see find_centroid()
  data <- data %>%
    mutate(start_y = case_when(
      start_polygon_names == "Meander Valley (M)" ~ -41.62647,
      start_polygon_names == "Hobart (C)" ~ -42.89699,
      start_polygon_names == "Tasman (M)" ~ -43.05465,
      TRUE ~ start_y
    )) %>%
    mutate(start_x = case_when(
      start_polygon_names == "Meander Valley (M)" ~ 146.4871,
      start_polygon_names == "Hobart (C)" ~  147.2873,
      start_polygon_names == "Tasman (M)" ~ 147.8344,
      TRUE ~ start_x
    ))%>%
    mutate(end_y = case_when(
      end_polygon_names == "Meander Valley (M)" ~ -41.62647,
      end_polygon_names == "Hobart (C)" ~ -42.89699,
      end_polygon_names == "Tasman (M)" ~ -43.05465,
      TRUE ~ end_y
    )) %>%
    mutate(end_x = case_when(
      end_polygon_names == "Meander Valley (M)" ~ 146.4871,
      end_polygon_names == "Hobart (C)" ~  147.2873,
      end_polygon_names == "Tasman (M)" ~ 147.8344,
      TRUE ~ end_x
    ))
    
  # fill in the state from the name stack, and scrape suffixes from the lga
  # names
  data <- data %>%
    mutate(
      start_state = str_split_fixed(start_name_stack, ' // ', n = 2)[, 1],
      end_state = str_split_fixed(end_name_stack, ' // ', n = 2)[, 1]
    ) %>%
    mutate(
      start_polygon_names = strip_suffix(start_polygon_names),
      end_polygon_names = strip_suffix(end_polygon_names)
    )
  
  # spread out the variables, drop unnecessary ones, and tidy up names
  data <- data %>%
    pivot_wider(names_from = metric_name,
                values_from = metric_value) %>%
    filter(level == "LEVEL3") %>%
    dplyr::select(-crisis_name,
                  -start_polygon_id, -end_polygon_id,
                  -start_name_stack, -end_name_stack,
                  -start_x, -start_y, -end_x, -end_y,
                  -n_difference, -percent_change, -z_score,
                  -level, -tile_size, -country) %>%
    rename(date = utc_date,
           start_lga = start_polygon_names,
           end_lga = end_polygon_names)
  
  data
  
}

load_fb_csvs <- function() {
  csv_files <- fs::dir_ls("../data/fb_data/interactive_admin", regexp = "\\.csv$")
  data <- csv_files %>%
    purrr::map_dfr(readr::read_csv)
  data
}

# get LGAs, transform to UTM 54S, simplify geometry
load_lgas <- function() {
  raster::getData("GADM", country = "AUS", level = 2) %>%
    as("sf") %>%
    st_transform(32754) %>%
    st_simplify(dTolerance = 1000)
}

# # read in LGA boundary polygons
# find_states <- function(lon, lat) {
#   
#   # get states, transform to UTM 54S, simplify geometry
#   lgas <- load_lgas()
#   
#   # create multipoint object and transform to UTM 54S
#   points <- map2(lon, lat, ~st_point(c(.x, .y))) %>%
#     st_sfc(crs = 4326) %>%
#     st_transform(32754) %>%
#     st_sf(id = 1)
#   
#   attributed <- points %>%
#     st_join(lgas, join = st_intersects)
#   
#   state <- attributed$NAME_1
#   state
#   
# }

# find the lat-long for the centroid of a region in the lgas data (used to get )
find_centroid <- function(lga_name) {
  load_lgas() %>%
    filter(NAME_2 == lga_name) %>%
    st_centroid %>%
    st_transform(4326) %>%
    st_coordinates
}


# list the lockdown region to which lga belongs
lockdown_regions <- function() {
  bind_rows(
    tibble::tibble(
      region = "Kimberley",
      lga = c(
        "Broome",
        "Derby-West Kimberley",
        "Halls Creek",
        "Wyndham-East Kimberley"
      )
    ),
    tibble::tibble(
      region = "Pilbara",
      lga = c("Ashburton", "East Pilbara", "Port Hedland", "Karratha")
    ),
    tibble::tibble(
      region = "Gascoyne",
      lga = c("Carnarvon", "Exmouth", "Shark Bay", "Upper Gascoyne")
    ),
    tibble::tibble(
      region = "Mid West",
      lga = c(
        "Carnamah",
        "Chapman Valley",
        "Coorow",
        "Cue",
        "Greater Geraldton",
        "Irwin",
        "Meekatharra",
        "Mingenew",
        "Morawa",
        "Mount Magnet",
        "Murchison",
        "Northampton",
        "Perenjori",
        "Sandstone",
        "Three Springs",
        "Wiluna",
        "Yalgoo"
      )
    ),
    tibble::tibble(
      region = "Wheatbelt",
      lga = c(
        "Beverley",
        "Brookton",
        "Bruce Rock",
        "Chittering",
        "Corrigin",
        "Cuballing",
        "Cunderdin",
        "Dalwallinu",
        "Dandaragan",
        "Dowerin",
        "Dumbleyung",
        "Gingin",
        "Goomalling",
        "Kellerberrin",
        "Kondinin",
        "Koorda",
        "Kulin",
        "Lake Grace",
        "Merredin",
        "Moora",
        "Mount Marshall",
        "Mukinbudin",
        "Narembeen",
        "Narrogin",
        "Northam",
        "Nungarin",
        "Pingelly",
        "Quairading",
        "Tammin",
        "Toodyay",
        "Trayning",
        "Victoria Plains",
        "Wagin",
        "Wandering",
        "West Arthur",
        "Westonia",
        "Wickepin",
        "Williams",
        "Wongan-Ballidu",
        "Wyalkatchem",
        "Yilgarn",
        "York"
      )
    ),
    tibble::tibble(
      region = "Perth & Peel",
      lga = c(
        "Armadale",
        "Bassendean",
        "Bayswater",
        "Belmont",
        "Cambridge",
        "Canning",
        "Claremont",
        "Cockburn",
        "Cottesloe",
        "East Fremantle",
        "Fremantle",
        "Gosnells",
        "Joondalup",
        "Kalamunda",
        "Kwinana",
        "Melville",
        "Mosman Park",
        "Mundaring",
        "Nedlands",
        "Peppermint Grove",
        "Perth",
        "Rockingham",
        "South Perth",
        "Stirling",
        "Subiaco",
        "Swan",
        "Victoria Park",
        "Vincent",
        "Wanneroo",
        "Boddington",
        "Murray",
        "Serpentine-Jarrahdale",
        "Waroona",
        "Mandurah"
      )
    ),
    tibble::tibble(
      region = "South West",
      lga = c(
        "Bunbury",
        "Busselton",
        "Augusta-Margaret River",
        "Boyup Brook",
        "Bridgetown-Greenbushes",
        "Capel",
        "Collie",
        "Dardanup",
        "Donnybrook-Balingup",
        "Harvey",
        "Manjimup",
        "Nannup"
      )
    ),
    tibble::tibble(
      region = "Great Southern",
      lga = c(
        "Albany",
        "Broomehill-Tambellup",
        "Cranbrook",
        "Denmark",
        "Gnowangerup",
        "Jerramungup",
        "Katanning",
        "Kent",
        "Kojonup",
        "Plantagenet",
        "Woodanilling"
      )
    ),
    tibble::tibble(
      region = "Goldfields-Esperance",
      lga = c(
        "Coolgardie",
        "Dundas",
        "Esperance",
        "Kalgoorlie/Boulder",
        "Laverton",
        "Leonora",
        "Menzies",
        "Ngaanyatjarraku",
        "Ravensthorpe"
      )
    )
  )
}

# get summarised movements between lockdown regions as an od matrix
get_od_matrix <- function (region_data, variable) {
  od_tibble <- region_data %>%
    dplyr::select(!!variable, start_region, end_region) %>%
    pivot_wider(names_from = end_region, values_from = !!variable)
  
  od_matrix <- od_tibble %>%
    dplyr::select(-start_region) %>%
    as.matrix
  row.names(od_matrix) <- od_tibble$start_region
  col_order <- order(colnames(od_matrix))
  row_order <- order(rownames(od_matrix))
  od_matrix <- od_matrix[row_order, col_order]
  od_matrix
}

strip_suffix <- function(x) {
  gsub("\\s*\\([^\\)]+\\)", "", x)
}

# compute the degree of evidence for a change in the ratio, based on the number of movements
one_p_value <- function (n_crisis, n_baseline) {
  
  if (is.na(n_crisis) | is.na(n_baseline)) {
    return (NA)
  }
  
  m <- glm(n_crisis ~ offset(log(n_baseline)), family = stats::poisson)
  summary(m)$coefficients[4]
}
p_values <- Vectorize(one_p_value, c("n_crisis", "n_baseline"))


# load and format Jono Carroll's scraping of Aus mobility data
# - remove the grocery and pharmacy category
# (affected by panic buying, not interventions)
google_mobility <- function() {
  url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  data <- readr::read_csv(url, ) %>%
    filter(country_region == "Australia") %>%
    tidyr::pivot_longer(ends_with("_percent_change_from_baseline"),
                        names_to = "category",
                        values_to = "trend") %>%
    mutate(source = "Google") %>%
    dplyr::select(
      state = sub_region_1,
      category = category,
      date = date,
      trend = trend
    ) %>%
    mutate(category = str_remove_all(category, "_percent_change_from_baseline"),
           category = str_replace_all(category, "_", " "),
    ) %>%
    filter(category != "grocery and pharmacy")
  
  data
}

# download and format Apple's mobility data - will need to update the url regularly
apple_mobility <- function() {
  
  # a list of the regions we'se ideally be interested in. Apple only provides
  # data for a handful of these (Australia and the four biggest cities)
  ideal_regions <- c(
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
  
  url <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev8/v1/en-us/applemobilitytrends-2020-04-17.csv"
  data <- readr::read_csv(url) %>%
    tidyr::pivot_longer(cols = starts_with("2020-"),
                        names_to = "date",
                        values_to = "trend") %>%
    mutate(date = lubridate::date(date)) %>%
    filter(region %in% ideal_regions)
  
  # add on a state label, rename the transportation type to 'category' to match
  # google, and add on the data source
  data <- data %>%
    mutate(state = case_when(region == "Sydney" ~ "New South Wales",
                             region == "Melbourne" ~ "Victoria",
                             region == "Brisbane" ~ "Queensland",
                             region == "Perth" ~ "Western Australia",
                             TRUE ~ as.character(NA))) %>%
    rename(category = transportation_type) %>%
    mutate(source = "Apple")
  
  data
   
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
summarise_vec_posterior <- function(vector, draws) {
  vector_draws <- calculate(vector, values = draws)[[1]]
  vector_mat <- as.matrix(vector_draws)
  posterior_mean <- colMeans(vector_mat)
  posterior_ci <- t(apply(vector_mat, 2, quantile, c(0.025, 0.975)))
  cbind(mean = posterior_mean, posterior_ci)
}

# add a polygon for a credible interval to a base plot
add_ci_poly <- function(posterior_summary,
                        dates,
                        col = grey(0.8),
                        border_col = grey(0.6)) {
  polygon(x = c(dates, rev(dates)),
          y = c(posterior_summary[, 2],
                rev(posterior_summary[, 3])),
          lwd = 0.5,
          col = col,
          border = border_col)
}