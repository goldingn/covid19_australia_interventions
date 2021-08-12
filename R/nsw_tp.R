# estimate TP for NSW lgas, using lga-level mobility data, but keeping
# everything else as for the state

source("R/functions.R")

# get mobility change models for LGAs
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
)

lga_data <- data %>%
  filter(
    country_region == "Australia" & sub_region_1 == "New South Wales" & !is.na(sub_region_2)
  ) %>%
  tidyr::pivot_longer(
    ends_with("_percent_change_from_baseline"),
    names_to = "category",
    values_to = "trend"
  ) %>%
  dplyr::select(
    lga = sub_region_2,
    category = category,
    date = date,
    trend = trend
  ) %>%
  mutate(
    category = str_remove_all(category, "_percent_change_from_baseline"),
    category = str_replace_all(category, "_", " ")
  ) %>%
  # tidy up LGA names
  mutate(
    lga = str_remove(lga, "The Council of the City of "),
    lga = str_remove(lga, "The Council of the Municipality of "),
    lga = str_remove(lga, "The Council of the Shire of "),
    lga = str_remove(lga, "Council of the City of "),
    lga = str_remove(lga, "City of "),
    lga = str_remove(lga, "\\sCity|\\sShire"),
    lga = str_remove(lga, "\\sCouncil"),
  ) %>%
  mutate(
    datastream = str_c("Google: time at ", category)
  ) %>%
  dplyr::select(-category)

n_weeks_ahead <- 6
first_date <- min(lga_data$date)
last_date <- max(lga_data$date)

mobility_fitted <- lga_data %>%
  filter(
    !is.na(lga) & !is.na(trend)
  ) %>%
  mutate(
    state_long = "New South Wales",
    state = "NSW"
  ) %>%
  group_by(lga, datastream) %>%
  do(
    predict_mobility_trend(
      .,
      min_date = first_date,
      max_date = last_date + 7 * n_weeks_ahead
    )
  ) %>%
  ungroup()

# keep only the LGAs where we managed to fit a model (others have too-small
# sample sizes for Google to provide data on the metrics we care about)
lgas_to_keep <- mobility_fitted %>%
  filter(!is.na(datastream)) %>%
  select(lga, date, datastream, predicted_trend) %>%
  group_by(lga, date) %>%
  pivot_wider(
    names_from = datastream,
    values_from = predicted_trend
  ) %>%
  filter(
    !is.na(`Google: time at parks`),
    !is.na(`Google: time at residential`),
    !is.na(`Google: time at retail and recreation`),
    !is.na(`Google: time at transit stations`),
    !is.na(`Google: time at workplaces`)
  ) %>%
  pull(lga) %>%
  unique()

# the datastreams we need to predict from the contact rate model
datastreams_to_keep <- c(
  "Google: time at parks",
  "Google: time at residential",
  "Google: time at retail and recreation",
  "Google: time at transit stations",
  "Google: time at workplaces"
)

mobility_fitted_raw <- mobility_fitted
mobility_fitted <- mobility_fitted %>%
  filter(
    datastream %in% datastreams_to_keep,
    lga %in% lgas_to_keep
  ) %>%
  arrange(lga, date)

all_lgas <- na.omit(unique(mobility_fitted$lga))

for (this_lga in all_lgas) {
  
  mobility_fitted %>%
    filter(
      lga == this_lga,
      !is.na(datastream)
    ) %>%
    ggplot() +
    aes(date, fitted_trend) +
    geom_hline(
      yintercept = 0,
      colour = "grey80",
      linetype = 3
    ) +
    geom_vline(
      aes(xintercept = date),
      data = interventions() %>%
        filter(state == "NSW"),
      colour = "grey80"
    ) +
    geom_vline(
      aes(xintercept = last_date),
      colour = "grey80",
      linetype = 2
    ) +
    facet_wrap(
      ~datastream,
      ncol = 3,
      scales = "free"
    ) +
    geom_ribbon(
      aes(
        ymin = fitted_trend_lower,
        ymax = fitted_trend_upper
      ),
      fill = grey(0.9),
      colour = grey(0.8),
      size = 0.1
    ) +
    # fitted trend
    geom_line(
      aes(date, fitted_trend),
      colour = "gray40"
    ) +
    geom_point(
      aes(date, trend),
      size = 0.2,
      col = "purple"
    ) +
    coord_cartesian(
      xlim = c(as.Date("2020-03-01"), last_date)# + 7 * n_weeks_ahead)
    ) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b",
      limits = range(mobility_fitted$date)
    ) +
    xlab("") +
    ylab("") +
    ggtitle(
      sprintf(
        "Percentage change in selected mobility datastreams up to %s, %s",
        format(last_date, format = "%B %d"),
        format(last_date, format = "%Y")
      )
    ) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"))
  
  dpi <- 150
  ggsave(
    filename = sprintf(
      "outputs/nsw/NSW_%s_datastream_model_fit_%s.png",
      this_lga,
      last_date
    ),
    width = 1500 / dpi,
    height = 1000 / dpi,
    dpi = dpi,
    scale = 1.2,
    bg = "white"
  )
  
}

# save predictions in correct format for macro and mobility models
google_change_trends_lga <- mobility_fitted %>%
  filter(
    grepl("^Google: ", datastream)
  ) %>%
  mutate(
    change = 1 + (predicted_trend / 100),
    lga_datastream = paste(lga, datastream)
  ) %>%
  select(
    lga_datastream,
    state = state_long,
    datastream,
    change,
    date
  )


# convert these into macrodistancing (non-household contact) timeseries
location_change_trends <- google_change_trends_lga %>%
  mutate(location = case_when(
    datastream == "Google: time at residential" ~ "home",
    datastream == "Google: time at transit stations" ~ "transit",
    datastream == "Google: time at parks" ~ "public",
    datastream == "Google: time at workplaces" ~ "work",
    datastream == "Google: time at retail and recreation" ~ "retail",
    TRUE ~ "other"
  )) %>%
  filter(location != "other") %>%
  mutate(
    lga = str_remove(
      lga_datastream,
      paste0("\\s", datastream)
    )
  ) %>%
  select(-lga_datastream, -datastream, -state) %>% 
  arrange(
    lga,
    date
  ) %>%
  pivot_wider(
    names_from = location,
    values_from = change
  ) %>%
  relocate(
    lga,
    .before = everything()
  ) %>%
  mutate_at(
    vars(public, home, retail, transit, work),
    ~replace_na(., 1)
  ) %>%
  # location indicator needs to be called state for the prediction function to work
  rename(state = lga)

# load fitted macrodistancing model
macro_model <- readRDS("outputs/fitted_macro_model.RDS")


# replace location_change_trends with the new data
macro_model$data$location_change_trends <- location_change_trends

macro_predictions <- macrodistancing_model(macro_model$data, macro_model$params)
OC_t_state <- macro_predictions$mean_daily_contacts
pred_sim <- calculate(c(OC_t_state), values = macro_model$draws, nsim = 1000)[[1]][, , 1]
quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

# predicted trends for downstream modelling
pred_trend <- macro_model$data$location_change_trends %>%
  select(date, state) %>%
  # add predictions
  mutate(mean = colMeans(pred_sim)) %>%
  bind_cols(as_tibble(quants))

saveRDS(pred_trend,
        file = "outputs/macrodistancing_trends_lga.RDS")

# combine these with NSW data for other components to get TP for each LGA

# load fitted reff model
fitted_model <- readRDS("outputs/fitted_reff_model.RDS")


n_lga <- length(all_lgas) 
ga <- fitted_model$greta_arrays
nsw_idx <- which(fitted_model$data$states == "NSW")
all_dates <- unique(location_change_trends$date)
n_dates <- length(all_dates)

# get index between dated for fitted model and for prediction
start_idx <- as.numeric(min(all_dates) - min(fitted_model$data$dates$mobility))
extra_dates <- as.numeric(max(all_dates) - max(fitted_model$data$dates$mobility))
date_idx <- pmin(start_idx + seq_len(n_dates), n_dates)


# time in household by LGA
h_t <- location_change_trends %>%
  select(state, date, home) %>%
  pivot_wider(names_from = state, values_from = home) %>%
  select(-date) %>%
  as.matrix()

# non-household contact rates by LGA
OC_t_lga <- trends_date_state(
  "outputs/macrodistancing_trends_lga.RDS",
  dates = all_dates
)

surveillance_effect <- ga$surveillance_reff_local_reduction[date_idx, nsw_idx]
extra_isolation_effect <- ga$extra_isolation_local_reduction[date_idx, nsw_idx]

de <- ga$distancing_effect
infectious_days <- infectious_period(gi_cdf)
HD_t <- de$HD_0 * h_t

# get the probability of not transmitting per unit time, for Delta
# p_star <- de$p_star[nrow(de$p_star), nsw_idx]

p_star_nsw <- extend(de$p_star[, nsw_idx], n_rows = nrow(de$p_star) + extra_dates)
p_star_nsw <- p_star_nsw[(start_idx + 1):length(p_star_nsw)]
p_star_lga <- sweep(zeros(nrow(OC_t_lga), ncol(OC_t_lga)), 1, p_star_nsw, FUN = "+")

# get the microdistancing effect, lining up dates
gamma_t_nsw <- extend(de$gamma_t_state[, nsw_idx], n_rows = nrow(de$gamma_t_state) + extra_dates)
gamma_t_nsw <- gamma_t_nsw[(start_idx + 1):length(gamma_t_nsw)]
gamma_t_lga <- sweep(zeros(nrow(OC_t_lga), ncol(OC_t_lga)), 1, gamma_t_nsw, FUN = "+")

household_infections <- de$HC_0 * (1 - p_star_lga ^ HD_t)
non_household_infections <- OC_t_lga * gamma_t_lga * infectious_days * (1 - p_star_lga ^ de$OD_0)
infections_distancing <- household_infections + non_household_infections
infections <- sweep(infections_distancing, 1, surveillance_effect * extra_isolation_effect, FUN = "*")

reff_sims <- calculate(c(infections), values = fitted_model$draws, nsim = 1000)[[1]][, , 1]
quants <- t(apply(reff_sims, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

reff_trend <- macro_model$data$location_change_trends %>%
  select(date, state) %>%
  # add predictions
  mutate(mean = colMeans(reff_sims)) %>%
  bind_cols(as_tibble(quants)) %>%
  rename(
    lga = state
  )

# load vaccination effect estimates
vaccine_effect <- read_csv(
  "outputs/nsw/nsw_lgas_vaccination_effect.csv",
  col_types = cols(
    lga = col_character(),
    date = col_date(format = ""),
    forecast = col_logical(),
    vaccination_transmission_multiplier = col_double(),
    vaccination_transmission_reduction_percent = col_double()
  )
) %>%
  mutate(
    lga = str_remove(lga, " \\(A\\)"),
    lga = str_remove(lga, " \\(C\\)"),
    lga = str_remove(lga, " \\(NSW\\)"),
  )



# add a lookup between Reff names and vaccine names
lga_lookup <- tibble::tribble(
  ~lga_reff, ~lga_vaccine,
  "Canterbury", "Canterbury-Bankstown",
  "Bankstown", "Canterbury-Bankstown",
  "MidCoast", "Mid-Coast",
  "Strathfield Municipal", "Strathfield",
  "Sutherland", "Sutherland Shire",
  "The Hills", "The Hills Shire",
  "Woolahra Municipal", "Woolahra"
)

reff_trend_vaccination <- reff_trend %>%
  left_join(
    lga_lookup,
    by = c("lga" = "lga_reff")
  ) %>%
  mutate(
    lga_vaccine = case_when(
      is.na(lga_vaccine) ~ lga,
      TRUE ~ lga_vaccine
    )
  ) %>%
  filter(
    lga_vaccine %in% vaccine_effect$lga
  ) %>%
  inner_join(
    vaccine_effect,
    by = c("lga_vaccine" = "lga", "date")
  ) %>%
  mutate(
    across(
      c(mean, starts_with("ci")),
      ~ . * vaccination_transmission_multiplier
    )
  ) %>%
  select(
    -starts_with("vaccination")
  )

# save the reff trend outputs
reff_trend %>%
  write_csv("outputs/nsw/tp_trends_no_vacc.csv")

reff_trend_vaccination %>%
  write_csv("outputs/nsw/tp_trends_with_vacc.csv")

for (this_lga in unique(reff_trend_vaccination$lga)) {
  
  reff_trend_vaccination %>%
    filter(lga == this_lga) %>%
    ggplot(
      aes(
        x = date,
        y = mean,
        linetype = forecast
      )
    ) +
    geom_ribbon(
      aes(
        ymax = ci_90_hi,
        ymin = ci_90_lo
      ),
      fill = green,
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(
        ymax = ci_50_hi,
        ymin = ci_50_lo
      ),
      fill = green,
      alpha = 0.2
    ) +
    geom_line(
      aes(y = ci_90_lo),
      colour = green,
      alpha = 0.8
    ) + 
    geom_line(
      aes(y = ci_90_hi),
      colour = green,
      alpha = 0.8
    ) +
    geom_hline(
      yintercept = 1,
      linetype = 2,
      colour = grey(0.5)
    ) +
    # geom_line() +
    geom_vline(
      aes(xintercept = date),
      data = interventions() %>%
        filter(state == "NSW"),
      colour = "grey75"
    ) +
    geom_vline(
      #data = prop_voc_date_state(),
      data = prop_variant_dates(),
      aes(xintercept = date),
      colour = "firebrick1",
      linetype = 5
    ) +
    ylab("Transmission potential") +
    xlab("") +
    ggtitle(this_lga) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%e/%m"
    ) +
    # coord_cartesian(xlim = c(as.Date("2021-01-27"), last_date)) +
    theme_cowplot() +
    theme(
      legend.position = "none"
    )
  
  ggsave(
    paste0("outputs/nsw/NSW_", this_lga, "_reff.png"),
    width = 1000 / dpi,
    height = 600 / dpi,
    bg = "white"
  )
  
}
