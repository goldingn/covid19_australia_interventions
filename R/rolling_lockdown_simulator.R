# simulate the impact of strategies for local lockdowns in response to different
# case count triggers
source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)
source("./objects_and_settings.R")

set.seed(2020-07-04)

# locally-acquired cases by LGA
local_cases <- latest_lga_cases()

# Reff model parameter samples
parameter_draws <- readRDS("outputs/projection/postcode_forecast_draws.RDS")
n_samples <- 1000
size_samples <- parameter_draws$vic_size[seq_len(n_samples)]
r_eff_reduction_samples <- parameter_draws$vic_r_eff_reduction_full[seq_len(n_samples)]
leaving_reduction_samples <- runif(n_samples, 0.05, 0.3)
non_household_fraction <- parameter_draws$vic_fraction_non_household

# get the baseline fraction of new infections that are in another LGA
# from contact questions, 33% of trips by cases in the time prior to symptom
# onset were not within the same LGA
# from Reff model the fraction of transmisions that are to non-household members
# is around 45%
outside_transmission_fraction <- mean(non_household_fraction) * 0.33

# load LGA geometries and populations
# prep_lgas("Victoria")
# prep_state_lgas("New South Wales")
# prep_state_lgas("Australian Capital Territory")
vic_lga <- readRDS("data/spatial/vic_lga.RDS")
act_lga <- readRDS("data/spatial/act_lga.RDS")
nsw_lga <- readRDS("data/spatial/nsw_lga.RDS")

# crop out Norflok Island
nsw_lga <- st_crop(
  nsw_lga,
  c(
    xmin = 140,
    ymin = -38.5,
    xmax = 154,
    ymax = -27
  )
)

nsw_act_lga <- st_union(
  nsw_lga,
  act_lga
)

vic_border <- simple_border(vic_lga)
act_border <- simple_border(act_lga)
nsw_border <- simple_border(nsw_lga)

# get geometry for Melbourne & Mitchell lockdown region (for plotting)
vic_lockdown <- vic_lga %>%
  filter(lga %in% lockdown_lgas()) %>%
  simple_border()

# get list of NSW and ACT active case LGAs
nsw_act_source_lgas <- nsw_lga_active_cases() %>%
  select(
    lga_short = Local.Government.Area,
    cases = Cases
  ) %>%
  filter(
    cases > 0
  ) %>%
  left_join(
    tibble(lga = nsw_lga$lga) %>%
      mutate(
        lga_short = lga_name(lga)
      ) %>%
      # need to deal with the new Bayside LGA (Botany bay and Rockdale combined)
      mutate(
        lga_short = case_when(
          lga %in% c("Botany Bay (C)", "Rockdale (C)") ~ "Bayside",
          TRUE ~ lga_short
        )
      )
  ) %>%
  pull(lga) %>%
  # add on all ACT
  c(act_lga$lga)

# importation rates from gravity models fitted to facebook data
vic_import_rate <- "data/facebook/vic_baseline_gravity_movement.RDS" %>%
  readRDS() %>%
  set_leaving_probability(outside_transmission_fraction)

nsw_act_import_rate <- "data/facebook/nsw_act_baseline_gravity_movement.RDS" %>%
  readRDS() %>%
  set_leaving_probability(outside_transmission_fraction)

vic_lga_infectious <- local_cases %>%
  filter(
    state == "VIC",
  ) %>%
  compute_infectious(
    lga = vic_lga,
    import_rate = vic_import_rate,
    sources = lockdown_lgas()
  )

nsw_act_lga_infectious <- local_cases %>%
  filter(
    state %in% c("NSW", "ACT"),
  ) %>%
  compute_infectious(
    lga = nsw_act_lga,
    import_rate = nsw_act_import_rate,
    sources = nsw_act_source_lgas
  )

# plot the infectious potential and import potential by LGA
p_vic_inf_area <- vic_lga_infectious %>%
  lga_map("infectious_potential_area",
          source_geometry = vic_lockdown,
          source_lockdown = TRUE,
          trans = "sqrt",
          risk_col = "blue") +
  border_line(vic_border) +
  labs(
    fill = "Infectious\npotential\nper sq. km"
  ) +
  ggtitle(
    "Infectious potential across VIC",
    paste(
      "Locally-acquired cases weighted by remaining",
      "potential to infect, as at",
      format(
        max(local_cases$date),
        format = "%d %B"
      )
    )
  )

p_vic_nl_imp <- vic_lga_infectious %>%
  lga_map("sink_import_potential",
          source_geometry = vic_lockdown,
          source_lockdown = TRUE,
          source_fill = grey(0.8),
          risk_col = "red") +
  border_line(vic_border) +
  labs(
    fill = "Relative risk"
  ) +
  ggtitle(
    "Case importation risk from restricted to non-restricted VIC LGAs",
    paste0(
      "Relative to the most at-risk non-restricted LGA (",
      max_risk_lga(vic_lga_infectious),
      ")"
    )
  )

ggsave(
  plot = p_vic_inf_area,
  filename = "outputs/figures/vic_infectious_potential_area_map.png",
  width = 8, height = 6,
  dpi = 250
)

ggsave(
  plot = p_vic_nl_imp,
  filename = "outputs/figures/vic_importation_potential_map.png",
  width = 8, height = 6,
  dpi = 250
)

# and to plot state boundaries

# get source lga shapefile
nsw_act_sources <- nsw_act_lga_infectious %>%
  filter(lga %in% nsw_act_source_lgas) %>%
  st_geometry() %>%
  st_union() %>%
  st_simplify(dTolerance = 0.001)

p_nsw_inf_area <- nsw_act_lga_infectious %>%
  lga_map("infectious_potential_area",
          trans = "sqrt",
          risk_col = "blue") +
  border_line(nsw_border) +
  labs(
    fill = "Infectious\npotential\nper sq. km"
  ) +
  ggtitle(
    "Infectious potential across NSW and ACT",
    paste(
      "Locally-acquired cases weighted by remaining",
      "potential to infect, as at",
      format(
        max(local_cases$date),
        format = "%d %B"
      )
    )
  )

p_nsw_nl_imp <- nsw_act_lga_infectious %>%
  lga_map("sink_import_potential",
          source_geometry = nsw_act_sources,
          source_fill = grey(0.8),
          risk_col = "red") +
  border_line(nsw_border) +
  labs(
    fill = "Relative risk"
  ) +
  ggtitle(
    "Case importation risk to case-free NSW LGAs",
    paste0(
      "Relative to the most at-risk case-free LGA (",
      max_risk_lga(nsw_act_lga_infectious),
      ")"
    )
  )

ggsave(
  plot = p_nsw_inf_area,
  filename = "outputs/figures/nsw_infectious_potential_area_map.png",
  width = 8, height = 6,
  dpi = 250
)

ggsave(
  plot = p_nsw_nl_imp,
  filename = "outputs/figures/nsw_importation_potential_map.png",
  width = 8, height = 6,
  dpi = 250
)

# plot versions clipped to Sydney
sydney_coord <- coord_sf(
  xlim = c(149.7, 152.5),
  ylim = c(-34.5, -32.7)
)

p_sydney_inf_area <- p_nsw_inf_area + sydney_coord
p_sydney_nl_imp <- p_nsw_nl_imp + sydney_coord

ggsave(
  plot = p_sydney_inf_area,
  filename = "outputs/figures/sydney_infectious_potential_area_map.png",
  width = 6, height = 6,
  dpi = 250
)

ggsave(
  plot = p_sydney_nl_imp,
  filename = "outputs/figures/sydney_importation_potential_map.png",
  width = 6, height = 6,
  dpi = 250
)

p_sydney_pop <- nsw_act_lga %>%
  lga_map(fill = "pop",
          source_geometry = nsw_act_sources,
          source_fill = "light blue") +
  sydney_coord +
  labs(
    fill = "Population"
  ) +
  ggtitle(
    "Population size of case-free NSW LGAs",
    "Light blue region indicates LGAs with active cases"
  )

ggsave(
  plot = p_sydney_pop,
  filename = "outputs/figures/sydney_population_map.png",
  width = 6, height = 6,
  dpi = 250
)

# convert to importation rate, with probability of leaving
import_rate <- vic_import_rate

# read in numbers of cases by date of infection in LGAs, and pad with 0s for
# other LGAs
lga_infections <- local_cases %>%
  filter(state == "VIC") %>%
  mutate(
    expected_infections = infections / detection_probability
  ) %>%
  filter(
    detection_probability > 0.5
  ) %>%
  filter(
    date > max(date) - 14
  ) %>%
  select(
    date,
    expected_infections,
    lga
  )

dates_prior <- seq(
  min(lga_infections$date),
  max(lga_infections$date),
  by = 1
)

# convert to a matrix
initial_expected_infections <- lga_infections %>%
  full_join(
    expand_grid(
      lga = setdiff(vic_lga$lga, lga_infections$lga),
      date = dates_prior,
      expected_infections = 0
    )
  ) %>%
  pivot_wider(
    names_from = lga,
    values_from = expected_infections,
    values_fill = list(expected_infections = 0)
  ) %>%
  select(-date) %>%
  as.matrix()

# take the last 2 weeks of case counts, and the expected fractions of cases in
# each of these postcodes, and get expected numbers of infections by postcode
rownames(initial_expected_infections) <- as.character(dates_prior)

# dates and suburbs
dates <- max(dates_prior) + seq_len(6 * 7)
n_dates <- length(dates)
n_dates_prior <- length(dates_prior)
dates_all <- min(dates) + seq(-n_dates_prior, n_dates - 1)

# R effective across suburbs and times
n_lga <- nrow(vic_lga)
idx <- match(dates, parameter_draws$dates)
r_eff_samples_mat <- parameter_draws$vic_r_eff[seq_len(n_samples), idx]
r_eff_samples_list <- apply(r_eff_samples_mat, 1, list)
r_eff_samples <- lapply(r_eff_samples_list, function(x) {
  matrix(rep(x[[1]], n_lga), n_dates, n_lga)
})

# prepare and run simulations in parallel
plan(multisession)

# generation interval convolution matrix for dynamic part and for dynamic &
# pre-dynamic part
# gi_cdf <- nishiura_cdf()
gi_mat <- gi_matrix(gi_cdf, dates)
gi_mat_all <- gi_matrix(gi_cdf, dates_all)

# detection delay distribution probabilities for each date
delay_probs <- lapply(dates, delay_prob)
delay_probs_all <- lapply(dates_all, delay_prob)

# set up current lockdown situation
lockdown_matrix <- matrix(0, n_dates, n_lga)
lockdown_cols <- match(lockdown_lgas(), vic_lga$lga)
lockdown_rows <- dates >= as.Date("2020-07-02")
lockdown_matrix[lockdown_rows, lockdown_cols] <- 1

configs_base <- future_lapply(
  seq_len(n_samples),
  prepare_config,
  nbinom_size_samples = size_samples,
  r_eff_samples = r_eff_samples,
  r_eff_reduction_samples = r_eff_reduction_samples,
  leaving_reduction_samples = leaving_reduction_samples,
  initial_expected_infections = initial_expected_infections,
  import_rate = import_rate,
  dates = dates,
  delay_probs = delay_probs,
  delay_probs_all = delay_probs_all,
  gi_mat = gi_mat,
  gi_mat_all = gi_mat_all,
  populations = vic_lga$pop,
  lockdown_matrix = lockdown_matrix
)

configs_no_response <- configs_base

configs_react_50 <- lapply(configs_base,
                          set,
                          "lockdown_policy",
                          reactive_policy(50))

configs_react_10 <- lapply(configs_base,
                           set,
                           "lockdown_policy",
                           reactive_policy(20))

statewide_lockdown_matrix <- lockdown_matrix
statewide_lockdown_matrix[dates > Sys.Date(), ] <- 1

configs_statewide <- lapply(configs_base,
                            set,
                            "lockdown_matrix",
                            statewide_lockdown_matrix)

results_no_response <- future_lapply(configs_no_response, run_simulation)
results_statewide <- lapply(configs_statewide, run_simulation)
results_react_50 <- future_lapply(configs_react_50, run_simulation)
results_react_10 <- future_lapply(configs_react_10, run_simulation)

df <- bind_rows(
  summarise_statewide(results_react_50, "New LGAs after 50 cases"),
  summarise_statewide(results_react_10, "New LGAs after 10 cases"),
  summarise_statewide(results_statewide, "All Victoria from July 14"),
  summarise_statewide(results_no_response, "Melbourne & Mitchell only")
)

base_colour <- blue

p <- df %>%
  group_by(scenario) %>%
  mutate(max = max(ci_90_hi)) %>%
  arrange(max) %>%
  ungroup() %>%
  mutate(
    scenario = factor(scenario),
    type = "Nowcast"
  ) %>%
  arrange(desc(max)) %>%
  ggplot() + 
  
  aes(date, median, fill = type) +
  
  xlab(element_blank()) +
  
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
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
  coord_cartesian(ylim = c(0, 600),
                  xlim = c(Sys.Date(), max(dates))) +
  
  facet_wrap(~scenario, ncol = 2) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines")) +
  ylab("confirmed cases per day") +
  ggtitle("Forecast under different restriction policies")

p

save_ggplot("scenario_forecast.png", multi = TRUE)

# to do:
#  start with current lockdown
#  add more lockdown policies - including statewide lockdown trigger
#  define loss of control & compute probability of reaching it
#  make nicer plots
