source("./lib.R")
source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)
source("./objects_and_settings.R")

# get the baseline next generation matrix
next_generation_matrix <- baseline_matrix(
  R0 = 3
)

# check R0
get_R(next_generation_matrix)

ages <- age_classes(80)
age_populations <- phase_age_populations()
age_distribution <- get_age_distribution(final_age_bin = 80)
#timeseries <- scrape_doses_timeseries()
timeseries <- vaccination_coverage()
ifr <- get_ifr()

# get effects for all timepoints
all_effects <- lapply(
  timeseries$doses,
  summarise_effect,
  age_populations = age_populations,
  age_distribution = age_distribution,
  next_generation_matrix = next_generation_matrix,
  ifr = ifr
)

timeseries$overall_transmission_effect <- vapply(
  X = all_effects,
  extract_overall_transmission_effect,
  FUN.VALUE = numeric(1)
)

timeseries$overall_ifr_effect <- vapply(
  X = all_effects,
  extract_overall_ifr_effect,
  FUN.VALUE = numeric(1),
  which = "odriscoll"
)

timeseries$over_70_ifr_effect <- vapply(
  X = all_effects,
  extract_over_70_ifr_effect,
  FUN.VALUE = numeric(1),
  which = "odriscoll"
)

png(
  filename = "outputs/figures/vacc_effect_tp_reduction.png",
  width = 11.69 / 2,
  height = 8.27 / 3,
  #scale = 1,
  units = "in",
  res = 150
)

plot(
  I(100 * (1 - overall_transmission_effect)) ~ date,
  data = timeseries,
  type = "l",
  ylab = "Percentage reduction in Reff",
  xlab = "",
  lwd = 2,
  las = 1,
  main = "Vaccination effect on population-wide\nCOVID-19 transmission potential"
)

dev.off()

png(
  filename = "outputs/figures/vacc_ifr_population.png",
  width = 11.69 / 2,
  height = 8.27 / 3,
  #scale = 1,
  units = "in",
  res = 150
)

  plot(
  overall_ifr_effect ~ date,
  data = timeseries,
  type = "l",
  ylab = "Population-wide IFR (%)",
  xlab = "",
  lwd = 2,
  col = "forestgreen",
  las = 1,
  ylim = c(0, 1.2),
  main = "Change in population-wide infection fatality risk"
)

dev.off()

range(timeseries$overall_ifr_effect)

png(
  filename = "outputs/figures/vacc_ifr_70plus.png",
  width = 11.69 / 2,
  height = 8.27 / 3,
  #scale = 1,
  units = "in",
  res = 150
)

plot(
  over_70_ifr_effect ~ date,
  data = timeseries,
  type = "l",
  ylab = "IFR (%)",
  xlab = "",
  lwd = 2,
  col = "deepskyblue",
  las = 1,
  ylim = c(0, 8),
  main = "Change in infection fatality risk among over-70s"
)
range(timeseries$over_70_ifr_effect)

dev.off()

# plot current vaccination coverage by age
current_coverage_by_age <- all_effects[[length(all_effects)]]$coverage_by_age

png(
  filename = "outputs/figures/vacc_coverage.png",
  width = 11.69 / 2,
  height = 8.27 / 3,
  #scale = 1,
  units = "in",
  res = 150
)

barplot(100 * current_coverage_by_age,
        names.arg = age_distribution$age_class,
        axes = FALSE,
        border = "white",
        xlab = "age group",
        main = "Assumed current vaccine coverage\n(with only single doses)",
        ylim = c(0, 100))
y_axis_ticks <- seq(0, 100, by = 20) 
axis(2,
     at = y_axis_ticks,
     labels = paste0(y_axis_ticks, "%"),
     las = 1)

dev.off()

# # The same, but assuming a complete phase 1 roll out
# n_doses_phase_1 <- sum(age_populations$phase_1A, age_populations$phase_1B)
# coverage_phase_1 <- doses_by_age(n_doses_phase_1, age_populations) / age_distribution$pop
# barplot(100 * coverage_phase_1,
#         names.arg = age_distribution$age_class,
# axes = FALSE,
# xlab = "age group",
#         main = "Assumed vaccination coverage\nafter complete phase 1 roll out",
#         ylim = c(0, 100))
# y_axis_ticks <- seq(0, 100, by = 20) 
# axis(2,
#      at = y_axis_ticks,
#      labels = paste0(y_axis_ticks, "%"),
#      las = 1)
# abline(h = 100, lty = 3)

# population-wide baseline IFRs based on these numbers
ifr_baseline_odriscoll <- sum(age_distribution$fraction * ifr$odriscoll)
ifr_baseline_brazeau <- sum(age_distribution$fraction * ifr$brazeau)

# apply this to vaccine coverage (single dose) estimates to get new IFRs by age
# and overall

# # check against tables
# lapply(populations_1A, sum)
# lapply(populations_1B, sum)
# 
# # check these look sensible
# 
# # populations by age in each phase
# par(mfrow = c(2, 1))
# barplot(age_populations_1A, ylim = range(age_populations_1B))
# barplot(age_populations_1B)
# 
# # proportional coverage with single doses in each age
# par(mfrow = c(1, 1))
# barplot(age_coverage)
# 
# # check this is true
# identical(
#   sum(age_doses_1A, age_doses_1B),
#   n_doses
# )

timeseries %>%
  select(date, fully_vaccinated, partially_vaccinated) %>%
  pivot_longer(
    cols = -date,
    names_to = "status",
    values_to = "population"
  ) %>%
  # mutate(status = factor(status))
  ggplot() +
  aes(date, population, fill = status) +
  geom_area() +
  scale_y_continuous(
    label = scales::comma
  ) +
  xlab("") +
  theme_minimal()

# simple 1 dose model with lag
ages <- age_classes(80)
age_populations <- phase_age_populations()
age_distribution <- get_age_distribution(final_age_bin = 80)
#timeseries <- scrape_doses_timeseries()
timeseries <- vaccination_coverage()
ifr <- get_ifr()

# fake data
n_days <- length(timeseries$doses)
time_to_protection <- 21
cumulative_vaccinees <- timeseries$doses
daily_vaccinees <- timeseries$new_doses
# construct linear weights
dummy_matrix <- matrix(0, n_days, n_days)
lags <- row(dummy_matrix) - col(dummy_matrix)
weights <- lags / time_to_protection
weights[] <- pmax(0, weights[])
weights[] <- pmin(1, weights[])
# apply them to the daily number of cases, to get the cumulative effect
cumulative_vaccinees_effective <- c(weights %*% daily_vaccinees)
# compare plot
plot(cumulative_vaccinees, type = "l")
lines(cumulative_vaccinees_effective, lty = 2)

all_effects <- lapply(
  cumulative_vaccinees_effective,
  summarise_effect,
  age_populations = age_populations,
  age_distribution = age_distribution,
  next_generation_matrix = next_generation_matrix,
  ifr = ifr
)

timeseries$overall_transmission_effect <- vapply(
  X = all_effects,
  extract_overall_transmission_effect,
  FUN.VALUE = numeric(1)
)

saveRDS(
 object = timeseries,
 file = "outputs/vaccine_timeseries.RDS"
)
