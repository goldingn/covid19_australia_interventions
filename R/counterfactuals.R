# do counterfactual simulations and save outputs to make plots

# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

set.seed(2020-09-16)
source("R/functions.R")
library(cowplot)

# counterfactuals to consider:
# 
# quarantine of overseas arrivals
# - none (imports have Reff of locals)
# - supervised quarantine
# 
# mobility restrictions
# - baseline
# - optimal contact reduction
# 
# microdistancing restrictions
# - baseline
# - optimal microdistancing
# 
# contact tracing
# - no contact tracing
# - optimal contact tracing throughout
# - suboptimal (initial levels) of contact tracing

# turn each policy off (FALSE) or leave as observed (TRUE)
# run for each phase

scenarios <- expand_grid(
  overseas_quarantine = c(FALSE, TRUE),
  mobility_restrictions = c(FALSE, TRUE),
  physical_distancing = c(FALSE, TRUE),
  contact_tracing = c(FALSE, TRUE),
  phase = c("importation", "community", "suppression")
) %>%
  mutate(
    n_implemented = overseas_quarantine +
      mobility_restrictions +
      physical_distancing +
      contact_tracing
  ) %>%
  filter(
    n_implemented == 4 |  # observed scenario
    n_implemented >= 3 |  # one policy removed
    n_implemented == 2 &   # both distancing policies removed
      !mobility_restrictions &
      !physical_distancing
  ) %>%
  select(-n_implemented)

saveRDS(scenarios, "outputs/counterfactuals/scenario_list.RDS")

# loop through all these scenarios generating posterior samples
scenarios_to_run <- seq_len(nrow(scenarios))

for (index in scenarios_to_run) {
  
  print(paste("scenario: ", index))
    
  time <- system.time(
    {
      # reload the model each time, otherwise the TF graph gets increasingly heavy
      fitted_model <- readRDS("outputs/fitted_reff_model.RDS")
      # simulate and save the outputs
      simulate_scenario(index, scenarios, fitted_model)
    }
  )
  
  print(time)
  
}

summarise_scenario <- function(scenario) {
  file <- paste0("outputs/counterfactuals/scenario", scenario, ".RDS")
  file %>%
    readRDS() %>%
    `[[`("results") %>%
    group_by(date) %>%
    summarise(
      bottom = quantile(cases, 0.05, na.rm = TRUE),
      lower = quantile(cases, 0.25, na.rm = TRUE),
      median = median(cases, na.rm = TRUE),
      upper = quantile(cases, 0.75, na.rm = TRUE),
      top = quantile(cases, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
}

add_scenario_ribbon <- function(base_plot, data, colour = "black") {
  base_plot +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper
      ),
      data = data,
      fill = colour,
      alpha = 0.2
    )
}

# set up plotting of different scenarios

# minus quarantine
sc_no_quarantine <- list(
  summarise_scenario(1),
  summarise_scenario(2), 
  summarise_scenario(3) 
)

# minus various distancing components
sc_no_distancing <- list(
  summarise_scenario(4),
  summarise_scenario(5), 
  summarise_scenario(6) 
)
sc_no_macro <- list(
  summarise_scenario(7),
  summarise_scenario(8), 
  summarise_scenario(9) 
)
sc_no_micro <- list(
  summarise_scenario(10),
  summarise_scenario(11), 
  summarise_scenario(12) 
)

# minus various contact tracing components
sc_no_contacts <- list(
  summarise_scenario(13),
  summarise_scenario(14), 
  summarise_scenario(15) 
)

# optimal scenario
sc_optimal <- list(
  summarise_scenario(16),
  summarise_scenario(17), 
  summarise_scenario(18) 
)

fitted_model <- readRDS("outputs/fitted_reff_model.RDS")

# observed case counts
observed <- tibble(
  date = fitted_model$data$dates$infection,
  cases = rowSums(fitted_model$data$local$cases)
)

# format large numbers with 'k' for thousands, 'M' for millions etc.
large_numbers <- function(n) {
  case_when(
    n < 1e3 ~ as.character(n),
    n < 1e6 ~ paste0(round(n/1e3, 1), 'k'),
    n < 1e9 ~ paste0(round(n/1e6, 1), 'M')
  )
}

base <- observed %>%
  ggplot() +
  aes(date, median) +
  geom_vline(
    aes(xintercept = date),
    data = interventions("national"),
    color = "grey80"
  ) + 
  geom_line(
    aes(date, cases),
    data = observed
  ) +
  theme_cowplot() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  ylab("") +
  xlab("") +
  scale_y_continuous(
    position = "right",
    labels = large_numbers
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %y"
  )  

make_plot <- function(..., base_plot, colours) {
  scenarios <- list(...)
  plot <- base_plot
  for(i in seq_along(scenarios)) {
    plot <- plot %>%
      add_scenario_ribbon(
        scenarios[[i]],
        colour = colours[[i]]
      )
  }
  plot
}

quarantine <- mapply(make_plot,
                     sc_optimal,
                     sc_no_quarantine,
                     MoreArgs = list(
                       base_plot = base,
                       colours = c(grey(0.4), orange)
                     ),
                     SIMPLIFY = FALSE)

distancing <- mapply(make_plot,
                     sc_optimal,
                     sc_no_micro,
                     sc_no_macro,
                     MoreArgs = list(
                       base_plot = base,
                       colours = c(grey(0.4), purple, blue)
                     ),
                     SIMPLIFY = FALSE)

contacts <- mapply(make_plot,
                   sc_optimal,
                   sc_no_contacts,
                   MoreArgs = list(
                     base_plot = base,
                     colours = c(grey(0.4), yellow, yellow)
                   ),
                   SIMPLIFY = FALSE)

library(patchwork)

coords <- function(phase = 1, max_cases = 1500) {
  phase_long <- c("importation", "community", "suppression")[phase]
  dates <- scenario_dates(list(phase = phase_long))
  xlim <- range(dates)
  coord_cartesian(xlim = xlim, ylim = c(0, max_cases))
}

p <- 
  (quarantine[[1]] + ggtitle("initial importations") + coords(1) |
     quarantine[[2]] + ggtitle("community transmission") + coords(2) |
     quarantine[[3]] + ggtitle("outbreak suppression") + coords(3)) /
  (distancing[[1]] + coords(1) |
     distancing[[2]] + coords(2)  |
     distancing[[3]] + coords(3) ) / 
  (contacts[[1]] + coords(1) |
     contacts[[2]] + coords(2) |
     contacts[[3]] + coords(3))

p

# add in colour label legend on RHS alongside correct row

ggsave("~/Desktop/multipanel.png", plot = p, width = 10, height = 8)

# Use new C2 instead of observed ones for counterfactuals


