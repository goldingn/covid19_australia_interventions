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
  phase = c("importation", "suppression", "community")
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
scenarios_to_run <- 5:17
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

scenario <- readRDS("outputs/counterfactuals/scenario1.RDS")


summarise_scenario <- function(scenario) {
  file <- paste0("outputs/counterfactuals/scenario", scenario, ".RDS")
  file %>%
    readRDS() %>%
    `[[`("results") %>%
    group_by(date) %>%
    summarise(
      bottom = quantile(cases, 0.05),
      lower = quantile(cases, 0.25),
      median = median(cases),
      upper = quantile(cases, 0.75),
      top = quantile(cases, 0.95),
      .groups = "drop"
    )
}

add_scenario_ribbon <- function(base_plot, data, colour = "black") {
  base_plot +
  geom_ribbon(
    aes(
      ymin = bottom,
      ymax = top
    ),
    data = data,
    fill = colour,
    alpha = 0.1
  ) +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper
      ),
      data = data,
      fill = colour,
      alpha = 0.2
    ) +
    geom_line(
      aes(
        date,
        median
      ),
      data = data,
      color = colour
    ) +
    coord_cartesian(
      xlim = range(data$date)
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

# observed case counts
observed <- tibble(
  date = fitted_model$data$dates$infection,
  cases = rowSums(fitted_model$data$local$cases)
)

base <- observed %>%
  ggplot() +
  aes(date, median) + 
  geom_line(
    aes(date, cases),
    data = observed,
    lty = 2
  ) +
  theme_cowplot() +
  ylab("new locally-acquired infections") +
  xlab("date of infection")

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
p <- 
  (quarantine[[1]] | quarantine[[2]] | quarantine[[3]]) /
  (distancing[[1]] | distancing[[2]] | distancing[[3]]) / 
  (contacts[[1]] | contacts[[2]] | contacts[[3]]) +
  plot_annotation("Epidemic curves under counterfactual scenarios")
p

ggsave("~/Desktop/multipanel.png", plot = p)

    
# # read in all the scenarios, summarise the Reffs, and visualise
# files <- paste0("outputs/counterfactuals/scenario", 1:72, ".RDS")
# 
# extract_reff <- function(scenario, file) {
#   object <- readRDS(file)
#   cbind(object$reffs, scenario = scenario)
# }
# 
# reff <- mapply(
#   extract_reff,
#   seq_along(files),
#   files,
#   SIMPLIFY = FALSE
# ) %>%
#   do.call(bind_rows, .) %>%
#   group_by(scenario) %>%
#   summarise(
#     local_reff_mean = mean(local),
#     local_reff_exceedance = mean(local > 1)
#   ) %>%
#   cbind(scenarios) %>%
#   select(
#     scenario,
#     phase,
#     mobility_restrictions,
#     physical_distancing,
#     contact_tracing,
#     overseas_quarantine,
#     local_reff_mean,
#     local_reff_exceedance
#   )
# 
# reff %>%
#   filter(phase == "importation" & mobility_restrictions & physical_distancing & overseas_quarantine)
