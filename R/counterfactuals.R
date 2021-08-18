# do counterfactual simulations and save outputs to make plots

# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

set.seed(2020-09-16)
source("./packages.R")
source("./conflicts.R")
lapply(list.files("./R/functions", full.names = TRUE), source)
source("./objects_and_settings.R")
## Load your R files

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

scenarios <- expand_grid(
  overseas_quarantine = c(FALSE, TRUE),
  mobility_restrictions = c(FALSE, TRUE),
  physical_distancing = c(FALSE, TRUE),
  contact_tracing = c("none", "suboptimal", "optimal"),
  phase = c("importation", "suppression", "community")
)

saveRDS(scenarios, "outputs/counterfactuals/scenario_list.RDS")

# loop through all these scenarios generating posterior samples
for(index in seq_len(nrow(scenarios))) {
  
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

# set up plotting of different scenarios
# optimal scenario
sc_optimal <- list(
  summarise_scenario(70),
  summarise_scenario(71), 
  summarise_scenario(72) 
)

# minus quarantine
sc_no_quarantine <- list(
  summarise_scenario(34),
  summarise_scenario(35), 
  summarise_scenario(36) 
)

# minus various distancing components
sc_no_distancing <- list(
  summarise_scenario(43),
  summarise_scenario(44), 
  summarise_scenario(45) 
)
sc_no_micro <- list(
  summarise_scenario(61),
  summarise_scenario(62), 
  summarise_scenario(63) 
)
sc_no_macro <- list(
  summarise_scenario(52),
  summarise_scenario(53), 
  summarise_scenario(54) 
)

# minus various contact tracing components
sc_no_contacts <- list(
  summarise_scenario(64),
  summarise_scenario(65), 
  summarise_scenario(66) 
)
sc_some_contacts <- list(
  summarise_scenario(67),
  summarise_scenario(68), 
  summarise_scenario(69) 
)

observed <- tibble(
  date = fitted_model$data$dates$infection,
  cases = rowSums(fitted_model$data$local$cases)
)

base <- observed %>%
  ggplot() +
  aes(date, median) + 
  # geom_line(
  #   aes(date, cases),
  #   data = observed,
  #   lty = 2
  # ) +
  theme_cowplot() +
  ylab("new locally-acquired infections") +
  xlab("date of infection")

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
                     sc_some_contacts,
                     MoreArgs = list(
                       base_plot = base,
                       colours = c(grey(0.4), yellow, yellow)
                     ),
                     SIMPLIFY = FALSE)

p <- 
  (quarantine[[1]] | quarantine[[2]] | quarantine[[3]]) /
  (distancing[[1]] | distancing[[2]] | distancing[[3]]) / 
  (contacts[[1]] | contacts[[2]] | contacts[[3]])
    
p + plot_annotation("Epidemic curves under counterfactual scenarios")

ggsave("~/Desktop/multipanel.png", plot = p)

    
scenarios[34, ]

# read in all the scenarios, summarise the Reffs, and visualise
files <- paste0("outputs/counterfactuals/scenario", 1:72, ".RDS")

reff <- mapply(
  extract_reff,
  seq_along(files),
  files,
  SIMPLIFY = FALSE
) %>%
  do.call(bind_rows, .) %>%
  group_by(scenario) %>%
  summarise(
    local_reff_mean = mean(local),
    local_reff_exceedance = mean(local > 1)
  ) %>%
  cbind(scenarios) %>%
  select(
    scenario,
    phase,
    mobility_restrictions,
    physical_distancing,
    contact_tracing,
    overseas_quarantine,
    local_reff_mean,
    local_reff_exceedance
  )

reff %>%
  filter(phase == "importation" & mobility_restrictions & physical_distancing & overseas_quarantine)

# other things to do:
# mobility
#   - add vic interventions to mobility model
#   - extract rate of uptake for each of these
# delay distributions
#   - compute moving-window delay distributions
#   - use new delay distributions for the contact tracing effect
#   - use new delay distributions for imputation and right-truncation
# microdistancing
#   - use mobility data in microdistancing trends
