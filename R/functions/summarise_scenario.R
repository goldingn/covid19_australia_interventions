# sc <- readRDS("outputs/counterfactuals/scenario34.RDS")
summarise_scenario <- function(scenario) {
  file <- paste0("outputs/counterfactuals/scenario", scenario, ".RDS")
  file %>%
    readRDS() %>%
    `[[`("local_cases") %>%
    group_by(date) %>%
    summarise(
      bottom = quantile(cases, 0.05),
      lower = quantile(cases, 0.25),
      median = median(cases),
      upper = quantile(cases, 0.75),
      top = quantile(cases, 0.95)
    )
}
