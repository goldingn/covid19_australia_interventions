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
