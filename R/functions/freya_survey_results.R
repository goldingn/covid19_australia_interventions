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
