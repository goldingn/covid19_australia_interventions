# return a tibble summarisinng statewode case counts over time
summarise_statewide <- function(results, scenario = NULL) {
  lga_case_list <- lapply(results, `[[`, "detections_matrix")
  lga_case_sims <- do.call(abind, c(lga_case_list, list(along = 0)))
  vic_case_sims <- apply(lga_case_sims, 1:2, sum)
  
  quants <- apply(vic_case_sims, 2, quantile, c(0.05, 0.25, 0.5, 0.75, 0.95))
  
  tibble(
    scenario = scenario,
    date = dates,
    median = quants[3, ],
    ci_50_lo = quants[2, ],
    ci_50_hi = quants[4, ],
    ci_90_lo = quants[1, ],
    ci_90_hi = quants[5, ]
  )
  
}
