# check convergence
convergence <- function(draws) {
  
  r_hats <- gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
  n_eff <- effectiveSize(draws)
  
  # sometimes n_eff underflows to 0 simply because the values beinng traced are
  # very small, so remove these (exactly 0 is not possible)
  n_eff <- n_eff[n_eff != 0]
  
  cat(sprintf("maximum R-hat: %.2f\nminimum n effective: %.2f",
              max(r_hats),
              min(n_eff)))
  
  result <- list(r_hats = r_hats,
                 n_eff = n_eff)
  
  invisible(result)
  
}
