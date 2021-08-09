get_cis <- function(date, state, ecdf, weight, use_national) {
  
  deciles_lower <- seq(0.05, 0.45, by = 0.05)
  deciles_upper <- 1 - deciles_lower
  deciles <- c(deciles_lower, deciles_upper)
  decile_names <- paste0("ci_", (1 - 2 * deciles_lower) * 100)
  decile_names <- c(paste0(decile_names, "_lo"),
                    paste0(decile_names, "_hi"))
  
  cis <- quantile(ecdf, deciles)
  names(cis) <- decile_names
  cis
}
