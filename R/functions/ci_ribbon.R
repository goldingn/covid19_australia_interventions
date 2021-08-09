ci_ribbon <- function(ci) {
  
  lo <- paste0("ci_", ci, "_lo")
  hi <- paste0("ci_", ci, "_hi")
  
  geom_ribbon(
    aes_string(ymin = lo,
               ymax = hi),
    alpha = 1/9
  )
}
