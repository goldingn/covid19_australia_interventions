# handle bad Cholesky factorizations
safe_objective <- function(par) {
  tryCatch(
    objective(par),
    error = function(e) { Inf }
  )
}
