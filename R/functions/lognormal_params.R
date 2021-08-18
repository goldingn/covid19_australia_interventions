lognormal_params <- function(mean, sd) {
  var <- sd ^ 2
  list(
       meanlog = log((mean ^ 2) / sqrt(var + mean ^ 2)),
       sdlog = sqrt(log(1 + var / (mean ^ 2)))
  )
}
