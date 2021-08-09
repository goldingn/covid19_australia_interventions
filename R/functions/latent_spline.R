# a b-spline constrained to 0-1 for day of the week, with Sunday (day 1)
# fixed at 1 (for identifiability)
latent_spline <- function (days = 1:7, knots = 4) {
  bases <- splines::bs(days, df = knots)
  # for identifiability, make one weight positive. That way the rest can define
  # their sign relative to this, and the loading defines the overall sign
  weights <- normal(0, 1, truncation = c(-Inf, 0), dim = knots)#, 1, dim = knots, truncation = c(-1, 1))
  spline <- bases %*% weights
  spline <- spline - min(spline)
  spline <- spline / max(spline)
  spline
  
}
