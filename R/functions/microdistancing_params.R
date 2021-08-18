microdistancing_params <- function(n_locations = 8, n_inflections = 1, inflection_max = 1) {
  
  # share information between peaks on both timing and amplitude
  inflection_means <- normal(0, 10, dim = n_inflections)
  inflection_sds <- normal(0, 0.5, truncation = c(0, Inf), dim = n_inflections)
  inflection_raw <- normal(0, 1, dim = c(n_locations, n_inflections))
  inflection_scaled <- sweep(inflection_raw, 2, inflection_sds, FUN = "*")
  inflection_centred <- sweep(inflection_raw, 2, inflection_means, FUN = "+")
  inflections <- ilogit(inflection_centred)

  height_means <- normal(0, 10, dim = n_inflections + 1)
  height_sds <- normal(0, 0.5, truncation = c(0, Inf), dim = n_inflections + 1)
  height_raw <- normal(0, 1, dim = c(n_locations, n_inflections + 1))
  height_scaled <- sweep(height_raw, 2, height_sds, FUN = "*")
  height_centred <- sweep(height_raw, 2, height_means, FUN = "+")
  heights <- ilogit(height_centred)
  
  # order inflections between 0 and 1 and constrain to earlier than a maximum value
  inflections <- apply(inflections, 1, "cumprod")
  inflections <- 1 - t(inflections)
  inflections <- inflections * inflection_max
  
  list(
    inflections = inflections,
    heights = heights
  )
  
}
