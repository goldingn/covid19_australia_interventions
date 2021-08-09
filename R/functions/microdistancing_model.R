# model for the trend in microdistancing
# compared to the macrodistancing effect, this has a linear transform of the
# distancing coefficient on the logit scale - corresponding to a different
# spread in adoption of microdistancing behaviour - and different dates of peak
# microdistancing and subsequent inflections
microdistancing_model <- function(data, parameters) {
  
  # shapes of inflection effects
  # timing of inflections in each state
  n_states <- nrow(parameters$inflections)
  segment_ends <- cbind(parameters$inflections, ones(n_states))
  denoms <- next_column(parameters$inflections) - parameters$inflections
  
  inflections_long <- parameters$inflections[data$state_id, ]
  denominators <- next_column(inflections_long) - inflections_long
  numerators <- sweep(-inflections_long, 1, data$time, FUN = "+")
  inflection_shape <- cbind(ones(length(data$state)), numerators / denominators)

  # masking columns to create weights matrix
  null1 <- (sign(inflection_shape) + 1) / 2
  null2 <- next_column(1 - null1)
  null3 <- next_column(null2)
  
  # piecewise linear weights matrix
  weights_up <- inflection_shape * null1 * null2
  anti_weights <- next_column(weights_up)
  weights_down <- (1 - anti_weights) * (1 - null2) * null3
  weights <- weights_up + weights_down 
  
  # apply weights to get inflections, and apply to initial distancing effect
  heights_long <- parameters$heights[data$state_id, ]
  inflection <- rowSums(weights * heights_long)
  
  # apply the initial distancing period (shrunk to meet the first height)
  initial <- (1 - data$distancing) * heights_long[, 1] 
  inflection - initial
  
}
