disaggregation_vec <- function(mask, distribution) {
  masked_distribution <- mask * distribution
  masked_distribution / sum(masked_distribution)
}
