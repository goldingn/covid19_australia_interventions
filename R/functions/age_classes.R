age_classes <- function(final_age_bin = 80, by = 5) {
  
  # compute age classes based on this spec
  ages_lower = seq(0, final_age_bin, by = by)
  n_ages <- length(ages_lower)
  ages_upper = ages_lower + by - 1
  ages_upper[n_ages] <- Inf
  
  age_classes <- c(
    paste(
      ages_lower[-n_ages],
      ages_upper[-n_ages],
      sep = "-"
    ),
    paste0(
      final_age_bin,
      "+"
    )
  )
  
  tibble::tibble(
    classes = age_classes,
    lower = ages_lower,
    upper = ages_upper
  )
  
}
