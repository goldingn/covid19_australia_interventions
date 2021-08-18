extract_overall_ifr_effect <- function(x, which = c("odriscoll", "brazeau")) {
  # extract results for the population-wide IFR from a list of outputs from
  # summarise_effect
  which <- match.arg(which)
  x$ifr$overall[[which]]
}
