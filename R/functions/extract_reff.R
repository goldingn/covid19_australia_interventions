extract_reff <- function(scenario, file) {
  object <- readRDS(file)
  cbind(object$reffs, scenario = scenario)
}
