# modify an import rate matrix to set the probability that a infectee is
# inside/outside the suburb
set_leaving_probability <- function(import_rate, leaving_probability) {
  
  # make off-diagnal elements on each row (probability of visiting each other
  # suburbs if leaving) sum to 1
  diag(import_rate) <- 0
  import_rate <- sweep(import_rate, 1, rowSums(import_rate), FUN = "/")
  
  # set the probability that a contact is outside the postcode
  diag(import_rate) <- 1 / leaving_probability - 1
  import_rate <- sweep(import_rate, 1, rowSums(import_rate), FUN = "/")
  
  import_rate
  
}
