# Given an date-by-state matrix of 'infectiousness' due to imports and previous
# locally-acquired cases, and a corresponding matrix 'R_local' of Reff for
# locally-acquired infections, compute the expected numbers of locally-acquired
# cases into the future. Also requires 'disaggregation_probs' giving the density
# of the serial interval over a sequence of days.
project_local_cases <- function(
  infectiousness,
  R_local,
  disaggregation_probs
) {
  
  if (!identical(dim(infectiousness), dim(R_local))) {
    stop ("infectiousness and R_local must have the same dimensions")
  }
  
  op("project_local_cases",
     infectiousness,
     R_local,
     disaggregation_probs,
     operation_args = list(
       T = nrow(infectiousness),
       K = length(disaggregation_probs)
     ),
     tf_operation = "tf_project_local_cases",
     dim = dim(infectiousness))
  
}
