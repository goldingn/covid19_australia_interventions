# Pull out generation interval distribution estimates from Ganyani et al. Use
# baseline GI estimates for China from (Table 1, row 3). This corresponds to a
# GI/SI mean of 3.95, which is around the 4 that seems to be a consensus for the
# SI mean. We do not use the version allowing for a negative serial interval
# (which is suggested by other including Du et al., and could possibly be an
# artefact of incorrect alignment of infector/infectee pairs), though when this is
# used as a prior the model is able to estimate parameters corresponding to
# that scenario.
ganyani_gi <- function(which = c("Tianjin", "Singapore")) {
  
  tianjin <- tribble(
    ~which, ~est, ~lower, ~upper,
    "mean", 3.95, 3.01, 4.91,
    "sd", 1.51, 0.74, 2.97
  )
  
  singapore <- tribble(
    ~which, ~est, ~lower, ~upper,
    "mean", 5.20, 3.78, 6.78,
    "sd", 1.72, 0.91, 3.93
  )
  
  ganyani <- switch(match.arg(which),
                    Tianjin = tianjin,
                    Singapore = singapore)
  
  ganyani <- ganyani %>%
    mutate(
      sd_above = (upper - est) / 1.96,
      sd_below = (est - lower) / 1.96,
      sd = (sd_above + sd_below) / 2
    ) %>%
    select(which, est, sd)
  
  ganyani %>%
    group_by(which) %>%
    nest() %>%
    pull(data) %>%
    `names<-`(ganyani$which)
  
}
