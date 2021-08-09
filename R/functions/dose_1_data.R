dose_1_data <- function(){
  
  dat <- readxl::read_xlsx(
    path = "~/not_synced/vaccination/Individuals received dose 1 by 10 year age group and jurisdiction[4].xlsx",
    skip = 1,
  )
  
  state_loc <- seq(from = 1, to = 22, by = 3)
  
  state_rep <- rep(state_loc, each = 3)
  
  dat[,"state"] <- dat[state_rep,1]
  
  dat[-state_loc,] %>%
    mutate(
      vaccine = ifelse(
        `Patient State` == "Pfizer Comirnaty",
        yes = "pf",
        no = "az"
      )
    ) %>%
    dplyr::select(-`Patient State`) %>%
    pivot_longer(
      cols = !state:vaccine,
      names_to = "age_class_10",
      values_to = "doses"
    )
  
}
