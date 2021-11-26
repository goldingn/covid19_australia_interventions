
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


dose_1_distributed <- function(){
  
  
  dose_class_fractions <- get_age_distribution() %>%
    filter(
      age_class != "0-4",
      age_class != "5-9",
      age_class != "10-14"
    ) %>% 
    mutate(
      lead_digit = substr(age_class, 1, 1)
    ) %>% 
    group_by(lead_digit) %>%
    mutate(class_fraction = fraction/sum(fraction)) %>%
    dplyr::select(-pop, -fraction)
  
  dose_1_data() %>% 
    mutate(
      lead_digit = substr(age_class_10, 1, 1),
      join_digit = ifelse(lead_digit == 9, 8, lead_digit)
    ) %>%
    full_join(
      dose_class_fractions,
      by = c("join_digit" =  "lead_digit")
    ) %>%
    filter(!(lead_digit == 9 & age_class != "85+")) %>%
    mutate(
      fraction = ifelse(lead_digit == 9, 1, class_fraction),
      doses = fraction*doses
    ) %>%
    group_by(state, vaccine, age_class) %>%
    summarise(doses = sum(doses)) %>%
    ungroup %>%
    dplyr::select(state, vaccine, age_class, doses)
    
  
  
}