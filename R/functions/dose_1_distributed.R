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
