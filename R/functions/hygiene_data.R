# load  all hygiene/microdistancing survey data
hygiene_data <- function () {
  parse_all_surveys()  %>%
    select(
      -starts_with("contact")
    ) %>%
    mutate(
      phys_contact = ifelse(phys_contact, "Yes", "No")
    ) %>%
    pivot_longer(
      cols = c(phys_contact, phys_distance, wash_hands, cough, face_covering),
      names_to = "question",
      values_to = "response"
    ) %>%
    mutate(
      question = recode(
        question,
        `phys_distance` = "1.5m compliance",
        `wash_hands` = "Hand washing",
        `cough` = "Cough etiquette",
        `face_covering` = "Face covering",
        `phys_contact` = "Physical contact"
      )
    ) %>%
    mutate(
      response = case_when(
        question == "1.5m compliance" &
          response %in% c("Always") ~ "yes",
        question == "1.5m compliance" &
          response %in% c("Often", "Sometimes", "Rarely", "No") ~ "no",
        TRUE ~ response) 
    ) %>%
    # recode hand washing to yes/no (whether they did it immediately afterwards)
    mutate(
      response = case_when(
        question == "Hand washing" & response != "No" ~ "yes",
        question == "Hand washing" & response == "No" ~ "no",
        TRUE ~ response) 
    ) %>%
    # recode cough etiquette to yes/no (whether they covered their mouth with anything)
    mutate(
      response = case_when(
        question == "Cough etiquette" & response != "Nothing" ~ "yes",
        question == "Cough etiquette" & response == "Nothing" ~ "no",
        TRUE ~ response) 
    ) %>%
    # recode face covering as Always or not
    mutate(
      response = case_when(
        question == "Face covering" &
          response %in% c("Always") ~ "yes",
        question == "Face covering" &
          response %in% c("Often", "Sometimes", "Rarely", "No") ~ "no",
        TRUE ~ response) 
    ) %>%
    # recode physical contact to the opposite, to reflect avoidance
    mutate(
      response = case_when(
        question == "Physical contact" & response == "No" ~ "yes",
        question == "Physical contact" & response == "Yes" ~ "no",
        TRUE ~ response) 
    ) %>%
    filter(
      !is.na(response),
      !is.na(state)
    ) %>%
    # collate responses into respondents and yeses
    group_by(state, wave_date, date, question, response) %>%
    summarise(count = n())%>%
    ungroup() %>%
    pivot_wider(
      names_from = "response",
      values_from  = "count",
    ) %>%
    mutate(
      yes = replace_na(yes, 0),
      no = replace_na(no, 0),
      respondents = yes + no
    ) %>%
    rename(count = yes) %>%
    select(-no) %>%
    mutate(proportion = count / respondents) %>%
    arrange(state, question, date)
  
}
