# fit gam to delta-omicron proportion switch

# data from https://covariants.org/per-country for Australia from just before Omicron.
# Includes only Omicron and Delta sequences
sequence_counts <- tribble(
  ~date1,       ~date2,       ~no_delta, ~no_omicron, ~dummy,
  "18/10/2021", "01/11/2021", 1900, 0, 1,
  "01/11/2021", "15/11/2021", 2095, 0, 2,
  "15/11/2021", "29/11/2021", 2036, 4, 3,
  "29/11/2021", "13/12/2021", 3190, 399, 4,
  "13/12/2021", "27/12/2021", 2229, 3756, 5,
  "27/12/2021", "10/01/2022", 329,  3571, 6,
  "10/01/2022", "24/01/2022", 57,   1992, 7,
  "25/01/2022", "07/02/2022", 8,    364, 8
) %>%
  mutate(
    across(starts_with("date"), as.Date, format = "%d/%m/%Y")
  )


glm1 <- glm(cbind(no_delta, no_omicron) ~ dummy, family = binomial(link = "logit"), data = sequence_counts)


predict(glm1, type = "response")


omicron_sequence <- sequence_counts %>%
  select(-date2) %>%
  rename(date = date1) %>%
  pivot_longer(
    cols = -date,
    names_to = "omicron",
    values_to = "count"
  ) %>%
  mutate(
    omicron = if_else(omicron == "no_delta", 0, 1)
  ) %>%
  uncount(count)


glm2 <- glm(omicron ~ date, family = binomial(link = "log"), data = omicron_sequence)


predict(glm2, newdata = sequence_counts %>% mutate(date = date1), type = "response")


aa <-predict(glm2, type = "response")


plot(x = omicron_sequence$date, y = aa)

#~~~~~~~

omicron_sequence <- sequence_counts %>%
  select(-starts_with("date")) %>%
  pivot_longer(
    cols = -dummy,
    names_to = "omicron",
    values_to = "count"
  ) %>%
  mutate(
    omicron = if_else(omicron == "no_delta", 0, 1)
  ) %>%
  uncount(count)


glm2 <- glm(omicron ~ dummy, family = binomial(link = "logit"), data = omicron_sequence)


predict(glm2, newdata = sequence_counts, type = "response")


aa <-predict(glm2, type = "response")


plot(x = omicron_sequence$dummy, y = aa)
