lgas_of_concern <- c(
  "Bayside (A)",
  "Blacktown (C)",
  "Burwood (A)",
  "Campbelltown (C) (NSW)",
  "Canterbury-Bankstown (A)",
  "Cumberland (A)",
  "Fairfield (C)",
  "Georges River (A)",
  "Liverpool (C)",
  "Parramatta (C)",
  "Strathfield (A)",
  "Penrith (C)"
)

nsw_ll <- read_csv(
  "~/not_synced/nsw/CASES_2021-08-17_UNSW.csv",
  col_types = cols(
    LGA_CODE19 = col_double(),
    LGA_NAME19 = col_character(),
    NOTIFICATION_DATE = col_date(format = ""),
    CALCULATED_ONSET_DATE = col_date(format = ""),
    INFECTIOUS_STATUS = col_character(),
    LIKELY_SOURCE_OF_INFECTION_LOCAL = col_character(),
    number_cases = col_double(),
    snapshot_date = col_date(format = "")
  )
) %>%
  select(
    lga = LGA_NAME19,
    date_notification = NOTIFICATION_DATE,
    date_onset = CALCULATED_ONSET_DATE,
    infectious_status = INFECTIOUS_STATUS,
    likely_source_of_infection = LIKELY_SOURCE_OF_INFECTION_LOCAL,
    number_cases
  ) %>%
  filter(
    likely_source_of_infection != "Not local",
    lga %in% lgas_of_concern,
    # poor data prior to June 21 (infectious status still 'under investigation')
    date_onset > as_date("2021-06-21"),
    # lop off last three weeks due to truncation
    date_onset < (max(date_onset) - 21),
    # then remove this where infectious status is under investigation (few of
    # these now)
    infectious_status != "Under investigation"
  ) %>%
  mutate(
    isolated_while_infectious = infectious_status == "Isolated for infectious period"
  ) %>%
  group_by(
    date_onset,
    likely_source_of_infection
  ) %>%
  summarise(
    number_cases_isolated = sum(number_cases * isolated_while_infectious),
    number_cases_not_isolated = sum(number_cases * !isolated_while_infectious),
    number_cases = sum(number_cases),
    fraction_cases_isolated = replace_na(number_cases_isolated / number_cases, 0),
    .groups = "drop"
  ) %>%
  filter(
    number_cases > 0
  ) %>%
  arrange(
    date_onset,
    likely_source_of_infection
  )

household <- nsw_ll %>%
  filter(
    likely_source_of_infection == "Household contact of a confirmed case"
  ) %>%
  mutate(
    date_onset_num = as.numeric(date_onset - min(date_onset))
  )


library(mgcv)
m_household <- gam(
  cbind(
    number_cases_isolated,
    number_cases_not_isolated
  ) ~
    s(date_onset_num),
  family = stats::binomial,
  select = TRUE,
  data = household,
  gamma = 2
)



household_pred <- tibble(
  date_onset = sort(unique(household$date_onset))
) %>%
  mutate(
    date_onset_num = as.numeric(date_onset - min(date_onset))
  )


pred <- predict(
  m_household, 
  newdata = household_pred,
  type = "response",
  se.fit = TRUE
)

household_pred <- household_pred %>%
  mutate(
    fraction_cases_isolated = pred$fit,
    fraction_cases_isolated_se = pred$se.fit,
    fraction_cases_isolated_upper = fraction_cases_isolated + fraction_cases_isolated_se * 1.96,
    fraction_cases_isolated_lower = fraction_cases_isolated - fraction_cases_isolated_se * 1.96
  )

red <- rgb(202, 78, 51, maxColorValue = 255)
green <- rgb(116, 193, 174, maxColorValue = 255)
amber <- rgb(226, 165, 88, maxColorValue = 255)

household_pred %>%
  ggplot(
    aes(
      x = date_onset,
      y = fraction_cases_isolated
    )
  ) +
  geom_ribbon(
    aes(
      ymin = fraction_cases_isolated_lower,
      ymax = fraction_cases_isolated_upper
    ),
    alpha = 0.2
  ) +
  geom_line() +
  geom_point(
    aes(
      size = number_cases
    ),
    data = household,
    alpha = 0.5
  ) +
  xlab("date of symptom onset") +
  ylab("fraction in isolation whilst infectious (where known)") +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle("Fraction of household cases isolated while infectious") +
  theme_cowplot() +
  theme(
    strip.background = element_blank(),
    legend.position = "none"
  )

ggsave(
  "~/Desktop/household_infectious_in_community.png",
  bg = "white",
  width = 9,
  height = 6
)
  