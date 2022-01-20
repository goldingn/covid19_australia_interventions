source("R/functions.R")


nsw_linelist <- get_nsw_linelist(nindss_compatible = FALSE)

unique(nsw_linelist$Omicron_Category)
table(nsw_linelist$Omicron_Category, useNA = "always")

nsw_linelist %>%
  filter(date_confirmation >= "2021-11-24") %>%
  group_by(date_confirmation, Omicron_Category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(date_confirmation) %>%
  mutate(proportion = count/sum(count)) %>%
  ggplot() +
  geom_bar(
    aes(
      x = date_confirmation,
      y = proportion,
      fill = Omicron_Category
    ),
    stat = "identity"
  )


nsw_linelist_extra_info <- read_csv(
  file = "~/not_synced/nsw/20211215 - Case list - Freya Shearer_extra_inf_dates.csv",
  col_types = cols(
    .default = col_character(),
    CASE_ID = col_double(),
    EARLIEST_CONFIRMED_OR_PROBABLE = col_date(format = "%d/%m/%y"),
    SYMPTOM_ONSET_DATE = col_date(format = "%d/%m/%y"),
    CALCULATED_ONSET_DATE = col_date(format = "%d/%m/%y"),
    AGE_AT_EVENT_YEARS = col_double(),
    DATE_ISOLATION_BEGAN = col_date(format = "%d/%m/%y"),
    #SETTING_OF_TRANSMISSION_DATE = col_date(format = "%Y-%m-%d"),
    SETTING_OF_TRANSMISSION_DATE = col_nsw_date(),
    INTERVIEWED_DATE = col_date(format = "%d/%m/%y"),
    S_gene_result_date = col_date(format = "%d/%m/%y"),
    Omicron_Category = col_factor()
  )
) %>%
  # remove some bogus dates
  mutate(across(
    all_of(c(
      "EARLIEST_CONFIRMED_OR_PROBABLE",
      "SYMPTOM_ONSET_DATE",
      "SETTING_OF_TRANSMISSION_DATE",
      "CALCULATED_ONSET_DATE",
      "DATE_ISOLATION_BEGAN",
      "SETTING_OF_TRANSMISSION_DATE",
      "INTERVIEWED_DATE",
      "S_gene_result_date"
    )),
    clean_date
  )
  ) %>%
  # if any infection dates are after onset, or on/after confirmation, set the infection date to NA
  mutate(
    SETTING_OF_TRANSMISSION_DATE = case_when(
      SETTING_OF_TRANSMISSION_DATE > SYMPTOM_ONSET_DATE ~ as.Date(NA),
      SETTING_OF_TRANSMISSION_DATE >= EARLIEST_CONFIRMED_OR_PROBABLE ~ as.Date(NA),
      TRUE ~ SETTING_OF_TRANSMISSION_DATE
    )
  ) %>%
  mutate(
    date_onset = case_when(
      !is.na(SETTING_OF_TRANSMISSION_DATE) ~ SETTING_OF_TRANSMISSION_DATE + 5,
      #TRUE ~ CALCULATED_ONSET_DATE
      TRUE ~ SYMPTOM_ONSET_DATE
    ),
    #date_onset = NA,
    date_detection = NA,
    date_confirmation = EARLIEST_CONFIRMED_OR_PROBABLE,
    date_quarantine = DATE_ISOLATION_BEGAN,
    state = "NSW",
    import_status = ifelse(
      PLACE_ACQUISITION == "Acquired in NSW",
      "local",
      "imported"
    ),
    postcode_of_acquisition = NA,
    postcode_of_residence = NA,
    state_of_acquisition = "NSW",
    state_of_residence = NA,
    report_delay = NA,
    date_linelist = date,
    interstate_import = FALSE
  )


nsw_linelist_merged <- nsw_linelist %>%
  left_join(
    y = nsw_linelist_extra_info %>%
      select(CASE_ID, onset_extra = date_onset),
    by = "CASE_ID"
  ) %>%
  mutate(
    date_onset = if_else(
      is.na(date_onset),
      onset_extra,
      date_onset
    )
  )


nsw_linelist_clean <- nsw_linelist_merged %>%
  select(
    date_onset,
    date_detection,
    date_confirmation,
    date_quarantine,
    state,
    import_status,
    postcode_of_acquisition,
    postcode_of_residence,
    state_of_acquisition,
    state_of_residence,
    report_delay,
    date_linelist,
    interstate_import
  ) %>%
  arrange(
    desc(date_onset)
  )


nsw_linelist_clean %>%
  group_by(date_confirmation) %>%
  summarise(count = n()) %>%
  filter(date_confirmation >= "2021-11-01") %>%
  ggplot() +
  geom_bar(
    aes(
      x = date_confirmation,
      y = count
    ),
    stat = "identity"
  )

linelist <- get_nndss_linelist()
  
  nsw_ll <- nsw_linelist_clean
  nsw_ll_date <- nsw_ll$date_linelist[1]
  nsw_ll_start <- min(nsw_ll$date_confirmation)
  
  
  linelist <- linelist %>%
    filter(
      !(state == "NSW" &
          import_status == "local" &
          date_detection >= nsw_ll_start & 
          date_detection <= nsw_ll_date
      )
    ) %>%
    bind_rows(
      nsw_ll
    )

# flag whether each case is an interstate import
linelist <- linelist %>%
  mutate(
    interstate_import = case_when(
      state == "ACT" ~ interstate_import_cvsi,
      # ACT have indicated that CV_SOURCE_INFECTION is a reliable indicator for their territory
      # whereas postcodes in ACT may also cover NSW so postcodes are less reliable
      state != state_of_acquisition ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% 
  select(-interstate_import_cvsi)


linelist


set.seed(2020-04-29)
source("R/functions.R")

# prepare data for Reff modelling
data <- reff_model_data(linelist_raw = linelist)

data$dates$linelist

model_data <- data

local_cases <- tibble::tibble(
  date_onset = rep(model_data$dates$onset, model_data$n_states),
  detection_probability = as.vector(model_data$detection_prob_mat),
  state = rep(model_data$states, each = model_data$n_dates),
  count = as.vector(model_data$local$cases_infectious),
  acquired_in_state = as.vector(model_data$local$cases)
) 



lc_long <- local_cases %>%
  filter(date_onset >"2021-11-30") %>%
  filter(detection_probability > 0.01) %>%
  select(-acquired_in_state) %>%
  mutate(projected_count = count/detection_probability) %>%
  group_by(state, date_onset) %>%
  mutate(proj = projected_count - count) %>%
  select(-projected_count) %>%
  pivot_longer(cols = c("count", "proj"), names_to = "type", values_to = "count")

prob_line <- lc_long %>%
  filter(type == "count") %>%
  filter(detection_probability >= 0.95) %>%
  group_by(state) %>%
  filter(detection_probability == min(detection_probability)) %>%
  select(state,date_onset)

prob_line_alt <- lc_long %>%
  filter(type == "count") %>%
  filter(detection_probability >= 0.9) %>%
  group_by(state) %>%
  filter(detection_probability == min(detection_probability)) %>%
  select(state,date_onset)

lc_long %>%
  ggplot() +
  geom_bar(
    aes(
      x = date_onset,
      y = count,
      fill = type
    ),
    stat = "identity"
  ) +
  geom_vline(
    data = prob_line,
    aes(
      xintercept = date_onset
    )
  ) +
  geom_vline(
    data = prob_line_alt,
    aes(
      xintercept = date_onset
    )
  ) +
  facet_wrap(
    facets = vars(state),
    ncol = 2,
    scales = "free_y"
  )
    
ggsave("outputs/figures/watermelon.png", bg = 'white')

linelist %>%
  filter(
    state == "NSW",
    date_onset >= "2021-11-01",
    date_confirmation >= "2021-11-01",
    import_status == "local"
  ) %>%
  mutate(
    day = weekdays(date_confirmation)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = date_confirmation#,
      #fill = day
    ),
    position = "dodge"
  )

