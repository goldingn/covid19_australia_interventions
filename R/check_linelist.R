# check NNDSS linelist for missing entries and local cases in last 3 weeks.

source("R/lib.R")

source("R/functions.R")

dat <- preprocess_nndss_linelist()

filter_date <- dat$data$date_time - lubridate::days(28)

check_dat <- dat$dat %>%
  filter(NOTIFICATION_RECEIVE_DATE >= filter_date)

check_dat %>% 
  ggplot() +
  geom_bar(
    aes(
      x = NOTIFICATION_RECEIVE_DATE,
    ),
    stat = "count"
  ) + 
  geom_vline(aes(xintercept = as_date(dat$data$date_time))) +
  facet_wrap(
    facets = vars(STATE),
    ncol = 2,
    scales = "free_y"
  ) 

ggsave("outputs/figures/NR_date_count.png")


# linelist_check <- df %>%
#   dplyr::select(
#     STATE,
#     NOTIFICATION_DATE,
#     NOTIFICATION_RECEIVE_DATE,
#     AGE_AT_ONSET,
#     SEX,
#     PLACE_OF_ACQUISITION,
#     CV_SOURCE_INFECTION,
#     import_status
#   ) %>% 
#   dplyr::arrange(STATE, desc(NOTIFICATION_RECEIVE_DATE)) %>%
#   print(n = 100)


# # watermelon --------------------------------------------------------------
# # takes time to run, probably should not source
# 
# set.seed(2020-04-29)
# 
# get_nndss_start <- Sys.time()
# 
# linelist <- get_nndss_linelist(preprocessed = dat)
# 
# Sys.time()-get_nndss_start
# 
# #quick fix for interstate import
# # flag whether each case is an interstate import
# linelist <- linelist %>%
#   mutate(
#     interstate_import = case_when(
#       state == "ACT" ~ interstate_import_cvsi,
#       # ACT have indicated that CV_SOURCE_INFECTION is a reliable indicator for their territory
#       # whereas postcodes in ACT may also cover NSW so postcodes are less reliable
#       state != state_of_acquisition ~ TRUE,
#       TRUE ~ FALSE
#     )
#   ) %>% 
#   select(-interstate_import_cvsi)
# 
# 
# reff_model_data_start <- Sys.time()
# 
# # prepare data for Reff modelling
# model_data <- reff_model_data(linelist_raw = linelist)
# 
# Sys.time()-reff_model_data_start
# 
# local_cases <- tibble::tibble(
#   date_onset = rep(model_data$dates$onset, model_data$n_states),
#   detection_probability = as.vector(model_data$detection_prob_mat),
#   state = rep(model_data$states, each = model_data$n_dates),
#   count = as.vector(model_data$local$cases_infectious),
#   acquired_in_state = as.vector(model_data$local$cases)
# ) 
# 
# 
# 
# lc_long <- local_cases %>%
#   filter(date_onset >"2021-11-30") %>%
#   filter(detection_probability > 0.01) %>%
#   select(-acquired_in_state) %>%
#   mutate(projected_count = count/detection_probability) %>%
#   group_by(state, date_onset) %>%
#   mutate(proj = projected_count - count) %>%
#   select(-projected_count) %>%
#   pivot_longer(cols = c("count", "proj"), names_to = "type", values_to = "count")
# 
# prob_line_95 <- lc_long %>%
#   filter(type == "count") %>%
#   filter(detection_probability >= 0.95) %>%
#   group_by(state) %>%
#   filter(detection_probability == min(detection_probability)) %>%
#   select(state,date_onset)
# 
# prob_line_90 <- lc_long %>%
#   filter(type == "count") %>%
#   filter(detection_probability >= 0.9) %>%
#   group_by(state) %>%
#   filter(detection_probability == min(detection_probability)) %>%
#   select(state,date_onset)
# 
# lc_long %>%
#   ggplot() +
#   geom_bar(
#     aes(
#       x = date_onset,
#       y = count,
#       fill = type
#     ),
#     stat = "identity"
#   ) +
#   geom_vline(
#     data = prob_line_95,
#     aes(
#       xintercept = date_onset
#     )
#   ) +
#   geom_vline(
#     data = prob_line_90,
#     aes(
#       xintercept = date_onset
#     )
#   ) +
#   facet_wrap(
#     facets = vars(state),
#     ncol = 2,
#     scales = "free_y"
#   )
# 
# ggsave("outputs/figures/watermelon_NINDSS_only.png", bg = 'white')




