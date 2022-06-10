#source("R/functions.R")

# #load fitted model for existing linelist
# fitted_model <- readRDS("outputs/fitted_reff_model.RDS")
# 
# states <- fitted_model$data$states


# scrape the cumulative number of doses and convert to daily new doses
scrape <- function(states = NULL) {
  
  if (is.null(states)) {
    states <- c("act","nsw","nt","qld","sa","tas","vic","wa")
  }
  #to lower incase input are upper
  states <- tolower(states)
  
  urls <- paste0("https://covidlive.com.au/report/daily-cases/",states)
  
  covidlive_ll <- tibble(date_confirmation = NULL,state = NULL)
  for (state in 1:length(states)) {
    url <- urls[state]
    state_ll <- url %>%
      read_html() %>%
      html_nodes(
        "table"
      ) %>%
      .[[2]] %>%
      html_table(
        fill = TRUE
      ) %>%
      mutate(
        date_confirmation = as.Date(DATE, format = "%d %B %y"),
        daily_notification = as.numeric(gsub(",","",NEW)),
        daily_notification = ifelse(is.na(daily_notification),0,daily_notification),
        daily_notification = ifelse(daily_notification < 0,0,daily_notification),
        state = toupper(states[state])
      ) %>%
      arrange(
        date_confirmation
      )  %>%
      select(
        date_confirmation,
        daily_notification,
        state
      ) 
    covidlive_ll <- rbind(covidlive_ll,state_ll)
  }
  covidlive_ll
} 

scraped <- scrape()







# #add column for onset
# scraped$date_onset <- NA
# 
# # compute delays from NINDSS
# regular_ll <- load_linelist(use_vic = FALSE)
# #regular_ll <- readRDS("outputs/preloaded_ll.RDS")
# 
# notification_delay_cdf <- get_notification_delay_cdf(regular_ll)
# 
# # impute onset dates and infection dates using this
# linelist_covidlive <- scraped %>%
#   impute_linelist(notification_delay_cdf = notification_delay_cdf)
# linelist_covidlive$date_onset <- as_date(linelist_covidlive$date_onset)
# linelist_covidlive$date <- as_date(linelist_covidlive$date)
# 
# #make into reff data format
# linelist_covidlive$import_status <- "local"
# 
# saveRDS(linelist_covidlive,"outputs/covid_live_imputed.RDS")
# linelist_covidlive <- readRDS("outputs/covid_live_imputed.RDS")
# 
# #remove the known imports
# 
# scrape_import <- function(states = NULL) {
#   
#   if (is.null(states)) {
#     states <- c("act","nsw","nt","qld","sa","tas","vic","wa")
#   }
#   #to lower incase input are upper
#   states <- tolower(states)
#   
#   urls <- paste0("https://covidlive.com.au/report/daily-source-overseas/",states)
#   
#   covidlive_ll <- tibble(date_confirmation = NULL,state = NULL)
#   for (state in 1:length(states)) {
#     url <- urls[state]
#     state_ll <- url %>%
#       read_html() %>%
#       html_nodes(
#         "table"
#       ) %>%
#       .[[2]] %>%
#       html_table(
#         fill = TRUE
#       ) %>%
#       mutate(
#         date_confirmation = as.Date(DATE, format = "%d %B %y"),
#         daily_notification = as.numeric(gsub(",","",NET)),
#         daily_notification = ifelse(is.na(daily_notification),0,daily_notification),
#         daily_notification = ifelse(daily_notification < 0 | daily_notification > 500 ,0,daily_notification),
#         state = toupper(states[state])
#       ) %>%
#       arrange(
#         date_confirmation
#       )  %>%
#       select(
#         date_confirmation,
#         daily_notification,
#         state
#       ) 
#     covidlive_ll <- rbind(covidlive_ll,state_ll)
#   }
#   covidlive_ll
# } 
# 
# scraped_import <- scrape_import()
# scraped_import <- scraped_import %>% filter(daily_notification > 0,date_confirmation >= "2021-12-01")
# 
# 
# for (i in 1:nrow(scraped_import)) {
#   
#   linelist_covidlive$import_status[linelist_covidlive$date_confirmation == scraped_import$date_confirmation[i] & 
#          linelist_covidlive$state == scraped_import$state[i]][scraped_import$daily_notification[i]] <- "imported" 
# }
# 
# summary(as.factor(linelist_covidlive$import_status))
# 
# 
# linelist <- regular_ll %>% filter(date_confirmation < "2021-12-01" | state == "NSW")
# 
# linelist <- linelist_covidlive %>% filter(date_confirmation >= "2021-12-01" & state != "NSW") %>% bind_rows(linelist)
# 
# linelist$interstate_import[is.na(linelist$interstate_import)] <- FALSE
# 
# linelist$date_linelist[is.na(linelist$date_linelist)] <- as_date(Sys.Date())
# 
# 
# data <- reff_model_data(linelist_raw = linelist)
# 
# 
# # get linelist date and state information
# earliest_date <- min(linelist_covidlive$date)
# latest_date <- max(linelist_covidlive$date)
# 
# states <- sort(unique(linelist_covidlive$state))
# dates <- seq(earliest_date, latest_date, by = 1)
# #mobility_dates <- seq(earliest_date, latest_mobility_date, by = 1)
# 
# n_states <- length(states)
# n_dates <- length(dates)
# n_extra <- as.numeric(Sys.Date() - max(dates)) + 7 * 6
# date_nums <- seq_len(n_dates + n_extra)
# dates_project <- earliest_date + date_nums - 1
# n_dates_project <- n_date_nums <- length(date_nums)
# 
# # build a vector of inducing points, regularly spaced over time but with one on
# # the most recent date
# inducing_date_nums <- rev(seq(n_date_nums, 1, by = -3))
# n_inducing <- length(inducing_date_nums)
# 
# # get detection probabilities for these dates and states
# detection_prob_mat <- detection_probability_matrix(
#   latest_date = latest_date - 1,
#   infection_dates = dates,
#   states = states,
#   notification_delay_cdf = notification_delay_cdf
# )
# 
# # subset to dates with reasonably high detection probabilities in some states
# detectable <- detection_prob_mat >= 0.95
# 
# # the last date with infection data we include
# last_detectable_idx <- which(!apply(detectable, 1, any))[1]
# latest_infection_date <- dates[ifelse(is.na(last_detectable_idx), length(dates), last_detectable_idx)]
# 
# # those infected in the state
# local_cases <- linelist_covidlive %>%
#   infections_by_region(
#     region_type = "state",
#     case_type = "local"
#   )
# 
# # and those infected in any state, but infectious in this one
# local_cases_infectious <- linelist_covidlive %>%
#   infections_by_region(
#     region_type = "state",
#     case_type = "local"
#   )
# 
# 
# detection_prob_mat[] <- pmax(detection_prob_mat, 1e-6)
# 
# 
# local_cases <- tibble::tibble(
#   date_onset = rep(dates + 5, n_states),
#   detection_probability = as.vector(detection_prob_mat),
#   state = rep(states, each = n_dates),
#   count = as.vector(local_cases_infectious),
#   acquired_in_state = as.vector(local_cases)
# )
# local_cases$source <- "covidlive.com.au"
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
# 
# #get regular daily onset count
# local_cases_reg <- tibble::tibble(
#   date_onset = rep(fitted_model$data$dates$onset, fitted_model$data$n_states),
#   detection_probability = as.vector(fitted_model$data$detection_prob_mat),
#   state = rep(fitted_model$data$states, each = fitted_model$data$n_dates),
#   count = as.vector(fitted_model$data$local$cases_infectious),
#   acquired_in_state = as.vector(fitted_model$data$local$cases)
# )
# local_cases_reg$source <- "NINDSS"
# local_cases_reg$source[local_cases_reg$state == "NSW"] <- "NCIMS"
# 
# summary(as.factor(local_cases_reg$source))
# 
# local_cases_combined <- rbind(local_cases_reg,local_cases)
# 
# lc_long <- local_cases_combined %>%
#   filter(date_onset >"2021-11-30" & date_onset <= "2022-02-08") %>%
#   select(-acquired_in_state) %>%
#   group_by(state, date_onset)
# 
# lc_long %>%
#   ggplot() +
#   geom_bar(
#     aes(
#       x = date_onset,
#       y = count,
#       fill = source
#     ),
#     stat = "identity",
#     position = "dodge"
#   ) +
#   facet_wrap(
#     facets = vars(state),
#     ncol = 2,
#     scales = "free_y"
#   ) + scale_fill_manual(values=c("#1b9e77", "#e7298a","#a6761d")) +
#   
#   #cowplot::theme_cowplot() +
#   #cowplot::panel_border(remove = TRUE) + 
#   theme(
#         strip.background = element_blank(),
#         strip.text = element_text(hjust = 0, face = "bold"),
#         axis.title.y.right = element_text(vjust = 0.5, angle = 90),
#         # axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.text.x = element_text(size = 9),
#         panel.spacing = unit(1.2, "lines"))
# 
# ggsave("outputs/figures/onset_comparison_covid_live.png", bg = 'white',height = 7, width = 12)
