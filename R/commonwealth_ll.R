source("R/functions.R")

#library(readxl); library(tidyverse); library(lubridate);library(rvest)

linelist_commonwealth <- read_xlsx("~/not_synced/PCR and RAT Breakdown (Power Query).xlsx",
                  range = "B4:AC95",sheet = 2,
                  col_types = c("date",rep("numeric",27))) %>% 
  select(-starts_with("Total"))

states <- names(read_xlsx("~/not_synced/PCR and RAT Breakdown (Power Query).xlsx",
                          range = "B3:AC3",sheet = 2))
states <- states[-grep("...",states,fixed = TRUE)]

states <- rep(states,each = 2)

#get test type designation
colnames(linelist_commonwealth) <- word(colnames(linelist_commonwealth),1,1, sep = fixed("..."))


colnames(linelist_commonwealth)[2:19] <- paste(colnames(linelist_commonwealth)[2:19],states,sep = "_")

#check colnames
colnames(linelist_commonwealth)
#remove total row
linelist_commonwealth <- linelist_commonwealth[-1,]

linelist_commonwealth <- linelist_commonwealth %>%
  select(-ends_with("Australia")) %>% 
  pivot_longer(-Date,
               names_to = "state",
               values_to = "daily_notification") %>% 
  mutate(
    date_confirmation = as_date(Date)
  ) %>%
  arrange(
    date_confirmation
  )  %>%
  select(
    date_confirmation,
    daily_notification,
    state
  ) %>% 
  mutate("test_type" = word(state,1,1, sep = fixed("_")),
         "state" = word(state,2,2, sep = fixed("_"))) %>% 
  uncount(weights = daily_notification) 

#Jennie_ll$source <- "Commonwealth"
#add column for onset
linelist_commonwealth$date_onset <- NA


# compute delays from NINDSS
regular_ll <- load_linelist(use_vic = FALSE)
#regular_ll <- readRDS("outputs/preloaded_ll.RDS")

#visualise dow wave
regular_ll %>% filter(date_confirmation >= "2022-02-01") %>% 
  ggplot() +
  geom_bar(
    aes(
      x = date_confirmation,
    ),
    stat = "count"
  ) + 
  geom_vline(aes(xintercept = unique(date_linelist)[1])) +
  facet_wrap(
    facets = vars(state),
    ncol = 2,
    scales = "free_y"
  ) 

ggsave("outputs/figures/NR_date_count_NINDSS_NCIMS.png")

delays_to_confirmation_NCIMS <- readRDS("outputs/delay_to_confirmation_cdfs.RDS")

notification_delay_cdf <- function(delays, possible_onset_dates, states) {
  
  #pullout the latest NSW delay to use for everything
  cdfs <- delays_to_confirmation_NCIMS$ecdf[delays_to_confirmation_NCIMS$date == max(delays_to_confirmation_NCIMS$date) & delays_to_confirmation_NCIMS$state == "NSW"][[1]]

    probs <- cdfs(delays)

  probs
  
}

#test delay ecdf behaviour
old_delay_cdf <- get_notification_delay_cdf(regular_ll)

saveRDS(old_delay_cdf,"outputs/old_method_delay_cdf.RDS")
delay_ecdf_plot <- tibble("days" = -3:14,
                          "new_delay" = notification_delay_cdf(days, NULL,NULL),
                          "old_delay_Vic" = old_delay_cdf(days,
                                                          possible_onset_dates = rep("2022-01-01",length(days)),
                                                                       "VIC"),
                          "old_delay_other" = old_delay_cdf(days,
                                                          possible_onset_dates = rep("2022-01-01",length(days)),
                                                          "NSW"))
delay_ecdf_plot <- delay_ecdf_plot %>% pivot_longer(cols = 2:4,names_to = "type")

delay_ecdf_plot %>% filter(days <= 10) %>% ggplot(aes(x = days,y = value, col = type)) + geom_line() + scale_x_continuous(breaks = -3:10, limits = c(-3,10))

ggsave("outputs/figures/ecdf_delay_compare.png")
# impute onset dates and infection dates using this
#linelist_commonwealth <- Jennie_ll #%>%
#  impute_linelist(notification_delay_cdf = notification_delay_cdf)
#linelist_commonwealth$date_onset <- as_date(linelist_commonwealth$date_onset)
#linelist_commonwealth$date <- as_date(linelist_commonwealth$date)

#make into reff data format
linelist_commonwealth$import_status <- "local"



linelist <- regular_ll %>% filter(date_confirmation < "2022-01-06" | state == "NSW")

linelist <- linelist_commonwealth %>% filter(date_confirmation >= "2022-01-06" & state != "NSW") %>% bind_rows(linelist)

linelist$interstate_import[is.na(linelist$interstate_import)] <- FALSE

linelist$date_linelist[is.na(linelist$date_linelist)] <- as_date(Sys.Date())

linelist$date_onset <- as_date(ifelse(linelist$date_onset < "2020-01-01",NA,linelist$date_onset))

write_linelist(linelist = linelist)

linelist <- linelist %>%
  impute_linelist(notification_delay_cdf = old_delay_cdf)

saveRDS(linelist,"outputs/commonwealth_ll_imputed_old_method.RDS")
linelist <- readRDS("outputs/commonwealth_ll_imputed.RDS")

data <- reff_model_data(linelist_raw = linelist,
                        notification_delay_cdf = old_delay_cdf)

saveRDS(data, "outputs/pre_loaded_reff_data_old_imputation.RDS")

source("R/watermelon_plot.R")



### plot comparison

# get linelist date and state information
earliest_date <- min(linelist_commonwealth$date)
latest_date <- max(linelist_commonwealth$date)

states <- sort(unique(linelist_commonwealth$state))
dates <- seq(earliest_date, latest_date, by = 1)
#mobility_dates <- seq(earliest_date, latest_mobility_date, by = 1)

n_states <- length(states)
n_dates <- length(dates)
n_extra <- as.numeric(Sys.Date() - max(dates)) + 7 * 6
date_nums <- seq_len(n_dates + n_extra)
dates_project <- earliest_date + date_nums - 1
n_dates_project <- n_date_nums <- length(date_nums)

# build a vector of inducing points, regularly spaced over time but with one on
# the most recent date
inducing_date_nums <- rev(seq(n_date_nums, 1, by = -3))
n_inducing <- length(inducing_date_nums)

# get detection probabilities for these dates and states
detection_prob_mat <- detection_probability_matrix(
  latest_date = latest_date - 1,
  infection_dates = dates,
  states = states,
  notification_delay_cdf = notification_delay_cdf
)

# subset to dates with reasonably high detection probabilities in some states
detectable <- detection_prob_mat >= 0.95

# the last date with infection data we include
last_detectable_idx <- which(!apply(detectable, 1, any))[1]
latest_infection_date <- dates[ifelse(is.na(last_detectable_idx), length(dates), last_detectable_idx)]

# those infected in the state
local_cases <- linelist_commonwealth %>%
  infections_by_region(
    region_type = "state",
    case_type = "local"
  )

# and those infected in any state, but infectious in this one
local_cases_infectious <- linelist_commonwealth %>%
  infections_by_region(
    region_type = "state",
    case_type = "local"
  )


detection_prob_mat[] <- pmax(detection_prob_mat, 1e-6)


local_cases <- tibble::tibble(
  date_onset = rep(dates + 5, n_states),
  detection_probability = as.vector(detection_prob_mat),
  state = rep(states, each = n_dates),
  count = as.vector(local_cases_infectious),
  acquired_in_state = as.vector(local_cases)
)
local_cases$source <- "commonwealth"

lc_long <- local_cases %>%
  filter(date_onset >"2021-11-30") %>%
  filter(detection_probability > 0.01) %>%
  select(-acquired_in_state) %>%
  mutate(projected_count = count/detection_probability) %>%
  group_by(state, date_onset) %>%
  mutate(proj = projected_count - count) %>%
  select(-projected_count) %>%
  pivot_longer(cols = c("count", "proj"), names_to = "type", values_to = "count")

prob_line_95 <- lc_long %>%
  filter(type == "count") %>%
  filter(detection_probability >= 0.95) %>%
  group_by(state) %>%
  filter(detection_probability == min(detection_probability)) %>%
  select(state,date_onset)

prob_line_90 <- lc_long %>%
  filter(type == "count") %>%
  filter(detection_probability >= 0.9) %>%
  group_by(state) %>%
  filter(detection_probability == min(detection_probability)) %>%
  select(state,date_onset)


#get regular daily onset count
local_cases_reg <- tibble::tibble(
  date_onset = rep(fitted_model$data$dates$onset, fitted_model$data$n_states),
  detection_probability = as.vector(fitted_model$data$detection_prob_mat),
  state = rep(fitted_model$data$states, each = fitted_model$data$n_dates),
  count = as.vector(fitted_model$data$local$cases_infectious),
  acquired_in_state = as.vector(fitted_model$data$local$cases)
)
local_cases_reg$source <- "NINDSS"
local_cases_reg$source[local_cases_reg$state == "NSW"] <- "NCIMS"

summary(as.factor(local_cases_reg$source))

local_cases_combined <- rbind(local_cases_reg,local_cases)

lc_long <- local_cases_combined %>%
  filter(date_onset >"2021-11-30" & date_onset <= "2022-02-08") %>%
  select(-acquired_in_state) %>%
  group_by(state, date_onset)

lc_long %>%
  ggplot() +
  geom_bar(
    aes(
      x = date_onset,
      y = count,
      fill = source
    ),
    stat = "identity",
    position = "dodge"
  ) +
  facet_wrap(
    facets = vars(state),
    ncol = 2,
    scales = "free_y"
  ) + scale_fill_manual(values=c("#1b9e77", "#e7298a","#a6761d")) +
  
  #cowplot::theme_cowplot() +
  #cowplot::panel_border(remove = TRUE) + 
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.x = element_text(size = 9),
    panel.spacing = unit(1.2, "lines"))

ggsave("outputs/figures/onset_comparison_commonwealth.png", bg = 'white',height = 7, width = 12)

