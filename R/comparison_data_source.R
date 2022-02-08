source("R/functions.R")

#library(readxl); library(tidyverse); library(lubridate);library(rvest)

test <- read_xlsx("~/not_synced/Combined_RAT_and_PCR_Notifications_by_Jurisdiction_from_6_January_2022.xlsx",
                  range = "B4:AC31",
                  col_types = c("date",rep("numeric",27))) %>% 
  select(c(Date,starts_with("Total")))

states <- names(read_xlsx("~/not_synced/Combined_RAT_and_PCR_Notifications_by_Jurisdiction_from_6_January_2022.xlsx",
                          range = "B3:AC3"))
states <- states[-grep("...",states,fixed = TRUE)]

colnames(test)[2:10] <- states


Jennie_ll <- test %>%
  select(-Australia) %>% 
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
  ) #%>% 
#uncount(weights = daily_notification)

Jennie_ll$source <- "Jennie"
#Jennie_ll$date_onset <- NA


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
      ) #%>% 
    #uncount(weights = daily_notification)
    covidlive_ll <- rbind(covidlive_ll,state_ll)
  }
  covidlive_ll
} 

scraped <- scrape()
#add column for onset
scraped$source <- "covidlive"

combined <- rbind(Jennie_ll,scraped) %>% filter(date_confirmation >= "2022-01-06")

#combine in regular
regular <- regular_ll %>% 
  select(date_confirmation,state) %>% 
  filter(date_confirmation >= "2022-01-06") %>% group_by(state,date_confirmation) %>% 
  count() %>% 
  rename(daily_notification = n) %>% ungroup() %>% 
  mutate(source = "regular") %>% 
  select(colnames(Jennie_ll))
  
combined <- rbind(combined,regular) %>% filter(date_confirmation >= "2022-01-06")

combined %>% ggplot(aes(x = date_confirmation, y = daily_notification)) + 
  geom_col(aes(fill = source), position = position_dodge2(width = 0.9, preserve = "single"))+
  facet_wrap(
    facets = vars(state),
    ncol = 2,
    scales = "free_y"
  ) + scale_fill_manual(values=c("#1b9e77", "#d95f02","#7570b3"))

ggsave("outputs/figures/covidlive vs Jennie vs NINDSS_NCIMS_DHHS.png")
