#calculate admission/infection ratio from hospitalisation and case linelists
source("R/lib.R")


source("R/functions.R")



#read in hospitalisation linelist

files <- list.files(
  "../not_synced/nsw/clinical_linelists/",
  pattern = "NSW_out_episode",
  full.names = TRUE
)

dates <- files %>%
  basename() %>%
  substr(17, 26) %>%
  as.Date(format = "%Y_%m_%d")
latest <- which.max(dates)
file <- files[latest]
date <- dates[latest]

nsw_clinic_linelist <- file %>%
  read_xlsx(sheet = 2,range = cell_cols("A:O"),
            col_types = c(
              person_id = "numeric",
              age = "numeric",
              roh_score = "numeric",
              roh3_band_txt = "text",
              load_date = "date",
              VISIT_FACILITY_NAME = "text",
              VISIT_FACILITY_ID = "numeric",
              admit_date = "date",
              discharge_date = "date",
              admit_date_dt = "date",
              discharge_date_dt = "date",
              still_in_hosp = "numeric",
              los_hours = "numeric",
              any_icu_flag = "numeric",
              still_in_icu = "numeric"
            )
  ) %>%    # remove some bogus dates
  mutate(across(
    all_of(c(
      "admit_date",
      "discharge_date"
    )),
    clean_date
  )
  ) %>% select(person_id, age, VISIT_FACILITY_NAME,
               admit_date,
               discharge_date,los_hours)

nsw_clinic_linelist_calculation_period <- nsw_clinic_linelist %>% 
  filter(admit_date < date)

# count daily hospitalisation
nsw_daily_admit <- as.data.frame(table(nsw_clinic_linelist_calculation_period$admit_date),
                                 stringsAsFactors = FALSE) %>% 
  rename(date = Var1, count_admit = Freq) %>% mutate(date = as_date(date)) %>% arrange(date)


#some quick codes to compare recent linelists
compare_admit <- left_join(data_imputed,nsw_daily_admit, by = c("date" = "date")) %>% 
  rename(admission_04 = count_admit) %>% select(date, admissions, admission_04)

compare_admit <- left_join(compare_admit,nsw_daily_admit, by = c("date" = "date")) %>% 
  rename(admission_12_21 = count_admit) 


#plot compare
compare_admit  %>% 
  filter(date >= "2021-12-01" & date <= "2022-01-11") %>% 
  rename("linelist_12_21" = admission_12_21,
         "linelist_01_04" = admission_04,
         "linelist_01_11" = admissions) %>% 
  pivot_longer(
    cols = -c(date),
    names_to = "type",
    values_to = "count"
  ) %>%
  ggplot(
    aes(
      x = date,
      y = count,
      colour = type
    )
  ) +
  geom_line(
    size = 1
  ) +
  theme_minimal()

ggsave("outputs/figures/admission_linelist_compare.png", bg = 'white')

#get daily infection from local cases input
nsw_daily_infect <- read_csv("outputs/local_cases_input.csv") %>% 
  filter(date_onset < date & date_onset >= min(nsw_daily_admit$date), state == "NSW")


nsw_daily_combined <- left_join(nsw_daily_infect,nsw_daily_admit,by = c("date_onset" = "date"))
#   
#   
#   cbind(nsw_daily_infect,nsw_daily_admit$count_admit) %>% 
#   rename(count_admit = "nsw_daily_admit$count_admit")
# nsw_daily_combined$ratio <- nsw_daily_combined$count/nsw_daily_combined$count_admit
# mean(nsw_daily_combined$ratio)

nsw_daily_combined$date_num <- seq(1,nrow(nsw_daily_combined))


m <- glm(count ~ offset(log(count_admit)) + date_num, family = stats::poisson, data = nsw_daily_combined)

nsw_daily_combined$infections_pred <- predict(m, newdata = data.frame(count_admit = nsw_daily_combined$count_admit, date_num = nsw_daily_combined$date_num), type = "response")

ggplot(data = nsw_daily_combined, aes(x = date_onset, y = ratio)) + geom_line()


ggplot(data = nsw_daily_combined, aes(x = count, y = infections_pred)) + geom_line()
m
