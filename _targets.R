source("./packages-not-greta.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)


# LGA activity mobility from google
# use st_overlap to get the weights of how the google LGAs map to
# the ABS LGA data so 
# need to find an ABS defn
## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  # read in all surveys
  doh_survey_weights = read_doh_add_weights(
    doh_path = "~/not_synced/survey_data/",
    abs_lga_postcodes_path = "~/not_synced/CA_POSTCODE_2018_LGA_2018.xlsx"
  ),
  vec_lgas_of_concern = generate_lgas_of_concern(),
  # contact_num and contact_work
  # filter to NSW
  # by date and NSW LGA,
  doh_survey_weights_nsw = prepare_survey_nsw(data = doh_survey_weights,
                                              pmin_contact_num = 100,
                                              vec_lgas_concern = vec_lgas_of_concern),
  # compute both the number of respondents, and
  # the average number of contacts per respondent
  doh_survey_weights_nsw_summarised_lga_concern = doh_survey_weights_nsw %>% 
    group_by(date_week, lga_of_concern) %>% 
    summarise_contacts_weights(),
  
  # # plotting contact numbers separately for 16-39yos and 40+ yos (shouldn't be any under 16s)
  doh_survey_nsw_weights_age_groups = doh_survey_weights_nsw %>%
    group_by(date_week, lga_of_concern, age_groups) %>%
    summarise_contacts_weights(),
  
  doh_survey_nsw_weights_16_39 = doh_survey_weights_nsw %>%
    group_by(date_week, lga_of_concern, youth_older) %>%
    summarise_contacts_weights(),
  
  tar_render(nsw_lgas, "doc/nsw-lgas.Rmd")
  
  # 16-39yos and 40+ yos
  # and also compare the 16-39yos vs the whole population
  
)
