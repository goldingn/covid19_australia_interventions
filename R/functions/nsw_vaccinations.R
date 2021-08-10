nsw_vaccinations <- function(
  file = "data/vaccinatinon/nsw/Coverage_data_July27/AIR Data 20210727 Special Interest LGAs -v2.xlsx"
) {
  
  # lookup to remove (C), (A) etc from lga in doses
  lga_lookup <- tribble(
    ~lga_long, ~lga_short,
    "Blacktown (C)", "Blacktown",
    "Canterbury-Bankstown (A)", "Canterbury-Bankstown",
    "Cumberland (A)", "Cumberland",
    "Fairfield (C)", "Fairfield",
    "Liverpool (C)", "Liverpool",
    "Rest of Greater Sydney", NA_character_,
    "Rest of NSW", NA_character_
  )
  
  # load and combine doses 1 and 2
  doses <- full_join(
    file %>%
      format_nsw_vaccination_data(
        sheet = "Dose 1"
      ) %>%
      rename(
        dose_1 = cumulative_count
      ),
    file %>%
      format_nsw_vaccination_data(
        sheet = "Dose 2"
      ) %>%
      rename(
        dose_2 = cumulative_count
      ),
    by = c("lga", "vaccine", "age", "date")
  ) %>%
    mutate(
      only_dose_1 = dose_1 - dose_2
    ) %>%
    # shorten LGA names for population join
    left_join(
      lga_lookup,
      by = c("lga" = "lga_long")
    ) %>%
    mutate(
      lga = lga_short
    ) %>%
    select(
      -lga_short
    )
  
  pops <- nsw_lga_populations()
  
  combinations <- expand_grid(
    lga = unique(doses$lga),
    vaccine = unique(doses$vaccine),
    age = unique(pops$age),
    date = unique(doses$date)
  )
  
  # join on populations including empty values for 0-15s 
  combinations %>%
    left_join(
      doses,
      by = c("lga", "vaccine", "age", "date")
    ) %>%
    left_join(
      pops,
      by = c("lga", "age")
    ) %>%
    mutate(
      across(
        c(dose_1, dose_2, only_dose_1),
        ~replace_na(., 0)
      )
    )
    
}
