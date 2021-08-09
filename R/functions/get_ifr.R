get_ifr <- function(voc = TRUE) {
  
  # Age-structured infection fatality ratio (%) estimates from O'Driscoll et al.
  # 2020 https://doi.org/10.1038/s41586-020-2918-0 (Table S3 in supplement) and
  # Brazeau et al. 2020 (Imperial report 34)
  # https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-34-ifr/
  # (Table 2 - estimate *with* seroreversion)
  
  # Brazeau et al has separate estimates for ages 80-84, 85-89, 90+, so we
  # recombine these based on ABS 2020 population age fractions into the 80+
  # category (50% ofover 80s are in 80-84, 30% are in 85-89, and 20% are 90 or
  # older)
  
  ifr <- tibble::tribble(
    ~age,      ~odriscoll,     ~brazeau,
    "0-4",          0.003,         0.00,
    "5-9",          0.001,         0.01,
    "10-14",        0.001,         0.01,
    "15-19",        0.003,         0.02,
    "20-24",        0.006,         0.02,
    "25-29",        0.013,         0.04,
    "30-34",        0.024,         0.06,
    "35-39",        0.040,         0.09,
    "40-44",        0.075,         0.15,
    "45-49",        0.121,         0.23,
    "50-54",        0.207,         0.36,
    "55-59",        0.323,         0.57,
    "60-64",        0.456,         0.89,
    "65-69",        1.075,         1.39,
    "70-74",        1.674,         2.17,
    "75-79",        3.203,         3.39,
    "80+",          8.292,         5.3*0.5 + 8.28*0.3 + 16.19*0.2
  )
  
  
  if(voc){
    # multiplier from Davis et al.
    ifr <- ifr %>%
      mutate(
        odriscoll = odriscoll*1.61,
        brazeau = brazeau*1.61
      )
  }
  
  return(ifr)
  
}
