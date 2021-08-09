# the vector of dates to use for each scenario
scenario_dates <- function(scenario) {
  
  switch(
    scenario$phase,
    importation = seq(as.Date("2020-03-01"),
                      as.Date("2020-04-30"),
                      by = 1),
    suppression = seq(as.Date("2020-05-01"),
                      as.Date("2020-06-30"),
                      by = 1),
    community = seq(as.Date("2020-07-01"),
                    as.Date("2020-08-31"),
                    by = 1)
  )
  
}
