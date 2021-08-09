# cases of spillover (import-local transmission) in during mandatory hotel quarantine
hotel_quarantine_spillover_data <- function() {

  tibble::tribble(
    ~earliest_date, ~latest_date, ~state, ~infectee, ~information_source,
    "2020-05-01", "2020-05-14", "VIC", "quarantine security guard (Rydges Hotel)",
    "https://www.dhhs.vic.gov.au/tracking-coronavirus-victoria",
    "2020-08-03", "2020-08-08", "NSW", "quarantine security guard (Sydney Harbour Marriott Hotel)",
    "https://www.health.nsw.gov.au/news/Pages/20200818_01.aspx",
    "2020-11-01", "2020-11-14", "SA", "quarantine security guard (Peppers Waymouth Hotel)",
    "https://www.sahealth.sa.gov.au/wps/wcm/connect/public+content/sa+health+internet/about+us/news+and+media/all+media+releases/covid-19+update+15+november",
    "2020-11-27", "2020-11-30", "NSW", "quarantine domestic worker (Novotel Darling Harbour)",
    "https://www.health.nsw.gov.au/news/Pages/20201204_01.aspx",
    "2020-12-07", "2020-12-12", "NSW", "quarantine airport driver (Sydney Ground Transport, Alexandria)",
    "https://www.abc.net.au/news/2020-12-16/nsw-confirms-new-locally-acquired-coronavirus-case/12988866",
    "2020-12-01", "2020-12-13", "NSW", "unknown (link from quarantine hotel to Northern Beaches)",
    "https://www.health.nsw.gov.au/news/Pages/20201216_03.aspx",
    "2020-12-31", "2021-01-02", "QLD", "quarantine cleaner (Hotel Grand Chancellor)",
    "https://www.health.qld.gov.au/news-events/doh-media-releases/releases/public-health-alert-brisbane",
  ) %>%
    mutate_at(
      c("earliest_date", "latest_date"),
      as.Date
    )
    
}
