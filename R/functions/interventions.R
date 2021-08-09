interventions <- function(
  which = c(
    "all",
    "national",
    "act",
    "nsw",
    "nt",
    "qld",
    "sa",
    "tas",
    "vic",
    "wa"
  ),
  end_dates = FALSE,
  exclude_after = NA
) {
  
  which <- match.arg(which)
  
  act_interventions <- tibble::tribble(
    ~date, ~state,
    
  )
  
  nsw_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-06-25", "NSW", # stay-at-home order for 4 LGAs from 11.59 PM 24th, extended to all greater sydney +++ from 11.59 PM 25th. 
    "2021-07-18", "NSW" # increased restrictions from midnight 17th https://www.nsw.gov.au/media-releases/restrictions-to-further-limit-spread-of-covid-19-delta-strain
  )
  
  nt_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-06-27", "NT" # https://coronavirus.nt.gov.au/updates/items/2021-06-27-covid-19-update-lockdown-restrictions-in-place
  )
  
  qld_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-01-09", "QLD",
    "2021-03-29", "QLD",
    "2021-06-29", "QLD" # starts 6 PM on 29th https://www.qld.gov.au/health/conditions/health-alerts/coronavirus-covid-19/current-status/public-health-directions/restrictions-in-qld-update
  )
  
  sa_interventions <- tibble::tribble(
    ~date, ~state,
    "2020-11-19", "SA",
    "2021-07-20", "SA" # lockdown, starts 6pm on the 20th, https://www.sahealth.sa.gov.au/wps/wcm/connect/public+content/sa+health+internet/about+us/news+and+media/all+media+releases/covid-19+update+20+july+2021
  )
  
  tas_interventions <- tibble::tribble(
    ~date, ~state,
    
  )
  
  vic_interventions <- tibble::tribble(
    ~date, ~state,
    "2020-07-01", "VIC",
    "2020-07-08", "VIC",
    "2020-08-02", "VIC",
    "2021-02-13", "VIC",
    "2021-05-28", "VIC",
    "2021-07-16", "VIC" # lockdown, 5 days from 11:59 the 15th, then extended https://www.dhhs.vic.gov.au/coronavirus-update-victoria-15-july-2021
  )
  
  wa_interventions <- tibble::tribble(
    ~date, ~state,
    "2021-01-31", "WA",
    "2021-04-24", "WA",
    "2021-06-29", "WA" # https://www.wa.gov.au/government/announcements/4-day-lockdown-introduced-perth-and-peel
  )
  
  
  national_interventions <- expand_grid(
    date = c("2020-03-16", "2020-03-24", "2020-03-29"),
    state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
  )
  
  
  if(end_dates){
    
    # act_interventions <-  act_interventions %>%
    #   bind_rows(
    #     tibble::tribble(
    #       ~date, ~state,
    #   
    #     )
    #   )
    
    # nsw_interventions <-  nsw_interventions %>%
    #   bind_rows(
    #     tibble::tribble(
    #       ~date, ~state,
    #       #"2021-07-17", "NSW" # https://www.abc.net.au/news/2021-07-07/covid-live-updates-coronavirus-press-conference-sydney-lockdown/100270832?utm_campaign=abc_news_web&utm_content=link&utm_medium=content_shared&utm_source=abc_news_web#live-blog-post-1201975138
    #     )
    #   )
    
    nt_interventions <-  nt_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-07-02", "NT" # https://coronavirus.nt.gov.au/updates/items/2021-06-28-covid-19-update-nt
        )
      )
    
    qld_interventions <-  qld_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-01-12", "QLD",
          "2021-04-01", "QLD",
          "2021-07-04", "QLD" # Lifted 6 PM 3rd July https://www.qld.gov.au/health/conditions/health-alerts/coronavirus-covid-19/current-status/public-health-directions/restrictions-impacted-areas
        )
      )
    
    sa_interventions <-  sa_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2020-11-22", "SA"
        )
      )
    
    # tas_interventions <-  tas_interventions %>%
    #   bind_rows(
    #     tibble::tribble(
    #       ~date, ~state,
    #       
    #     )
    #   )
    
    vic_interventions <-  vic_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-02-18", "VIC",
          "2021-06-11", "VIC"
        )
      )
    
    wa_interventions <-  wa_interventions %>%
      bind_rows(
        tibble::tribble(
          ~date, ~state,
          "2021-02-05", "WA",
          "2021-04-27", "WA",
          "2021-07-03", "WA" # https://www.wa.gov.au/government/announcements/end-of-lockdown-perth-and-peel-1201am-saturday-3-july
        )
      )
  }
  
  interventions <- switch(
    which,
    national = national_interventions %>%
      filter(state == "ACT") %>%
      mutate(state = "all"),
    act = act_interventions,
    nsw = nsw_interventions,
    nt  = nt_interventions,
    qld = qld_interventions,
    sa  = sa_interventions,
    tas = tas_interventions,
    vic = vic_interventions,
    wa =  wa_interventions,
    all = bind_rows(
      national_interventions,
      act_interventions,
      nsw_interventions,
      nt_interventions,
      qld_interventions,
      sa_interventions,
      tas_interventions,
      vic_interventions,
      wa_interventions,
    )
  ) %>%
    mutate(
      date = as.Date(date),
      state = factor(state)
    ) %>% 
    arrange(state, date)
  
  if(!is.na(exclude_after)){
    
    interventions <- interventions %>%
      filter(date <= as.Date(exclude_after))
  }
  
  return(interventions)
}
