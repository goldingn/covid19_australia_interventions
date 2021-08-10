# dates of school holidays by state, from:
# 2020: https://info.australia.gov.au/about-australia/special-dates-and-events/school-term-dates
# 2021 onwards:
# https://www.education.act.gov.au/__data/assets/pdf_file/0007/801583/Term-Date-2019-2025.pdf
# https://education.nsw.gov.au/public-schools/going-to-a-public-school/calendars
# https://nt.gov.au/learning/primary-and-secondary-students/school-term-dates-in-nt
# https://education.qld.gov.au/about-us/calendar/term-dates
# https://www.education.sa.gov.au/teaching/south-australian-state-schools-term-dates
# https://www.education.tas.gov.au/about-us/term-dates/term-dates-2021/
# https://www.education.vic.gov.au/about/department/Pages/datesterm.aspx
# https://www.education.wa.edu.au/future-term-dates/
school_holiday_dates <- function() {
  bind_rows(
    tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-26",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-19", "2021-01-28",
                    5, "2021-04-02", "2021-04-18",
                    6, "2021-06-26", "2021-07-11",
                    7, "2021-09-18", "2021-10-04",
                    8, "2021-12-18", "2022-01-27"
    ) %>%
      mutate(
        state = "Australian Capital Territory"
      ),
    tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-26",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-19", "2021-01-26",
                    5, "2021-04-05", "2021-04-16",
                    6, "2021-06-28", "2021-07-09",
                    7, "2021-09-20", "2021-10-01",
                    8, "2021-12-20", "2022-01-27"
    ) %>%
      mutate(
        state = "New South Wales"
      ),
    tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-19",
                    2, "2020-06-27", "2020-07-20",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-18", "2021-01-31",
                    5, "2021-04-10", "2021-04-18",
                    6, "2021-06-26", "2021-07-19",
                    7, "2021-09-25", "2021-10-10",
                    8, "2021-12-17", "2022-01-30"
    ) %>%
      mutate(
        state = "Northern Territory"
      ),
    tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-04", "2020-04-19",
                    2, "2020-06-27", "2020-07-12",
                    3, "2020-09-19", "2020-10-05",
                    4, "2020-12-12", "2021-01-26",
                    5, "2021-04-02", "2021-04-18",
                    6, "2021-06-26", "2021-07-11",
                    7, "2021-09-18", "2021-10-05",
                    8, "2021-12-11", "2022-01-26"
    ) %>%
      mutate(
        state = "Queensland"
      ),
    tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-26",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-12", "2021-01-26",
                    5, "2021-04-10", "2021-04-26",
                    6, "2021-07-03", "2021-07-18",
                    7, "2021-09-25", "2021-10-10",
                    8, "2021-12-11", "2022-01-30"
    ) %>%
      mutate(
        state = "South Australia"
      ),
    tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-26",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-18", "2021-02-02",
                    5, "2021-04-10", "2021-04-25",
                    6, "2021-07-03", "2021-07-19",
                    7, "2021-09-25", "2021-10-10",
                    8, "2021-12-17", "2022-02-02"
    ) %>%
      mutate(
        state = "Tasmania"
      ),
    tribble(~school_holiday, ~start, ~end,
                    # Vic extended each school holiday by a week during the
                    # pandemic
                    # https://www.education.vic.gov.au/about/department/Pages/datesterm.aspx
                    1, "2020-03-25", "2020-04-13",
                    2, "2020-06-27", "2020-07-19",
                    3, "2020-09-19", "2020-10-04",
                    4, "2020-12-19", "2021-01-26",
                    5, "2021-04-02", "2021-04-18",
                    6, "2021-06-26", "2021-07-11",
                    7, "2021-09-18", "2021-10-03",
                    8, "2021-12-18", "2022-01-27"
    ) %>%
      mutate(
        state = "Victoria"
      ),
    tribble(~school_holiday, ~start, ~end,
                    1, "2020-04-10", "2020-04-27",
                    2, "2020-07-04", "2020-07-19",
                    3, "2020-09-26", "2020-10-11",
                    4, "2020-12-18", "2021-01-31",
                    5, "2021-04-02", "2021-04-18",
                    6, "2021-07-03", "2021-07-18",
                    7, "2021-09-25", "2021-10-10",
                    8, "2021-12-17", "2022-01-30"
    ) %>%
      mutate(
        state = "Western Australia"
      )
  ) %>%
    mutate(
      start = date(start),
      end = date(end)
    ) %>%
    # loop through, expanding out into dates within term time
    mutate(id = row_number()) %>%
    group_by(id) %>%
    do(
      tibble(
        state = .$state,
        school_holiday = .$school_holiday, 
        date = seq(from = .$start, to =.$end, by = 1)
      )
    ) %>%
    ungroup() %>%
    select(-id)
}
