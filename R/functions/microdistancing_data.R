# get data for fitting and predicting from microdistancing model
microdistancing_data <- function(dates = NULL) {
  
  # assume adoption of microdistancing follows the same trend as macrodistancing,
  # and that waning starts at the same time, but don't assume it wanes at the same
  # rate
  distancing <- readRDS("outputs/social_distancing_latent.RDS")
  
  # use dates from start of distancing to present if no others are specified
  if (is.null(dates)) {
    dates <- seq(
      min(distancing$date),
      Sys.Date(),
      by = 1
    )
  }
  
  survey <- hygiene_data() %>%
    filter(
      date %in% dates
    )
  
  # clip distancing to non-degenerate values
  range <- range(distancing$mean[!distancing$mean %in% c(0,  1)])
  
  # get data to predict to
  pred_data <- distancing %>%
    rename(distancing = mean) %>%
    mutate(
      distancing = pmax(distancing, range[1]),
      distancing = pmin(distancing, range[2])
    ) %>%
    select(date, distancing) %>%
    old_right_join(
      expand_grid(
        date = dates,
        state = unique(survey$state)
      )
    ) %>%
    mutate(
      distancing = case_when(
        is.na(distancing) & date < min(dates) ~ 0,
        is.na(distancing) & date >= min(dates) ~ 1,
        TRUE ~ distancing
      )
    ) %>%
    mutate(
      state_id = match(state, unique(state)),
      time = as.numeric(date - interventions("national")$date[3]),
      time = time / max(time)
    ) %>%
    arrange(state, date)
  
  # subset to 1.5m question and add data for modelling
  survey_distance <- survey %>%
    filter(question == "1.5m compliance") %>%
    left_join(pred_data)
  
  result <- list(survey_distance = survey_distance,
                 prediction_data = pred_data)
  
  result
  
}
