# given a dataframe of mobility data subsetted to a particular mobility metric,
# fit a generalised additive model for the trend and return a dataframe with the
# modelled mobility trend for all dates between min_date and max_date
predict_mobility_trend <- function(
  mobility,
  min_date = min(mobility$date),
  max_date = max(mobility$date)
) {
  
  print(mobility$state[[1]])
  print(mobility$datastream[[1]])
  
  all_dates <- seq(min_date, max_date, by = 1)
  
  min_data_date = min(mobility$date)
  max_data_date = max(mobility$date)
  
  public_holidays <- holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    ) %>%
    rename(
      holiday = name
    )
  
  school_holidays <- school_holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    )
  
  # create intervention step-change covariates
  intervention_steps <- interventions(end_dates = TRUE) %>%
    # add events for SA and QLD ending short lockdowns, to enable effects to be
    # reversed
    # bind_rows(
    #   tibble(
    #     date = as.Date("2020-11-22"),
    #     state = "SA"
    #   ),
    #   tibble(
    #     date = as.Date("2021-01-12"),
    #     state = "QLD"
    #   ),
    #   tibble(
    #     date = as.Date("2021-02-05"),
    #     state = "WA"
    #   ),
    #   tibble(
    #     date = as.Date("2021-02-18"),
    #     state = "VIC"
    #   )
    # ) %>% # this code now superceded by end_dates = TRUE
    filter(date <= max_data_date) %>%
    mutate(
      intervention_id = paste0(
        "intervention_",
        match(date, unique(date))
      )
    ) %>%
    group_by(intervention_id, state) %>%
    do(
      tibble(
        date = all_dates,
        intervention_effect = as.numeric(all_dates >= .$date)
      )
    ) %>%
    group_by(state, date) %>%
    summarise(
      intervention_stage = sum(intervention_effect),
      .groups = "drop"
    ) %>%
    mutate(
      intervention_stage = factor(intervention_stage)
    )
  
  df <- mobility %>%
    left_join(
      public_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      school_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      intervention_steps,
      by = c("state", "date")
    ) %>%
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      is_a_school_holiday = !is.na(school_holiday),
      holiday = factor(holiday),
      date_num = as.numeric(date - min_date),
      dow = wday(date, label = TRUE),
      dow = as.character(dow)
    ) %>%
    filter(!is.na(trend))

  library(mgcv)
  
  m <- gam(trend ~
             
             # smooth variations in mobility
             s(date_num, k = 50) +
             
             # step changes around intervention impositions
             intervention_stage +
             
             # random effect on holidays (different for each holiday, but shrunk
             # to an average holiday effect which used to predict into future)
             is_a_holiday +
             s(holiday, bs = "re") +
             
             # constant effect for school holidays
             is_a_school_holiday +
             
             # day of the week effect
             dow,
           
           select = TRUE,
           gamma = 2,
           data = df)
  
  # compute mean and standard deviation of Gaussian observation model for fitted data
  fit <- predict(m, se.fit = TRUE)
  fit$sd <- sqrt(var(residuals(m)) + fit$se.fit ^ 2)

  df_fitted <- df %>%
    # predict with fitted model (and get 90% CIs)
    mutate(
      fitted_trend = fit$fit,
      fitted_trend_upper = fit$fit + fit$sd * qnorm(0.025),
      fitted_trend_lower = fit$fit + fit$sd * qnorm(0.975),
    )
  
  # predict each date, *averaging over the weekday effect for each date*
  pred_df <- expand_grid(
    state_long = unique(df$state_long),
    dow = unique(df$dow),
    date = all_dates,
  ) %>%
    mutate(
      state = abbreviate_states(state_long)
    ) %>% 
    left_join(
      public_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      school_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      intervention_steps,
      by = c("state", "date")
    ) %>%
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      # remove any named holidays not in the training data
      holiday = case_when(
        holiday %in% unique(df$holiday) ~ holiday,
        TRUE ~ "none"
      ),
      holiday = factor(holiday),
      is_a_school_holiday = !is.na(school_holiday),
      date_num = as.numeric(date - min_date),
      # clamp the smooth part of the prediction at both ends
      date_num = pmax(date_num, min_data_date - min_date),
      date_num = pmin(date_num, max_data_date - min_date)
    )
  
  # predict trends under these conditions, and average over day of the week
  pred_df <- pred_df %>%
    mutate(
      predicted_trend = predict(m, newdata = pred_df)
    ) %>%
    group_by(
      state_long, state, date
    ) %>%
    summarise(
      predicted_trend = mean(predicted_trend),
      .groups = "drop"
    ) %>%
    group_by(
      state
    ) %>%
    # smooth fitted curve over days of the week and holidays
    mutate(
      predicted_trend = slide_dbl(
        predicted_trend,
        gaussian_smooth,
        na.rm = TRUE,
        sd = 2.8,
        .before = 5,
        .after = 5
      )
    ) %>%
    ungroup() %>%
    left_join(
      df_fitted %>%
        select(
          state, state_long, date,
          trend,
          fitted_trend,
          fitted_trend_lower,
          fitted_trend_upper
        ),
      by = c("state", "state_long", "date")
    )

  pred_df
  
}
