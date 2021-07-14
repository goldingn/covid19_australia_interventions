source("R/lib.R")

source("R/functions.R")

# sync aggregated data for Dennis
format_raw_survey_data()

data <- microdistancing_data()

saveRDS(data, file = "outputs/cached_micro.RDS")
# data <- readRDS(file = "outputs/cached_micro.RDS")


survey_distance <- data$survey_distance
pred_data <- data$prediction_data #%>%
  #mutate(date_num = as.numeric(date_num))

# pred_plot %>%
#   filter(
#     date > min(survey_distance$date),
#     date < max(survey_distance$date)
#   ) %>%
#   filter(
#     mean == min(mean)
#   ) %>%
#   pull(mean)

# use the lowest modelled compliance proportion (WA on Jan 30th)
# to get the value to add to the pre-survey period to impute the trend
min_observed <- 0.16
pred_data <- pred_data %>%
  mutate(
    distancing2 = (1 - distancing) * min_observed
  )
  
range(pred_data$distancing2)


# mess with distancing effect in pred_data
# set to lowest observed distancing metric (relative to highest) in any state


min_date <- min(survey_distance$date)
max_date <- max(survey_distance$date)

all_dates <- seq(min_date, max_date, by = 1)
  
min_data_date <- min(survey_distance$date)
max_data_date <- max(survey_distance$date)



intervention_steps <- interventions(
  end_dates = TRUE#,
  # exclude_after = "2021-07-01"
) %>%
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


min_intervention_stage <- intervention_steps %>%
  filter(date == min_data_date) %>%
  dplyr::rename(min_intervention_stage = intervention_stage) %>%
  dplyr::select(-date)

max_intervention_stage <- intervention_steps %>%
  filter(date == max_data_date) %>%
  dplyr::rename(max_intervention_stage = intervention_stage) %>%
  dplyr::select(-date)  



df_fit <- survey_distance %>%
  left_join(
    intervention_steps,
    by = c("state", "date")
  )%>%
  dplyr::select(
    state,
    date,
    count,
    respondents,
    intervention_stage,
    distancing
  ) %>%
  nest(
    fit_dat = c(
      date,
      count,
      respondents,
      intervention_stage,
      distancing,
      distancing2
    )
  )



df_pred <- pred_data %>%
  left_join(
    intervention_steps,
    by = c("state", "date")
  ) %>%
  left_join(
    min_intervention_stage,
    by = "state"
  ) %>%
  left_join(
    max_intervention_stage,
    by = "state"
  ) %>%
  mutate(
    intervention_stage = case_when(
      is.na(intervention_stage) & date < min_data_date ~ min_intervention_stage,
      is.na(intervention_stage) & date > max_data_date ~ max_intervention_stage,
      state == "VIC" & intervention_stage == 4 ~  factor(5, levels = levels(intervention_stage)),
      TRUE ~ intervention_stage
    )
  ) %>%
  dplyr::select(
    state,
    date,
    intervention_stage,
    distancing,
    distancing2
  ) %>%
  nest(
    pred_dat = c(
      date,
      intervention_stage,
      distancing,
      distancing2
    )
  )



df_mic <- full_join(
  df_fit,
  df_pred,
  by = "state"
) 




survey_fit <- mapply(
  FUN = fit_survey_gam,
  fit_dat = df_mic$fit_dat,
  pred_dat = df_mic$pred_dat,
  SIMPLIFY = FALSE
)


pred_plot <- df_mic %>%
  mutate(fit = survey_fit) %>% 
  unnest(fit) %>%
  dplyr::select(-fit_dat, -pred_dat)


line_df <- pred_plot %>%
  mutate_at(
    vars(mean, ci_90_lo, ci_90_hi, ci_50_lo, ci_50_hi),
    ~ . * 100
  ) %>%
  filter(date >= as.Date("2020-03-01")) %>%
  mutate(type = "Nowcast")



point_df <- survey_distance %>%
  group_by(state, wave_date) %>%
  summarise(
    count =  sum(count),
    respondents = sum(respondents)
  ) %>%
  ungroup() %>%
  mutate(
    proportion = count / respondents,
    percentage = proportion * 100
  ) %>%
  rename(date = wave_date) %>%
  mutate(type = "Nowcast")

# Compute confidence intervals for the proportions for plotting. Need to fudge
# the sample size for one survey round with 100% adherence on a small sample
pred <- point_df %>%
  mutate(
    id = factor(row_number()),
    respondents = ifelse(respondents == count,
                         respondents + 1,
                         respondents)
  ) %>%
  glm(cbind(count, respondents - count) ~ id,
      data = .,
      family = stats::binomial) %>%
  predict(se.fit = TRUE)

# Monte Carlo integration based on normal approximation to logit-probability
logit_sims <- replicate(
  10000,
  rnorm(length(pred$fit),
        pred$fit,
        pred$se.fit)
)

p_sims <- plogis(logit_sims)
estimate <- rowMeans(p_sims)
cis <- t(apply(
  X = p_sims,
  MARGIN = 1,
  FUN = quantile,
  c(0.025, 0.975)
))

point_df <- point_df %>%
  mutate(
    percentage = estimate * 100,
    lower = cis[, 1] * 100,
    upper = cis[, 2] * 100
  )

# save these fits for plotting later
module(line_df, point_df) %>%
  saveRDS("outputs/micro_plotting_data.RDS")



base_colour <- purple

p <- ggplot(line_df) +
  
  aes(date, mean, fill = type) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "1 month", date_labels = "%e/%m") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_vline(
    aes(xintercept = date),
    data = interventions(),
    colour = "grey80"
  ) +
  
  geom_ribbon(aes(ymin = ci_90_lo,
                  ymax = ci_90_hi),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = ci_50_lo,
                  ymax = ci_50_hi),
              alpha = 0.5) +
  geom_line(aes(y = ci_90_lo),
            colour = base_colour,
            alpha = 0.8) + 
  geom_line(aes(y = ci_90_hi),
            colour = base_colour,
            alpha = 0.8) + 
  
  facet_wrap(~state, ncol = 2, scales = "free") +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 7)) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage),
    data = point_df,
    size = 2,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper),
    data = point_df,
    size = 1,
    alpha = 0.2,
    width = 0
  ) +
  
  # and titles  
  ggtitle(
    label = "Micro-distancing trend",
    subtitle = "Calibrated against self-reported adherence to physical distancing"
  ) +
  ylab("Estimate of percentage 'always' keeping 1.5m distance")

p


save_ggplot("microdistancing_effect.png")

# save the model fit
saveRDS(pred_plot, file = "outputs/microdistancing_trends.RDS")

# estimates at peak and at latest date
pred_summary <- pred_plot %>%
  group_by(state) %>%
  summarise(peak = which.max(mean),
            peak_estimate = mean[peak] * 100,
            peak_low = ci_90_lo[peak] * 100,
            peak_high = ci_90_hi[peak] * 100,
            peak_date = date[peak],
            latest = which.max(date),
            latest_estimate = mean[latest] * 100,
            latest_low = ci_90_lo[latest] * 100,
            latest_high = ci_90_hi[latest] * 100,
            latest_date = date[latest]) %>%
  select(-peak, -latest)

saveRDS(pred_summary,
        file = "outputs/microdistancing_trend_summary.RDS")


