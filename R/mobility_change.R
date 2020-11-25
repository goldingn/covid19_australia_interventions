source("R/functions.R")

# load, cache, and format the mobility data
mobility <- all_mobility() %>%
  append_google_data()
  
saveRDS(mobility, file = "outputs/cached_mobility.RDS")

n_weeks_ahead <- 6
first_date <- min(mobility$date)
last_date <- max(mobility$date)

mobility_fitted <- mobility %>%
  rename(
    state_long = state,
  ) %>%
  filter(
    !is.na(state_long) &
      !is.na(trend)
  ) %>%
  mutate(
    state = abbreviate_states(state_long)
  ) %>%
  group_by(state, datastream) %>%
  do(
    predict_mobility_trend(
      .,
      min_date = first_date,
      max_date = last_date + 7 * n_weeks_ahead
    )
  ) %>%
  ungroup()

all_states <- na.omit(unique(mobility_fitted$state_long))

for (this_state in all_states) {

  mobility_fitted %>%
    filter(state_long == this_state) %>%
    ggplot() +
    aes(date, fitted_trend) +
    geom_hline(
      yintercept = 0,
      colour = "grey80",
      linetype = 3
    ) +
    geom_vline(
      aes(xintercept = date),
      data = interventions() %>%
        filter(state == abbreviate_states(this_state)),
      colour = "grey80"
    ) +
    geom_vline(
      aes(xintercept = last_date),
      colour = "grey80",
      linetype = 2
    ) +
    facet_wrap(
      ~datastream,
      ncol = 3,
      scales = "free"
    ) +
    geom_ribbon(
      aes(
        ymin = fitted_trend_lower,
        ymax = fitted_trend_upper
      ),
      fill = grey(0.9),
      colour = grey(0.8),
      size = 0.1
    ) +
    # fitted trend
    geom_line(
      aes(date, fitted_trend),
      colour = "gray40"
    ) +
    geom_point(
      aes(date, trend),
      size = 0.2,
      col = "purple"
    ) +
    # predicted trend
    geom_line(
      aes(date, predicted_trend),
      size = 1
    ) +
    coord_cartesian(
      xlim = c(as.Date("2020-03-01"), last_date + n_weeks_ahead * 7)
    ) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b",
      limits = range(mobility_fitted$date)
    ) +
    xlab("") +
    ylab("") +
    ggtitle(
      paste(
        this_state,
        "- data and model fit up to",
        format(last_date, format = "%B %d")
      )
    ) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"))
  
  dpi <- 150
  ggsave(filename = paste0("outputs/figures/", this_state, "_datastream_model_fit.png"),
         width = 1500 / dpi,
         height = 1250 / dpi,
         dpi = dpi,
         scale = 1.2)
}

# save predictions in correct format for macro and mobility models
mobility_fitted %>%
  filter(
    grepl("^Google: ", datastream)
  ) %>%
  mutate(
    change = 1 + (predicted_trend / 100),
    state_datastream = paste(state_long, datastream)
  ) %>%
  select(
    state_datastream,
    state = state_long,
    datastream,
    change,
    date
  ) %>%
  saveRDS("outputs/google_change_trends.RDS")

# output 3-column plot
# run hierarchically on LGA-level data



