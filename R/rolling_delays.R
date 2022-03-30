# Define rolling delay distributions:

source("R/functions.R")

#linelist <- load_linelist()
linelist_date <- linelist$date_linelist[1]

# get delays for locally-acquired infections, truncated differently for forward/backward delays 
detection_delay_data <- linelist %>%
  mutate(
    delay = as.numeric(date_confirmation - date_onset)
  ) %>%
  filter(
    import_status == "local",
    !is.na(date_onset),
    delay <= 42,
    delay >= -5
  )

delay_data_from_onset <- detection_delay_data %>%
  filter(
    date_onset <= (linelist_date - 15)
  )

delays_from_onset <- estimate_delays(
  state = delay_data_from_onset$state,
  date = delay_data_from_onset$date_onset,
  delay = delay_data_from_onset$delay,
  direction = "forward"
)

#override NT flat period
delays_from_onset[delays_from_onset$date >= "2022-01-03" & delays_from_onset$state == "NT","ecdf"] <- delays_from_onset[delays_from_onset$date == "2022-01-03" & delays_from_onset$state == "NT","ecdf"]

delay_data_to_confirmation <- detection_delay_data %>%
  filter(
    date_confirmation <= (linelist_date - 3)
  )

delays_to_confirmation <- estimate_delays(
  state = delay_data_to_confirmation$state,
  date = delay_data_to_confirmation$date_confirmation,
  delay = delay_data_to_confirmation$delay,
  direction = "backward"
)

saveRDS(delays_from_onset, "outputs/delay_from_onset_cdfs_legacy.RDS")
saveRDS(delays_to_confirmation, "outputs/delay_to_confirmation_cdfs_legacy.RDS")

p <- delays_from_onset %>%
  filter(
    date <= max(delay_data_from_onset$date_onset) 
  ) %>%
  plot_delays(
    date = delay_data_from_onset$date_onset,
    state = delay_data_from_onset$state,
    delay = delay_data_from_onset$delay,
    ylim = c(-2, 20)
  ) +
  ggtitle(
    label = "Surveillance trend",
    subtitle = "Time from symptom onset to notification for locally-acquired cases"
  ) +
  xlab("Symptom onset date") +
  ylab("Days to case notification")

p

save_ggplot("surveillance_effect.png", multi = TRUE)

# plot delays aggregated by notifiation date; used for imputation
p2 <- plot_delays(
  delay_distributions = delays_to_confirmation,
  date = delay_data_to_confirmation$date_confirmation,
  state = delay_data_to_confirmation$state,
  delay = delay_data_to_confirmation$delay,
  base_colour = grey(0.4)
) +
  ggtitle(
    label = "Notification delay",
    subtitle = "Time from symptom onset to notification for locally-acquired cases"
  ) +
  xlab("Notification date") +
  ylab("Days since symptom onset")

p2

save_ggplot("notification_delays.png", multi = TRUE)

#replace delay for Omicron period with those from NSW
delays_to_confirmation_NCIMS <- delays_to_confirmation

delays_to_confirmation_NCIMS[delays_to_confirmation_NCIMS$date >= "2021-12-01" &
                               delays_to_confirmation_NCIMS$state != "NSW",3:5] <- 
  delays_to_confirmation_NCIMS[rep(which(delays_to_confirmation_NCIMS$date >= "2021-12-01" &
                                 delays_to_confirmation_NCIMS$state == "NSW"),each = 7),3:5]

saveRDS(delays_to_confirmation_NCIMS, "outputs/delay_to_confirmation_cdfs.RDS")

p2 <- plot_delays(
  delay_distributions = delays_to_confirmation_NCIMS,
  date = delay_data_to_confirmation$date_confirmation,
  state = delay_data_to_confirmation$state,
  delay = delay_data_to_confirmation$delay,
  base_colour = grey(0.4)
) +
  ggtitle(
    label = "Notification delay with modification",
    subtitle = "Time from symptom onset to notification for locally-acquired cases"
  ) +
  xlab("Notification date") +
  ylab("Days since symptom onset")

p2

save_ggplot("notification_delays_modified.png", multi = TRUE)

delays_from_onset_NCIMS <- delays_from_onset

delays_from_onset_NCIMS[delays_from_onset_NCIMS$date >= "2021-12-01" &
                          delays_from_onset_NCIMS$state != "NSW",3:5] <- 
  delays_from_onset_NCIMS[rep(which(delays_from_onset_NCIMS$date >= "2021-12-01" &
                                      delays_from_onset_NCIMS$state == "NSW"),each = 7),3:5]

saveRDS(delays_from_onset_NCIMS, "outputs/delays_from_onset_cdfs.RDS")


p <- delays_from_onset_NCIMS %>%
  filter(
    date <= max(delay_data_from_onset$date_onset) 
  ) %>%
  plot_delays(
    date = delay_data_from_onset$date_onset,
    state = delay_data_from_onset$state,
    delay = delay_data_from_onset$delay,
    ylim = c(-2, 20)
  ) +
  ggtitle(
    label = "Surveillance trend with modification",
    subtitle = "Time from symptom onset to notification for locally-acquired cases"
  ) +
  xlab("Symptom onset date") +
  ylab("Days to case notification")

p

save_ggplot("surveillance_effect_modified.png", multi = TRUE)



#onset missingness plot

week_grid <- linelist %>%
  filter(import_status == "local", date_confirmation >= (max(date_confirmation) - months(6))) %$%
  expand_grid(
    state = unique(.$state),
    week = .$date_confirmation %>%
      cut.Date(breaks = "1 week", labels = NULL) %>% 
      #arrange %>%
      unique
  )

missingness <- linelist %>%
  filter(import_status == "local", date_confirmation >= (max(date_confirmation) - months(6))) %>%
  mutate(
    week = cut.Date(date_confirmation, breaks = "1 week", labels = NULL)
  ) %>%
  group_by(
    state,
    week
  ) %>%
  summarise(
    n_cases = n(),
    prop_onset = sum(!is.na(date_onset))/n(),
    .groups = "drop"
  ) %>%
  full_join(
    week_grid
  ) %>%
  mutate(
    n_cases = ifelse(is.na(n_cases), 0, n_cases)
  ) %>%
  arrange(week, state)


missingness %>%
  ggplot() +
  geom_point(
    aes(
      x = week,
      y = prop_onset,
      size = log10(n_cases)
    ),
    col = "springgreen2"
  ) +
  facet_wrap(
    ~ state,
    ncol = 2
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Proportion",
    size = expression(Log["10"]~"(cases)")
  ) +
  ggtitle(
    label = "Proportion symptom onset dates",
    subtitle = "Proportion of local cases with symptom onset dates recorded, by week"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90),
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(size = 9, angle = 270)
  ) +
  scale_y_continuous(
    position = "right",
    limits = c(-0.2, 1.2),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
  ) 

save_ggplot("proportion_with_onset_date.png", multi = TRUE)
