# Define rolling delay distributions:

source("R/functions.R")

linelist <- load_linelist()
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

delay_data_to_confirmation <- detection_delay_data %>%
  filter(
    date_confirmation <= (linelist_date - 3)
  )
  
delays_from_onset <- estimate_delays(
  state = delay_data_from_onset$state,
  date = delay_data_from_onset$date_onset,
  delay = delay_data_from_onset$delay,
  direction = "forward"
)

delays_to_confirmation <- estimate_delays(
  state = delay_data_from_onset$state,
  date = delay_data_from_onset$date_confirmation,
  delay = delay_data_from_onset$delay,
  direction = "backward"
)

saveRDS(delays_from_onset, "outputs/delay_from_onset_cdfs.RDS")
saveRDS(delays_to_confirmation, "outputs/delay_to_confirmation_cdfs.RDS")

p <- plot_delays(
  delay_distributions = delays_from_onset,
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
