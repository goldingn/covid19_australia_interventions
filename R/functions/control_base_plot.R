control_base_plot <- function(
  data,
  ylab = "Transmission potential"
) {
  data %>%
    ggplot(
      aes(
        x = scenario,
        middle = as.numeric(NA),
        ymin = as.numeric(NA),
        ymax = as.numeric(NA),
        width = 0.6
      )
    ) +
    scale_x_discrete() +
    xlab("") +
    scale_y_continuous(
      position = "right",
      breaks = c(0.5, 1, 2, 4, 6, 8),
      trans = 'log'
    ) +
    coord_cartesian(clip = "off") +
    ylab(ylab) +
    control_plot_theme()
}
