add_scenario_ribbon <- function(base_plot, data, colour = "black") {
  base_plot +
  geom_ribbon(
    aes(
      ymin = bottom,
      ymax = top
    ),
    data = data,
    fill = colour,
    alpha = 0.1
  ) +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper
      ),
      data = data,
      fill = colour,
      alpha = 0.2
    ) +
    geom_line(
      aes(
        date,
        median
      ),
      data = data,
      color = colour
    ) +
    coord_cartesian(
      xlim = range(data$date)
    )
}
