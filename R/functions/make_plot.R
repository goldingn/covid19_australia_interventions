make_plot <- function(..., base_plot, colours) {
  scenarios <- list(...)
  plot <- base_plot
  for(i in seq_along(scenarios)) {
    plot <- plot %>%
      add_scenario_ribbon(
        scenarios[[i]],
        colour = colours[[i]]
      )
  }
  plot
}
