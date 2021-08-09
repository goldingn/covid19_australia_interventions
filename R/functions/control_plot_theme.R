control_plot_theme <- function() {
  cowplot::theme_cowplot() +
    # turn off the x axis and add some space for annotation on RHS
    theme(
      plot.margin = unit(
        c(1, 1, 0, 1),
        "line"
      ),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.title.y.right = element_text(
        size = 12,
        margin = margin(l = 10),
      )
    )
}
