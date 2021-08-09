add_single_box <- function(
  p,
  top,
  bottom,
  text_main = "",
  use_reduction_text = FALSE,
  only_scenarios = c(),
  text_size = 3,
  box_colour = grey(0.9),
  text_colour = grey(0.3),
  border_colour = grey(0.6)
) {

  top <- enquo(top)
  bottom <- enquo(bottom)
  p <- p +
    geom_boxplot(
      aes(
        lower = !!bottom,
        upper = !!top
      ),
      stat = "identity",
      fill = box_colour,
      col = border_colour
    )
  
  if (use_reduction_text) {
  
    # restriction labels
    p <- p + geom_text(
      aes(
        label = ifelse(
          scenario %in% only_scenarios,
          sprintf(
            "%s\n%s\n coverage",
            text_main,
            scenario
          ),
          NA
        ),
        y = !!bottom * (!!top / !!bottom) ^ 0.5  # (midpoint on log scale!)
      ),
      size = text_size,
      col = text_colour
    )
  } else {
    p <- p + geom_text(
      aes(
        label = ifelse(
          scenario %in% only_scenarios,
          text_main,
          NA
        ),
        y = !!bottom * (!!top / !!bottom) ^ 0.5  # (midpoint on log scale!)
      ),
      size = text_size,
      col = text_colour
    )
  }
  
  p
  
}
