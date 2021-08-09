add_stacked_box <- function(
  p,
  bottom,  # variable for bottom of the box
  top,  # variable for top of the box
  reference,  # variable against which to calculate % reduction of 'bottom'
  text_main = "",
  text_size = 3,
  only_scenarios = c(),
  box_colour = grey(0.9),
  border_colour = grey(0.6),
  text_colour = grey(0.3),
  use_reduction_text = FALSE
) {

  bottom <- enquo(bottom)
  top <- enquo(top)
  reference <- enquo(reference)

  p <- p +
    geom_boxplot(
      aes(
        lower = !!bottom,
        upper = !!top,
      ),
      stat = "identity",
      fill = box_colour,
      col = border_colour
    )
  
  if (use_reduction_text) {
    p <- p + 
      geom_text(
        aes(
          label = ifelse(
            scenario %in% only_scenarios,
            sprintf(
              "%s\n%i%s",
              text_main,
              round(100 * (1 - !!bottom/!!reference)),
              "% lower TP"
            ),
            NA
          ),
          y = !!bottom * (!!top /!!bottom) ^ 0.5  # (midpoint on log scale!)
        ),
        size = text_size,
        col = text_colour
      )
  } else {
    p <- p + 
      geom_text(
        aes(
          label = ifelse(
            scenario %in% only_scenarios,
            text_main,
            NA
          ),
          y = !!bottom * (!!top /!!bottom) ^ 0.5  # (midpoint on log scale!)
        ),
        size = text_size,
        col = text_colour
      )
  }
  
  p

}
