border_line <- function(border) {
  geom_sf(
    aes(fill = 1),
    data = border,
    col = grey(0.4),
    linetype = "dashed",
    size = 0.4,
    fill = NA
  )
}
