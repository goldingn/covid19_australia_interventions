# make a washed-out colour, by a vector of amounts between 0 and 1
washout <- function(colour, amount = 0.7) {

  stopifnot(
    all(amount >= 0) & all(amount <= 1)
  )

  n <- 1000
  indices <- pmax(1, ceiling(amount * n))

  palette <- colorRampPalette(c(colour, "white"))
  palette(n)[indices]

}
