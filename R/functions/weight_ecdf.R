# calculate a weighted average ecdf out of two (weight is the probability of the first)
weight_ecdf <- function(ecdf_1, ecdf_2, weight) {
  
  if (is.null(ecdf_1) | weight == 0) {
    return(ecdf_2)
  }
  if (is.null(ecdf_2) | weight == 1) {
    return(ecdf_1)
  }
  
  e1 <- environment(ecdf_1)
  e2 <- environment(ecdf_2)
  
  # reconcile the xs
  x_1 <- e1$x
  x_2 <- e2$x
  
  x <- sort(unique(c(x_1, x_2)))
  
  # get the two CDFs
  y_1 <- ecdf_1(x)
  y_2 <- ecdf_2(x)
  
  # get the two pdfs
  pdf_1 <- diff(c(0, y_1))
  pdf_2 <- diff(c(0, y_2))
  
  # get a weighted average of them
  pdf <- pdf_1 * weight + pdf_2 * (1 - weight)
  
  # convert back to a CDF
  y <- cumsum(pdf)
  
  # rebuild an ecdf object, the slow way
  method <- 2L
  yleft <- 0
  yright <- 1
  f <- e1$f
  n <- e1$nobs
  rval <- function (v) {
    stats:::.approxfun(x, y, v, method, yleft, yright, f)
  }
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- attr(ecdf_1, "call")
  rval
  
}
