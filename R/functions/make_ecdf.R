# convert a vector fo cumulative probabilities into an ecdf object
make_ecdf <- function(y, x) {
  
  sims <- sample(x,
                 100,
                 prob = y,
                 replace = TRUE)
  
  ecdf_null <- ecdf(sims)
  envir <- environment(ecdf_null)
  
  # rebuild an ecdf object, the slow way
  method <- 2L
  yleft <- 0
  yright <- 1
  f <- envir$f
  n <- envir$nobs
  rval <- function (v) {
    stats:::.approxfun(x, y, v, method, yleft, yright, f)
  }
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- attr(ecdf_null, "call")
  rval  
}
