mean.ecdf <- function(x, ...) {
  mean(evalq(rep.int(x, diff(c(0, round(nobs * y)))), environment(x)), ...)
}
