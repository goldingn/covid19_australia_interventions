# random truncated normal samples
rtnorm <- function(n, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
  truncdist::rtrunc(n = n,
                    spec = "norm",
                    a = lower,
                    b = upper,
                    mean = mean,
                    sd = sd)
}
