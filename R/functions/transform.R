  transform <- function(free) {
    list(meanlogit = free[1],
         sdlogit = exp(free[2]))
  }
