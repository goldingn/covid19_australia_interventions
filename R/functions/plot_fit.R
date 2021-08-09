plot_fit <- function(observed_cases, cases_sim, data) {
  
  valid <- which(data$valid_mat, arr.ind = TRUE)
  dates <- data$dates$infection
  states <- data$data$states
  
  # compute quantiles and plot timeseries for each state
  quants <- apply(
    cases_sim,
    2,
    quantile,
    c(0.025, 0.25, 0.5, 0.75, 0.975)
  )
  
  # PPC check for each state
  par(mfrow = c(4, 2),
      mar = c(2, 3, 2, 0.5))
  for(i in 1:8) {
    idx <- valid[, 2] == i
    y <- observed_cases[idx]
    x <- dates[valid[idx, 1]]
    plot(y ~ x,
         type = "n",
         ylim = range(c(quants[, idx], y)),
         xlim = range(dates[valid[, 1]]),
         ylab = "",
         las = 1,
         xlab = "")
    mtext(states[i], side = 3, adj = 0, cex = 0.8)
    polygon(c(x, rev(x)),
            c(quants[1, idx], rev(quants[5, idx])),
            col = grey(0.9),
            lty = 0)
    polygon(c(x, rev(x)),
            c(quants[2, idx], rev(quants[4, idx])),
            col = grey(0.8),
            lty = 0)
    points(y ~ x, pch = 16, cex = 0.5)
    
  }
  
}
