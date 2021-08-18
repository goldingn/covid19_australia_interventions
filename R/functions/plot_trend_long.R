# plot_trend_long is outdated at this point as plot_trend has been updated
# was intended as alternative to plot_trend with wider x-axis for longer timeseries
# still may be useful and need updating in that case to include voc
plot_trend_long <- function(simulations,
                       data,
                       base_colour = grey(0.4),
                       multistate = FALSE,
                       hline_at = 1,
                       ylim = c(0, 4),
                       intervention_at = interventions(),
                       projection_at = NA,
                       keep_only_rows = NULL,
                       max_date = data$dates$latest_mobility,
                       min_date = as.Date("2020-03-01")) {
  
  mean <- colMeans(simulations)
  ci_90 <- apply(simulations, 2, quantile, c(0.05, 0.95))
  ci_50 <- apply(simulations, 2, quantile, c(0.25, 0.75))
  
  if (multistate) {
    states <- rep(data$states, each = data$n_dates_project)
    dates <- rep(data$dates$infection_project, data$n_states)
  } else {
    dates <- data$dates$infection_project
    states <- NA
  }
  
  df <- tibble(date = dates,
               state = states,
               mean = mean,
               ci_50_lo = ci_50[1, ],
               ci_50_hi = ci_50[2, ],
               ci_90_lo = ci_90[1, ],
               ci_90_hi = ci_90[2, ])
  
  if (!is.null(keep_only_rows)) {
    df <- df[keep_only_rows, ]
  }
  
  df <- df %>%
    filter(
      date >= min_date,
      date <= max_date
    ) %>%
    mutate(type = "Nowcast")
  
  if (is.null(ylim)) {
    ylim <- c(min(df$ci_90_lo), max(df$ci_90_hi)) 
  }
  
  p <- ggplot(df) + 
    
    aes(date, mean, fill = type) +
    
    xlab(element_blank()) +
    
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_breaks = "1 month", date_labels = "%e/%m") +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    geom_vline(
      aes(xintercept = date),
      data = intervention_at,
      colour = "grey80"
    ) +
    
    geom_ribbon(aes(ymin = ci_90_lo,
                    ymax = ci_90_hi),
                alpha = 0.2) +
    geom_ribbon(aes(ymin = ci_50_lo,
                    ymax = ci_50_hi),
                alpha = 0.5) +
    geom_line(aes(y = ci_90_lo),
              colour = base_colour,
              alpha = 0.8) + 
    geom_line(aes(y = ci_90_hi),
              colour = base_colour,
              alpha = 0.8) + 
    
    geom_hline(yintercept = hline_at, linetype = "dotted") +
    
    theme_cowplot() +
    panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = 8))
  
  if (multistate) {
    p <- p + facet_wrap(
      facets = ~ state,
      ncol = 2,
      scales = "free",
      strip.position = "left"
    ) +
      theme(
        strip.text.y.left = element_text(
          hjust = 0,
          vjust = 1,
          face = "bold",
          angle = 0
        )
      )
  }
  
  if (!is.na(projection_at)) {
    p <- p +
      geom_vline(xintercept = projection_at, linetype = "dashed", colour = "grey60") +
      annotate("rect",
               xmin = projection_at,
               xmax = max(df$date),
               ymin = -Inf,
               ymax = Inf,
               fill = grey(0.5), alpha = 0.1)
  }
  
  p    
  
}
