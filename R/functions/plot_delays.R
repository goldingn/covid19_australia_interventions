# plot changing delay distributions by state over time
plot_delays <- function(
  delay_distributions,
  date,
  state,
  delay,
  ylim = c(0, 20),
  hline_at = 0,
  intervention_at = interventions(), 
  base_colour = yellow
) {
  

  # mutate to output quantiles and then plot them
  quantiles <- delay_distributions %>%
    # plot it as the date of infection, not date of onset!
    mutate(date = date - 5) %>%
    pmap_dfr(get_cis) %>%
    bind_cols(delay_distributions, .) %>%
    mutate(
      median = vapply(
        ecdf,
        quantile,
        0.5,
        FUN.VALUE = numeric(1)
      ),
      mean = vapply(
        ecdf,
        mean,
        FUN.VALUE = numeric(1)
      )
    )
  
  # observed delays
  df_obs <- tibble(
    date = date,
    state = state,
    delay = delay,
    type = "Nowcast"
  )
  
  p <- quantiles %>%
    mutate(type = "Nowcast") %>%
    ggplot() + 
    
    aes(date, mean, fill = type) +
    
    facet_wrap(~state, ncol = 2) +
    
    xlab(element_blank()) +
    
    coord_cartesian(
      ylim = ylim,
      xlim = c(as.Date("2020-03-01"), max(delay_distributions$date))
    ) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_breaks = "1 month", date_labels = "%e/%m") +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    ci_ribbon("90") +
    ci_ribbon("80") +
    ci_ribbon("70") +
    ci_ribbon("60") +
    ci_ribbon("50") +
    ci_ribbon("40") +
    ci_ribbon("30") +
    ci_ribbon("20") +
    ci_ribbon("10") +
    
    geom_line(aes(y = ci_90_lo),
              colour = base_colour,
              alpha = 0.8) + 
    geom_line(aes(y = ci_90_hi),
              colour = base_colour,
              alpha = 0.8) + 
    
    geom_line(aes(y = mean),
              colour = grey(0.4),
              alpha = 1,
              size = 1) +
    
    # add shading for regions where the national distribution is used
    geom_ribbon(
      aes(ymin = -10, ymax = use_national * 100 - 10),
      fill = grey(1),
      alpha = 0.5,
      colour = grey(0.9),
      linetype = 3
    ) +
    
    # add horizontal and vertical lines for context
    geom_vline(
      aes(xintercept = date),
      data = intervention_at,
      colour = "grey80"
    ) +
    
    geom_hline(
      yintercept = hline_at,
      colour = "grey80"
    ) +
    
    # add points for true delays
    geom_point(
      aes(date, delay),
      data = df_obs,
      pch = 16,
      size = 0.2,
      alpha = 0.1
    ) +
    
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = 8)
          )
  
  p
  
}
