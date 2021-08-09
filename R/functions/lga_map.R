# plot an LGA-level risk map, with overlaid lockdown border
lga_map <- function (object, fill,
                     source_geometry = NA,
                     source_lockdown = FALSE,
                     source_fill = NA,
                     risk_col = grey(0.4),
                     trans = "identity") {
  p <- object %>%
    select(!!fill) %>%
    ggplot() +
    aes(fill = !!as.name(fill)) +
    geom_sf(
      size = 0.1
    ) +
    scale_fill_gradient(
      low = grey(0.9),
      high = risk_col,
      trans = trans
    ) +
    theme_minimal()
  
  if (!is.na(source_geometry)) {
    
    p <- p +
      geom_sf(
        aes(fill = 1),
        data = source_geometry,
        col = "white",
        size = ifelse(source_lockdown, 1.5, 0.5),
        fill = source_fill
      )
    
    if (source_lockdown) {
      p <- p +
        geom_sf(
          aes(fill = 1),
          data = source_geometry,
          col = "black",
          size = 0.2,
          fill = NA
        )
    }

  }
  
  p
  
}
