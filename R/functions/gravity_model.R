# gravity model of mobility between suburbs - geometry must be an sf object with
# polygons
gravity_model <- function(geometry, population, coef = c(-10, -3, 1, 1)) {
  # distance matrix in km
  n_suburbs <- nrow(geometry)
  centroids <- st_centroid(geometry)
  distance <- st_distance(centroids, centroids)
  distance_km <- drop_units(distance) / 1e3
  
  # population of suburbs in matrix form
  log_pop <- log(population)
  log_pop_matrix <- matrix(rep(log_pop, n_suburbs),
                           n_suburbs,
                           n_suburbs)
  
  # mobility (ignoring probability of staying)
  log_mobility <- coef[1] +
    coef[2] * log(distance_km) +
    coef[3] * log_pop_matrix +
    coef[4] * t(log_pop_matrix)
  
  mobility <- exp(log_mobility)
  mobility
  
}
