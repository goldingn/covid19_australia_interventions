simple_border <- function(object, tol= 0.001) {
  object %>%
    st_geometry() %>%
    st_union() %>%
    st_simplify(dTolerance = tol)
}
