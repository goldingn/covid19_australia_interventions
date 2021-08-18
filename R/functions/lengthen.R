# convert a date-by-region matrix into long format 
lengthen <- function(matrix, dates, region_name, value_name) {
  matrix %>%
    as.data.frame() %>%
    bind_cols(date = dates) %>%
    pivot_longer(-date,
                 names_to = region_name,
                 values_to = value_name)
  
}
