write_linelist <- function(dir = "outputs"){
  
  linelist <- load_linelist()
  
  ll_date <- linelist$date_linelist[1]
  
  write.csv(
    x = linelist,
    file = sprintf(
      "%s/interim_linelist_%s.csv",
      dir,
      ll_date
    ),
    row.names = FALSE
  )
  
}
