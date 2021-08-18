col_nsw_date <- function(type = c("short", "long")) {
  type <- match.arg(type)
  switch(
    type,
    short = col_date(format = "%Y-%m-%d"),
    long = col_date(format = "%d/%m/%Y %H:%M:%S AM")
  )
}
