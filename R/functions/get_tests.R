get_tests <- function() {
  
  # scrape data on daily numbers of tests by state from covid19data.com.au
  url <- "https://e.infogram.com/_/3osqzRmYBiJsJafg79YC?parent_url=https%3A%2F%2Fwww-covid19data-com-au.filesusr.com%2Fhtml%2F2aed08_944ecbfd558f24812177bca5a8a74000.html&src=embed"
  
  text <- readLines(url) %>%
    paste(collapse = "\n")
  
  start <- str_locate(text, "window.infographicData=")[2] + 1
  text <- substr(text, start, 1000000L)
  end <- str_locate(text, ";</script>")[1]
  text <- substr(text, start = 1, stop = end)
  json <- fromJSON(text)
  
  data_lines <- json$elements[[2]]$data[[1]]
  states <- data_lines[[1]][-1]
  dates <- vapply(data_lines[-1], `[`, 1, FUN.VALUE = character(1))
  dates <- paste0(dates, "/2020")
  dates <- as.Date(dates, format = "%d/%m/%Y")
  
  values_text <- sapply(data_lines[-1], `[`, -1)
  values_text <- gsub(",", "", values_text)
  values <- as.numeric(values_text)
  values <- matrix(values, ncol = length(states), byrow = TRUE)
  colnames(values) <- states
  
  df <- values %>%
    as.data.frame() %>%
    cbind(date = dates) %>%
    pivot_longer(cols = -date,
                 names_to = "state",
                 values_to = "cumulative_tests") %>%
    group_by(state) %>%
    arrange(date) %>%
    mutate(daily_tests = c(NA, diff(cumulative_tests)),
           daily_tests = pmax(0, daily_tests))
  
  df
  
}
