# strip the parenthetical nonsense from LGA names
lga_name <- function(lga) {
  strsplit(lga, " \\(") %>%
    lapply(`[`, 1) %>%
    unlist()
}
