# possibly deprecated?
load_vic <- function (file) {
  get_vic_linelist(file) %>%
    impute_linelist()
} # possibly deprecated?
