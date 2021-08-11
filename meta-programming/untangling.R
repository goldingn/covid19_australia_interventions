library(sinew)
library(fs)
library(purrr)
# https://github.com/njtierney/codelens
library(codelens)

untangle_file <- function(file){
  untangle(
    file = file,
    dir.out = "R/functions/",
    keep.body = TRUE
  )
}

r_files <- dir_ls(path = "R/", glob = "*.R$") 

which_code_calls_ggsave <- map_dfr(r_files, file_find_fun, "ggsave")
which_code_calls_ggsave

r_Files

which_code_calls_library <- map_dfr(r_source_files, file_library)
which_code_calls_library

