library(codelens)
library(dplyr)
library(purrr)
function_used("R/check_linelist.R") 
function_names("R/functions/abbreviate_states.R")

fun_dir_ls <- fs::dir_ls(
  "R/functions",
  glob = "*.R$"
  )

safe_function_name <- safely(function_names)
safe_list_of_fun_names <- purrr::map(fun_dir_ls, safe_function_name)
t_safe_list_of_fun_names <- transpose(safe_list_of_fun_names)
potential_funs <- flatten_chr(t_safe_list_of_fun_names$result)

library(tidyr)

potential_funs_df <- expand_grid(potential_funs, fun_dir_ls)

read_line_collapse <- function(file){
  paste0(readr::read_lines(file = file), collapse = "")
}

word_in_file <- function(files, word){
  file_lines <- read_line_collapse(files)
  stringr::str_detect(string = file_lines,
                      pattern = word)
}

expanded_potential_funs_df <- potential_funs_df %>% 
  nest(nest = potential_funs) %>% 
  mutate(data = map_chr(fun_dir_ls, read_line_collapse))
         # data_chr = map_chr(data, paste0, collapse = ""))

expanded_potential_funs_df %>% 
  unnest(cols = nest) %>% 
  mutate(fun_detect = stringr::str_count(data, potential_funs))


word_in_file(files = "R/check_linelist.R",
             word = potential_funs_df$funs[1])


words_in_file <- function(words, file){
  map2(.x = file,
       .y = words,
       .f = word_in_file) %>% 
    set_names(words)
}

words_in_file(file = "R/check_linelist.R",
              words = potential_funs_df$funs)


potential_funs_df



fs_linelist <- readr::read_lines("R/check_linelist.R")

str_

function_names <- basename(fun_dir_ls) %>% 
  tools::file_path_sans_ext()



options(error = recover)
purrr::map_dfr(.x = fun_dir_ls, 
               .f = function_names)

library(CodeDepends)

fs_dir
test_fun <- parse("R/check_linelist.R")
# formula involves non-df variables
inputs <- lapply(test_fun, getInputs)

inputs

purrr::map(inputs, slot, "functions")
