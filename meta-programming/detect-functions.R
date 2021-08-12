library(codelens)
library(dplyr)
library(purrr)
debugonce(extract_functions)
extract_functions("R/check_linelist.R")
function_names("R/check_linelist.R")
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

potential_funs

potential_funs_df <- tibble(
  funs = potential_funs
)

potential_funs_df

word_in_file <- function(files, word){
  file_lines <- readr::read_lines(file = files)
  stringr::str_detect(string = file_lines,
                      pattern = word)
}

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
