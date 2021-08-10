untangle_file <- function(file){
  sinew::untangle(
    file = file,
    dir.out = "R/functions/",
    keep.body = TRUE
  )
}

# file to revisit 
# - "R/plot_reff_voc.R"
# - "R/Reff_voc.R"
# - "R/temp_reff_1_vaccine.R"
# - "R/uk_voc_infection_rate.R"
# - "R/vacccination_effect.R"
# - "R/vaccination_reff.R"

# In the file uk_voc_infection_rate.R 
# This has source("spartan/lib.R") in it - need to check this out.

file_to_untangle <- "R/vaccination_reff.R"
untangle_file(file_to_untangle)

fs::file_move(path = "R/body.R",
              new_path = file_to_untangle)

library(codelens)
library(fs)
library(purrr)

r_files <- dir_ls(path = "R/", glob = "*.R$") 

which_code_calls_ggsave <- map_dfr(r_files, file_find_fun, "ggsave")
which_code_calls_ggsave

source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)

# look for which files use `library()`

r_source_files <- list.files(path = "R/",
                             full.names = TRUE,
                             pattern = ".R$")

r_source_files

which_code_calls_library <- map_dfr(r_source_files, file_library)
which_code_calls_library

# looking through these files to see where the changes are made
untangle_file("R/plot_reff_voc.R")

# this one seems to only create new code for `write_reff_sims.R`
untangle_file("R/Reff_voc.R")
  #  - only adds write_reff_2, which is only turned on in some other functions
  # -  logically, that seems fine?

untangle_file("R/temp_reff_1_vaccine.R")
  # - this changes uses of "p" to "p_star", or vice versa
  # - and comments out a few lines of reff_1_vaccine_effect.R

untangle_file("R/uk_voc_infection_rate.R")
  # - this only adds "main = posterior" to hist_prior_posterior.R
  # probably quite harmless?

untangle_file("R/vacccination_effect.R")
  # this changes `average_transmission_efficacy()`, changing definition of
  # the efficacy of the doses.

untangle_file("R/vaccination_reff.R")
  # this also changes `average_transmission_efficacy()`, changing definition of
  # the efficacy of the doses.
