source("R/lib.R")

Sys.setenv(RETICULATE_AUTOCONFIGURE = FALSE)
library(readr)
library(dplyr)
library(stringr)
library(rjson)
library(tidyr)
library(greta)
library(greta.gp)
library(readxl)
library(RColorBrewer)
library(tensorflow)
library(purrr)
library(ggplot2)
library(R6)
library(slider)
library(cowplot)
library(lubridate)
library(readxl)
library(rvest)
library(lubridate)

tfp <- reticulate::import("tensorflow_probability")

module <- greta::.internals$utils$misc$module
fl <- greta:::fl
tf_float <- greta:::tf_float

# Google dropped a bunch of previous data from the latest file. Pull a cached
# version from the tidycovid package on GitHub and replace it.
tidycovid_url <- "https://github.com/goldingn/tidycovid19/raw/e4db3ab3007576f34dcb1e8c3299b235cff6198e/cached_data/google_cmr.RDS"

    

as.greta_array <- greta:::as.greta_array

# weighted mean and standard error of the weighted mean, computed with a
# bootstrap
weighted_mean <- weighted.mean

op <- greta::.internals$nodes$constructors$op

# greta distribution object for the grouped negative binomial distribution
right_aggregated_negative_binomial_distribution <- R6Class(
  "right_aggregated_negative_binomial_distribution",
  inherit = greta:::distribution_node,
  public = list(
    
    max_count = NA,
    
    initialize = function(size, prob, max_count, dim) {
      
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)
      
      self$max_count <- max_count
      
      # add the nodes as parents and parameters
      dim <- greta:::check_dims(size, prob, target_dim = dim)
      super$initialize(
        "right_aggregated_negative_binomial",
        dim,
        discrete = TRUE
      )
      self$add_parameter(size, "size")
      self$add_parameter(prob, "prob")
      
    },
    
    tf_distrib = function(parameters, dag) {
      
      size <- parameters$size
      prob <- parameters$prob
      
      
      
      list(log_prob = log_prob, sample = sample)
      
    }
    
  )
)

# greta distribution object for the grouped negative binomial distribution
discrete_lognormal_distribution <- R6Class(
  "discrete_lognormal_distribution",
  inherit = greta:::distribution_node,
  public = list(
    
    breaks = NA,
    lower_bounds = NA,
    upper_bounds = NA,
    
    initialize = function(meanlog, sdlog, breaks, dim) {
      
      meanlog <- as.greta_array(meanlog)
      sdlog <- as.greta_array(sdlog)
      
      # handle gradient issue between sdlog and 0s
      breaks <- pmax(breaks, .Machine$double.eps)
      self$breaks <- breaks
      self$lower_bounds <- breaks[-length(breaks)]
      self$upper_bounds <- breaks[-1]
      
      # add the nodes as parents and parameters
      dim <- greta:::check_dims(meanlog, sdlog, target_dim = dim)
      super$initialize("discrete_lognormal", dim, discrete = TRUE)
      self$add_parameter(meanlog, "meanlog")
      self$add_parameter(sdlog, "sdlog")
      
    },
    
    tf_distrib = function(parameters, dag) {
      
      meanlog <- parameters$meanlog
      sdlog <- parameters$sdlog
      
      tf_breaks <- fl(self$breaks)
      tf_lower_bounds <- fl(self$lower_bounds)
      tf_upper_bounds <- fl(self$upper_bounds)
      
      
      
      list(log_prob = log_prob, sample = sample)
      
    }
    
  )
)

# which household types correspond to the respondent being a parent (guessing at
# BETA's 'parent' field)
parenty_households <- c(
  "Couple with non-dependent child(ren)",
  "Couple with dependent child(ren)",
  "Couple with dependent and non-dependent children",
  "Single parent with non-dependent child(ren)",
  "Single parent with dependent child(ren)",
  "Single parent with dependent and non-dependent children"
)

# colours for plotting
blue <- "steelblue3"
purple <- "#C3A0E8"
green <- brewer.pal(8, "Set2")[1]
yellow <- brewer.pal(8, "Set2")[6]
blue_green <- colorRampPalette(c("blue", green))(10)[8]
yellow_green <- colorRampPalette(c("yellow", green))(10)[8]
orange <- brewer.pal(8, "Set2")[2]
pink <- brewer.pal(8, "Set2")[4]
fifo <- "#A8EB12"

# default cdf
gi_cdf <- nishiura_cdf()

