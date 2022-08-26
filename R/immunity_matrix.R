library(dplyr)
library(conmat)
library(tidyr)


# 
# weeks <- 0:10
# 
# 
states <- c("non-immune", "wk_since_exposure")

margin <- c(NA, 0:10)

immunity_matrix <- matrix(
  nrow = length(margin),
  ncol = length(margin),
  dimnames = list(margin, margin),
  data = 0
)


#pr_infection <- 0.1

margin_length <- length(margin)

pr_infection <- rep(0.1, margin_length)

pr_not_infection <- 1 - pr_infection

sub_diag <- (row(immunity_matrix) - col(immunity_matrix)) == 1
sub_diag[1, 1] <- TRUE
sub_diag[2, 1] <- FALSE
sub_diag[margin_length, margin_length] <- TRUE
sub_diag_index <- which(sub_diag)

immunity_matrix[sub_diag_index] <- pr_not_infection[]
immunity_matrix[2, ] <- pr_infection

immunity_matrix



