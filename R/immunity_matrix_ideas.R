# # ideas and constructor functions for building up the population immunity
# # transition matrix
# 
# # given the names of the baseline immunity types, get all possible immunity combinations
# make_immunity_combinations <- function(basic_immune_types = c("I", "V"),
#                                        nonimmune_name = "S") {
#   
#   make_list <- function(name) {
#     tbl <- list(x = c("", name))
#     names(tbl) <- name
#     tbl
#   }
#   
#   immunity_types <- basic_immune_types %>%
#     lapply(make_list) %>%
#     unlist(recursive = FALSE) %>%
#     do.call(expand_grid, .) %>%
#     rowwise() %>%
#     mutate(
#       combined = paste0(
#         c_across(everything()),
#         collapse = ""
#       ),
#       combined = case_when(
#         combined == "" ~ nonimmune_name,
#         TRUE ~ combined
#       )
#     ) %>%
#     pull(combined)
#   
#   immunity_types
# }
# 
# # get transition rates into a newly immune state, using *relative* probabilities
# # of transitioning from each state, and the size of each state
# prob_new_immunity <- function(
#   relative_probabilities,
#   current_state,
#   total_new_immunities
# ) {
#   
#   n_states <- length(current_state)
#   stopifnot(length(relative_probabilities) == n_states)
#   
#   # the number of newly immune individuals new_{ij} of a given immunity type j,
#   # coming from each current state i is:
#   #   new_{ij} = new_j * n_i * rel_i / sum_i(n_i * rel_i)
#   # where new_j is the total number of newly immune individuals in the
#   # population, n_i is the number of individuals in current state i,and  rel_i
#   # is the (unnormalised) relative probability of transitioning to newly immune
#   # state j across states j (e.g. relative risk of infection).
#   # Rearranging we have:
#   #   new_{ij} = n_i * (new_j * rel_i / sum_i(n_i * rel_i))
#   # and so the probability prob_{ij} that a random individual in state i moves
#   # into state j is:
#   #   prob_{ij} = new_j * rel_i / sum_i(n_i * rel_i)
#   # because:
#   #   new_{ij} = n_i * prob_{ij}
#   
#   normalisation <- sum(current_state * relative_probabilities)
#   disagg_probabilities <- relative_probabilities / normalisation
#   newly_immune_fraction <- total_new_immunities * disagg_probabilities
#   newly_immune_fraction
#   
#   
# }
# 
# empty_matrix <- function(states) {
#   n_states <- length(states)
#   matrix(0,
#          n_states,
#          n_states,
#          dimnames = list(states, states))  
# }
# 
# # construct a matrix encoding the possible state inheritance structure of the
# # transition matrix, from a tibble of the inter-state transitions that are
# # possible (ignoring self-transitions, which are added automatically)
# manual_inheritance_matrix <- function(possible_transitions,
#                                       self_inherit = FALSE,
#                                       states = NULL) {
#   if (is.null(states)) {
#     states <- unique(c(possible_transitions$from, possible_transitions$to))
#   }
#   
#   possible_transitions_index <- as.matrix(possible_transitions[, c("to", "from")])
#   inheritance <- empty_matrix(states)
#   inheritance[possible_transitions_index] <- 1
#   if(self_inherit) {
#     diag(inheritance) <- 1
#   }
#   inheritance
# }
# 
# # given a matrix with only one 1 in each columns, and the rest 0s, and a vector
# # of the values to go in the '1' cell in the order of the columns, return a
# # matrix with the ones replaces by thier values
# assign_row_values <- function(matrix, values) {
#   stopifnot(all(colSums(matrix) == 1))
#   stopifnot(sort(unique(as.vector(matrix))) == c(0, 1))
#   coords <- which(matrix == 1, arr.ind = TRUE)
#   coords <- coords[order(coords[, 2]), ]
#   matrix[coords] <- values
#   matrix
# }
# 
# # given a vector of immunity types, a vector of immunity types that have waning
# # (must be a subset), and a number of waning states, get a vector corresponding
# # to the immunity types giving the number of states it needs to be expanded to
# get_immunity_type_expansion <- function(
#   immunity_types,
#   immunity_types_with_waning = setdiff(immunity_types, "S"),
#   n_waning_states
# ) {
#   has_waning <- immunity_types %in% immunity_types_with_waning
#   expansion <- ifelse(has_waning, n_waning_states, 1)
# }
# 
# 
# # given the vector of immunity types, and the matching expansion vector (number
# # of states in that immunity type), return a vector of the names of those
# # expanded states
# get_expanded_state_names <- function(
#   immunity_types,
#   expansion
# ) {
#   
#   has_waning <- expansion > 1
#   
#   # create the state names
#   immunity_types_expanded <- rep(immunity_types, expansion)
#   waning_expanded <- unlist(lapply(expansion, seq_len)) - 1
#   
#   # remove 0 for the non-waning immunity types, and coerce to character
#   mask_waning_count <- rep(!has_waning, expansion)
#   waning_expanded <- paste0("_", waning_expanded)
#   waning_expanded[mask_waning_count] <- ""
#   
#   # combine immunity type with waning evel, and return
#   states <- paste0(immunity_types_expanded, waning_expanded)
#   
#   states
#   
# }
# 
# # given a scalar value, a number of rows and a number of columns, return a
# # matrix of the correct dimensions with 1s on the first row and 0s elsewhere
# make_rowwise_submatrix <- function(value, expansion_rows, expansion_cols) {
#   submatrix <- matrix(0, expansion_rows, expansion_cols)
#   submatrix[1, ] <- value
#   submatrix
# }
# 
# # given an inhereitance matrix between immunity types, a number of waning states
# # for those immunity types that wane, and the vector of immunity types that wane
# # (must be a subset of the immunity_types), expand the original inheritance
# # matrix out to get an inheritance matrix with waning
# expand_inheritance_matrix <- function(inheritance_matrix,
#                                       n_waning_states = 4,
#                                       immunity_types_with_waning = setdiff(colnames(inheritance_matrix), "S")) {
#   
#   immunity_types <- colnames(inheritance_matrix)
#   n_immunity_types <- length(immunity_types)
#   
#   # expand out the immunity states to levels of waning
#   expansion <- get_immunity_type_expansion(
#     immunity_types = immunity_types,
#     immunity_types_with_waning = immunity_types_with_waning,
#     n_waning_states = n_waning_states
#   )
#   
#   # create the state names
#   expanded_state_names <- get_expanded_state_names(immunity_types, expansion)
#   
#   # loop through the original inheritance matrix, making submatrices for the
#   # expanded components
#   columns <- list()
#   for (col in seq_len(n_immunity_types)) {
#     rows <- list()
#     for (row in seq_len(n_immunity_types)) {
#       rows[[row]] <- make_rowwise_submatrix(
#         inheritance_matrix[row, col],
#         expansion_rows = expansion[row],
#         expansion_cols = expansion[col]
#       )
#     }
#     columns[[col]] <- rows
#   }
#   
#   # compress this nested list into a matrix
#   columns_bound <- lapply(columns, function(x) {do.call(rbind, x)})
#   matrix <- do.call(cbind, columns_bound)
#   rownames(matrix) <- colnames(matrix) <- expanded_state_names
#   matrix
#   
# }
# 
# # given a vector of immunity types, a number of waning states for those immunity
# # types that wane, and a vector of the immunity types that wane (must be a
# # subset of immunity_types), create an inheritance matrix between these expanded
# # states for individuals staying in the same immunity type (either waning one
# # state, or staying in the same state if in the terminal waning phase, or
# # susceptible)
# same_type_inheritance_matrix <- function(immunity_types,
#                                          n_waning_states = 4,
#                                          immunity_types_with_waning = setdiff(immunity_types, "S")) {
#   
#   # get the number of expansions per state, and the names of the expanded states
#   expansion <- get_immunity_type_expansion(
#     immunity_types = immunity_types,
#     immunity_types_with_waning = immunity_types_with_waning,
#     n_waning_states = n_waning_states
#   )
#   
#   expanded_states <- get_expanded_state_names(
#     immunity_types = immunity_types,
#     expansion = expansion
#   )
#   
#   # determine whether it is an unexpanded or a terminal state; to put 1 on the
#   # diagonal (stay in same state) or otherwise put a 1 on the subdiagonal (wane
#   # to the next state).
#   is_unexpanded <- rep(expansion == 1, expansion)
#   is_terminal <- unlist(lapply(expansion, seq_len)) == n_waning_states
#   same_state <- is_unexpanded | is_terminal
#   
#   # now find diagonal and subdiagonal elements, and mask them
#   n_expanded_states <- length(expanded_states)
#   empty_matrix <- matrix(0, n_expanded_states, n_expanded_states)
#   
#   # put diagonals in for the same state
#   same_state_matrix <- empty_matrix
#   rownames(same_state_matrix) <- colnames(same_state_matrix) <- expanded_states
#   diag(same_state_matrix) <- same_state
#   
#   # put on sub-diagonals for waning by one state
#   next_state_matrix <- row(empty_matrix) - col(empty_matrix) == 1
#   next_state_matrix[] <- ifelse(next_state_matrix[], 1, 0)
#   next_state_matrix <- sweep(next_state_matrix, 2, !same_state, FUN = "*")
#   
#   # combine these and return
#   next_state_matrix + same_state_matrix
#   
# }
# 
# get_state_names <- function(
#   basic_immunity_types = c("I", "V"),
#   n_waning_states = 5
# ) {
#   
#   immunity_types <- make_immunity_combinations(basic_immunity_types)
#   
#   # get number of states within each immunity type
#   expansion <- get_immunity_type_expansion(
#     immunity_types = immunity_types,
#     n_waning_states = n_waning_states
#   )
#   
#   state_names <- get_expanded_state_names(
#     immunity_types,
#     expansion = expansion
#   )
#   
#   state_names
#   
# }
# 
# # manual definition of immunity type inheritance structures, harded coded to the
# # I, V case - these to be repalced with code to construct these from arbitrary
# # basic immunity types
# get_immunity_inheritance <- function(
#   which = c("infection", "vaccination", "vaccination_and_infection"),
#   basic_immunity_types = c("I", "V")
# ) {
# 
#   which <- match.arg(which)
#   
#   transitions <- switch(
#     which,
#     vaccination = tribble(~from, ~to,
#                           "S", "V",
#                           "V", "V",
#                           "I", "IV",
#                           "IV", "IV"),
#     infection = tribble(~from, ~to,
#                         "S", "I",
#                         "I", "I",
#                         "V", "IV",
#                         "IV", "IV"),
#     vaccination_and_infection = tribble(~from, ~to,
#                                         "S", "IV",
#                                         "I", "IV",
#                                         "V", "IV",
#                                         "IV", "IV")
#   )
#   
#   manual_inheritance_matrix(
#     possible_transitions = transitions,
#     states = make_immunity_combinations(basic_immunity_types)
#   )
#   
# }
# 
# create_transition_matrix <- function(
#   # A vector of the current proportion of the population in each state
#   # (combination of immunity type and waning phase). Must sum to 1, and have the
#   # length of the vector returned by get_state_names(basic_immunity_types,
#   # n_waning_states).
#   current_state_prop,
#   # a vector of the relative probability that a given an individual in each
#   # state is infected in this iteration. Must have the same length as
#   # current_state_prop
#   relative_probability_I,
#   # a vector of the relative probability that a given an individual in each
#   # state is vaccinated in this iteration. Must have the same length as
#   # current_state_prop
#   relative_probability_V,
#   # the total number of new vaccinations in this iteration. Must be less than or
#   # equal to n_population
#   n_new_vaccinations,
#   # the total number of new infections in this iteration. Must be less than or
#   # equal to n_population
#   n_new_infections,
#   # the number of people in this population. Must be greater than or
#   # equal to both n_new_vaccinations and n_new_infections
#   n_population = 1e6,
#   # the number of waning phases to consider for the immunity types that wane
#   n_waning_states = 52,
#   # matrices of the inheritance structure between the four immunity types, for
#   # individuals being vaccinated, infected, and both vaccinated and infected
#   immunity_inheritance_vaccination = get_immunity_inheritance("vaccination"),
#   immunity_inheritance_infection = get_immunity_inheritance("infection"),
#   immunity_inheritance_vaccination_and_infection = get_immunity_inheritance("vaccination_and_infection")
# ) {
#   
#   # construct all possible immunity types from these
#   immunity_types <- make_immunity_combinations(c("I", "V"))
#   
#   # get number of states within each immunity type
#   expansion <- get_immunity_type_expansion(
#     immunity_types = immunity_types,
#     n_waning_states = n_waning_states
#   )
#   
#   # the names of all the states
#   states <- get_expanded_state_names(
#     immunity_types,
#     expansion = expansion
#   )
#   
#   # create inheritance matrices at the scale of immunity types for each transition
#   # process, then expand these out to add waning to get inheritance structure for
#   # all states
#   inheritance_vaccination <- expand_inheritance_matrix(
#     immunity_inheritance_vaccination,
#     n_waning_states = n_waning_states
#   )
#   
#   inheritance_infection <- expand_inheritance_matrix(
#     immunity_inheritance_infection,
#     n_waning_states = n_waning_states
#   )
#   
#   inheritance_vaccination_and_infection <- expand_inheritance_matrix(
#     immunity_inheritance_vaccination_and_infection,
#     n_waning_states = n_waning_states
#   )
#   
#   # inheritance for remaining in the same immunity type (either wane or stay in
#   # same state if susceptible or in the terminal state for a given immunity
#   # type)
#   inheritance_remaining <- same_type_inheritance_matrix(
#     immunity_types = immunity_types,
#     n_waning_states = n_waning_states
#   )
#   
#   # get the probability of being infected in this iteration for each individual
#   # of each state
#   prob_I_ind <- prob_new_immunity(
#     relative_probabilities = relative_probability_I,
#     current_state = current_state_prop,
#     total_new_immunities = n_new_infections / n_population
#   )
#   
#   # get the probability of being vaccinated in this iteration for each
#   # individual of each state
#   prob_V_ind <- prob_new_immunity(
#     relative_probabilities = relative_probability_V,
#     current_state = current_state_prop,
#     total_new_immunities = n_new_vaccinations / n_population
#   )
#   
#   # convert these into the probabilities of moving into the I *only*, V *only*,
#   # and the IV immunity types in this iteration, for each state:
#   prob_I <- prob_I_ind * (1 - prob_V_ind)
#   prob_V <- prob_V_ind * (1 - prob_I_ind)
#   prob_IV <- prob_I_ind * prob_V_ind
#   
#   # assign these to their appropriate places in the component transition matrices,
#   # using the inheritance matrices
#   infection_rates <- assign_row_values(inheritance_infection, prob_I)
#   vaccination_rates <- assign_row_values(inheritance_vaccination, prob_V)
#   vaccination_infection_rates <- assign_row_values(inheritance_vaccination_and_infection, prob_IV)
#   
#   # combine these to get all the transitions *between* immunity types (all
#   # transitions except for remaining in the same immunity type)
#   transitions_between_immunity_types <- infection_rates + vaccination_rates + vaccination_infection_rates
#   
#   # get the probability of remaining in that immunity type, and combine to get all transitions
#   prob_remaining <- 1 - colSums(transitions_between_immunity_types)
#   transitions_within_immmunity_type <- assign_row_values(inheritance_remaining, prob_remaining)
#   
#   # create and return the combined transition matrix
#   transitions <- transitions_between_immunity_types + transitions_within_immmunity_type
#   transitions
#   
# }
# 
# # given the inputs to create a transition matrix for each of a number of
# # timepoints, iterate the dynamic transition matrix the required number of
# # times, and return a matrix of the proportion of individuals in each state
# # (rows) at each time point (columns)
# evaluate_timeseries <- function(
#   # A vector of the proportion of the population in each state (combination of
#   # immunity type and waning phase) at the initial timepoint. Must sum to 1, and
#   # have the length of the vector returned by
#   # get_state_names(basic_immunity_types, n_waning_states).
#   initial_state_prop,
#   # a list of vectors of the relative probability that a given an individual in
#   # each state is vaccinated in this iteration. Each vector in the list must
#   # have the same length as current_state_prop, and the list must have the same
#   # length as all other input timeseries
#   relative_probability_V_timeseries,
#   # a vector of the relative probability that a given an individual in each
#   # state is infected in each iteration. Must have the same length as
#   # current_state_prop and doesn't change over time
#   relative_probability_I,
#   # a vector of the total number of new vaccinations in each iteration. Must be
#   # less than or equal to n_population, and must have the same length as all
#   # other input timeseries
#   n_new_vaccinations_timeseries,
#   # a vector of the total number of new infections in each iteration. Must be
#   # less than or equal to n_population, and must have the same length as all
#   # other input timeseries
#   n_new_infections_timeseries,
#   # the number of people in this population. Must be greater than or
#   # equal to both n_new_vaccinations and n_new_infections
#   n_population = 1e6,
#   # the number of waning phases to consider for the immunity types that wane
#   n_waning_states = 52,
#   # matrices of the inheritance structure between the four immunity types, for
#   # individuals being vaccinated, infected, and both vaccinated and infected
#   immunity_inheritance_vaccination = get_immunity_inheritance("vaccination"),
#   immunity_inheritance_infection = get_immunity_inheritance("infection"),
#   immunity_inheritance_vaccination_and_infection = get_immunity_inheritance("vaccination_and_infection")
# ) {
#   
#   n_states <- length(initial_state_prop)
#   n_times <- length(relative_probability_V_timeseries)
#   
#   # check the timeseries lengths all match
#   all_timeseries_match <- length(n_new_vaccinations_timeseries) == n_times &
#     length(n_new_infections_timeseries) == n_times
#   
#   if (!all_timeseries_match) {
#     stop(
#       "all arguments with 'timeseries' in the name must have the same lengths"
#     )
#   }
#   
#   # make output data object
#   output <- matrix(NA, n_states, n_times + 1) 
#   output[, 1] <- initial_state_prop
#   
#   # loop through timesteps, creating the matrix and iterating the states
#   current_state_prop <- initial_state_prop
#   for (time in seq_len(n_times)) {
#     transition_matrix <- create_transition_matrix(
#       current_state_prop = current_state_prop,
#       relative_probability_V = relative_probability_V_timeseries[[time]],
#       relative_probability_I = relative_probability_I,
#       n_new_vaccinations = n_new_vaccinations_timeseries[[time]],
#       n_new_infections = n_new_infections_timeseries[[time]],
#       n_population = n_population,
#       n_waning_states = n_waning_states,
#       immunity_inheritance_vaccination = immunity_inheritance_vaccination,
#       immunity_inheritance_infection = immunity_inheritance_infection,
#       immunity_inheritance_vaccination_and_infection = immunity_inheritance_vaccination_and_infection
#     )
#     
#     current_state_prop <- transition_matrix %*% current_state_prop
#     output[, time + 1] <- current_state_prop
#   }
#   
#   rownames(output) <- rownames(current_state_prop)
#   colnames(output) <- paste0("t", seq_len(n_times + 1) - 1)
#   output
#   
# }
# 
# library(tidyverse)
# 
# basic_immunity_types <- c("I", "V")
# n_waning_states <- 52
# 
# # get all states, to make fake population
# immunity_types <- make_immunity_combinations(basic_immunity_types)
# states <- get_state_names(basic_immunity_types = basic_immunity_types,
#                           n_waning_states = n_waning_states)
# 
# # set up fake population
# n_states <- length(states)
# 
# n_population <- 1e6
# 
# # proportion of the population in each state
# current_state_prop <- c(50, runif(n_states - 1))
# current_state_prop <- current_state_prop / sum(current_state_prop)
# 
# # relative probability of being infected: e.g. RR of infection for each immunity type and level of waning, from VEs
# # should remain the same at each iteration since it's just based on the immunity model
# relative_probability_I <- runif(n_states)
# 
# # relative probability of being vaccinated: e.g. from number of new vaccinations
# # and which groups is vaccinated. For all I-only states, should be the same as
# # susceptibles (the unvaccinated), for IV states should be the same as for V
# # states (with same waning) - should change at each iteration based on the AIR
# # data
# relative_probability_V <- runif(n_states)
# 
# # total number of new infections and total number of new vaccinations
# n_new_vaccinations <- 2e4
# n_new_infections <- 1e4
# 
# transition_matrix <- create_transition_matrix(
#   current_state_prop = current_state_prop,
#   relative_probability_V = relative_probability_V,
#   relative_probability_I = relative_probability_I,
#   n_new_vaccinations = n_new_vaccinations,
#   n_new_infections = n_new_infections,
#   n_population = 1e5,
#   n_waning_states = n_waning_states,
#   # manually-specified matrices of the inheritance structure between the four
#   # immunity types, for individuals being vaccinated, infected, and both
#   # vaccinated and infected
#   immunity_inheritance_vaccination = get_immunity_inheritance("vaccination"),
#   immunity_inheritance_infection = get_immunity_inheritance("infection"),
#   immunity_inheritance_vaccination_and_infection = get_immunity_inheritance("vaccination_and_infection")
# )
# 
# # check columns sum to 1
# all(colSums(transition_matrix) %>% near(1))
# 
# # get next iteration of the state
# new_state_prop <- transition_matrix %*% current_state_prop
# 
# 
# # simulate a timeseries of these
# 
# n_times <- 50
# relative_probability_V_timeseries <- replicate(n_times, runif(n_states), simplify = FALSE)
# n_new_vaccinations_timeseries <- round(runif(n_times, 1e0, 1e2))
# n_new_infections_timeseries <- round(runif(n_times, 1e3, 1e5))
# 
# # iterate over multiple infection and vaccination timeseries
# timeseries <- evaluate_timeseries(
#   initial_state_prop = current_state_prop,
#   relative_probability_V_timeseries = relative_probability_V_timeseries,
#   relative_probability_I = relative_probability_I,
#   n_new_vaccinations_timeseries = n_new_vaccinations_timeseries,
#   n_new_infections_timeseries = n_new_infections_timeseries,
#   n_population = n_population,
#   n_waning_states = n_waning_states
# )
# 
# 
# 
# 
# timeseries %>%
#   as.data.frame() %>%
#   rownames_to_column("state") %>%
#   pivot_longer(
#     cols = !contains("state"),
#     names_to = "time",
#     values_to = "proportion",
#     names_prefix = "t"
#   ) %>%
#   mutate(
#     time = as.numeric(time),
#     immunity_type = str_remove(state, "(_\\d+)"),
#     immunity_type = case_when(
#       immunity_type == "S" ~ "non-immune",
#       immunity_type == "I" ~ "infection only",
#       immunity_type == "V" ~ "vaccination only",
#       immunity_type == "IV" ~ "infection and vaccination"
#     ),
#     immunity_type = factor(immunity_type, levels = c("infection and vaccination",
#                                                      "infection only",
#                                                      "vaccination only",
#                                                      "non-immune"))
#   ) %>%
#   group_by(immunity_type, time) %>%
#   summarise(
#     across(
#       proportion,
#       sum
#     ),
#     .groups = "drop"
#   ) %>%
#   ggplot(
#     aes(
#       x = time,
#       y = proportion,
#       group = immunity_type,
#       fill = immunity_type
#     ),
#   ) +
#   geom_area() +
#   theme_minimal()
# 
# 
# 
# 
# 
# 
# # experiment: building a dense matrix and converting it to sparse before matrix
# # multiplying does not speed up the multiply operation; if we want to get a
# # speed up from sparse matrix operations, we will have to avoid constructing the
# # dense matrix in the first place. This should be doable, since each nonzero
# # component is created from a row vector, and the templates of where to put
# # those values can be computed once, in advance.
# 
# # drop dimnames as they confuse the checking
# transition_matrix_nude <- transition_matrix
# dimnames(transition_matrix_nude) <- NULL
# 
# library(spam)
# library(Matrix)
# 
# # build a sparseMatrix from the dense, given the known nonzero elements (will remain the same throughout)
# sparseMatrix_method <- function(dense_matrix, vector) {
#   indices <- which(dense_matrix != 0, arr.ind = TRUE)
#   values <- dense_matrix[indices]
#   sparse_matrix <- sparseMatrix(i = indices[, 1], j = indices[, 2], x = values)
#   output <- as.matrix(sparse_matrix %*% vector)
#   dimnames(output) <- NULL
#   output
# }
#   
# bench::mark(
#   transition_matrix_nude %*% current_state_prop,
#   spam::as.spam(transition_matrix_nude) %*% current_state_prop,
#   sparseMatrix_method(transition_matrix_nude, current_state_prop)
# )
# 
# dim(transition_matrix_nude)
# 
# 
# # convert to a sparse matrix and iterate
# 
# 
# 
# # to do:
# 
# # Gerry
# # programmatically create inheritance matrices for multiple different types of
# # vaccination, infection, and combinations thereof
# 
# # August
# # calculate the relative probabilities of vaccination from AIR


library(tidyverse)

# latest attempt: construct inheritance and states for vaccine and infection
# separately, and then combine into hybrid and add waning. We need to do these
# separately and combine because for inheritance only one vaccine can happen per
# timestep, and only one infection can occur per timestep, but an individual can
# have both one infection and one vaccine in each timestep, and we need to
# compute the probability of both happening when constructing the transition
# matrix

# given max number of doses, construct all possible numbers of doses (0-max)
possible_doses <- function(max_doses) {
  seq_len(max_doses + 1) - 1
}

# given a name for a dose schedule summary, a (minimum) number of doses
# corresponding to it, and whether it is the highest level of number of doses,
# construct a formula to reclassify the vaccine_doses column in a dataframe
dose_schedule_to_formula_text <- function(name, doses, terminal) {
  sprintf("vaccine_doses %s %i ~ '%s'",
          ifelse(terminal, ">=", "=="),
          doses,
          name)
}

dominance_to_formula_text <- function(name) {
  sprintf("%s > 0 ~ '%s'",
          name,
          name)
}

additional_event <- function(diff) {
  expected <- c(rep(0, length(diff) - 1), 1)
  all(sort(diff) == expected)
}

# given a vector of dominance of immunity types, in decreasing order of
# dominance, return a text string for a case_when operation to recode immunity
# types as their dominant type
dominance_case_when_text <- function(domininance_order, default_class) {
  
  dominant_type_formula_text <- lapply(
    domininance_order,
    dominance_to_formula_text
  ) %>%
    c(list(
      sprintf(".default = '%s'", default_class)
    ))
  
  dominant_type_case_when_text <- paste0(
    "case_when(",
    paste(dominant_type_formula_text,
          collapse = ",\n"),
    ")"
  )
  
  dominant_type_case_when_text
  
}

base_inheritance <- function(states_base) {
  states_base_matrix <- as.matrix(states_base)
  inheritance <- outer(states_base_matrix, states_base_matrix, FUN = "-") %>%
    apply(c(1, 3), diag) %>%
    apply(c(2:3), additional_event) %>%
    t()
}

simplify_inheritance <- function(states_simplified, base_inheritance, state_column) {
  # summarise inheritance to simplified states
  expand_grid(
    from = states_simplified[[state_column]],
    to = states_simplified[[state_column]]
  ) %>%
    mutate(
      inheritance = c(t(base_inheritance))
    ) %>%
    group_by(from, to) %>%
    summarise(inheritance = any(inheritance),
              .groups = "drop") %>%
    arrange(
      desc(from),
      desc(to)
    )
}

# given a specification for the possible vaccine types and maximum numbers of
# doses of each (a named list in decreasing order of dominance), and how we
# would classify these as dose schedules (a named list in decreasing order of
# dominance/ numbers of doses), this will return the simplified set of vaccine
# immunity types including an unvaccinated class (which does not need to be
# included in the vaccine specification), and the inheritance structures between
# these
vaccine_immunity <- function(vaccines_and_max_doses, dose_schedules) {
  
  # construct all combinations of vaccine types and doses
  vaccines_and_doses <- lapply(vaccines_and_max_doses, possible_doses)
  states_base <- do.call(expand_grid, vaccines_and_doses)
  
  # encode dose schedules, adding on unvaccinated state, and making the final
  # level capture all higher numbers
  dose_schedules_full <- c(
    dose_schedules,
    list(unvaccinated = 0)
  )
  
  dose_schedule_formula_text <- mapply(
    dose_schedule_to_formula_text,
    name = names(dose_schedules_full),
    dose = dose_schedules_full,
    terminal = seq_along(dose_schedules_full) == 1,
    SIMPLIFY = FALSE
  )
  
  dose_case_when_text <- paste0(
    "case_when(",
    paste(dose_schedule_formula_text,
          collapse = ",\n"),
    ")"
  )

  # prepare case_when text to define dominance structure
  dominant_vaccine_case_when_text <- dominance_case_when_text(
    names(vaccines_and_max_doses),
    default_class = "unvaccinated"
  )
  
  # simplify down to summarised types
  states_simplified <- states_base %>%
    # count doses and summarise by dose_schedules and sominaint vaccine type
    rowwise() %>%
    mutate(
      vaccine_doses = sum(across(everything())),
      dose_schedule = eval(parse(text = dose_case_when_text)),
      dominant_vaccine = eval(parse(text = dominant_vaccine_case_when_text)),
      vaccine_state = paste(
        dominant_vaccine,
        dose_schedule,
        sep = "_"
      ),
      vaccine_state = case_when(
        vaccine_state == "unvaccinated_unvaccinated" ~ "unvaccinated",
        .default = vaccine_state
      )
    )
  
  # construct inheritance from base types
  inheritance <- base_inheritance(states_base)
  
  # summarise inheritance to simplified states
  inheritance_simplified <- simplify_inheritance(
    states_simplified = states_simplified,
    base_inheritance = inheritance,
    state_column = "vaccine_state")

  # return all structures
  list(
    states = unique(states_simplified$vaccine_state),
    inheritance = inheritance_simplified,
    base_states = states_simplified
  )
  
}

# given a specification for the possible infection types and maximum numbers of
# infections of each  (a named list in decreasing order of dominance, >1
# infections currently ignored), this will return the simplified set of dominant
# infection immunity types including an uninfected class (which does not need to
# be included in the infection specification), and the inheritance structures
# between these
infection_immunity <- function(infections) {
  
  # construct all combinations of vaccine types and doses
  infections_and_counts <- lapply(infections, possible_doses)
  states_base <- do.call(expand_grid, infections_and_counts)
  
  # prepare case_when text to define dominance structure
  dominant_infection_case_when_text <- dominance_case_when_text(
    names(infections),
    default_class = "uninfected"
  )
  
  # simplify down to summarised types
  states_simplified <- states_base %>%
    mutate(
      dominant_infection = eval(parse(text = dominant_infection_case_when_text)),
      infection_state = dominant_infection
    )
  
  # construct inheritance from base types, and apply to simplified types
  inheritance <- base_inheritance(states_base)
  inheritance_simplified <- simplify_inheritance(
    states_simplified = states_simplified,
    base_inheritance = inheritance,
    state_column = "infection_state")
  
  # return all structures
  list(
    states = unique(states_simplified$infection_state),
    inheritance = inheritance_simplified,
    base_states = states_simplified
  )
  
}


vaccine_immunity_structure <- vaccine_immunity(
  # note this must be *decreasing order of dominance*
  vaccines_and_max_doses = list(
    mrna_bival = 2,
    mrna_mono = 5,
    subunit_mono = 2
  ),
  # note this must be in *decreasing order of dominance*
  dose_schedules = list(
    booster = 3,
    full = 2,
    partial = 1
  )
)


infection_immunity_structure <- infection_immunity(
  infections = list(
    omicron = 1,
    nonomicron = 1
  )
)

# visualise these inheritances to manually check the logic
# (node location is stochastic, so rerun if you can't see the labels)
library(DiagrammeR)
vaccine_immunity_structure$inheritance %>%
  pivot_wider(names_from = to,
              values_from = inheritance) %>%
  column_to_rownames("from") %>%
  as.matrix() %>%
  DiagrammeR::from_adj_matrix(mode = "directed") %>%
  DiagrammeR:::visnetwork()

infection_immunity_structure$inheritance %>%
  pivot_wider(names_from = to,
              values_from = inheritance) %>%
  column_to_rownames("from") %>%
  as.matrix() %>%
  DiagrammeR::from_adj_matrix(mode = "directed") %>%
  DiagrammeR:::visnetwork()

# to population the vaccine transition *rates* for each timestep, we need to
# compute the number of individuals in each of the 'from' states in
# vaccine_transition_rates_to_fill at the start of that timestep, and the number
# of those individuals that move into the 'to' states during that timestep
vaccine_transition_rates_to_fill <- vaccine_immunity_structure$inheritance %>%
  filter(inheritance) %>%
  select(-inheritance)

vaccine_transition_rates_to_fill



# next: combine to get hybrid immunity structure
# - all combinations of simplified states are possible, so inheritance should be
# something simple like a kronecker product tiling.

# then: put it all together in the inheritance matrix constructor

# How do we capture the fact that vaccination and infection can happen in the same
# week, so that the probability can be calculated in construction of the
# transition matrix?

