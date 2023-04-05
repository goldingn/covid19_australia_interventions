# ideas and constructor functions for building up the population immunity
# transition matrix

# given the names of the baseline immunity types, get all possible immunity combinations
make_immunity_combinations <- function(basic_immune_types = c("I", "V"),
                                       nonimmune_name = "S") {
  
  make_list <- function(name) {
    tbl <- list(x = c("", name))
    names(tbl) <- name
    tbl
  }
  
  immunity_types <- basic_immune_types %>%
    lapply(make_list) %>%
    unlist(recursive = FALSE) %>%
    do.call(expand_grid, .) %>%
    rowwise() %>%
    mutate(
      combined = paste0(
        c_across(everything()),
        collapse = ""
      ),
      combined = case_when(
        combined == "" ~ nonimmune_name,
        TRUE ~ combined
      )
    ) %>%
    pull(combined)
  
  immunity_types
}

# get transition rates into a newly immune state, using *relative* probabilities
# of transitioning from each state, and the size of each state
prob_new_immunity <- function(
  relative_probabilities,
  current_state,
  total_new_immunities
) {
  
  n_states <- length(current_state)
  stopifnot(length(relative_probabilities) == n_states)
  
  # the number of newly immune individuals new_{ij} of a given immunity type j,
  # coming from each current state i is:
  #   new_{ij} = new_j * n_i * rel_i / sum_i(n_i * rel_i)
  # where new_j is the total number of newly immune individuals in the
  # population, n_i is the number of individuals in current state i,and  rel_i
  # is the (unnormalised) relative probability of transitioning to newly immune
  # state j across states j (e.g. relative risk of infection).
  # Rearranging we have:
  #   new_{ij} = n_i * (new_j * rel_i / sum_i(n_i * rel_i))
  # and so the probability prob_{ij} that a random individual in state i moves
  # into state j is:
  #   prob_{ij} = new_j * rel_i / sum_i(n_i * rel_i)
  # because:
  #   new_{ij} = n_i * prob_{ij}
  
  normalisation <- sum(current_state * relative_probabilities)
  disagg_probabilities <- relative_probabilities / normalisation
  newly_immune_fraction <- total_new_immunities * disagg_probabilities
  newly_immune_fraction
  
  
}

empty_matrix <- function(states) {
  n_states <- length(states)
  matrix(0,
         n_states,
         n_states,
         dimnames = list(states, states))  
}

# construct a matrix encoding the possible state inheritance structure of the
# transition matrix, from a tibble of the inter-state transitions that are
# possible (ignoring self-transitions, which are added automatically)
manual_inheritance_matrix <- function(possible_transitions,
                                      self_inherit = FALSE,
                                      states = NULL) {
  if (is.null(states)) {
    states <- unique(c(possible_transitions$from, possible_transitions$to))
  }
  
  possible_transitions_index <- as.matrix(possible_transitions[, c("to", "from")])
  inheritance <- empty_matrix(states)
  inheritance[possible_transitions_index] <- 1
  if(self_inherit) {
    diag(inheritance) <- 1
  }
  inheritance
}

# given a matrix with only one 1 in each columns, and the rest 0s, and a vector
# of the values to go in the '1' cell in the order of the columns, return a
# matrix with the ones replaces by thier values
assign_row_values <- function(matrix, values) {
  stopifnot(all(colSums(matrix) == 1))
  stopifnot(sort(unique(as.vector(matrix))) == c(0, 1))
  coords <- which(matrix == 1, arr.ind = TRUE)
  coords <- coords[order(coords[, 2]), ]
  matrix[coords] <- values
  matrix
}

# given a vector of immunity types, a vector of immunity types that have waning
# (must be a subset), and a number of waning states, get a vector corresponding
# to the immunity types giving the number of states it needs to be expanded to
get_immunity_type_expansion <- function(
  immunity_types,
  immunity_types_with_waning = setdiff(immunity_types, "S"),
  n_waning_states
) {
  has_waning <- immunity_types %in% immunity_types_with_waning
  expansion <- ifelse(has_waning, n_waning_states, 1)
}


# given the vector of immunity types, and the matching expansion vector (number
# of states in that immunity type), return a vector of the names of those
# expanded states
get_expanded_state_names <- function(
  immunity_types,
  expansion
) {
  
  has_waning <- expansion > 1
  
  # create the state names
  immunity_types_expanded <- rep(immunity_types, expansion)
  waning_expanded <- unlist(lapply(expansion, seq_len)) - 1
  
  # remove 0 for the non-waning immunity types, and coerce to character
  mask_waning_count <- rep(!has_waning, expansion)
  waning_expanded <- paste0("_", waning_expanded)
  waning_expanded[mask_waning_count] <- ""
  
  # combine immunity type with waning evel, and return
  states <- paste0(immunity_types_expanded, waning_expanded)
  
  states
  
}

# given a scalar value, a number of rows and a number of columns, return a
# matrix of the correct dimensions with 1s on the first row and 0s elsewhere
make_rowwise_submatrix <- function(value, expansion_rows, expansion_cols) {
  submatrix <- matrix(0, expansion_rows, expansion_cols)
  submatrix[1, ] <- value
  submatrix
}

# given an inhereitance matrix between immunity types, a number of waning states
# for those immunity types that wane, and the vector of immunity types that wane
# (must be a subset of the immunity_types), expand the original inheritance
# matrix out to get an inheritance matrix with waning
expand_inheritance_matrix <- function(inheritance_matrix,
                                      n_waning_states = 4,
                                      immunity_types_with_waning = setdiff(colnames(inheritance_matrix), "S")) {
  
  immunity_types <- colnames(inheritance_matrix)
  n_immunity_types <- length(immunity_types)
  
  # expand out the immunity states to levels of waning
  expansion <- get_immunity_type_expansion(
    immunity_types = immunity_types,
    immunity_types_with_waning = immunity_types_with_waning,
    n_waning_states = n_waning_states
  )
  
  # create the state names
  expanded_state_names <- get_expanded_state_names(immunity_types, expansion)
  
  # loop through the original inheritance matrix, making submatrices for the
  # expanded components
  columns <- list()
  for (col in seq_len(n_immunity_types)) {
    rows <- list()
    for (row in seq_len(n_immunity_types)) {
      rows[[row]] <- make_rowwise_submatrix(
        inheritance_matrix[row, col],
        expansion_rows = expansion[row],
        expansion_cols = expansion[col]
      )
    }
    columns[[col]] <- rows
  }
  
  # compress this nested list into a matrix
  columns_bound <- lapply(columns, function(x) {do.call(rbind, x)})
  matrix <- do.call(cbind, columns_bound)
  rownames(matrix) <- colnames(matrix) <- expanded_state_names
  matrix
  
}

# given a vector of immunity types, a number of waning states for those immunity
# types that wane, and a vector of the immunity types that wane (must be a
# subset of immunity_types), create an inheritance matrix between these expanded
# states for individuals staying in the same immunity type (either waning one
# state, or staying in the same state if in the terminal waning phase, or
# susceptible)
same_type_inheritance_matrix <- function(immunity_types,
                                         n_waning_states = 4,
                                         immunity_types_with_waning = setdiff(immunity_types, "S")) {
  
  # get the number of expansions per state, and the names of the expanded states
  expansion <- get_immunity_type_expansion(
    immunity_types = immunity_types,
    immunity_types_with_waning = immunity_types_with_waning,
    n_waning_states = n_waning_states
  )
  
  expanded_states <- get_expanded_state_names(
    immunity_types = immunity_types,
    expansion = expansion
  )
  
  # determine whether it is an unexpanded or a terminal state; to put 1 on the
  # diagonal (stay in same state) or otherwise put a 1 on the subdiagonal (wane
  # to the next state).
  is_unexpanded <- rep(expansion == 1, expansion)
  is_terminal <- unlist(lapply(expansion, seq_len)) == n_waning_states
  same_state <- is_unexpanded | is_terminal
  
  # now find diagonal and subdiagonal elements, and mask them
  n_expanded_states <- length(expanded_states)
  empty_matrix <- matrix(0, n_expanded_states, n_expanded_states)
  
  # put diagonals in for the same state
  same_state_matrix <- empty_matrix
  rownames(same_state_matrix) <- colnames(same_state_matrix) <- expanded_states
  diag(same_state_matrix) <- same_state
  
  # put on sub-diagonals for waning by one state
  next_state_matrix <- row(empty_matrix) - col(empty_matrix) == 1
  next_state_matrix[] <- ifelse(next_state_matrix[], 1, 0)
  next_state_matrix <- sweep(next_state_matrix, 2, !same_state, FUN = "*")
  
  # combine these and return
  next_state_matrix + same_state_matrix
  
}

get_state_names <- function(
  basic_immunity_types = c("I", "V"),
  n_waning_states = 5
) {
  
  immunity_types <- make_immunity_combinations(basic_immunity_types)
  
  # get number of states within each immunity type
  expansion <- get_immunity_type_expansion(
    immunity_types = immunity_types,
    n_waning_states = n_waning_states
  )
  
  state_names <- get_expanded_state_names(
    immunity_types,
    expansion = expansion
  )
  
  state_names
  
}

# manual definition of immunity type inheritance structures, harded coded to the
# I, V case - these to be repalced with code to construct these from arbitrary
# basic immunity types
get_immunity_inheritance <- function(
  which = c("infection", "vaccination", "vaccination_and_infection"),
  basic_immunity_types = c("I", "V")
) {

  which <- match.arg(which)
  
  transitions <- switch(
    which,
    vaccination = tribble(~from, ~to,
                          "S", "V",
                          "V", "V",
                          "I", "IV",
                          "IV", "IV"),
    infection = tribble(~from, ~to,
                        "S", "I",
                        "I", "I",
                        "V", "IV",
                        "IV", "IV"),
    vaccination_and_infection = tribble(~from, ~to,
                                        "S", "IV",
                                        "I", "IV",
                                        "V", "IV",
                                        "IV", "IV")
  )
  
  manual_inheritance_matrix(
    possible_transitions = transitions,
    states = make_immunity_combinations(basic_immunity_types)
  )
  
}

create_transition_matrix <- function(
  # A vector of the current proportion of the population in each state
  # (combination of immunity type and waning phase). Must sum to 1, and have the
  # length of the vector returned by get_state_names(basic_immunity_types,
  # n_waning_states).
  current_state_prop,
  # a vector of the relative probability that a given an individual in each
  # state is infected in this iteration. Must have the same length as
  # current_state_prop
  relative_probability_I,
  # a vector of the relative probability that a given an individual in each
  # state is vaccinated in this iteration. Must have the same length as
  # current_state_prop
  relative_probability_V,
  # the total number of new vaccinations in this iteration. Must be less than or
  # equal to n_population
  n_new_vaccinations,
  # the total number of new infections in this iteration. Must be less than or
  # equal to n_population
  n_new_infections,
  # the number of people in this population. Must be greater than or
  # equal to both n_new_vaccinations and n_new_infections
  n_population = 1e6,
  # the number of waning phases to consider for the immunity types that wane
  n_waning_states = 52,
  # matrices of the inheritance structure between the four immunity types, for
  # individuals being vaccinated, infected, and both vaccinated and infected
  immunity_inheritance_vaccination = get_immunity_inheritance("vaccination"),
  immunity_inheritance_infection = get_immunity_inheritance("infection"),
  immunity_inheritance_vaccination_and_infection = get_immunity_inheritance("vaccination_and_infection")
) {
  
  # construct all possible immunity types from these
  immunity_types <- make_immunity_combinations(c("I", "V"))
  
  # get number of states within each immunity type
  expansion <- get_immunity_type_expansion(
    immunity_types = immunity_types,
    n_waning_states = n_waning_states
  )
  
  # the names of all the states
  states <- get_expanded_state_names(
    immunity_types,
    expansion = expansion
  )
  
  # create inheritance matrices at the scale of immunity types for each transition
  # process, then expand these out to add waning to get inheritance structure for
  # all states
  inheritance_vaccination <- expand_inheritance_matrix(
    immunity_inheritance_vaccination,
    n_waning_states = n_waning_states
  )
  
  inheritance_infection <- expand_inheritance_matrix(
    immunity_inheritance_infection,
    n_waning_states = n_waning_states
  )
  
  inheritance_vaccination_and_infection <- expand_inheritance_matrix(
    immunity_inheritance_vaccination_and_infection,
    n_waning_states = n_waning_states
  )
  
  # inheritance for remaining in the same immunity type (either wane or stay in
  # same state if susceptible or in the terminal state for a given immunity
  # type)
  inheritance_remaining <- same_type_inheritance_matrix(
    immunity_types = immunity_types,
    n_waning_states = n_waning_states
  )
  
  # get the probability of being infected in this iteration for each individual
  # of each state
  prob_I_ind <- prob_new_immunity(
    relative_probabilities = relative_probability_I,
    current_state = current_state_prop,
    total_new_immunities = n_new_infections / n_population
  )
  
  # get the probability of being vaccinated in this iteration for each
  # individual of each state
  prob_V_ind <- prob_new_immunity(
    relative_probabilities = relative_probability_V,
    current_state = current_state_prop,
    total_new_immunities = n_new_vaccinations / n_population
  )
  
  # convert these into the probabilities of moving into the I *only*, V *only*,
  # and the IV immunity types in this iteration, for each state:
  prob_I <- prob_I_ind * (1 - prob_V_ind)
  prob_V <- prob_V_ind * (1 - prob_I_ind)
  prob_IV <- prob_I_ind * prob_V_ind
  
  # assign these to their appropriate places in the component transition matrices,
  # using the inheritance matrices
  infection_rates <- assign_row_values(inheritance_infection, prob_I)
  vaccination_rates <- assign_row_values(inheritance_vaccination, prob_V)
  vaccination_infection_rates <- assign_row_values(inheritance_vaccination_and_infection, prob_IV)
  
  # combine these to get all the transitions *between* immunity types (all
  # transitions except for remaining in the same immunity type)
  transitions_between_immunity_types <- infection_rates + vaccination_rates + vaccination_infection_rates
  
  # get the probability of remaining in that immunity type, and combine to get all transitions
  prob_remaining <- 1 - colSums(transitions_between_immunity_types)
  transitions_within_immmunity_type <- assign_row_values(inheritance_remaining, prob_remaining)
  
  # create and return the combined transition matrix
  transitions <- transitions_between_immunity_types + transitions_within_immmunity_type
  transitions
  
}

# given the inputs to create a transition matrix for each of a number of
# timepoints, iterate the dynamic transition matrix the required number of
# times, and return a matrix of the proportion of individuals in each state
# (rows) at each time point (columns)
evaluate_timeseries <- function(
  # A vector of the proportion of the population in each state (combination of
  # immunity type and waning phase) at the initial timepoint. Must sum to 1, and
  # have the length of the vector returned by
  # get_state_names(basic_immunity_types, n_waning_states).
  initial_state_prop,
  # a list of vectors of the relative probability that a given an individual in
  # each state is vaccinated in this iteration. Each vector in the list must
  # have the same length as current_state_prop, and the list must have the same
  # length as all other input timeseries
  relative_probability_V_timeseries,
  # a vector of the relative probability that a given an individual in each
  # state is infected in each iteration. Must have the same length as
  # current_state_prop and doesn't change over time
  relative_probability_I,
  # a vector of the total number of new vaccinations in each iteration. Must be
  # less than or equal to n_population, and must have the same length as all
  # other input timeseries
  n_new_vaccinations_timeseries,
  # a vector of the total number of new infections in each iteration. Must be
  # less than or equal to n_population, and must have the same length as all
  # other input timeseries
  n_new_infections_timeseries,
  # the number of people in this population. Must be greater than or
  # equal to both n_new_vaccinations and n_new_infections
  n_population = 1e6,
  # the number of waning phases to consider for the immunity types that wane
  n_waning_states = 52,
  # matrices of the inheritance structure between the four immunity types, for
  # individuals being vaccinated, infected, and both vaccinated and infected
  immunity_inheritance_vaccination = get_immunity_inheritance("vaccination"),
  immunity_inheritance_infection = get_immunity_inheritance("infection"),
  immunity_inheritance_vaccination_and_infection = get_immunity_inheritance("vaccination_and_infection")
) {
  
  n_states <- length(initial_state_prop)
  n_times <- length(relative_probability_V_timeseries)
  
  # check the timeseries lengths all match
  all_timeseries_match <- length(n_new_vaccinations_timeseries) == n_times &
    length(n_new_infections_timeseries) == n_times
  
  if (!all_timeseries_match) {
    stop(
      "all arguments with 'timeseries' in the name must have the same lengths"
    )
  }
  
  # make output data object
  output <- matrix(NA, n_states, n_times + 1) 
  output[, 1] <- initial_state_prop
  
  # loop through timesteps, creating the matrix and iterating the states
  current_state_prop <- initial_state_prop
  for (time in seq_len(n_times)) {
    transition_matrix <- create_transition_matrix(
      current_state_prop = current_state_prop,
      relative_probability_V = relative_probability_V_timeseries[[time]],
      relative_probability_I = relative_probability_I,
      n_new_vaccinations = n_new_vaccinations_timeseries[[time]],
      n_new_infections = n_new_infections_timeseries[[time]],
      n_population = n_population,
      n_waning_states = n_waning_states,
      immunity_inheritance_vaccination = immunity_inheritance_vaccination,
      immunity_inheritance_infection = immunity_inheritance_infection,
      immunity_inheritance_vaccination_and_infection = immunity_inheritance_vaccination_and_infection
    )
    
    current_state_prop <- transition_matrix %*% current_state_prop
    output[, time + 1] <- current_state_prop
  }
  
  rownames(output) <- rownames(current_state_prop)
  colnames(output) <- paste0("t", seq_len(n_times + 1) - 1)
  output
  
}

library(tidyverse)

basic_immunity_types <- c("I", "V")
n_waning_states <- 52

# get all states, to make fake population
immunity_types <- make_immunity_combinations(basic_immunity_types)
states <- get_state_names(basic_immunity_types = basic_immunity_types,
                          n_waning_states = n_waning_states)

# set up fake population
n_states <- length(states)

n_population <- 1e6

# proportion of the population in each state
current_state_prop <- c(50, runif(n_states - 1))
current_state_prop <- current_state_prop / sum(current_state_prop)

# relative probability of being infected: e.g. RR of infection for each immunity type and level of waning, from VEs
# should remain the same at each iteration since it's just based on the immunity model
relative_probability_I <- runif(n_states)

# relative probability of being vaccinated: e.g. from number of new vaccinations
# and which groups is vaccinated. For all I-only states, should be the same as
# susceptibles (the unvaccinated), for IV states should be the same as for V
# states (with same waning) - should change at each iteration based on the AIR
# data
relative_probability_V <- runif(n_states)

# total number of new infections and total number of new vaccinations
n_new_vaccinations <- 2e4
n_new_infections <- 1e4

transition_matrix <- create_transition_matrix(
  current_state_prop = current_state_prop,
  relative_probability_V = relative_probability_V,
  relative_probability_I = relative_probability_I,
  n_new_vaccinations = n_new_vaccinations,
  n_new_infections = n_new_infections,
  n_population = 1e5,
  n_waning_states = n_waning_states,
  # manually-specified matrices of the inheritance structure between the four
  # immunity types, for individuals being vaccinated, infected, and both
  # vaccinated and infected
  immunity_inheritance_vaccination = get_immunity_inheritance("vaccination"),
  immunity_inheritance_infection = get_immunity_inheritance("infection"),
  immunity_inheritance_vaccination_and_infection = get_immunity_inheritance("vaccination_and_infection")
)

# check columns sum to 1
all(colSums(transition_matrix) %>% near(1))

# get next iteration of the state
new_state_prop <- transition_matrix %*% current_state_prop


# simulate a timeseries of these

n_times <- 50
relative_probability_V_timeseries <- replicate(n_times, runif(n_states), simplify = FALSE)
n_new_vaccinations_timeseries <- round(runif(n_times, 1e0, 1e2))
n_new_infections_timeseries <- round(runif(n_times, 1e3, 1e5))

# iterate over multiple infection and vaccination timeseries
timeseries <- evaluate_timeseries(
  initial_state_prop = current_state_prop,
  relative_probability_V_timeseries = relative_probability_V_timeseries,
  relative_probability_I = relative_probability_I,
  n_new_vaccinations_timeseries = n_new_vaccinations_timeseries,
  n_new_infections_timeseries = n_new_infections_timeseries,
  n_population = n_population,
  n_waning_states = n_waning_states
)




timeseries %>%
  as.data.frame() %>%
  rownames_to_column("state") %>%
  pivot_longer(
    cols = !contains("state"),
    names_to = "time",
    values_to = "proportion",
    names_prefix = "t"
  ) %>%
  mutate(
    time = as.numeric(time),
    immunity_type = str_remove(state, "(_\\d+)"),
    immunity_type = case_when(
      immunity_type == "S" ~ "non-immune",
      immunity_type == "I" ~ "infection only",
      immunity_type == "V" ~ "vaccination only",
      immunity_type == "IV" ~ "infection and vaccination"
    ),
    immunity_type = factor(immunity_type, levels = c("infection and vaccination",
                                                     "infection only",
                                                     "vaccination only",
                                                     "non-immune"))
  ) %>%
  group_by(immunity_type, time) %>%
  summarise(
    across(
      proportion,
      sum
    ),
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = time,
      y = proportion,
      group = immunity_type,
      fill = immunity_type
    ),
  ) +
  geom_area() +
  theme_minimal()






# experiment: building a dense matrix and converting it to sparse before matrix
# multiplying does not speed up the multiply operation; if we want to get a
# speed up from sparse matrix operations, we will have to avoid constructing the
# dense matrix in the first place. This should be doable, since each nonzero
# component is created from a row vector, and the templates of where to put
# those values can be computed once, in advance.

# drop dimnames as they confuse the checking
transition_matrix_nude <- transition_matrix
dimnames(transition_matrix_nude) <- NULL

library(spam)
library(Matrix)

# build a sparseMatrix from the dense, given the known nonzero elements (will remain the same throughout)
sparseMatrix_method <- function(dense_matrix, vector) {
  indices <- which(dense_matrix != 0, arr.ind = TRUE)
  values <- dense_matrix[indices]
  sparse_matrix <- sparseMatrix(i = indices[, 1], j = indices[, 2], x = values)
  output <- as.matrix(sparse_matrix %*% vector)
  dimnames(output) <- NULL
  output
}
  
bench::mark(
  transition_matrix_nude %*% current_state_prop,
  spam::as.spam(transition_matrix_nude) %*% current_state_prop,
  sparseMatrix_method(transition_matrix_nude, current_state_prop)
)

dim(transition_matrix_nude)


# convert to a sparse matrix and iterate



# to do:

# Gerry
# programmatically create inheritance matrices for multiple different types of
# vaccination, infection, and combinations thereof

# August
# calculate the relative probabilities of vaccination from AIR





# need to construct these matrices for different types of immunity

n_waning_states <- 26 # 6 months of waning, then plateau

# get combinations of immune events. Names are types of immune event, numbers
# are the possible numbers of those events that could have occurred for an
# individual (except infections, where we don't distinguish multiple infections)
states <- expand_grid(
  subunit_monovalent = 0:2, # (astrazeneca no used as booster)
  mrna_monovalent = 0:5, # (mrna available since first, and up to maximum doses)
  mrna_bivalent = 0:1, # (mrna bivalent only available for 1 round so far)
  omicron_infection = 0:1, # we only count infections once
  non_omicron_infection = 0:1 # we only count infections once
) %>%
  # now map to how we encode these immunity types
  mutate(
    vaccine_doses = subunit_monovalent + mrna_monovalent + mrna_bivalent,
    dominant_vaccine = case_when(
      mrna_bivalent > 0 ~ "mrna_bivalent",
      mrna_monovalent > 0 ~ "mrna_monovalent",
      subunit_monovalent > 0 ~ "subunit_monovalent",
      .default = "unvaccinated" 
    ),
    dose_count = case_when(
      vaccine_doses >= 3 ~ "boosted",
      vaccine_doses == 2 ~ "full",
      vaccine_doses == 1 ~ "single",
      .default = ""
    ),
    dominant_infection = case_when(
      omicron_infection > 0 ~ "omicron",
      non_omicron_infection > 0 ~ "non_omicron",
      .default = "uninfected"
    ),
    vaccine_state = paste(
      dominant_vaccine,
      dose_count,
      sep = "_"
    ),
    vaccine_state = gsub("_$", "", vaccine_state),
    immunity_type = paste(
      vaccine_state,
      dominant_infection,
      sep = "/"
    )
  ) %>%
  select(immunity_type) %>%
  distinct() %>%
  expand_grid(
    waning = c(0, seq_len(n_waning_states))
  ) %>%
  # no waning of nonimmunity
  filter(
    !(immunity_type == "unvaccinated/uninfected" & waning > 0)
  ) %>%
  mutate(
    waning = paste(
      waning,
      "weeks",
      sep = "_"
    ),
    immunity_state = paste(
      immunity_type,
      waning,
      sep = "/"
    ),
    immunity_state = case_when(
      immunity_state == "unvaccinated/uninfected/0_weeks" ~ "nonimmune",
      .default = immunity_state
    )
  ) %>%
  pull(immunity_state)

# can name these better!
length(states)
head(states)
tail(states)


# now, need to set up transitions between types of immunity


# set up fake population
n_states <- length(states)

n_population <- 1e6

# proportion of the population in each state
current_state_prop <- c(50, runif(n_states - 1))
current_state_prop <- current_state_prop / sum(current_state_prop)

# relative probability of being infected: e.g. RR of infection for each immunity type and level of waning, from VEs
# should remain the same at each iteration since it's just based on the immunity model
relative_probability_I <- runif(n_states)

# relative probability of being vaccinated: e.g. from number of new vaccinations
# and which groups is vaccinated. For all I-only states, should be the same as
# susceptibles (the unvaccinated), for IV states should be the same as for V
# states (with same waning) - should change at each iteration based on the AIR
# data
relative_probability_V <- runif(n_states)

# total number of new infections and total number of new vaccinations
n_new_vaccinations <- 2e4
n_new_infections <- 1e4

transition_matrix <- create_transition_matrix(
  current_state_prop = current_state_prop,
  relative_probability_V = relative_probability_V,
  relative_probability_I = relative_probability_I,
  n_new_vaccinations = n_new_vaccinations,
  n_new_infections = n_new_infections,
  n_population = 1e5,
  n_waning_states = n_waning_states,
  # manually-specified matrices of the inheritance structure between the four
  # immunity types, for individuals being vaccinated, infected, and both
  # vaccinated and infected
  immunity_inheritance_vaccination = get_immunity_inheritance("vaccination"),
  immunity_inheritance_infection = get_immunity_inheritance("infection"),
  immunity_inheritance_vaccination_and_infection = get_immunity_inheritance("vaccination_and_infection")
)
