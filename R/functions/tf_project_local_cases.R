# tensorflow function to project the expected number of locally-acquired cases
# into the future
tf_project_local_cases <- function(infectiousness, R_local, disaggregation_probabilities, T, K) {
  
  # continuing condition of TF while loop
  cond <- function(C, I, R, p, t, T, K, sequence) {
    tf$less(t, T)
  }
  
  # body of TF while loop
  body <- function(C, I, R, p, t, T, K, sequence) {
    
    # increase the expected infectiousness on subsequent days due to cases
    # infected on this day
    new_C <- R[, t, ] * I[, t, ]
    new_C <- tf$expand_dims(new_C, 1L)
    
    # distribute infectiousness of these cases across their infectious profile
    new_I <- new_C * p
    
    # add to cases and cumulative infectiousness
    perm_to <- c(1L, 2L, 0L)
    perm_from <- c(2L, 0L, 1L)
    
    I_t <- tf$transpose(I, perm_to)
    new_I_t <- tf$transpose(new_I, perm_to)
    I_t <- tf$tensor_scatter_nd_add(I_t,
                                    indices = t + sequence,
                                    updates = new_I_t)
    I <- tf$transpose(I_t, perm_from)
    
    C_t <- tf$transpose(C, perm_to)
    new_C_t <- tf$transpose(new_C, perm_to)
    C_t <- tf$tensor_scatter_nd_add(C_t,
                                    indices = tf$reshape(t, shape = c(1L, 1L)),
                                    updates = new_C_t)
    C <- tf$transpose(C_t, perm_from)
    
    list(C, I, R, p, t + 1L, T, K, sequence)
    
  }
  
  # pad I and R with K zeros so we don't need to mess with indexing inside the loop
  batch_size <- greta:::get_batch_size()
  n_locations <- dim(infectiousness)[[3]]
  pad <- tf$zeros(
    shape = tf$stack(list(batch_size, K + 1L, n_locations)),
    dtype = greta:::tf_float()
  )
  I <- tf$concat(list(infectiousness, pad), axis = 1L)
  R <- tf$concat(list(R_local, pad), axis = 1L)
  C <- tf$zeros(
    shape = tf$stack(list(batch_size, T, n_locations)),
    dtype = greta:::tf_float()
  )
  
  # initial variables for loop
  values <- list(
    C,
    I,
    R,
    disaggregation_probabilities,
    0L,
    T,
    K,
    as.matrix((1:K) - 1L)
  )
  
  # iterate to compute infectiousness
  result <- tf$while_loop(cond, body, values)
  
  # return expected numbers of new local cases
  result[[1]]
  
}
