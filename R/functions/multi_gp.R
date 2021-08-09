# copy of greta.gp::gp with v manually passed in, enabling different
# (hierarchical) variance on each gp draw
multi_gp <- function (x, v, kernel, inducing = NULL, tol = 1e-04) {
  
  sparse <- !is.null(inducing)
  
  x <- as.greta_array(x)
  
  if (!sparse)
    inducing <- x
  else
    inducing <- as.greta_array(inducing)
  
  # calculate key objects
  Kmm <- kernel(inducing)
  
  m <- nrow(v)
  
  if (!identical(tol, 0))
    Kmm <- Kmm + diag(m) * tol
  
  Lm <- t(chol(Kmm))
  
  # evaluate gp at x
  if (sparse) {
    
    Kmn <- kernel(inducing, x)
    A <- forwardsolve(Lm, Kmn)
    f <- t(A) %*% v
    
  } else {
    
    f <- Lm %*% v
    
  }
  
  # add the info to the greta array
  attr(f, "gp_info") <- list(kernel = kernel,
                             inducing = inducing,
                             v = v,
                             Lm = Lm)
  f
}
