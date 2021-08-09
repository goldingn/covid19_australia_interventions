log_imultilogit <- function(x) {
  
  dim <- dim(x)
  
  # check it's a matrix
  if (length(dim) != 2) {
    stop("log_imultilogit expects a 2D greta array",
         call. = FALSE)
  }
  
  op("log_imultilogit", x,
     dim = dim + c(0, 1),
     tf_operation = "tf_log_imultilogit")
}
