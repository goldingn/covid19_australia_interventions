tf_log_imultilogit <- function(x) {
  batch_size <- tf$shape(x)[[0]]
  shape <- c(batch_size, dim(x)[[2]], 1L)
  zeros <- tf$zeros(shape, tf_float())
  latent <- tf$concat(list(x, zeros), 2L)
  tf$nn$log_softmax(latent)
}
