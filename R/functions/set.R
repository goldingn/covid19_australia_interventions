# change the policy in a config
set <- function(config, which, new_value) {
  config[[which]] <- new_value
  config
}
