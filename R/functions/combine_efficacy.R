# compute the vaccine efficacy on overall transmission by combining the 
# efficacies in preventing iinfection, and prevensting onwards transmission
# for breakthrough infections
combine_efficacy <- function(infection, transmission) {
  1 - ((1 - infection) * (1 - transmission)) 
}
