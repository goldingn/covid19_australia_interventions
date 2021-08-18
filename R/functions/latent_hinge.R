# a hinge effect, 0 before the inflection point, then linear to 1 at the most recent period
latent_hinge <- function(inflection_dates, date_num) {
  last_date_num <- max(date_num)
  effect_periods <- last_date_num - inflection_dates
  time_remaining <- date_num - last_date_num
  distancing_effects <- 1 + kronecker(time_remaining, effect_periods, FUN = "/")
  nullify <- (sign(distancing_effects) + 1) / 2
  distancing_effects * nullify
}
