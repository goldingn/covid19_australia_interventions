# discrete probability distribution over days to case detection, for infections on this date
delay_prob <- function(date, state) {
  days <- seq_len(101) - 1 
  survival <- ttd_survival(
    days,
    rep(date, length(days)),
    target_state = state
  )
  diff(c(0, 1 - survival))
}
