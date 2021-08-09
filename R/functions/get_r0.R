# extract R0 from a summarised trajectory of TP values, by taking TP at the
# baseline time period (limited surveillance effect, but no behavioural change),
# and dividing out the surveillance effect, and the extra isolation effect, if
# that was used to calculate this TP trajectory
get_r0 <- function(sample_summary, use_extra_effect = TRUE) {
  
  start <- sample_summary %>%
    filter(
      state == "ACT",
      date == as_date("2020-01-08")
    )

  start %>%
    mutate(
      surveillance = surveillance_effect(
        dates = start$date,
        state = start$state,
        cdf = gi_cdf
      )[1, 1],
      extra_isolation = extra_isolation_effect(
        dates = start$date,
        state = start$state,
        cdf = gi_cdf
      )[1, 1],
      correction = surveillance * extra_isolation ^ use_extra_effect,
      tp_corrected = tp / correction
    ) %>%
    pull(
      tp_corrected
    )
}
