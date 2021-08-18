# plot apparent duplication of records in doh surveys by wave and state to spot
# bot entries
plot_age_duplication <- function(doh_surveys, max_fraction = 0.12) {
  doh_surveys %>%
    # remove any state with fewer than 50 respondents per week, on average
    group_by(state, wave) %>%
    mutate(respondents = n()) %>%
    group_by(state) %>%
    mutate(mean_respondents = mean(respondents)) %>%
    filter(mean_respondents > 50) %>%
    # count fraction of respondents by age in each wave/state
    group_by(wave, state, age) %>%
    count() %>%
    group_by(state, wave) %>%
    mutate(
      fraction = n / sum(n)
    ) %>%
    ungroup() %>%
    select(-n) %>%
    complete(
      wave, state, age,
      fill = list(fraction = 0)
    ) %>%
    ggplot() +
    aes(state, age, fill = fraction) +
    geom_tile() +
    facet_wrap(~wave) +
    scale_fill_viridis_c(
      na.value = grey(0.6),
      limits = c(0, max_fraction)
    ) +
    theme_minimal()
}
