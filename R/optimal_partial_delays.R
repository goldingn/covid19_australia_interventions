
delays_optimal <- delay_data_from_onset %>%
  filter(
    date_confirmation >= "2020-07-01",
    date_confirmation <= "2021-01-31",
    state == "NSW"
  ) %>%
  pull(delay)

mean(delays_optimal)
sd(delays_optimal)

tibble(
  delay = delays_optimal + 5,
  type = "empirical"
) %>% 
  bind_rows(
    tibble(
      delay = rlnorm(1000, meanlog = log(7), sdlog = log(1.6)) %>% round,
      type = "simulated"
    )
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = delay
    )
  ) +
  facet_grid(
    rows = vars(type),
    scales = "free_y"
  )


delays_partial <- delay_data_from_onset %>%
  filter(
    date_confirmation == "2020-08-04",
    state == "VIC"
  ) %>%
  pull(delay)

mean(delays_partial)
sd(delays_partial)

tibble(
  delay = delays_partial + 5,
  type = "empirical"
) %>% 
  bind_rows(
    tibble(
      delay = rlnorm(1000, meanlog = log(8), sdlog = log(1.8)) %>% round,
      type = "simulated"
    )
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = delay
    )
  ) +
  facet_grid(
    rows = vars(type),
    scales = "free_y"
  )
